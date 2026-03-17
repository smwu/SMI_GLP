# ====================================================================
# Clean Glucose values
# Author: SM Wu
# Date Created: 2026/02/02
# Date Updated: 2026/02/02
# 
# Details:
# 
# Inputs:
# 
# Outputs:
# 
# =====================================================================

# Clear memory
rm(list = ls())
gc()

library(DBI)
library(duckdb)
library(dbplyr)
library(dplyr)
library(data.table)
library(tidylog)

# ### For running in Data Safe Haven
# Set working directory
wd <- "S:/CDSTP_CPRD_25_005368/"
setwd(wd)

# Set input and output paths
path_gold <- "GOLD/"
path_aurum <- c("Aurum_1/", "Aurum_2/", "Aurum_3/")
path_input <- paste0("SMI_GLP/Data/Extraction_Files/Glucose/")
path_output <- paste0("SMI_GLP/Data/Cleaning_Files/")


# DuckDB in-memory connection
connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# Allow spilling to local disk after hitting memory limit
DBI::dbExecute(connection, "PRAGMA memory_limit = '35GB';")
spill_dir <- "N:/Temp/duckdb_spill"
dir.create(spill_dir, showWarnings = FALSE, recursive = TRUE)
DBI::dbExecute(connection, sprintf("PRAGMA temp_directory='%s';", spill_dir))

# Set up progress bar
DBI::dbExecute(connection, "SET enable_progress_bar = true;")
DBI::dbExecute(connection, "SET enable_progress_bar_print = true;")
# Start timer
start_time <- Sys.time()


# Read in datasets
# Memory: 12 GB
lab_path <- paste0(path_input, "pat_comb_final.parquet")
lab_data <- DBI::dbGetQuery(connection, sprintf(
  "SELECT * FROM read_parquet('%s');", 
  lab_path))

# Read in patient files
# Gold. Memory: 1 GB
gold_pat <- read.table(
  file = paste0(path_gold, "Patient/25_005368_SW_Extract_Patient_001.txt"),
  header = TRUE, fill = TRUE, sep = "\t", quote = "", 
  colClasses = c(patid = "character"))

# Aurum. Memory: 1 GB
aurum_pat <- list() # patient
# aurum_prac_cmd <- list() # practice
for (i in 1:length(path_aurum)) {
  aurum_pat[[i]] <- read.table(
    file = paste0(path_aurum[i], "Patient/25_005368_SW_Extract_Patient_001.txt"),
    header = TRUE, fill = TRUE, sep = "\t", quote = "", 
    colClasses = c(patid = "character"))
}
aurum_pat_all <- do.call(rbind, aurum_pat)

pat_all <- rbind(gold_pat %>% 
                   rename(regstartdate = crd,
                          regenddate = tod) %>%
                   select(patid, gender, yob, regstartdate, 
                          regenddate, deathdate) %>%
                   mutate(patid = paste0(patid, "-G")), 
                 aurum_pat_all %>% 
                   rename(deathdate = cprd_ddate) %>%
                   select(patid, gender, yob, regstartdate, 
                          regenddate, deathdate) %>%
                   mutate(patid = paste0(patid, "-A")))

rm(aurum_pat, aurum_pat_all, gold_pat)

summary(as.numeric(lab_data$value))



# ======= Remove NA and values before birth or after death =============

lab_data_clean <- lab_data %>%
  # Join in patient variables: 70,891,843 records
  inner_join(pat_all %>% select(patid, yob, deathdate), by = "patid") %>%
  # Convert to dates
  mutate(eventdate = as.Date(eventdate, "%Y-%m-%d"),
         event_year = year(eventdate),
         deathdate = as.Date(deathdate, "%d/%m/%Y")) %>%
  # Remove values in or prior to birth year: removed 2202 (<1%)
  filter(event_year > yob) %>%
  # Remove values after death date: Removed 10,650 (<1%)
  filter(deathdate > eventdate | is.na(deathdate)) %>%
  select(-yob, -deathdate, -event_year) %>%
  # Drop duplications: dropped 853,439 (1%)
  distinct(patid, eventdate, value, term, .keep_all = TRUE) %>%
  # Set NA unit: changed 1,836,189 (3%)
  mutate(unit = case_when(
    unit == "No Data Entered" ~ NA_character_,
    grepl("unknown", unit, ignore.case = TRUE) ~ NA_character_, 
    TRUE ~ unit)) %>%
  # Drop NA values: removed 35,580,014 (56%). 27,920,715 rows remaining
  filter(!is.na(value)) %>%
  distinct()

sort(table(lab_data_clean$unit), decreasing = TRUE)
rm(lab_data)
gc()


#============= Fix values and units ============================

# Remove measurements relating to time and frequency and identify type of measurement
# Removed 178,511 (1%) rows
lab_data_clean2 <- lab_data_clean %>%
  filter(term != "(?i)frequency of blood glucose self-monitoring") %>%
  filter(!grepl("diabetes mellitus screening risk score", term)) %>%
  filter(!grepl("(?i)times|week|day|daily|hour|hrs|minute|year", unit)) %>%
  # Identify type of measurement. Set as fasting if mentioned in term or unit
  mutate(
    type = case_when(
      grepl("(?i)fasting", term) ~ "fasting",
      grepl("(?i)fasting", unit) ~ "fasting",
      TRUE ~ "random"))

# Fix recorded values
lab_data_clean3 <- lab_data_clean2 %>%
  # Infer units where missing or unreliable
  mutate(
    value = as.numeric(value),
    # Check if explicit unit is already reliable
    reliable_unit = case_when(
      grepl("(?i)mg", unit) ~ "mg/dL",
      grepl("(?i)mg", term) & is.na(unit) ~ "mg/dL",
      grepl("%|percent", unit) ~ "mg/dL",  # assume mg% = mg/100mL = mg/dL
      grepl("(?i)%", term) & is.na(unit) ~ "mg/dL",
      grepl("(?i)mol", unit) ~ "mmol/L",
      grepl("(?i)mol", term) & is.na(unit) ~ "mmol/L",
      TRUE ~ NA_character_ ),
    # If explicit unit is already reliable, keep it
    inferred_unit = case_when(
      grepl("(?i)mg", unit) ~ "mg/dL",
      grepl("(?i)mg", term) & is.na(unit) ~ "mg/dL",
      grepl("%|percent", unit) ~ "mg/dL",  # assume mg% = mg/100mL = mg/dL
      grepl("(?i)%", term) & is.na(unit) ~ "mg/dL",

      grepl("(?i)mol", unit) ~ "mmol/L",
      grepl("(?i)mol", term) & is.na(unit) ~ "mmol/L",
      
      # Infer missing unit from number (<35 is mmol/L, >35 is mg/dL)
      is.na(unit) & value < 35 ~ "mmol/L",
      is.na(unit) & value >= 35 ~ "mg/dL",
      TRUE ~ NA_character_
      )) %>%
  # Fill in inferred units based on values
  mutate(
    inferred_unit = case_when(
      is.na(inferred_unit) & value < 35 ~ "mmol/L",
      is.na(inferred_unit) & value >= 35 ~ "mg/dL",
      TRUE ~ inferred_unit
    )) %>%
  # Flag when unit and number don't align
  mutate(
    unit_value_flag = case_when(
      inferred_unit == "mg/dL" & value < 35 ~ "possible mmol/L",
      inferred_unit == "mmol/L" & value >= 35 ~ "possible mg/dL",
      is.na(inferred_unit) ~ NA_character_,
      TRUE ~ "okay"
    )) %>%
  # Convert everything to mmol/L
  mutate(glucose_mmol = case_when(
    inferred_unit == "mmol/L" ~ value,
    inferred_unit == "mg/dL" ~ round(value * 0.0555, 1), 
    TRUE ~ NA_real_
  )) %>%
  # Flag extreme values
  mutate(extreme = case_when(
    glucose_mmol > 35 ~ "high",
    glucose_mmol < 2 ~ "low", 
    is.na(glucose_mmol) ~ NA_character_,
    TRUE ~ "plausible"
  ))

# Number with unreliable units: 2,110,451 (8%)
table(lab_data_clean3$reliable_unit, useNA = "always")

# Number of NA inferred units: 0
table(lab_data_clean3$inferred_unit, useNA = "always")

# Number with potentially incorrect units: 25,603+58,317 = 83928
table(lab_data_clean3$unit_value_flag, useNA = "always")

# Number with extreme values: 27,219+703,173 = 730,392, 0 NAs
table(lab_data_clean3$extreme, useNA = "always")

### Check NAs and extremes
check_na <- lab_data_clean3 %>%
  filter(is.na(extreme))
check_extreme <- lab_data_clean3 %>%
  filter(extreme %in% c("high", "low"))
View(check_extreme)

rm(lab_data_clean, lab_data_clean2)


# Create variable for elevated glucose
lab_data_clean4 <- lab_data_clean3 %>%
  # Create variable for elevated glucose: >= 7 mmol/L fasting, >= 11.1 mmol/L random
  mutate(elevated_glucose = case_when(
    type == "fasting" & glucose_mmol >= 7 ~ 1,
    type == "random" & glucose_mmol >= 11.1 ~ 1,
    is.na(inferred_unit) ~ NA,
    TRUE ~ 0
  ))

rm(lab_data_clean3)

# Apply hard measurement cutoffs: mmol/mol must be between 0-417
# Removed 675,492 (2%). 27,066,712 remaining
glucose_clean <- lab_data_clean4 %>%
  filter(glucose_mmol >= 0.6 & glucose_mmol <= 45)

# Number of patients: 3,107,122
length(unique(glucose_clean$patid))

summary(glucose_clean$glucose_mmol)
hist(glucose_clean$glucose_mmol, breaks = 30)

# Save cleaned hba1c data
save(glucose_clean, file = paste0(wd, path_output, "glucose_clean.RData"))

rm(lab_data_clean4, check_extreme)
gc()

#==================== Handle multiple values a day =============================

# # Memory: 9 GB
# load(paste0(wd, path_output, "glucose_clean.RData"))

# Convert dataframe of elevated values to data table
DT <- as.data.table(glucose_clean)

# Stable row ID so we can keep first occurrence in original order
DT[, rowid := .I]

# De-duplicate exact repeats within (patid, eventdate, glucose_mmol)
# 212,965 removed. 26,853,747 records remaining
DT <- DT[!duplicated(DT, by = c("patid", "eventdate", "glucose_mmol"))]

# Compute days with multiple measurements and get min/max measurements
DT[, `:=`(
  n_day = .N,
  min_mmol = min(glucose_mmol, na.rm = TRUE),
  max_mmol = max(glucose_mmol, na.rm = TRUE)
), by = .(patid, eventdate)]

# Number of records with n_day > 1: 1,606,295
DT[n_day > 1, .N]
# Number of patid-eventdate pairs with multiple measurements: 760,281
DT[n_day > 1, .N, by = .(patid, eventdate)][, .N]

# Where there are multiple measurements, restrict to rows based on plausible values
# If there are values above and below 2 mmol/L, drop any <2: 5595 dropped
DT <- DT[!(n_day > 1L & min_mmol < 2 & max_mmol >= 2 & glucose_mmol < 2)] 
# If there are values above and below 35 mmol/L, drop any >35: 987 records dropped. 26,847,165 remaining
DT <- DT[!(n_day > 1L & min_mmol <= 35 & max_mmol > 35 & glucose_mmol > 35)]

# Recompute multiple counts after filtering
DT[, n_day := .N, by = .(patid, eventdate)]

gc()

# Where there are still multiple measurements, select the one with the most reliable unit
DT[, unit_rank := fifelse(!is.na(reliable_unit) & reliable_unit == "mmol/L" & inferred_unit == "mmol/L", 1,
                          fifelse(!is.na(reliable_unit) & reliable_unit == "mg/dL" & inferred_unit == "mg/dL", 2,
                                  fifelse(inferred_unit == "mmol/L", 3, 
                                          fifelse(inferred_unit == "mg/dL", 4, 5))))]

# Distribution of ranks
# 1: 25,290,404. 2: 24,936. 3: 1,525,349. 4: 6476
DT[, .N, by = .(unit_rank)]

gc()

# Keep only the best-ranked rows per (patid, eventdate)
# 30,677 dropped. 26,816,488 remaining
DT[, best_rank := min(unit_rank), by = .(patid, eventdate)]
DT <- DT[unit_rank == best_rank]
# Distribution of ranks
# 1: 25,290,404. 2: 24,313. 3: 1,498,620. 4: 3151
DT[, .N, by = .(unit_rank)]

# Recompute multiple counts after filtering
DT[, n_day := .N, by = .(patid, eventdate)]
# Number of records with n_day > 1: 1,537,948
DT[n_day > 1, .N]
# Number of patid-eventdate pairs with multiple measurements: 729,190
DT[n_day > 1, .N, by = .(patid, eventdate)][, .N]

gc()

# If there are still multiple at that rank, keep the first entry: 808,758
# Note: 'SD[1]' returns the first row of each group
DT[, row_id := .I]
setorder(DT, patid, eventdate, row_id)
unique_DT <- DT[, .SD[1], by = .(patid, eventdate)]

# Drop unnecessary columns
unique_DT[, c("rowid", "n_day", "unit_rank", "row_id") := NULL]

# Final: 26,007,730 records from 3,107,122 patients
unique_DT[, .N, by = patid][, .N]
rm(DT)

# Convert back to dataframe
glucose_clean_unique <- as.data.frame(unique_DT)

table(glucose_clean_unique$database)

glucose_clean_unique <- glucose_clean_unique %>%
  select(medcode, term, patid, eventdate, database, source, inferred_unit, glucose_mmol, 
         value, unit, reliable_unit, extreme, min_mmol, max_mmol, best_rank)

# 3,107,122 patients with elevated records
length(unique(glucose_clean_unique$patid))
length(unique(glucose_clean_unique$patid[glucose_clean_unique$database == "Gold"]))
length(unique(glucose_clean_unique$patid[glucose_clean_unique$database == "Aurum"]))

# Save cleaned glucose data filtered to one unique measurement per day
save(glucose_clean_unique, file = paste0(wd, path_output, "glucose_clean_unique.RData"))

#=================== Filter to records with elevated glucose ==================

# 7,638,178 elevated records
glucose_elevated <- glucose_clean %>%
  filter(elevated_glucose == 1)
table(glucose_elevated$database)

glucose_elevated <- glucose_elevated %>%
  select(medcode, term, patid, eventdate, database, source, value, unit, inferred_unit, glucose_mmol)

# 1,812,178 patients with elevated records
length(unique(glucose_elevated$patid))
length(unique(glucose_elevated$patid[glucose_elevated$database == "Gold"]))
length(unique(glucose_elevated$patid[glucose_elevated$database == "Aurum"]))

# Save cleaned elevated glucose data
save(glucose_elevated, file = paste0(wd, path_output, "glucose_clean_elevated.RData"))


### Remove data and shut down connection
dbDisconnect(connection, shutdown = TRUE)


#=============== Miscellaneous old code ========================================

# # Get number of measurements per patient-day
# glucose_count <- glucose_clean %>%
#   select(-c(sysdate, constype, consid, source, data2, enttype_description, unit_value_flag, extreme)) %>%
#   distinct(patid, eventdate, glucose_mmol, .keep_all = TRUE) %>%  # remove duplicates: 212,965
#   group_by(patid, eventdate) %>%
#   mutate(num_glucose = n()) %>%
#   ungroup()
# 
# # 1,606,295 multiple rows
# table(glucose_count$num_glucose)
# 
# multiples <- glucose_count %>% 
#   filter(num_glucose > 1)


