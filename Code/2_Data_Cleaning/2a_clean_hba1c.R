# ====================================================================
# Clean HbA1c values
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
path_input <- paste0("SMI_GLP/Data/Extraction_Files/HbA1c/")
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
# lab_path <- dbQuoteString(connection, paste0(path_input, "pat_comb_final.parquet"))
# # Load into database, then connect so can use with dbplyr
# DBI::dbExecute(connection, sprintf("
#   CREATE VIEW lab_data AS
#   SELECT * FROM read_parquet(%s)",
#   lab_path
# ))
# lab_data <- tbl(connection, "lab_data") 
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
  # Join in patient variables: 71,416,350 records
  inner_join(pat_all %>% select(patid, yob, deathdate), by = "patid") %>%
  # Convert to dates
  mutate(eventdate = as.Date(eventdate, "%Y-%m-%d"),
         event_year = year(eventdate),
         deathdate = as.Date(deathdate, "%d/%m/%Y")) %>%
  # Remove values in or prior to birth year: removed 1259 (<1%)
  filter(event_year > yob) %>%
  # Remove values after death date: Removed 8868 (<1%)
  filter(deathdate > eventdate | is.na(deathdate)) %>%
  select(-yob, -deathdate, -event_year) %>%
  # Drop duplications: dropped 1,174,368 (2%)
  distinct(patid, eventdate, value, term, .keep_all = TRUE) %>%
  # Set NA unit: changed 2,424,528 (3%)
  mutate(unit = case_when(
    unit == "No Data Entered" ~ NA,
    grepl("unknown", unit, ignore.case = TRUE) ~ NA, 
    TRUE ~ unit)) %>%
  # Drop NA values: removed 12,589,281 (18%). 57,642,574 rows remaining
  filter(!is.na(value)) %>%
  distinct()

sort(table(lab_data_clean$unit), decreasing = TRUE)
rm(lab_data, pat_all)
gc()


#============= Fix values and units ============================
  
# Fix recorded values
# Plausible % range: 2.5-20
# Plausible mmol/mol range: 20-250
# Source: 
lab_data_clean2 <- lab_data_clean %>%
  # Infer units where missing or unreliable
  mutate(
    value = as.numeric(value),
    # Check if explicit unit is already reliable
    reliable_unit = case_when(
      unit %in% c("percent", "per cent", "%") ~ "%",
      grepl("%", unit) ~ "%",
      grepl("(?i)%", term) & is.na(unit) ~ "%",
      grepl("(?i)DCCT|NGSP", term) & is.na(unit) ~ "%",
      
      grepl("(?i)mol", unit) ~ "mmol/mol",
      grepl("(?i)ifcc|federation", unit) ~ "mmol/mol",
      grepl("(?i)ifcc|federation", term) & is.na(unit) ~ "mmol/mol",
      TRUE ~ NA_character_ ),
    
    # If explicit unit is already reliable, keep it
    inferred_unit = case_when(
      unit %in% c("percent", "per cent", "%") ~ "%",
      grepl("%", unit) ~ "%",
      grepl("(?i)%", term) & is.na(unit) ~ "%",
      grepl("(?i)DCCT|NGSP", term) & is.na(unit) ~ "%",
      
      grepl("(?i)mol", unit) ~ "mmol/mol",
      grepl("(?i)ifcc|federation", unit) ~ "mmol/mol",
      grepl("(?i)ifcc|federation", term) & is.na(unit) ~ "mmol/mol",
      
      # Infer missing unit from number (<20 is %, >20 is mol)
      is.na(unit) & value < 20 ~ "%",
      is.na(unit) & value >= 20 ~ "mmol/mol",
      TRUE ~ NA_character_
    )) %>%
  # Fill in inferred units based on values
  mutate(
    inferred_unit = case_when(
      is.na(inferred_unit) & value < 20 ~ "%",
      is.na(inferred_unit) & value >= 20 ~ "mmol/mol",
      TRUE ~ inferred_unit
    )) %>%
  # Flag when unit and number don't align
  mutate(
    unit_value_flag = case_when(
      value < 2 ~ "small value",
      inferred_unit == "mmol/mol" & value < 20 ~ "possible %",
      inferred_unit == "%" & value >= 20 ~ "possible mmol/mol",
      is.na(inferred_unit) ~ NA_character_,
      TRUE ~ "okay"
    )) %>%
  # Convert everything to IFCC mmol/mol
  mutate(hba1c_mmol = case_when(
    inferred_unit == "mmol/mol" ~ value,
    inferred_unit == "%" ~ round((value - 2.152) / 0.09148, 3), # from NGSP->IFCC
    TRUE ~ NA_real_
  )) %>%
  # Flag extreme values
  mutate(extreme = case_when(
    hba1c_mmol > 140 ~ "high",
    hba1c_mmol < 20 ~ "low", 
    is.na(hba1c_mmol) ~ NA_character_,
    TRUE ~ "plausible"
  ))

# Number with unreliable units: 2,347,117
table(lab_data_clean2$reliable_unit, useNA = "always")

# Number of NA inferred units: 0
table(lab_data_clean2$inferred_unit, useNA = "always")

# Number with potentially incorrect units: 266333+87020+514682 = 868035
table(lab_data_clean2$unit_value_flag, useNA = "always")

# Number with extreme values: 190280+791298 = 981,578, 0 NAs
table(lab_data_clean2$extreme, useNA = "always")

### Check NAs and extremes
check_na <- lab_data_clean2 %>%
  filter(is.na(extreme))
check_extreme <- lab_data_clean2 %>%
  filter(extreme %in% c("high", "low"))
View(check_extreme)

rm(lab_data_clean)

lab_data_clean3 <- lab_data_clean2 %>%
  # Create variable for elevated HbA1c: >= 48 mmol/mol or >= 6.5%
  mutate(elevated_hba1c = case_when(
    inferred_unit == "%" & value >= 6.5 ~ 1,
    inferred_unit == "mmol/mol" & value >= 48 ~ 1,
    is.na(inferred_unit) ~ NA,
    TRUE ~ 0
  ))

rm(lab_data_clean2)

# Apply hard measurement cutoffs: mmol/mol must be between 15-200
# Removed 604,695 (1%). 57,120,079 remaining
hba1c_clean <- lab_data_clean3 %>%
  filter(hba1c_mmol >= 4 & hba1c_mmol <= 250)

# Number of patients: 3,505,817
length(unique(hba1c_clean$patid))

summary(hba1c_clean$hba1c_mmol)
hist(hba1c_clean$hba1c_mmol, breaks = 30)

# Save cleaned hba1c data
save(hba1c_clean, file = paste0(wd, path_output, "hba1c_clean.RData"))

rm(lab_data_clean3, check_extreme)
gc()

#==================== Handle multiple values a day =============================

# # Memory: 13.5 GB
# load(paste0(wd, path_output, "hba1c_clean.RData"))

# Convert dataframe of elevated values to data table
DT <- as.data.table(hba1c_clean)

# Stable row ID so we can keep first occurrence in original order
DT[, rowid := .I]

# De-duplicate exact repeats within (patid, eventdate, hba1c_mmol)
# 283,894 removed. 56,753,985 records remaining
DT <- DT[!duplicated(DT, by = c("patid", "eventdate", "hba1c_mmol"))]

# Compute days with multiple measurements and get min/max measurements
DT[, `:=`(
  n_day = .N,
  min_mmol = min(hba1c_mmol, na.rm = TRUE),
  max_mmol = max(hba1c_mmol, na.rm = TRUE)
), by = .(patid, eventdate)]

# Number of records with n_day > 1: 15,986,765
DT[n_day > 1, .N]
# Number of patid-eventdate pairs with multiple measurements: 7,963,734
DT[n_day > 1, .N, by = .(patid, eventdate)][, .N]

# Where there are multiple measurements, restrict to rows based on plausible values
# If there are values above and below 20 mmol/mol, drop any <20: 0 dropped
DT <- DT[!(n_day > 1L & min_mmol < 20 & max_mmol >= 20 & hba1c_mmol < 20)] 
# If there are values above and below 140 mmol/mol, drop any >140: 92,181 records dropped. 56,661,804 remaining
DT <- DT[!(n_day > 1L & min_mmol <= 140 & max_mmol > 140 & hba1c_mmol > 140)]

# Recompute multiple counts after filtering
DT[, n_day := .N, by = .(patid, eventdate)]

gc()

# Where there are still multiple measurements, select the one with the most reliable unit
DT[, unit_rank := fifelse(!is.na(reliable_unit) & reliable_unit == "mmol/mol" & inferred_unit == "mmol/mol", 1,
                          fifelse(!is.na(reliable_unit) & reliable_unit == "%" & inferred_unit == "%", 2,
                                  fifelse(inferred_unit == "mmol/mol", 3, 
                                          fifelse(inferred_unit == "%", 4, 5))))]

# Distribution of ranks
# 1: 35,000,523. 2: 19,695,492. 3: 274,979. 4: 1,690,810
DT[, .N, by = .(unit_rank)]

gc()

# Keep only the best-ranked rows per (patid, eventdate)
# 7,816,754 dropped. 48,845,050 remaining
DT[, best_rank := min(unit_rank), by = .(patid, eventdate)]
DT <- DT[unit_rank == best_rank]
# Distribution of ranks
# 1: 35,000,523. 2: 12,234,732. 3: 21,434. 4: 1,588,361
DT[, .N, by = .(unit_rank)]

# Recompute multiple counts after filtering
DT[, n_day := .N, by = .(patid, eventdate)]
# Number of records with n_day > 1: 227,472
DT[n_day > 1, .N]
# Number of patid-eventdate pairs with multiple measurements: 113,148
DT[n_day > 1, .N, by = .(patid, eventdate)][, .N]

gc()

# If there are still multiple at that rank, keep the first entry
# Note: 'SD[1]' returns the first row of each group
DT[, row_id := .I]
setorder(DT, patid, eventdate, row_id)
unique_DT <- DT[, .SD[1], by = .(patid, eventdate)]

# Drop unnecessary columns
unique_DT[, c("rowid", "n_day", "unit_rank", "row_id") := NULL]

    # # Old code that averaged across multiples
    # unique_DT <- DT[, .(
    #   hba1c_mmol_mean = mean(hba1c_mmol, na.rm = TRUE),
    #   n_used = .N
    # ), by = .(patid, eventdate)]
# Final: 48,730,726 records from 3,505,817 patients
unique_DT[, .N, by = patid][, .N]
rm(DT)

# Convert back to dataframe
hba1c_clean_unique <- as.data.frame(unique_DT)

table(hba1c_clean_unique$database)

hba1c_clean_unique <- hba1c_clean_unique %>%
  select(medcode, term, patid, eventdate, database, source, inferred_unit, hba1c_mmol, 
         value, unit, reliable_unit, extreme, min_mmol, max_mmol, best_rank)

# 2,963,047 patients with elevated records
length(unique(hba1c_clean_unique$patid))
length(unique(hba1c_clean_unique$patid[hba1c_clean_unique$database == "Gold"]))
length(unique(hba1c_clean_unique$patid[hba1c_clean_unique$database == "Aurum"]))

# Save cleaned hba1c data filtered to one unique measurement per day
save(hba1c_clean_unique, file = paste0(wd, path_output, "hba1c_clean_unique.RData"))


#=================== Filter to records with elevated hba1c ==================

rm(unique_DT, hba1c_clean)
gc()

# 35,133,130 elevated records
hba1c_elevated <- hba1c_clean_unique %>%
  filter(elevated_hba1c == 1)
table(hba1c_elevated$database)

hba1c_elevated <- hba1c_elevated %>%
  select(medcode, term, patid, eventdate, database, source, inferred_unit, hba1c_mmol, 
         value, unit, reliable_unit, extreme, min_mmol, max_mmol, best_rank)

# 2,963,047 patients with elevated records
length(unique(hba1c_elevated$patid))
length(unique(hba1c_elevated$patid[hba1c_elevated$database == "Gold"]))
length(unique(hba1c_elevated$patid[hba1c_elevated$database == "Aurum"]))

# Save cleaned elevated hba1c data
save(hba1c_elevated, file = paste0(wd, path_output, "hba1c_clean_elevated.RData"))

### Remove data and shut down connection
dbDisconnect(connection, shutdown = TRUE)

rm(hba1c_elevated) 


#==================== Miscellaneous old code ==================================

# # Get number of measurements per patient-day
# # Memory: 14 GB
# hba1c_count <- hba1c_clean %>%
#   select(-c(sysdate, constype, consid, source, data2, enttype_description, unit_value_flag, extreme)) %>%
#   distinct(patid, eventdate, hba1c_mmol, .keep_all = TRUE) %>%  # remove duplicates
#   group_by(patid, eventdate) %>%
#   mutate(num_hba1c = n()) %>%
#   ungroup()
# 
# # 15,728,787 multiple rows
# table(hba1c_count$num_hba1c)
# 
# multiples <- hba1c_count %>% 
#   filter(num_hba1c > 1)

# # Fix potentially incorrect units by applying unit conversions based on values.
# # Then convert all to mmol/mol
# lab_data_clean3 <- lab_data_clean2 %>%
#   # Apply unit conversions based on values: <20 is %, >=20 is mmol/mol
#   mutate(
#     inferred_unit = case_when(
#       value < 20 ~ "%",
#       value >= 20 ~ "mmol/mol",
#       TRUE ~ inferred_unit
#     )) %>%
#   # Create variable for elevated HbA1c: >= 48 mmol/mol or >= 6.5%
#   mutate(elevated_hba1c = case_when(
#     inferred_unit == "%" & value >= 6.5 ~ 1,
#     inferred_unit == "mmol/mol" & value >= 48 ~ 1,
#     is.na(inferred_unit) ~ NA,
#     TRUE ~ 0
#   )) %>%
#   # Convert everything to IFCC mmol/mol
#   mutate(hba1c_mmol = case_when(
#     inferred_unit == "mmol/mol" ~ value,
#     inferred_unit == "%" ~ round((value - 2.152) / 0.09148, 3), # from NGSP->IFCC
#     TRUE ~ NA_real_
#   )) %>%
#   # Flag extreme values
#   mutate(extreme = case_when(
#     hba1c_mmol > 140 ~ "high",
#     hba1c_mmol < 20 ~ "low", 
#     is.na(hba1c_mmol) ~ NA_character_,
#     TRUE ~ "plausible"
#   ))
# 
# # Number with extreme values: 107680 + 525598 = 633,278 (1%)
# table(lab_data_clean3$extreme, useNA = "always")
# 
# # Check extreme values again
# check_extreme <- lab_data_clean3 %>%
#   filter(extreme %in% c("high", "low"))
# 
# # Apply hard measurement cutoffs: mmol/mol must be between 15-200
# # Removed 522,495 (1%). 57,120,079 remaining
# hba1c_clean <- lab_data_clean3 %>%
#   filter(hba1c_mmol >= 15 & hba1c_mmol <= 200)
