# ====================================================================
# Clean BMI and weight values
# Author: SM Wu
# Date Created: 2026/06/04
# Date Updated: 2026/06/04
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
path_input <- paste0("SMI_GLP/Data/Extraction_Files/BMI/")
path_cleaning <- paste0("SMI_GLP/Data/Cleaning_Files/")
path_output <- paste0("SMI_GLP/Data/Cleaned_Data/")


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


# Read in final T2DM cohort with T2DM, SMI, and demog information 
# Loads as 'cohort_demog_dx_date_smi_eth'
load(paste0(path_output, "cohort_demog_cleaned_with_smi_eth.RData")) # created in 2g_clean_ethnicity.R
length(unique(cohort_demog_dx_date_smi_eth$patid)) # 2,151,230


# Upload cohort IDs to duckdb
DBI::dbWriteTable(connection,
  "patid_cohort",
  data.frame(patid = cohort_demog_dx_date_smi_eth$patid),
  overwrite = TRUE
)

# Read in BMI raw dataset, restricting to those in the T2DM cohort
# Memory: 20 GB
bmi_path <- paste0(wd, path_input, "pat_comb_final.parquet")
bmi_raw <- DBI::dbGetQuery(connection, sprintf(
  "SELECT 
    a.*
  FROM read_parquet('%s') AS a 
  WHERE EXISTS (
    SELECT 1
    FROM patid_cohort p
    WHERE p.patid = a.patid
  )",
  bmi_path))

# Disconnect and wipe the spillover folder
dbDisconnect(connection, shutdown = TRUE)
closeAllConnections()
unlink(spill_dir, recursive = TRUE, force = TRUE)
rm(connection)
gc()


# ======= Remove NA and values before birth or after death =============

# Remove NA and values before birth or after death

setDT(bmi_raw)
setDT(cohort_demog_dx_date_smi_eth)

bmi_clean <- bmi_raw[
  # Join in patient variables: 111,391,630 records
  # 6992 patienst w/ no BMI records
  cohort_demog_dx_date_smi_eth[, .(patid, yob, deathdate)],
  on = "patid",
  nomatch = 0
]
# Save memory
rm(bmi_raw)
gc()
# Convert to dates
bmi_clean[
  , `:=` (
    eventdate = as.Date(eventdate, "%Y-%m-%d"),
    deathdate = as.Date(deathdate, "%d/%m/%Y")
  )
]
# Remove values in or prior to birth year: removed 3649 (<1%)
# Remove values after death date: Removed 5975 (<1%)
bmi_clean <- bmi_clean[  
  year(eventdate) > yob & 
    (deathdate > eventdate | is.na(deathdate))
]
bmi_clean[, c("yob", "deathdate") := NULL]
gc()
# Drop duplications: dropped 329,681 (<1%)
bmi_clean <- unique(
  bmi_clean,
  by = c("patid", "eventdate", "value", "term")
)
# Set NA unit: changed 731 (<1%)
bmi_clean[
  unit == "No Data Entered" | grepl("unknown", unit, ignore.case = TRUE),
  unit := NA_character_
]
# Drop NA values: removed 24,013,379 (22%). 87,038,946 rows remaining
bmi_clean <- bmi_clean[!is.na(value)]
gc()

sort(table(bmi_clean$unit), decreasing = TRUE)

# Save memory
rm(cohort_demog_dx_date_smi_eth)
gc()


#============= Fix values and units ============================

# Remove unrelated or non-specific terms
# 79,187,270 rows remaining
bmi_clean <- bmi_clean[
  !grepl(paste0("clinic|creatinine|fracture|target|ideal|waist to height|",
                "ideal weight discussed|loss from baseline|programme|care|",
                "service|disability|assessment|signposted|waterlow|score|",
                "referral|diet|symptom|advice|step|frail|screening|complaining"), 
         term, ignore.case = TRUE)
]
gc()
# Categorize into bmi, weight, and height terms
bmi_clean[
  , type := fcase(
    grepl("body mass index|bmi", term, ignore.case = TRUE), "bmi",
    grepl("obesity|obese", term, ignore.case = TRUE) & 
      grepl("kg/m2", unit, ignore.case = TRUE), "bmi",
    grepl("weight", term, ignore.case = TRUE), "weight",
    grepl("obesity|obese", term, ignore.case = TRUE) & 
      grepl("kg|kilo|lb|pound|stone", unit, ignore.case = TRUE), "weight",
    grepl("height", term, ignore.case = TRUE), "height",
    default = NA_character_
  )
]

# # Save intermediary cleaned bmi file
# save(bmi_clean, file = paste0(wd, path_cleaning, "bmi_clean.RData"))


#==================== Clean BMI ======================================

bmi_only <- bmi_clean[type == "bmi"] # 31,517,845
# weight_only <- bmi_clean[type == "weight"] # 32,003,433
# height_only <- bmi_clean[type == "height"] # 15,665,796
# other <- bmi_clean[is.na(type)]

# Fix recorded units
bmi_only[
  , `:=` (
    value = as.numeric(value),
    # Check if explicit unit is already reliable
    reliable_unit = fcase(
      grepl("kg.*m", unit, ignore.case = TRUE), "kg/m2",
      default = NA_character_
    ),
    # Everything else is inferred to be kg/m2
    inferred_unit = "kg/m2"
  )
]

# Drop values outside acceptable range of 15-100 (Exeter Diabetes cutoffs)
# 31,398,587 remaining. Dropped 119,258 (<1%)
bmi_only <- bmi_only[
  value >= 15 & value <= 100
]

# Number with reliable units: 17,183,378 (55%)
table(bmi_only$reliable_unit, useNA = "always")
gc()



# Create variable for obesity (BMI >= 30 kg/m2, according to NICE)
bmi_only[
  , obesity := fcase(
    value >= 30, 1, 
    default = 0
  )
]

# Rename value variable to bmi_kgm2
setnames(bmi_only, old = "value", new = "bmi_kgm2")

# Proportion of BMI measurements above obesity cutoff: 54%
mean(bmi_only$obesity)

# Number of patients in cohort w/ BMI measurements: 1,647,121 (77%)
length(unique(bmi_only$patid))

summary(bmi_only$bmi_kgm2)
hist(bmi_only$bmi_kgm2, breaks = 30)

gc()


### Handle multiple values a day 

# Stable row ID so we can keep first occurrence in original order
bmi_only[, rowid := .I]

# De-duplicate exact repeats within (patid, eventdate, bmi_kgm2)
# 23,356 removed. 31,375,231 records remaining
bmi_only <- bmi_only[!duplicated(bmi_only, by = c("patid", "eventdate", "bmi_kgm2"))]

# Compute days with multiple measurements and get min/max measurements
bmi_only[, `:=`(
  n_day = .N,
  min_bmi = min(bmi_kgm2, na.rm = TRUE),
  max_bmi = max(bmi_kgm2, na.rm = TRUE)
), by = .(patid, eventdate)]

# Number of records with n_day > 1: 5,384,793
bmi_only[n_day > 1, .N]
# Number of patid-eventdate pairs with multiple measurements: 2,676,312
bmi_only[n_day > 1, .N, by = .(patid, eventdate)][, .N]
gc()

# Where there are multiple measurements, select the one with the most reliable unit
bmi_only[, unit_rank := fifelse(is.na(reliable_unit), 2, 1)]

# Distribution of ranks
# 1: 17,167,577. 2: 14,204,266
bmi_only[, .N, by = .(unit_rank)]


# Keep only the best-ranked rows per (patid, eventdate)
# None dropped
bmi_only[, best_rank := min(unit_rank), by = .(patid, eventdate)]
bmi_only <- bmi_only[unit_rank == best_rank]
# Distribution of ranks
# 1: 17,167,577. 2: 14,204,266
bmi_only[, .N, by = .(unit_rank)]
gc()

# Recompute multiple counts after filtering
bmi_only[, n_day := .N, by = .(patid, eventdate)]
# Number of records with n_day > 1: 5,384,793
bmi_only[n_day > 1, .N]
# Number of patid-eventdate pairs with multiple measurements: 2,676,312
bmi_only[n_day > 1, .N, by = .(patid, eventdate)][, .N]
# Drop unnecessary columns
bmi_only[, c("rowid", "n_day", "unit_rank") := NULL]
gc()

# If there are still multiple at that rank, take the mean: now 28,666,750 
# For the other variables, take the first row value (SD[1])
bmi_only[, row_id := .I]
setorder(bmi_only, patid, eventdate, row_id)
unique_bmi_only <- bmi_only[, c(.SD[1], 
                                .(bmi_kgm2_mean = mean(bmi_kgm2, na.rm = TRUE))),
                            by = .(patid, eventdate)]
unique_bmi_only[, "bmi_kgm2" := NULL]
setnames(unique_bmi_only, old = "bmi_kgm2_mean", new = "bmi_kgm2")

# Final: 28,666,750 records from 1,647,121 patients
unique_bmi_only[, .N, by = patid][, .N]
rm(bmi_only)
gc()

# Convert back to dataframe
bmi_clean_unique <- as.data.frame(unique_bmi_only)

# Almost all from Aurum 
table(bmi_clean_unique$database)

bmi_clean_unique <- bmi_clean_unique %>%
  select(medcode, term, patid, eventdate, database, source, inferred_unit, bmi_kgm2, 
         obesity, unit, reliable_unit, min_bmi, max_bmi, best_rank)

# 1,647,121 patients with elevated records
length(unique(bmi_clean_unique$patid))
length(unique(bmi_clean_unique$patid[bmi_clean_unique$database == "Gold"]))
length(unique(bmi_clean_unique$patid[bmi_clean_unique$database == "Aurum"]))

# Save cleaned bmi data filtered to one unique measurement per day
save(bmi_clean_unique, file = paste0(wd, path_cleaning, "bmi_clean_unique.RData"))

# Save memory
rm(bmi_clean_unique, unique_bmi_only)
gc()

#==================== Clean Weight ======================================

weight_only <- bmi_clean[type == "weight"] # 32,003,433
# height_only <- bmi_clean[type == "height"] # 15,665,796
# other <- bmi_clean[is.na(type)]

# Fix recorded units
weight_only[
  , `:=` (
    value = as.numeric(value),
    # Check if explicit unit is already reliable
    reliable_unit = fcase(
      grepl("kg|kilo", unit, ignore.case = TRUE), "kg",
      grepl("stone|\\bst\\b", unit, ignore.case = TRUE), "stones",
      grepl("lb|pound", unit, ignore.case = TRUE), "lbs",
      grepl("\\bg\\b|gram", unit, ignore.case = TRUE), "grams",
      default = NA_character_
    )
  )
]
# Set inferred unit to reliable unit if it exists
# Everything else is inferred to be kg
weight_only[, inferred_unit := reliable_unit]
weight_only[is.na(inferred_unit), inferred_unit := "kg"]

# Convert everything to kg
weight_only[
  , weight_kg := fcase(
    inferred_unit == "kg", value,
    inferred_unit == "stones", value * 6.35029318, # NHS weight conversion (1 stone = 14 pounds)
    inferred_unit == "lbs", value * 0.45359237, # NIST 
    inferred_unit == "grams", value / 1000,
    default = NA_character_
  )
]

# Drop values outside acceptable range of 40-350 (Exeter Diabetes cutoffs)
# 31,874,702 remaining. Dropped 128,731 (<1%)
weight_only <- weight_only[
  weight_kg >= 40 & weight_kg <= 350
]

# Number with reliable units: 31,636,561 (99%)
table(weight_only$reliable_unit, useNA = "always")
gc()

# Number of patients in cohort w/ weight measurements: 1,653,163 (77%)
length(unique(weight_only$patid))

summary(weight_only$weight_kg)
hist(weight_only$weight_kg, breaks = 30)

gc()


### Handle multiple values a day 

# Stable row ID so we can keep first occurrence in original order
weight_only[, rowid := .I]

# De-duplicate exact repeats within (patid, eventdate, weight_kgm2)
# 7203 removed. 31,867,499 records remaining
weight_only <- weight_only[!duplicated(weight_only, by = c("patid", "eventdate", "weight_kg"))]

# Compute days with multiple measurements and get min/max measurements
weight_only[, `:=`(
  n_day = .N,
  min_weight = min(weight_kg, na.rm = TRUE),
  max_weight = max(weight_kg, na.rm = TRUE)
), by = .(patid, eventdate)]

# Number of records with n_day > 1: 227,154
weight_only[n_day > 1, .N]
# Number of patid-eventdate pairs with multiple measurements: 112,473
weight_only[n_day > 1, .N, by = .(patid, eventdate)][, .N]
gc()

# Where there are multiple measurements, select the one with the most reliable unit
# 1) reliable unit is kg, 2) has other reliable unit, 3) no reliable unit
weight_only[, unit_rank := fifelse(!is.na(reliable_unit) & reliable_unit == "kg", 1,
                           fifelse(!is.na(reliable_unit), 2, 3))]

# Distribution of ranks
# 1: 31,630,016. 2: 4. 3: 237,479
weight_only[, .N, by = .(unit_rank)]


# Keep only the best-ranked rows per (patid, eventdate)
# 716 dropped; 31,866,783 remaining
weight_only[, best_rank := min(unit_rank), by = .(patid, eventdate)]
weight_only <- weight_only[unit_rank == best_rank]
# Distribution of ranks
# 1: 31,630,016. 2: 2. 3: 236,765
weight_only[, .N, by = .(unit_rank)]
gc()

# Recompute multiple counts after filtering
weight_only[, n_day := .N, by = .(patid, eventdate)]
# Number of records with n_day > 1: 225,736
weight_only[n_day > 1, .N]
# Number of patid-eventdate pairs with multiple measurements: 111,771
weight_only[n_day > 1, .N, by = .(patid, eventdate)][, .N]
# Drop unnecessary columns
weight_only[, c("rowid", "n_day", "unit_rank") := NULL]
gc()

# If there are still multiple at that rank, take the mean: now 31,752,818
# For the other variables, take the first row value (SD[1])
weight_only[, row_id := .I]
setorder(weight_only, patid, eventdate, row_id)
unique_weight_only <- weight_only[, c(.SD[1], 
                                .(weight_kg_mean = mean(weight_kg, na.rm = TRUE))),
                            by = .(patid, eventdate)]
unique_weight_only[, "weight_kg" := NULL]
setnames(unique_weight_only, old = "weight_kg_mean", new = "weight_kg")

# Final: 31,752,818 records from 1,653,163 patients
unique_weight_only[, .N, by = patid][, .N]
rm(weight_only)
gc()

# Convert back to dataframe
weight_clean_unique <- as.data.frame(unique_weight_only)

# Almost all from Aurum 
table(weight_clean_unique$database)

weight_clean_unique <- weight_clean_unique %>%
  select(medcode, term, patid, eventdate, database, source, inferred_unit, weight_kg, 
         unit, reliable_unit, min_weight, max_weight, best_rank)

# 1,653,163 patients with elevated records
length(unique(weight_clean_unique$patid))
length(unique(weight_clean_unique$patid[weight_clean_unique$database == "Gold"]))
length(unique(weight_clean_unique$patid[weight_clean_unique$database == "Aurum"]))

# Save cleaned weight data filtered to one unique measurement per day
save(weight_clean_unique, file = paste0(wd, path_cleaning, "weight_clean_unique.RData"))

# Save memory
rm(weight_clean_unique, unique_weight_only)
gc()


 

#=============== Miscellaneous old code ========================================

# bmi_clean <- bmi_raw %>%
#   # Join in patient variables: 111,391,630 records
#   # 6992 patienst w/ no BMI records
#   inner_join(cohort_demog_dx_date_smi_eth %>% 
#                select(patid, yob, deathdate), 
#              by = "patid") %>%
#   # Convert to dates
#   mutate(eventdate = as.Date(eventdate, "%Y-%m-%d"),
#          event_year = year(eventdate),
#          deathdate = as.Date(deathdate, "%d/%m/%Y")) %>%
#   # Remove values in or prior to birth year: removed 3649 (<1%)
#   filter(event_year > yob) %>%
#   # Remove values after death date: Removed 5975 (<1%)
#   filter(deathdate > eventdate | is.na(deathdate)) %>%
#   select(-yob, -deathdate, -event_year) %>%
#   # Drop duplications: dropped 329,681 (<1%)
#   distinct(patid, eventdate, value, term, .keep_all = TRUE) %>%
#   # Set NA unit: changed 731 (<1%)
#   mutate(unit = case_when(
#     unit == "No Data Entered" ~ NA_character_,
#     grepl("unknown", unit, ignore.case = TRUE) ~ NA_character_, 
#     TRUE ~ unit)) %>%
#   # Drop NA values: removed 24,013,379 (22%). 87,038,946 rows remaining
#   filter(!is.na(value)) %>%
#   distinct()


# # Remove unrelated terms
# # Removed 178,511 (1%) rows
# bmi_clean <- bmi_clean %>%
#   filter(!grepl(paste0("(?i)clinic|creatinine|fracture|target|ideal|waist to height|",
#                        "ideal weight discussed|loss from baseline|programme|care|",
#                        "service|disability|assessment|signposted|waterlow|score|",
#                        "referral|diet|symptom|advice|step|frail|screening|complaining", term))) %>%
#   # Remove 
#   # Identify type of measurement
#   mutate(
#     type = case_when(
#       grepl("(?i)body mass index|bmi", term) ~ "bmi",
#       grepl("(?i)weight", term) ~ "weight",
#       grepl("(?i)height", term) ~ "height",
#        TRUE ~ NA))
