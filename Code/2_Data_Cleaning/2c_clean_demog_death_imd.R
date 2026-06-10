# ==============================================================================
# Clean dataset of patients with T2DM diagnoses using linked mortality data
# Author: SM Wu
# Date Created: 2025/06/17
# Date Updated: 2026/05/25
# 
# Details:
# 1) Set up and load data
# 2) Harmonize datasets
# 3) Restrict to those with extracted T2DM or antidiabetic files
# 4) Clean death date using ONS
# 5) Generate registration, Date18, follow-up variables
# 6) Create region variable and combine
# 7) Link IMD and save data
#
# Inputs:
# 1) GOLD/Patient/25_005368_SW_Extract_Patient_001.txt: GOLD patient files
# 2) GOLD/Practice/25_005368_SW_Extract_Practice_001.txtt: GOLD practice files
# 3) Aurum_1/Patient/25_005368_SW_Extract_Patient_001.txt: Aurum patient files for folders Aurum_1, Aurum_2, Aurum_3
# 4) Aurum_1/Practice/25_005368_SW_Extract_Practice_001.txt: Aurum practice files for folders Aurum_1, Aurum_2, Aurum_3
# 
# Intermediate Outputs:
# 1) SMI_GLP/Data/Cleaning_Files/pat_t2dm_last.RData: Date of last T2DM medcode for all patients
# 2) SMI_GLP/Data/Cleaning_Files/pat_antidiab_last.RData: Date of last antidiabetic prodcode for all patients
# 
# Final Outputs:
# 1) SMI_GLP/Data/Cleaning_Files/cohort_demog.Rdata: Cleaned Aurum and 
#     GOLD cohort data with basic demographics containing the following columns:
#     pracid: practice ID
#     patid: patient ID
#     gender: gender
#     yob: year of birth
#     regstartdate: registration start date
#     regenddate: registration end date
#     deathdate: death date, using both CPRD and linked ONS death information
#     acceptable: whether (1) or not (0) patient satisfied CPRD data quality check, see: https://www.cprd.com/sites/default/files/2023-02/CPRD%20GOLD%20Glossary%20Terms%20v2.pdf
#     region: UK region
#     lcd: last collection date from the practice
#     uts: date of CPRD up-to-standard determination. Missing for Aurum. 
#     database: whether patient is from GOLD or Aurum
#     died: 0/1 whether patient died
#     year18: date patient turned 18
#     startfollow: date of start of follow-up
#     endfollow: date of end of follow-up
#     daysfollow: days in follow-up
#     up_to_standard: startfollow - uts date
#     imd: IMD quintile
#     imd_type: whether IMD is patient-level IMD or practice-level IMD

# ==============================================================================


# ================= 1) Set up and load data ====================================

# Clear memory
rm(list = ls())
gc()

# Packages
library(readxl)
library(openxlsx)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(DBI)
library(duckdb)
library(dbplyr)
library(tidylog)

# ### For running in Data Safe Haven
# Set working directory
wd <- "S:/CDSTP_CPRD_25_005368/"
setwd(wd)

# Set input and output paths
path_input <- paste0("SMI_GLP/Data/Extraction_Files/")
path_output <- paste0("SMI_GLP/Data/Cleaning_Files/")
path_gold <- "GOLD/"
path_aurum <- c("Aurum_1/", "Aurum_2/", "Aurum_3/")
path_linkage_gold <- paste0("Linkage/Gold_linked/")
path_linkage_aurum <- paste0("Linkage/Aurum_linked/")

# Set up duckdb database for SQL
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


## Load data

# Read in patient and practice files from GOLD and Aurum
# Patient files contain: patid, gender, date of birth, marital, registration date, etc.
# Practice files contain: pracid, region, up-to-standard date, last collection date

# GOLD patient
gold_pat_raw <- read.table(
  file = paste0(path_gold, "Patient/25_005368_SW_Extract_Patient_001.txt"),
  header = TRUE, fill = TRUE, sep = "\t", quote = "", 
  colClasses = c(patid = "character"))
# GOLD practice
gold_prac_raw <- read.table(
  file = paste0(path_gold, "Practice/25_005368_SW_Extract_Practice_001.txt"),
  header = TRUE, sep = "\t", quote = "")

# Aurum patient and practice
aurum_pat_list <- list() # patient
aurum_prac_list <- list() # practice
# aurum_prac_cmd <- list() # practice
for (i in 1:length(path_aurum)) {
  aurum_pat_list[[i]] <- read.table(
    file = paste0(path_aurum[i], "Patient/25_005368_SW_Extract_Patient_001.txt"),
    header = TRUE, fill = TRUE, sep = "\t", quote = "", 
    colClasses = c(patid = "character"))
  aurum_prac_list[[i]] <- read.table(
    file = paste0(path_aurum[i], "Practice/25_005368_SW_Extract_Practice_001.txt"),
    header = TRUE, sep = "\t", quote = "")
}
aurum_pat_raw <- do.call(rbind, aurum_pat_list)
aurum_prac_raw <- do.call(rbind, aurum_prac_list)

rm(aurum_pat_list, aurum_prac_list)

gold_pat <- gold_pat_raw
gold_prac <- gold_prac_raw
aurum_pat <- aurum_pat_raw
aurum_prac <- aurum_prac_raw


# ================= 2) Harmonize datasets ======================================


# Generate practice ID for GOLD using the last five digits of the patient ID
gold_pat$pracid <- substring(gold_pat$patid, nchar(gold_pat$patid) - 4)

# Append 'G' or 'A' to the patient and practice IDs to distinguish GOLD and Aurum
gold_pat$patid <- paste0(gold_pat$patid, "-", "G")
gold_pat$pracid <- paste0(gold_pat$pracid, "-", "G")
gold_prac$pracid <- paste0(gold_prac$pracid, "-", "G")
aurum_pat$patid <- paste0(aurum_pat$patid, "-", "A")
aurum_pat$pracid <- paste0(aurum_pat$pracid, "-", "A")
aurum_prac$pracid <- paste0(aurum_prac$pracid, "-", "A")

# Add in the practice columns by outer joining the patient and practice files, 
# merging by 'pracid' and keeping all entries from either dataset
gold_pat <- gold_pat %>%
  left_join(gold_prac, by = "pracid", relationship = "many-to-one")
aurum_pat <- aurum_pat %>%
  left_join(aurum_prac %>% distinct(), by = "pracid", relationship = "many-to-one")

# Add in variable denoting dataset database
gold_pat$database <- "Gold"
aurum_pat$database <- "Aurum"

# Check missingness of mob: high missingness for aurum => remove
sum(is.na(gold_pat$mob)) / nrow(gold_pat)
sum(is.na(aurum_pat$mob)) / nrow(aurum_pat)

# Reorder and rename columns so datasets are comparable
colnames(gold_pat)
colnames(aurum_pat)

# Extra gold column to keep: toreason
gold_pat <- gold_pat %>%
  select(pracid, patid, gender, yob, crd, tod, deathdate,
         accept, region, lcd, uts, database, toreason) %>%
  rename(regstartdate = crd, 
         regenddate = tod,
         acceptable = accept)

# Extra aurum column to keep: emis_ddate
aurum_pat <- aurum_pat %>%
  select(pracid, patid, gender, yob, regstartdate, regenddate, 
         cprd_ddate, acceptable, region, lcd, uts, database, emis_ddate) %>%
  rename(deathdate = cprd_ddate) # set to the CPRD-curated death date


# ================= 3) Restrict to those with extracted T2DM or antidiabetic files =========

### Read in T2DM records
extraction_files_t2dm <- paste0(path_input, "T2Diabetes/", "pat_comb_final.parquet")
# Get unique patients: 3,113,415
DBI::dbExecute(connection, sprintf(
  "CREATE OR REPLACE TABLE pat_t2dm_final AS
  SELECT DISTINCT
    patid, database
  FROM read_parquet('%s');", 
  extraction_files_t2dm))
# Create dbplyr table
t2dm_tbl <- tbl(connection, "pat_t2dm_final")


### Read in antidiabetic records
antidiab_file_names <-  c("pat_gold_final_1", "pat_aurum_final_1", "pat_aurum_final_2", "pat_aurum_final_3")
extraction_files_antidiab <- paste0(path_input, "Antidiabetics/", antidiab_file_names, ".parquet")
# Get unique patients after stacking all of the Gold and Aurum antidiabetic files
# 3,218,404 patients
merge_sql <- paste(sprintf(
  "SELECT DISTINCT
    patid, database
  FROM read_parquet('%s')",
  extraction_files_antidiab), 
  collapse = "\n UNION ALL\n"
)
dbExecute(connection, sprintf(
  "CREATE OR REPLACE TABLE pat_antidiab_final AS\n%s", merge_sql))
# Create dbyplr table
antidiab_tbl <- tbl(connection, "pat_antidiab_final")

# Get list of unique patients with T2DM or antidiabetic code: 3,703,508
comb_diab_tbl <- t2dm_tbl %>%
  union_all(antidiab_tbl) %>%
  distinct(patid) %>% 
  collect() # pull into R
nrow(comb_diab_tbl)

# Restrict to those with T2DM or antidiabetic code extracted (all patients included)
# Gold 906,880; Aurum 2,796,628. Total 3,703,508
gold_pat <- gold_pat %>%
  filter(patid %in% comb_diab_tbl$patid)
aurum_pat <- aurum_pat %>%
  filter(patid %in% comb_diab_tbl$patid)
nrow(gold_pat)
nrow(aurum_pat)
sum(c(nrow(gold_pat), nrow(aurum_pat)))

#================== 4) Clean death date using ONS ============================

# Aurum

# If CPRD-curated death date is missing, check if emis_ddate is available 
# 13 death dates filled in; 2,206,831 still no death date
length(which(aurum_pat$deathdate == ""))
aurum_pat <- aurum_pat %>%
  mutate(deathdate = ifelse(deathdate == "", emis_ddate, deathdate)) %>%
  select(-emis_ddate)
length(which(aurum_pat$deathdate == ""))

# Convert to date
aurum_pat$deathdate <- as.Date(aurum_pat$deathdate, format= "%d/%m/%Y")


# GOLD

# Convert to date
gold_pat$deathdate <- as.Date(gold_pat$deathdate, format = "%d/%m/%Y")

# Check none have toreason as death (value = 1) and no death date!
length(which(gold_pat$toreason == 1 & is.na(gold_pat$deathdate)))

# Remove toreason variable
gold_pat <- select(gold_pat, -toreason)


### Read in ONS death data for those with linkage (restricted to England)

# Aurum
ons_aurum <- read.table(
  paste0(path_linkage_aurum, "Death/", "25_005368_Aurum_death_patient.txt"), 
  header = TRUE, fill = TRUE, sep = "\t", dec = ".", colClasses = "character")
# Clean and only preserve necessary variables
ons_aurum_clean <- ons_aurum %>%
  mutate(deathdate_ons = as.Date(reg_date_of_death, format = "%Y-%m-%d"),
         pracid = paste0(pracid, "-A"),
         patid = paste0(patid, "-A")) %>%
  select(pracid, patid, deathdate_ons) %>%
  distinct()
# If there are multiple ONS death dates, take the latest
ons_aurum_clean <- ons_aurum_clean %>%
  group_by(pracid, patid) %>%
  slice(which.max(deathdate_ons))

# Gold
ons_gold <- read.table(
  paste0(path_linkage_gold, "Death/", "25_005368_Gold_death_patient.txt"), 
  header = TRUE, fill = TRUE, sep = "\t", dec = ".", colClasses = "character")
# Clean and only preserve necessary variables
ons_gold_clean <- ons_gold %>%
  mutate(deathdate_ons = as.Date(reg_date_of_death, format = "%Y-%m-%d"),
         pracid = paste0(pracid, "-G"),
         patid = paste0(patid, "-G")) %>%
  select(pracid, patid, deathdate_ons) %>%
  distinct()
# If there are multiple ONS death dates, take the latest
ons_gold_clean <- ons_gold_clean %>%
  group_by(pracid, patid) %>%
  slice(which.max(deathdate_ons))


### Update death date using ONS linkage

## Read in antidiabetic records to get last antidiabetic record per patient

# Get list of patients for whom medication information is needed
dx_patids <- data.frame(patid = unique(c(aurum_pat$patid, gold_pat$patid)))
# Add to duckdb
dbWriteTable(connection, "dx_patids", dx_patids, temporary = TRUE, overwrite = TRUE)


pat_t2dm_last <- dbGetQuery(connection, sprintf(
  "SELECT 
    a.patid, 
    MAX(CAST(a.eventdate AS DATE)) as last_t2dm_date
  FROM read_parquet('%s') a
  JOIN dx_patids p USING(patid)
  WHERE a.eventdate IS NOT NULL
  GROUP BY a.patid", 
  extraction_files_t2dm))

# Get last antidiabetic prescription date using SQL

merge_sql_last <- paste(sprintf(
  "SELECT
    patid, eventdate
  FROM read_parquet('%s')",
  extraction_files_antidiab), 
  collapse = "\n UNION ALL\n"
)
dbExecute(connection, sprintf(
  "CREATE OR REPLACE TABLE pat_antidiab_last AS\n%s", merge_sql_last))

pat_antidiab_last <- dbGetQuery(connection, sprintf(
  "SELECT 
    a.patid,
    MAX(CAST(a.eventdate AS DATE)) as last_antidiab_date
  FROM pat_antidiab_last a
  JOIN dx_patids p USING(patid)
  WHERE a.eventdate IS NOT NULL
  GROUP BY a.patid"
))

# # Save pat_t2dm_last and pat_antidiab_last
# save(pat_t2dm_last, file = paste0(wd, "SMI_GLP/Data/Cleaning_Files/", "pat_t2dm_last.RData"))
# save(pat_antidiab_last, file = paste0(wd, "SMI_GLP/Data/Cleaning_Files/", "pat_antidiab_last.RData"))
# load(paste0(wd, "SMI_GLP/Data/Cleaning_Files/", "pat_t2dm_last.RData"))
# load(paste0(wd, "SMI_GLP/Data/Cleaning_Files/", "pat_antidiab_last.RData"))


## Aurum

# Merge ONS death dates into CPRD cohort data: 824,734 matched
aurum_pat_death <- aurum_pat %>%
  left_join(ons_aurum_clean, by = c("pracid", "patid"),
            relationship = "one-to-one")

# Merge in last t2dm record and last antidiabetic record
aurum_pat_death <- aurum_pat_death %>%
  left_join(pat_t2dm_last, by = "patid") %>%
  left_join(pat_antidiab_last, by = "patid")
# Get latest dateof t2dm or antidiabetic record
aurum_pat_death$last_t2dm_antidiab <- pmax(aurum_pat_death$last_t2dm_date, 
                                           aurum_pat_death$last_antidiab_date, 
                                           na.rm = TRUE)
# Calculate the discrepancies between ONS and CPRD death dates
aurum_pat_death <- aurum_pat_death %>%
  mutate(deathdate_cprd = deathdate,
         death_diff = as.numeric(deathdate_ons - deathdate_cprd))
# Investigate death date differences
summary(aurum_pat_death$death_diff)
# 119,203 patients with diff dates
death_discrepancies <- aurum_pat_death %>%
  filter(death_diff != 0) %>%
  arrange(death_diff)

# Assignment rule for death dates:
# 1. If both are NA, set date to NA
# 2. If only one is NA, use the other
# 3. If both are not NA and there is a discrepancy, if one occurs before the 
# final T2DM or antidiabetic eventdate, choose the other
# 4. Otherwise (tie between the two or no discrepancy), use the ONS date 
aurum_pat_death <- aurum_pat_death %>%
  mutate(deathdate = case_when(
    is.na(deathdate_ons) & is.na(deathdate_cprd) ~ NA,
    is.na(deathdate_ons) & !is.na(deathdate_cprd) ~ deathdate_cprd,
    !is.na(deathdate_ons) & is.na(deathdate_cprd) ~ deathdate_ons,
    !is.na(death_diff) & (death_diff != 0) & 
      (deathdate_ons < last_t2dm_antidiab) ~ deathdate_cprd,
    !is.na(death_diff) & (death_diff != 0) & 
      (deathdate_cprd < last_t2dm_antidiab) ~ deathdate_ons,
    # Prefer ONS 
    .default = deathdate_ons
  ))


# Create 'died' variable specifying if patient died (deathdate is not NA)
aurum_pat_death$died <- 0
aurum_pat_death$died[!is.na(aurum_pat_death$deathdate)] <- 1

# Drop variables used for cleaning
aurum_pat_clean <- aurum_pat_death %>%
  select(-c(deathdate_ons, deathdate_cprd, last_t2dm_date, last_antidiab_date, 
            last_t2dm_antidiab, death_diff))


## Gold

# Merge ONS death dates into CPRD cohort data: 138,972 matched
gold_pat_death <- gold_pat %>%
  left_join(ons_gold_clean, by = c("pracid", "patid"))

# Merge in last t2dm record and last antidiabetic record
gold_pat_death <- gold_pat_death %>%
  left_join(pat_t2dm_last, by = "patid") %>%
  left_join(pat_antidiab_last, by = "patid")
# Get latest dateof t2dm or antidiabetic record
gold_pat_death$last_t2dm_antidiab <- pmax(gold_pat_death$last_t2dm_date, 
                                          gold_pat_death$last_antidiab_date, 
                                          na.rm = TRUE)
# Calculate the discrepancies between ONS and CPRD death dates
gold_pat_death <- gold_pat_death %>%
  mutate(deathdate_cprd = deathdate,
         death_diff = as.numeric(deathdate_ons - deathdate_cprd))
# Investigate death date differences
summary(gold_pat_death$death_diff)
# 13,943 patients with diff dates
death_discrepancies <- gold_pat_death %>%
  filter(death_diff != 0) %>%
  arrange(death_diff)

# Assignment rule for death dates:
# 1. If both are NA, set date to NA
# 2. If only one is NA, use the other
# 3. If both are not NA and there is a discrepancy, if one occurs before the 
# final T2DM or antidiabetic eventdate, choose the other
# 4. Otherwise (tie between the two or no discrepancy), use the ONS date 
gold_pat_death <- gold_pat_death %>%
  mutate(deathdate = case_when(
    is.na(deathdate_ons) & is.na(deathdate_cprd) ~ NA,
    is.na(deathdate_ons) & !is.na(deathdate_cprd) ~ deathdate_cprd,
    !is.na(deathdate_ons) & is.na(deathdate_cprd) ~ deathdate_ons,
    !is.na(death_diff) & (death_diff != 0) & 
      (deathdate_ons < last_t2dm_antidiab) ~ deathdate_cprd,
    !is.na(death_diff) & (death_diff != 0) & 
      (deathdate_cprd < last_t2dm_antidiab) ~ deathdate_ons,
    # Prefer ONS 
    .default = deathdate_ons
  ))


# Create 'died' variable specifying if patient died (deathdate is not NA)
gold_pat_death$died <- 0
gold_pat_death$died[!is.na(gold_pat_death$deathdate)] <- 1

# Drop variables used for cleaning
gold_pat_clean <- gold_pat_death %>%
  select(-c(deathdate_ons, deathdate_cprd, last_t2dm_date, last_antidiab_date, 
            last_t2dm_antidiab, death_diff))


#================== Restrict to those aged 18 - 100 years ======================

# Create variable to ensure all patients are at least 18 years old within study period
aurum_pat_clean$year18 <- paste0(aurum_pat_clean$yob + 18, "-12-31")
gold_pat_clean$year18 <- paste0(gold_pat_clean$yob + 18, "-12-31")

# Create variable to ensure all patients are not over 100 years old within study period
aurum_pat_clean$year100 <- paste0(aurum_pat_clean$yob + 100, "-12-31")
gold_pat_clean$year100 <- paste0(gold_pat_clean$yob + 100, "-12-31")

# Define start of study period: Jan 1, 2005
# Define end of study period: May 5, 2025, since we are using June 2025 release
study_start_date <- as.Date("2005-01-01", format = "%Y-%m-%d")
study_end_date <- as.Date("2025-05-05", format = "%Y-%m-%d")

# Restrict to those aged 18-100 within the study period
# Removals: 
#   Aurum 339,599 (12%); 2,457,029 remaining
#   Gold 98,665 (11%); 808,215 remaining
#   Total 438,264 removed; 3,265,244 remaining
aurum_pat_clean <- aurum_pat_clean %>%
  filter(year18 <= study_start_date & year100 >= study_end_date)
gold_pat_clean <- gold_pat_clean %>%
  filter(year18 <= study_start_date & year100 >= study_end_date)


#================== 5) Generate registration, Date18, follow-up variables ======

## Create variables for registration start date, registration end date,  
# date of last collection, and up-to-standard date (for Gold only)
# Convert to dates
aurum_pat_clean <- aurum_pat_clean %>%
  mutate_at(c("regstartdate", "regenddate", "lcd"), 
            ~as.Date(., format = "%d/%m/%Y"))

gold_pat_clean <- gold_pat_clean %>%
  mutate_at(c("regstartdate", "regenddate", "lcd", "uts"), 
            ~as.Date(., format = "%d/%m/%Y"))

# Check values
summary(aurum_pat_clean$regenddate)
summary(gold_pat_clean$regenddate)


## Create variables for the date that follow-up started, ended, and how many 
## days did that patient have follow up

# Date of valid research quality data post-registration: 365 days after the latest of 
# registration start and date at which data are considered adequate for research (Gold)
aurum_pat_clean$date_valid <- pmax(aurum_pat_clean$regstartdate, 
                             aurum_pat_clean$uts, na.rm = TRUE) + 365
gold_pat_clean$date_valid <- pmax(gold_pat_clean$regstartdate, 
                            gold_pat_clean$uts, na.rm = TRUE) + 365

# Follow-up start date: latest of date of valid research quality post-registration, 
# year turned 18, and start of study period
aurum_pat_clean$startfollow <- pmax(aurum_pat_clean$date_valid, 
                              aurum_pat_clean$year18, 
                              study_start_date,  na.rm=TRUE)
gold_pat_clean$startfollow <- pmax(gold_pat_clean$date_valid, 
                             gold_pat_clean$year18, 
                             study_start_date,  na.rm=TRUE)

# Follow-up end date: earliest of registration end, death, date of last collection,
# and end of study period.
aurum_pat_clean$endfollow <- pmin(aurum_pat_clean$regenddate, 
                            aurum_pat_clean$deathdate, 
                            aurum_pat_clean$lcd,
                            study_end_date,  na.rm=TRUE)
gold_pat_clean$endfollow <- pmin(gold_pat_clean$regenddate, 
                           gold_pat_clean$deathdate, 
                           gold_pat_clean$lcd,
                           study_end_date,  na.rm=TRUE)

# Days within follow-up period
aurum_pat_clean$daysfollow <- as.numeric(aurum_pat_clean$endfollow - aurum_pat_clean$startfollow)
gold_pat_clean$daysfollow <- as.numeric(gold_pat_clean$endfollow - gold_pat_clean$startfollow)

summary(aurum_pat_clean$daysfollow)
summary(gold_pat_clean$daysfollow)

# How many were not in the follow-up period: Aurum 229,340, Gold 88,851
sum(aurum_pat_clean$daysfollow <= 0)
sum(gold_pat_clean$daysfollow <= 0)

# Filter to those with at least 1 day of follow-up aged 18+ between 
# 2005/01/01 and 2025/05/05. Total 318,191 removed, 2,947,053 remaining
# Aurum: removed 229,340 (9%), 2,227,689 remaining. 
# Gold: removed 88,851 (11%), 719,364 remaining
aurum_pat_study <- aurum_pat_clean %>% filter(daysfollow > 0)
gold_pat_study <- gold_pat_clean %>% filter(daysfollow > 0)
nrow(aurum_pat_study)
nrow(gold_pat_study)


    # # Filter to those with at least 365 days of follow-up aged 18+ between 
    # # 2005/01/01 and 2025/05/05 (OLD CODE)
    # # Aurum 2,474,415. Gold 783,596
    # aurum_pat_study <- aurum_pat %>% filter(daysfollow >= 365)
    # gold_pat_study <- gold_pat %>% filter(daysfollow >= 365)
    # nrow(aurum_pat_study)
    # nrow(gold_pat_study)
    # 
    # # What if used 180 days (6 months)?
    # # Aurum 2,606,092. Gold 820,511
    # nrow(aurum_pat %>% filter(daysfollow >= 180))
    # nrow(gold_pat %>% filter(daysfollow >= 180))
    # 
    # # Filter to those with data in the follow-up period. what if used 1 day?
    # # Aurum 2,773,443. Gold 858,185
    # nrow(aurum_pat %>% filter(daysfollow > 0))
    # nrow(gold_pat %>% filter(daysfollow > 0))

#================== 5) Create region variable and combine ==========

# Combine aurum and gold into one dataset
pat_study_all <- bind_rows(aurum_pat_study, gold_pat_study)

# Define region varialbe
pat_study_all <- pat_study_all %>% 
  mutate(region = case_when(region == 1 ~ "North East",
                            region == 2 ~ "North West",
                            region == 3 ~ "Yorkshire & The Humber",
                            region == 4 ~ "East Midlands",
                            region == 5 ~ "West Midlands",
                            region == 6 ~ "East of England",
                            region == 7 ~ "London",
                            region == 8 ~ "South East",
                            region == 9 ~ "South West",
                            region == 10 ~ "Wales",
                            region == 11 ~ "Scotland",
                            region == 12 ~ "Northern Ireland",
                            TRUE ~ NA))

# Check Aurum is mostly England and GOLD includes Scotland, Wales, Northern Ireland
table(pat_study_all$region, pat_study_all$database, useNA="ifany")
# See where the Aurum practices are from (mostly London and North West)
table(aurum_prac$region, useNA = "ifany")


## Sanity checks
cohort_demog <- pat_study_all

# Total number of patients: 2,947,053
nrow(cohort_demog)

# Check if up-to-standard date is after follow-up start date
cohort_demog <- cohort_demog %>%
  mutate(up_to_standard = as.numeric(startfollow - as.Date(uts, format = "%d/%m/%Y")))

# How many patients have data quality issues? 0
sum(cohort_demog$acceptable == 0)

# How many patients have data that is not up to standard within follow-up? 0
sum(cohort_demog$up_to_standard < 0, na.rm = TRUE) # 0
sum(is.na(cohort_demog$up_to_standard)) # 2,227,689
sum(is.na(cohort_demog$uts))

# Save memory
rm(pat_t2dm_last, pat_antidiab_last, t2dm_tbl, antidiab_tbl, 
   aurum_pat, aurum_pat_death, aurum_pat_study, 
   gold_pat, gold_pat_death, gold_pat_study, death_discrepancies, dx_patids, 
   ons_aurum_clean, ons_gold_clean)

# ============= 7) Link IMD and save data ====================================

### Read in IMD patient and practice linkages

# Aurum patient and practice
imd_pat_aurum <- read.table(
  paste0(path_linkage_aurum, "IMD/", "25_005368_Aurum_patient_imdcomposite.txt"), 
  header = TRUE, fill = TRUE, sep = "\t", dec = ".", 
  colClasses = c(patid = "character", pracid = "character"))
imd_prac_aurum <- read.table(
  paste0(path_linkage_aurum, "IMD/", "25_005368_Aurum_Practice_IMD_2025_06.txt"), 
  header = TRUE, fill = TRUE, sep = "\t", dec = ".", 
  colClasses = c(pracid = "character"))

# Gold patient and practice
imd_pat_gold <- read.table(
  paste0(path_linkage_gold, "IMD/", "25_005368_Gold_patient_imdcomposite.txt"), 
  header = TRUE, fill = TRUE, sep = "\t", dec = ".", 
  colClasses = c(patid = "character", pracid = "character"))
imd_prac_gold <- read.table(
  paste0(path_linkage_gold, "IMD/", "25_005368_Gold_Practice_IMD_2025_06.txt"), 
  header = TRUE, fill = TRUE, sep = "\t", dec = ".", 
  colClasses = c(pracid = "character"))

# Combine Aurum and Gold IMD linkages
imd_pat <- bind_rows(
  imd_pat_aurum %>% mutate(patid = paste0(patid, "-A")), 
  imd_pat_gold %>% mutate(patid = paste0(patid, "-G"))) %>%
  rename(pat_imd = e2019_imd_5) %>%
  select(patid, pat_imd) %>%
  distinct()

imd_prac <- bind_rows(
  imd_prac_aurum %>% mutate(pracid = paste0(pracid, "-A")), 
  imd_prac_gold %>% mutate(pracid = paste0(pracid, "-G"))) %>%
  rename(prac_imd = e2019_imd_5) %>%
  select(pracid, prac_imd) %>%
  distinct()

### Merge into demographic cohort data
# 2,406,564 patient IMD matched; 540,489 unmatched
cohort_demog <- cohort_demog %>%
  left_join(imd_pat, by = "patid")
# 2,463,199 patients w/ practice IMD matched; 483,854 unmatched
cohort_demog <- cohort_demog %>%
  left_join(imd_prac, by = "pracid", relationship = "many-to-one")

# Create IMD variable, prioritizing patient IMD over practice IMD
cohort_demog <- cohort_demog %>%
  mutate(
    imd = case_when(
      !is.na(pat_imd) ~ pat_imd, 
      !is.na(prac_imd) ~ prac_imd, 
      .default = NA),
    imd_type = case_when(
      !is.na(pat_imd) ~ "patient", 
      !is.na(prac_imd) ~ "practice", 
      .default = NA))
table(cohort_demog$imd, useNA = "always")
prop.table(table(cohort_demog$imd, useNA = "always"))
table(cohort_demog$imd_type, useNA = "always")
prop.table(table(cohort_demog$imd_type, useNA = "always"))

# Remove unnecessary variables
cohort_demog <- cohort_demog %>%
  select(-c(pat_imd, prac_imd))


# # Save cohort data
# save(cohort_demog, file = paste0(wd, path_output, "cohort_demog.Rdata"))


### Remove data and shut down connection
dbDisconnect(connection, shutdown = TRUE)

