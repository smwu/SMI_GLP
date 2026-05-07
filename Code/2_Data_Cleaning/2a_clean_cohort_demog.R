# ==============================================================================
# Clean dataset of patients with T2DM diagnoses using linked mortality data
# Author: SM Wu
# Date Created: 2025/06/17
# Date Updated: 2026/03/06
# 
# Details:
# 1) Set up and load data
# 2) Harmonize datasets
# 3) Clean death date
# 4) Generate registration, Date18, follow-up variables
# 5) Create region variable, combine, and save data
#
# Inputs:
# 1) GOLD/Patient/25_005368_SW_Extract_Patient_001.txt: GOLD patient files
# 2) GOLD/Practice/25_005368_SW_Extract_Practice_001.txtt: GOLD practice files
# 3) Aurum_1/Patient/25_005368_SW_Extract_Patient_001.txt: Aurum patient files for folders Aurum_1, Aurum_2, Aurum_3
# 4) Aurum_1/Practice/25_005368_SW_Extract_Practice_001.txt: Aurum practice files for folders Aurum_1, Aurum_2, Aurum_3
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
#     deathdate: death date
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

# ==============================================================================


# ================= 1) Set up and load data ====================================

# Clear memory
rm(list = ls())

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


# ================= Restrict to those with extracted T2DM or antidiabetic files

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

#================== 4) Clean death date ========================================

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

# Create 'died' variable specifying if patient died (deathdate is not NA)
aurum_pat$died <- 0
aurum_pat$died[!is.na(aurum_pat$deathdate)] <- 1

# GOLD

# Convert to date
gold_pat$deathdate <- as.Date(gold_pat$deathdate, format = "%d/%m/%Y")

# Check none have toreason as death (value = 1) and no death date!
length(which(gold_pat$toreason == 1 & is.na(gold_pat$deathdate)))

# Create 'died' variable
gold_pat$died <- 0
gold_pat$died[!(is.na(gold_pat$deathdate))] <- 1

# Remove toreason variable
gold_pat <- select(gold_pat, -toreason)


#================== Restrict to those aged 18 - 100 years ======================

# Create variable to ensure all patients are at least 18 years old within study period
aurum_pat$year18 <- paste0(aurum_pat$yob + 18, "-12-31")
gold_pat$year18 <- paste0(gold_pat$yob + 18, "-12-31")

# Create variable to ensure all patients are not over 100 years old within study period
aurum_pat$year100 <- paste0(aurum_pat$yob + 100, "-12-31")
gold_pat$year100 <- paste0(gold_pat$yob + 100, "-12-31")

# Define start of study period: Jan 1, 2005
# Define end of study period: May 5, 2025, since we are using June 2025 release
study_start_date <- as.Date("2005-01-01", format = "%Y-%m-%d")
study_end_date <- as.Date("2025-05-05", format = "%Y-%m-%d")

# Restrict to those aged 18-100 within the study period
# Removals: 
#   Aurum 339,599 (12%); 2,457,029 remaining
#   Gold 98,665 (11%); 808,215 remaining
#   Total 438,264; 3,265,244 remaining
aurum_pat <- aurum_pat %>%
  filter(year18 <= study_start_date & year100 >= study_end_date)
gold_pat <- gold_pat %>%
  filter(year18 <= study_start_date & year100 >= study_end_date)


#================== 5) Generate registration, Date18, follow-up variables ======

## Create variables for registration start date, registration end date,  
# date of last collection, and up-to-standard date (for Gold only)
# Convert to dates
aurum_pat <- aurum_pat %>%
  mutate_at(c("regstartdate", "regenddate", "lcd"), 
            ~as.Date(., format = "%d/%m/%Y"))

gold_pat <- gold_pat %>%
  mutate_at(c("regstartdate", "regenddate", "lcd", "uts"), 
            ~as.Date(., format = "%d/%m/%Y"))

# Check values
summary(aurum_pat$regenddate)
summary(gold_pat$regenddate)


## Create variables for the date that follow-up started, ended, and how many 
## days did that patient have follow up

# Date of valid research quality data post-registration: 365 days after the latest of 
# registration start and date at which data are considered adequate for research (Gold)
aurum_pat$date_valid <- pmax(aurum_pat$regstartdate, 
                             aurum_pat$uts, na.rm = TRUE) + 365
gold_pat$date_valid <- pmax(gold_pat$regstartdate, 
                            gold_pat$uts, na.rm = TRUE) + 365

# Follow-up start date: latest of date of valid research quality post-registration, 
# year turned 18, and start of study period
aurum_pat$startfollow <- pmax(aurum_pat$date_valid, 
                              aurum_pat$year18, 
                              study_start_date,  na.rm=TRUE)
gold_pat$startfollow <- pmax(gold_pat$date_valid, 
                             gold_pat$year18, 
                             study_start_date,  na.rm=TRUE)

# Follow-up end date: earliest of registration end, death, date of last collection,
# and end of study period.
aurum_pat$endfollow <- pmin(aurum_pat$regenddate, 
                            aurum_pat$deathdate, 
                            aurum_pat$lcd,
                            study_end_date,  na.rm=TRUE)
gold_pat$endfollow <- pmin(gold_pat$regenddate, 
                           gold_pat$deathdate, 
                           gold_pat$lcd,
                           study_end_date,  na.rm=TRUE)

# Days within follow-up period
aurum_pat$daysfollow <- as.numeric(aurum_pat$endfollow - aurum_pat$startfollow)
gold_pat$daysfollow <- as.numeric(gold_pat$endfollow - gold_pat$startfollow)

summary(aurum_pat$daysfollow)
summary(gold_pat$daysfollow)

# How many were not in the follow-up period: Aurum 229,179, Gold 88,829
sum(aurum_pat$daysfollow <= 0)
sum(gold_pat$daysfollow <= 0)

# Filter to those with at least 1 day of follow-up aged 18+ between 
# 2005/01/01 and 2025/05/05
# Aurum 2,227,850. Gold 719,386
aurum_pat_study <- aurum_pat %>% filter(daysfollow > 0)
gold_pat_study <- gold_pat %>% filter(daysfollow > 0)
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

#================== 5) Create region variable, combine, and save data ==========

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

# Total number of patients: 2,947,236
nrow(cohort_demog)

# Check if up-to-standard date is after follow-up start date
cohort_demog <- cohort_demog %>%
  mutate(up_to_standard = as.numeric(startfollow - as.Date(uts, format = "%d/%m/%Y")))

# How many patients have data quality issues? 0
sum(cohort_demog$acceptable == 0)

# How many patients have data that is not up to standard within follow-up? 0
sum(cohort_demog$up_to_standard < 0, na.rm = TRUE) # 0
sum(is.na(cohort_demog$up_to_standard)) # 2,227,850 
sum(is.na(cohort_demog$uts))

# # Save cohort data
# save(cohort_demog, file = paste0(wd, path_output, "cohort_demog.Rdata"))


### Remove data and shut down connection
dbDisconnect(connection, shutdown = TRUE)

