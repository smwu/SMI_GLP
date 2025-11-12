# ==============================================================================
# Clean dataset of patients with SMI diagnoses using linked mortality data
# Author: SM Wu
# Date Created: 2025/06/17
# Date Updated: 2025/06/17
# 
# Details:
# 1) Set up and load data
# 2) Harmonize datasets
# 3) Clean death date
# 4) Generate registration, Date18, follow-up variables
# 5) Create region variable, combine, and save data
#
# Inputs:
# 1) 2023 CPRD/GOLD/Patient/SMI_GOLD_Extract_Patient_001.txt: GOLD patient files
# 2) 2023 CPRD/GOLD/Practice/SMI_GOLD_Extract_Practice_001.txt: GOLD practice files
# 3) 2023 CPRD/Aurum/Patient/SMI_AURUM_Extract_Patient_001.txt: Aurum patient files
# 4) 2023 CPRD/Aurum/Practice/SMI_AURUM_Extract_Practice_001.txt: Aurum practice files
# 5) Code_Lists/MASTER_Lists/CPRD_Aurum_Medical_14Oct2025.txt: Aurum medical master code list
# 6) Code_Lists/MASTER_Lists/CPRD_GOLD_Medical_14Oct2025.txt: GOLD medical master code list
# 
# Final Outputs:
# 1) Stephanie/SMI_GLP/Data/Final_Data_Files/cohort_demog.Rdata: Cleaned Aurum and 
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
#     source: whether patient is from GOLD or Aurum
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

# Set working directory
wd <- "/Volumes/ritd-ag-project-rd00qv-jfhay18/" # VPN connection
# wd <- "//live.rd.ucl.ac.uk/ritd-ag-project-rd00qv-jfhay18/" #Desktop@UCL
setwd(wd)

# Set input and output paths
path_input <- "2023 CPRD/"
path_output <- "Stephanie/SMI_GLP/Data/Final_Data_Files/"

## Load data

# Read in patient and practice files from GOLD and Aurum
# Patient files contain: patid, gender, date of birth, marital, registration date, etc.
# Practice files contain: pracid, region, up-to-standard date, last collection date
# GOLD patient
gold_pat_raw <- read.table(
  file = paste0(wd, path_input, "GOLD/Patient/SMI_GOLD_Extract_Patient_001.txt"),
  header = TRUE, fill = TRUE, sep = "\t", quote = "", 
  colClasses = c(patid = "character"))
# GOLD practice
gold_prac_raw <- read.table(
  file = paste0(wd, path_input, "GOLD/Practice/SMI_GOLD_Extract_Practice_001.txt"),
  header = TRUE, sep = "\t", quote = "")

# Aurum patient
aurum_pat_raw <- read.table(
  file = paste0(wd, path_input, "Aurum/Patient/SMI_AURUM_Extract_Patient_001.txt"),
  header = TRUE, fill = TRUE, sep = "\t", quote = "", 
  colClasses = c(patid = "character"))
# Aurum practice
aurum_prac_raw <- read.table(
  file = paste0(wd, path_input, "Aurum/Practice/SMI_AURUM_Extract_Practice_001.txt"),
  header = TRUE, sep = "\t", quote = "")


# Read in Aurum medical dictionary
cprd_aurum_medical_raw <- 
  read_delim(
    paste0(wd, "Stephanie/SMI_GLP/Code_Lists/MASTER_Lists/",
           "CPRD_Aurum_Medical_14Oct2025.txt"), 
    delim = "\t", escape_double = FALSE, 
    col_types = cols(MedCodeId = col_character(), 
                     OriginalReadCode = col_character(), 
                     CleansedReadCode = col_character(), 
                     SnomedCTConceptId = col_character(), 
                     SnomedCTDescriptionId = col_character()), 
    trim_ws = TRUE)
cprd_aurum_medical <- cprd_aurum_medical_raw %>%
  select(-Release) %>%
  dplyr::rename(term = Term, medcodeid = MedCodeId) %>%
  mutate(term = str_to_lower(term))

# Read in Gold medical dictionary
cprd_gold_medical_raw <- 
  read_delim(
    paste0(wd, "Stephanie/SMI_GLP/Code_Lists/MASTER_Lists/",
           "CPRD_GOLD_Medical_14Oct2025.txt"), 
    delim = "\t", escape_double = FALSE, 
    col_types = cols(medcode = col_character(), 
                     readcode = col_character()), 
    trim_ws = TRUE) 
cprd_gold_medical <- cprd_gold_medical_raw %>%
  dplyr::rename(term = readterm) %>%
  mutate(term = str_to_lower(term))


# ================= 2) Harmonize datasets ======================================

gold_pat <- gold_pat_raw
gold_prac <- gold_prac_raw
aurum_pat <- aurum_pat_raw
aurum_prac <- aurum_prac_raw

# Generate practice ID for GOLD using the first four digits of the patient ID
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
gold_pat <- merge(x = gold_pat, y = gold_prac, by = "pracid", all.x = TRUE, 
                  all.y = TRUE)
aurum_pat<-merge(x = aurum_pat, y = aurum_prac, by = "pracid", all.x = TRUE, 
                 all.y = TRUE)

# Add in variable denoting dataset source
gold_pat$source <- "Gold"
aurum_pat$source <- "Aurum"

# Check missingness of mob: high missingness for aurum => remove
sum(is.na(gold_pat$mob)) / nrow(gold_pat)
sum(is.na(aurum_pat$mob)) / nrow(aurum_pat)

# Reorder and rename columns so datasets are comparable
colnames(aurum_pat)
colnames(gold_pat)

# Extra gold column to keep: toreason
gold_pat <- gold_pat %>%
  select(pracid, patid, gender, yob, crd, tod, deathdate,
         accept, region, lcd, uts, source, toreason) %>%
  rename(regstartdate = crd, 
         regenddate = tod,
         acceptable = accept)

# Extra aurum column to keep: emis_ddate
aurum_pat <- aurum_pat %>%
  select(pracid, patid, gender, yob, regstartdate, regenddate, 
         cprd_ddate, acceptable, region, lcd, uts, source, emis_ddate) %>%
  rename(deathdate = cprd_ddate) # set to the CPRD-curated death date

#================== 3) Clean death date ========================================

# Aurum

# If CPRD-curated death date is missing, check if emis_ddate is available 
# 1 death date filled in; 366,259 still no death date
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

#================== 4) Generate registration, Date18, follow-up variables ======

## Create variables for registration start date, registration end date, and 
# date of last collection
# Convert to dates
aurum_pat <- aurum_pat %>%
  mutate_at(c("regstartdate", "regenddate", "lcd"), 
            ~as.Date(., format = "%d/%m/%Y"))

gold_pat <- gold_pat %>%
  mutate_at(c("regstartdate", "regenddate", "lcd"), 
            ~as.Date(., format = "%d/%m/%Y"))

# Check values
summary(aurum_pat$regenddate)
summary(gold_pat$regenddate)


## Create variable for the year that the patient turned 18
aurum_pat$year18 <- paste0(aurum_pat$yob + 18, "-01-01")
gold_pat$year18 <- paste0(gold_pat$yob + 18, "-01-01")


## Create variables for the date that follow-up started, ended, and how many 
## days did that patient have follow up

# Define start of study period: Jan 1, 2005
# Define end of study period: May 5, 2025, since we are using June 2025 release
study_start_date <- as.Date("2005-01-01")
study_end_date <- as.Date("2025-05-05")

# Follow-up start date: latest of registration start, year turned 18, and start 
# of study period
aurum_pat$startfollow <- pmax(aurum_pat$regstartdate, 
                              aurum_pat$year18, 
                              study_start_date,  na.rm=TRUE)
gold_pat$startfollow <- pmax(gold_pat$regstartdate, 
                             gold_pat$year18, 
                             study_start_date,  na.rm=TRUE)

# Follow-up end date: earliest of registration end, death, and end of study period
aurum_pat$endfollow <- pmin(aurum_pat$regenddate, 
                            aurum_pat$deathdate, 
                            study_end_date,  na.rm=TRUE)
gold_pat$endfollow <- pmin(gold_pat$regenddate, 
                           gold_pat$deathdate, 
                           study_end_date,  na.rm=TRUE)

# Days within follow-up period
aurum_pat$daysfollow <- as.numeric(aurum_pat$endfollow - aurum_pat$startfollow)
gold_pat$daysfollow <- as.numeric(gold_pat$endfollow - gold_pat$startfollow)

# How many were not in the follow-up period: Aurum 44774 (10%), Gold 40472 (19%)
sum(aurum_pat$daysfollow <= 0)
sum(gold_pat$daysfollow <= 0)

# Filter to those with data in the follow-up period
aurum_pat_study <- aurum_pat %>% filter(daysfollow > 0)
gold_pat_study <- gold_pat %>% filter(daysfollow > 0)

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
table(pat_study_all$region, pat_study_all$source, useNA="ifany")
# See where the Aurum practices are from (mostly London and North West)
table(aurum_prac$region, useNA = "ifany")



## Sanity checks
cohort_demog <- pat_study_all

# Total number of patients: 557,266
nrow(cohort_demog)

# Check if up-to-standard date is after follow-up start date
cohort_demog <- cohort_demog %>%
  mutate(up_to_standard = as.numeric(startfollow - as.Date(uts, format = "%d/%m/%Y")))

# How many patients have data quality issues? 4240
sum(cohort_demog$acceptable == 0)

# How many patients have data that is not up to standard within follow-up? 403,293
sum(cohort_demog$up_to_standard < 0, na.rm = TRUE) # 18,794 
sum(is.na(cohort_demog$up_to_standard)) # 384,499 

sum(is.na(cohort_demog$uts))

# # Save cohort data
# save(cohort_demog, file = paste0(wd, path_output, "cohort_demog.Rdata"))

