# ==============================================================================
# Extract patients with SMI diagnoses using code lists
# Author: SM Wu
# Date Created: 2025/06/16
# Date Updated: 2025/08/01
# 
# Details:
# 1) Set up and read in code lists
# 2) Read in CPRD GOLD data
# 3) Read in CPRD Aurum data
# 4) Combine GOLD and Aurum and create data files
#
# Inputs:
# 1) Stephanie/SMI_GLP/Code_Lists/SMI/Aurum_SMI_codelist_20250725.txt: Updated Aurum SMI code list
# 2) Stephanie/SMI_GLP/Code_Lists/SMI/Gold_SMI_codelist_20250725.txt: Updated GOLD SMI code list
# 3) Stephanie/SMI_GLP/Code/1_Data_Extraction/helper_fns_data_extraction.R: Helper functions
# 4) 2023 CPRD/GOLD/ Clinical, Test, and Referral files
# 5) 2023 CPRD/Aurum/ Observation files
# 
# Intermediate outputs:
# 1) Stephanie/SMI_GLP/Data/Extraction_Files/pat_smi_gold.RData: GOLD patient files for SMI diagnosis
# 2) Stephanie/SMI_GLP/Data/Extraction_Files/pat_smi_aurum.RData: Aurum patient files for SMI diagnosis
# 
# Final Outputs:
# 1) Stephanie/SMI_GLP/Data/Extraction_Files/pat_smi_comb.RData: Combined GOLD and Aurum patient files for SMI diagnosis

# ==============================================================================


# ================= 1) Set up and read in code lists ===========================

# Clear memory
rm(list = ls())

# Packages
library(dplyr)
library(gtsummary)
library(lubridate)
library(readr)
library(forcats)
library(data.table)
library(tidylog)

# Set working directory
wd <- "/Volumes/ritd-ag-project-rd00qv-jfhay18/" # VPN connection
# wd <- "//live.rd.ucl.ac.uk/ritd-ag-project-rd00qv-jfhay18/" #Desktop@UCL
setwd(wd)

# Set input and output paths
path_input <- "Stephanie/SMI_GLP/Code_Lists/SMI/"
path_output <- "Stephanie/SMI_GLP/Data/"

# Load in helper functions
source(paste0(wd, "Stephanie/SMI_GLP/Code/1_Data_Extraction/",
              "helper_fns_data_extraction.R"))

## Read in final code lists used to define the CPRD data extraction

# GOLD code list
smi_gold <- read_delim(
  file = paste0(wd, path_input, "Gold_SMI_codelist_20250725.txt"), 
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character()),trim_ws = TRUE) %>%
  rename(readterm = Term, group = Group) %>%
  select(medcode, readterm, group) %>%
  filter(medcode != 0)


# AURUM code list
smi_aurum <- read_delim(
  file = paste0(wd, path_input, "Aurum_SMI_codelist_20250725.txt"), 
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character(), SNOMED = col_character()),
  trim_ws = TRUE) %>%
  mutate(readterm = coalesce(TermRead, TermSNOMED, TermEMIS)) %>%
  select(medcodeid, readterm, group = Group) %>%
  filter(medcodeid != "Not in current release")


# ================= 2) Read in CPRD GOLD data ==================================

# GOLD CLINICAL

# Get list of all .txt files in the GOLD/Clinical folder
gold_clin_files <- list.files(path = paste0(wd, "2023 CPRD/GOLD/Clinical/"),
                              pattern = "\\.txt$")

# Extract patient files matching conditions from code list
pat_smi_gold_clin <- read_obs_condition(
  file_path = paste0(wd, "2023 CPRD/GOLD/Clinical/"),
  file_names = gold_clin_files,
  code_list = smi_gold,
  database = "gold",
  medcode = TRUE)

# GOLD TEST

# Get list of all .txt files in the GOLD/Test folder
gold_test_files <- list.files(path = paste0(wd, "2023 CPRD/GOLD/Test/"),
                              pattern = "\\.txt$")

# Extract patient files matching conditions from code list
pat_smi_gold_test <- read_obs_condition(
  file_path = paste0(wd, "2023 CPRD/GOLD/Test/"),
  file_names = gold_test_files,
  code_list = smi_gold,
  database = "gold",
  medcode = TRUE)

# GOLD REFERRAL

# Get list of all .txt files in the GOLD/Referral folder
gold_ref_files <- list.files(path = paste0(wd, "2023 CPRD/GOLD/Referral/"),
                             pattern = "\\.txt$")

# Extract patient files matching conditions from code list
pat_smi_gold_ref <- read_obs_condition(
  file_path = paste0(wd, "2023 CPRD/GOLD/Referral/"),
  file_names = gold_ref_files,
  code_list = smi_gold,
  database = "gold",
  medcode = TRUE)

# MERGE ALL GOLD FILES TOGETHER
pat_smi_gold <- pat_smi_gold_clin %>%
  bind_rows(pat_smi_gold_test) %>%
  bind_rows(pat_smi_gold_ref)
# Select relevant columns and create new column to indicate database
pat_smi_gold <- pat_smi_gold %>%
  select(medcode, readterm, group, patid, eventdate, sysdate) %>%
  mutate(database = "Gold")

# Number of unique patients with condition
n_distinct(pat_smi_gold$patid) # 213,239

# # Save extracted patient files matching code list conditions 
# save(pat_smi_gold,
#      file = paste0(wd, path_output, "Extraction_Files/pat_smi_gold.RData"))

# Remove separate files to save memory
rm(pat_smi_gold_clin, pat_smi_gold_test, pat_smi_gold_ref)


# ================= 3) Read in CPRD Aurum data ==================================

# AURUM CLINICAL

# Get list of all .txt files in the Aurum/Observation folder
aurum_obs_files <- list.files(path = paste0(wd, "2023 CPRD/Aurum/Observation/"),
                              pattern = "\\.txt$")

# Extract patient files matching conditions from code list
pat_smi_aurum_obs <- read_obs_condition(
  file_path = paste0(wd, "2023 CPRD/Aurum/Observation/"),
  file_names = aurum_obs_files,
  code_list = smi_aurum,
  database = "aurum",
  medcode = TRUE)

# Select relevant columns and create new column to indicate database
pat_smi_aurum <- pat_smi_aurum_obs %>%
  select(medcodeid, readterm, group, patid, obsdate, enterdate, pracid) %>% 
  mutate(database = "Aurum")

# Number of unique patients with condition
n_distinct(pat_smi_aurum$patid) # 429,273

# # Save extracted patient files matching code list conditions 
# save(pat_smi_aurum,
#      file = paste0(wd, path_output, "Extraction_Files/pat_smi_aurum.RData"))


# ================= 4) Combine GOLD and Aurum and create data files ============

# # Load extraction files if necessary
# load(file = paste0(wd, path_output, "Extraction_Files/pat_smi_aurum.RData"))
# load(file = paste0(wd, path_output, "Extraction_Files/pat_smi_gold.RData"))

# Combine GOLD and Aurum extracted patient files
pat_smi_comb <- pat_smi_aurum %>% 
  # Drop pracid column as it's not in GOLD
  select(-pracid) %>%
  # Standardise field names as per GOLD
  rename(eventdate = obsdate, 
         sysdate = enterdate,
         medcode = medcodeid) %>%
  # Add in GOLD patient files
  bind_rows(pat_smi_gold)

# Transform dates and exclude entries with invalid SMI dates
# 1,660 excluded. 6,071,771 remaining
pat_smi_comb <- transform_dates(patient_data = pat_smi_comb,
                                earliest_date = '1900-01-01',
                                latest_date = '2023-06-01')
# Rearrange columns, add Gold and Aurum identifiers to patid, and drop duplicates
pat_smi_comb <- pat_smi_comb %>%
  select(patid, database, everything()) %>%
  mutate(
    patid = case_when(
      database == "Gold" ~ paste0(patid, "-G"),
      database == "Aurum" ~ paste0(patid, "-A"),
      .default = patid)) %>%
  distinct()  # Removed 520,996 duplicates. 5,550,775 remaining


# Number of unique patients with condition
n_distinct(pat_smi_comb$patid) # 642,512

# # Save patient data for GOLD and Aurum
# save(pat_smi_comb, 
#      file = paste0(wd, path_output, "Extraction_Files/pat_smi_comb.RData"))




#================ Old Code =====================================================
# smi_gold_raw <- read_delim(
#   file = paste0(wd, path_input, "Gold_SMI_codelist_20250707.txt"), 
#   delim = "\t", escape_double = FALSE, 
#   col_types = cols(medcode = col_character(), 
#                    Read.code = col_skip()),  trim_ws = TRUE) %>%
#   rename(readterm = Term, group = Group) %>%
#   select(medcode, everything()) %>%
#   filter(medcode != 0)

# smi_aurum <- read_delim(
#   file = paste0(wd, path_input, "Aurum_SMI_codelist_20250707.txt"), 
#   delim = "\t", escape_double = FALSE, 
#   col_types = cols(medcodeid = col_character(), 
#                    EMIS = col_skip(),  
#                    READ = col_skip(), 
#                    SNOMED = col_skip()), trim_ws = TRUE) %>%
#   mutate(readterm = coalesce(TermSNOMED, TermRead, TermEMIS)) %>%
#   select(medcodeid, readterm, group = Group) %>%
#   filter(medcodeid != "Not in current release")
