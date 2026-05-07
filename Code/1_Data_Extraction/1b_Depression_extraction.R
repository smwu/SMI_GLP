# ==============================================================================
# Extract patients with Depression diagnoses using code lists
# Author: SM Wu
# Date Created: 2025/06/17
# Date Updated: 2025/11/25
# 
# Details:
# 1) Set up and read in code lists
# 2) Read in CPRD GOLD data
# 3) Read in CPRD Aurum data
# 4) Combine GOLD and Aurum and create data files
#
# Inputs:
# 1) ~/SMI_GLP/Code_Lists/Depression/Aurum_Depression_codelist_20250725.txt: Updated Aurum Depression code list
# 2) ~/SMI_GLP/Code_Lists/Depression/Gold_Depression_codelist_20250725.txt: Updated GOLD Depression code list
# 3) ~/SMI_GLP/Code/1_Data_Extraction/helper_fns_data_extraction.R: Helper functions
# 4) ~/GOLD/ Clinical, Test, and Referral files
# 5) ~/Aurum/ Observation files
# 
# Intermediate outputs:
# 1) ~/SMI_GLP/Data/Extraction_Files/pat_depr_gold.RData: GOLD patient files for Depression diagnosis
# 2) ~/SMI_GLP/Data/Extraction_Files/pat_depr_aurum.RData: Aurum patient files for Depression diagnosis
# 
# Final Outputs:
# 1) ~/SMI_GLP/Data/Extraction_Files/pat_depr_comb.RData: Combined GOLD and Aurum patient files for Depression diagnosis

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

# ### For running locally
# # Set working directory
# wd <- "/Volumes/ritd-ag-project-rd00qv-jfhay18/" # VPN connection
# # wd <- "//live.rd.ucl.ac.uk/ritd-ag-project-rd00qv-jfhay18/" #Desktop@UCL
# setwd(wd)
# 
# # Set input and output paths
# path_input <- "Stephanie/SMI_GLP/Code_Lists/Depression/"
# path_extract_gold <- "2023 CPRD/GOLD/"
# path_extract_aurum <- "2023 CPRD/Aurum/"
# path_output <- "Stephanie/SMI_GLP/Data/"
# 
# # Load in helper functions
# source(paste0(wd, "Stephanie/SMI_GLP/Code/1_Data_Extraction/",
#               "helper_fns_data_extraction.R"))


### For running in Data Safe Haven
# Set working directory
wd <- "S:/CDSTP_CPRD_25_005368/" 
setwd(wd)

# Set input and output paths
path_input <- "SMI_GLP/Code_Lists/Depression/"
path_extract_gold <- "GOLD/"
path_extract_aurum <- c("Aurum_1/", "Aurum_2/", "Aurum_3/")
path_output <- "SMI_GLP/Data/"

# Load in helper functions
source(paste0(wd, "SMI_GLP/Code/1_Data_Extraction/",
              "helper_fns_data_extraction.R"))

## Read in final code lists used to define the CPRD data extraction

# GOLD code list
gold_file_name <- list.files(path = paste0(wd, path_input),
                             pattern = "^Gold_Depression_codelist")
# Check date
gold_file_name
depr_gold <- read_delim(
  file = paste0(wd, path_input, gold_file_name), 
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character()),  trim_ws = TRUE) %>%
  select(medcode, term) %>%
  filter(medcode != 0)

# AURUM code list
aurum_file_name <- list.files(path = paste0(wd, path_input),
                              pattern = "^Aurum_Depression_codelist")
# Check date
aurum_file_name
depr_aurum <- read_delim(
  file = paste0(wd, path_input, aurum_file_name), 
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character()), trim_ws = TRUE)



# ================= 2) Read in CPRD GOLD data ==================================

# GOLD CLINICAL

# Get list of all .txt files in the GOLD/Clinical folder
gold_clin_files <- list.files(path = paste0(wd, path_extract_gold, "Clinical/"),
                              pattern = "\\.txt$")

# Extract patient files matching conditions from code list
pat_depr_gold_clin <- read_obs_condition(
  file_path = paste0(wd, path_extract_gold, "Clinical/"),
  file_names = gold_clin_files,
  code_list = depr_gold,
  database = "gold",
  medcode = TRUE)

# GOLD TEST

# Get list of all .txt files in the GOLD/Test folder
gold_test_files <- list.files(path = paste0(wd, path_extract_gold, "Test/"),
                              pattern = "\\.txt$")

# Extract patient files matching conditions from code list
pat_depr_gold_test <- read_obs_condition(
  file_path = paste0(wd, path_extract_gold, "Test/"),
  file_names = gold_test_files,
  code_list = depr_gold,
  database = "gold",
  medcode = TRUE)

# GOLD REFERRAL

# Get list of all .txt files in the GOLD/Referral folder
gold_ref_files <- list.files(path = paste0(wd, path_extract_gold, "Referral/"),
                             pattern = "\\.txt$")

# Extract patient files matching conditions from code list
pat_depr_gold_ref <- read_obs_condition(
  file_path = paste0(wd, path_extract_gold, "Referral/"),
  file_names = gold_ref_files,
  code_list = depr_gold,
  database = "gold",
  medcode = TRUE)

# MERGE ALL GOLD FILES TOGETHER
pat_depr_gold <- pat_depr_gold_clin %>%
  bind_rows(pat_depr_gold_test) %>%
  bind_rows(pat_depr_gold_ref)
# Select relevant columns and create new column to indicate database
pat_depr_gold <- pat_depr_gold %>%
  select(medcode, term, patid, eventdate, sysdate, constype, consid) %>%
  mutate(database = "Gold")

# Number of unique patients with condition
n_distinct(pat_depr_gold$patid) # 304,720

# # Save extracted patient files matching code list conditions 
save(pat_depr_gold,
     file = paste0(wd, path_output, "Extraction_Files/pat_depr_gold.RData"))

# Remove separate files to save memory
rm(pat_depr_gold_clin, pat_depr_gold_test, pat_depr_gold_ref)


# ================= 3) Read in CPRD Aurum data ==================================

# AURUM CLINICAL

# Number of Aurum folders
num_folders <- length(path_extract_aurum)

if (num_folders > 1) {
  
  # Initialize to allow for multiple folders
  pat_depr_aurum_all <- vector(mode = "list", length = num_folders)
  
  for (i in 1:num_folders) {
    path_extract_aurum_i <- path_extract_aurum[i]
    
    # Get list of all .txt files in the Aurum/Observation folder
    aurum_obs_files <- list.files(path = paste0(wd, path_extract_aurum_i, "Observation/"),
                                  pattern = "\\.txt$")
    
    # Extract patient files matching conditions from code list
    pat_depr_aurum_obs <- read_obs_condition(
      file_path = paste0(wd, path_extract_aurum_i, "Observation/"),
      file_names = aurum_obs_files,
      code_list = depr_aurum,
      database = "aurum",
      medcode = TRUE)
    
    # Select relevant columns and create new column to indicate database
    pat_depr_aurum_i <- pat_depr_aurum_obs %>%
      select(medcodeid, term, patid, obsdate, enterdate, obstypeid, consid, pracid) %>% 
      mutate(database = "Aurum")
    
    pat_depr_aurum_all[[i]] <- pat_depr_aurum_i
    
  }
  
  pat_depr_aurum <- dplyr::bind_rows(pat_depr_aurum_all)
  
} else {
  # Get list of all .txt files in the Aurum/Observation folder
  aurum_obs_files <- list.files(path = paste0(wd, path_extract_aurum, "Observation/"),
                                pattern = "\\.txt$")
  
  # Extract patient files matching conditions from code list
  pat_depr_aurum_obs <- read_obs_condition(
    file_path = paste0(wd, path_extract_aurum, "Observation/"),
    file_names = aurum_obs_files,
    code_list = depr_aurum,
    database = "aurum",
    medcode = TRUE)
  
  # Select relevant columns and create new column to indicate database
  pat_depr_aurum <- pat_depr_aurum_obs %>%
    select(medcodeid, term, patid, obsdate, enterdate, obstypeid, consid, pracid) %>% 
    mutate(database = "Aurum")
}

# Number of unique patients with condition
n_distinct(pat_depr_aurum$patid) # 910,000

# # Save extracted patient files matching code list conditions 
save(pat_depr_aurum,
     file = paste0(wd, path_output, "Extraction_Files/pat_depr_aurum.RData"))


# ================= 4) Combine GOLD and Aurum and create data files ============

# # Load extraction files if necessary
# load(file = paste0(wd, path_output, "Extraction_Files/pat_depr_aurum.RData"))
# load(file = paste0(wd, path_output, "Extraction_Files/pat_depr_gold.RData"))

# Combine GOLD and Aurum extracted patient files
pat_depr_comb <- pat_depr_aurum %>% 
  # Drop columns not in GOLD
  select(-obstypeid, -consid, -pracid) %>%
  # Standardise field names as per GOLD
  rename(eventdate = obsdate, 
         sysdate = enterdate,
         medcode = medcodeid) %>%
  # Add in GOLD patient files
  bind_rows(pat_depr_gold %>% select(-constype, -consid))

# Transform dates and exclude entries with invalid Depression dates
pat_depr_comb <- transform_dates(patient_data = pat_depr_comb,
                                earliest_date = '1900-01-01',
                                latest_date = '2025-06-01')
# Rearrange columns, add Gold and Aurum identifiers to patid, and drop duplicates
pat_depr_comb <- pat_depr_comb %>%
  select(patid, database, everything()) %>%
  mutate(
    patid = case_when(
      database == "Gold" ~ paste0(patid, "-G"),
      database == "Aurum" ~ paste0(patid, "-A"),
      .default = patid)) %>%
  distinct()  # Removed 363,522 duplicates. 7,666,090 remaining


# Number of unique patients with condition
n_distinct(pat_depr_comb$patid) # 1,214,409

# # Save patient data for GOLD and Aurum
save(pat_depr_comb,
     file = paste0(wd, path_output, "Extraction_Files/pat_depr_comb.RData"))




