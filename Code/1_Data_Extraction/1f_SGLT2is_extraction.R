# ==============================================================================
# Extract patients with SGLT-2i medications using code lists
# Author: SM Wu
# Date Created: 2025/06/26
# Date Updated: 2025/06/26
# 
# Details:
# 1) Set up and read in code lists
# 2) Read in CPRD GOLD data
# 3) Read in CPRD Aurum data
# 4) Add in look up information
# 5) Combine GOLD and Aurum and create data files
#
# Inputs:
# 1) Stephanie/SMI_GLP/Code_Lists/SGLT2is/Aurum_SGLT2is_codelist_20250626.txt: Updated Aurum SGLT2is code list
# 2) Stephanie/SMI_GLP/Code_Lists/SGLT2is/Gold_SGLT2is_codelist_20250626.txt: Updated GOLD SGLT2is code list
# 3) Stephanie/SMI_GLP/Code/1_Data_Extraction/helper_fns_data_extraction.R: Helper functions
# 4) 2023 CPRD/GOLD/ Therapy files
# 5) 2023 CPRD/Aurum/ DrugIssue files
# 6) 2023 CPRD/LookUps/202303_Lookups_CPRDGold/common_dosages.txt: GOLD common dosages
# 7) 2023 CPRD/LookUps/202303_Lookups_CPRDGold/bnfcodes.txt: GOLD BNF codes
# 8) 2023 CPRD/LookUps/202303_Lookups_CPRDGold/packtype.txt: GOLD pack types
# 9) 2023 CPRD/LookUps/202205_Lookups_CPRDAurum/common_dosages.txt: Aurum common dosages
# 10) 2023 CPRD/LookUps/202205_Lookups_CPRDAurum/QuantUnit.txt: Aurum quantity units
# 
# Intermediate outputs:
# 1) Stephanie/SMI_GLP/Data/Extraction_Files/pat_sglt2i_gold.RData: GOLD patient files for SGLT2is medication
# 2) Stephanie/SMI_GLP/Data/Extraction_Files/pat_sglt2i_aurum.RData: Aurum patient files for SGLT2is medication
# 
# Final Outputs:
# 1) Stephanie/SMI_GLP/Data/Extraction_Files/pat_sglt2i_comb.RData: Combined GOLD and Aurum patient files for SGLT2is medication

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
path_input <- "Stephanie/SMI_GLP/Code_Lists/SGLT2is/"
path_output <- "Stephanie/SMI_GLP/Data/"

# Load in helper functions
source(paste0(wd, "Stephanie/SMI_GLP/Code/1_Data_Extraction/",
              "helper_fns_data_extraction.R"))

## Read in final code lists used to define the CPRD data extraction

# GOLD code list
sglt2i_gold <- read_delim(
  file = paste0(wd, path_input, "Gold_SGLT2is_codelist_20250626.txt"), 
  delim = "\t", escape_double = FALSE, 
  col_types = cols(prodcode = col_character()),  trim_ws = TRUE) 

# AURUM code list
sglt2i_aurum <- read_delim(
  file = paste0(wd, path_input, "Aurum_SGLT2is_codelist_20250626.txt"), 
  delim = "\t", escape_double = FALSE, 
  col_types = cols(prodcodeid = col_character(),
                   BNFChapter = col_character()), 
  trim_ws = TRUE)



# ================= 2) Read in CPRD GOLD data ==================================

# GOLD THERAPY

# Get list of all .txt files in the GOLD/Therapy folder
gold_therapy_files <- list.files(path = paste0(wd, "2023 CPRD/GOLD/Therapy/"),
                                 pattern = "\\.txt$")

# Extract patient files matching conditions from code list
pat_sglt2i_gold_therapy <- read_obs_condition(
  file_path = paste0(wd, "2023 CPRD/GOLD/Therapy/"),
  file_names = gold_therapy_files,
  code_list = sglt2i_gold,
  database = "gold",
  medcode = FALSE)

# Create new column to indicate database
pat_sglt2i_gold <- pat_sglt2i_gold_therapy %>%
  mutate(database = "Gold")

# Number of unique patients with condition
n_distinct(pat_sglt2i_gold$patid) # 1,886

# # Save extracted patient files matching code list conditions 
# save(pat_sglt2i_gold,
#      file = paste0(wd, path_output, "Extraction_Files/pat_sglt2i_gold.RData"))


# ================= 3) Read in CPRD Aurum data ==================================

# AURUM CLINICAL

# Get list of all .txt files in the Aurum/DrugIssue folder
aurum_drug_files <- list.files(path = paste0(wd, "2023 CPRD/Aurum/DrugIssue/"),
                               pattern = "\\.txt$")

# Extract patient files matching conditions from code list
pat_sglt2i_aurum_drug <- read_obs_condition(
  file_path = paste0(wd, "2023 CPRD/Aurum/DrugIssue/"),
  file_names = aurum_drug_files,
  code_list = sglt2i_aurum,
  database = "aurum",
  medcode = FALSE)

# Create new column to indicate database
pat_sglt2i_aurum <- pat_sglt2i_aurum_drug %>%
  mutate(database = "Aurum")

# Number of unique patients with condition
n_distinct(pat_sglt2i_aurum$patid) # 5,281

# # Save extracted patient files matching code list conditions 
# save(pat_sglt2i_aurum,
#      file = paste0(wd, path_output, "Extraction_Files/pat_sglt2i_aurum.RData"))



# ================= 4) Add in look up information ==============================
# # Load extraction files if necessary
# load(file = paste0(wd, path_output, "Extraction_Files/pat_sglt2i_aurum.RData"))
# load(file = paste0(wd, path_output, "Extraction_Files/pat_sglt2i_gold.RData"))

## GOLD

# Read in look up files
common_dosages_g <- read.delim(
  file = paste0(wd, "/2023 CPRD/LookUps/202303_Lookups_CPRDGold/common_dosages.txt"))
bnfcodes <- read.delim(
  file = paste0(wd, "/2023 CPRD/LookUps/202303_Lookups_CPRDGold/bnfcodes.txt"))
packtype <- read.delim(
  file = paste0(wd, "/2023 CPRD/LookUps/202303_Lookups_CPRDGold/packtype.txt"))

# Standardise field names to AURUM and add in look up information
pat_sglt2i_gold_lookup <- pat_sglt2i_gold %>%
  rename(issuedate = eventdate,
         enterdate = sysdate,
         prodcodeid = prodcode,
         duration = numdays,
         quantity = qty) %>%
  # Add in common dosages
  left_join(common_dosages_g, by = "dosageid") %>%
  select(-dosageid) %>%
  # Add in BNF codes
  left_join(bnfcodes, by = "bnfcode") %>%
  select(-bnfcode, -consid) %>%
  mutate(bnf = as.character(bnf)) %>%
  # Add in pack type
  left_join(packtype, by = "packtype") %>%
  select(-packtype) %>%
  rename(packtype = packtype_desc)


## AURUM

# Read in look up files
common_dosages_a <- read.delim(
  file = paste0(wd, "/2023 CPRD/LookUps/202205_Lookups_CPRDAurum/common_dosages.txt"))
quantunit <- read.delim(
  file = paste0(wd, "/2023 CPRD/LookUps/202205_Lookups_CPRDAurum/QuantUnit.txt"))

# Add in look up information
pat_sglt2i_aurum_lookup <- pat_sglt2i_aurum %>%
  # Add in common dosages
  left_join(common_dosages_g, by = "dosageid") %>%
  select(-dosageid) %>%
  # Add in quantity unit
  left_join(quantunit, by = "quantunitid") %>%
  select(-quantunitid) %>%
  rename(packtype = Description)


# ================= 5) Combine GOLD and Aurum and create data files ============


# Combine GOLD and Aurum extracted patient files
pat_sglt2i_comb <- pat_sglt2i_aurum_lookup %>% 
  # Add in GOLD patient files
  bind_rows(pat_sglt2i_gold_lookup)

# Transform dates and exclude entries with invalid SGLT2is dates
# 0 excluded. 29,604 remaining
pat_sglt2i_comb <- transform_dates_meds(patient_data = pat_sglt2i_comb,
                                       earliest_date = '1900-01-01',
                                       latest_date = '2023-06-01')
# Rearrange columns, add Gold and Aurum identifiers to patid, and drop duplicates
pat_sglt2i_comb <- pat_sglt2i_comb %>%
  mutate(bnf = coalesce(bnf, BNFChapter)) %>%
  select(-BNFChapter, -drugdmd, -staffid) %>%
  select(patid, productname, issuedate, enterdate, database, everything()) %>%
  mutate(
    patid = case_when(
      database == "Gold" ~ paste0(patid, "-G"),
      database == "Aurum" ~ paste0(patid, "-A"),
      .default = patid)) %>%
  distinct()  # Removed 49 duplicates. 251,492 remaining


# Number of unique patients with condition
n_distinct(pat_sglt2i_comb$patid) # 7,167

# # Save patient data for GOLD and Aurum
# save(pat_sglt2i_comb,
#      file = paste0(wd, path_output, "Extraction_Files/pat_sglt2i_comb.RData"))




