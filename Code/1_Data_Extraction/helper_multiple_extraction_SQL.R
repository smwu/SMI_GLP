# ==============================================================================
# Extract patients using multiple code lists 
# Author: SM Wu
# Date Created: 2026/03/04
# Date Updated: 2026/03/04
# 
# Details:
# Given inputs 'code_names' and 'med_code_vec', allows for multiple code lists 
# to undergo data extraction sequentially using helper functions in 
# 'helper_extract_sql.R'. 
#
# ================================================================================


# Clear memory
rm(list = ls())

# Input arguments
earliest_date <- "1900-01-01"
latest_date <- "2025-12-31"

# Vector of code list folder names for which data extraction is desired
code_names <- c("Pregnancy")
# Boolean vector specifying if medcode (TRUE) or prodcode (FALSE)
med_code_vec <- c(TRUE)

# Packages
library(dplyr)
library(gtsummary)
library(lubridate)
library(readr)
library(forcats)
library(data.table)
library(tidylog)
library(DBI)     # database interface
library(duckdb)  # connect to SQL

### For running in Data Safe Haven
# Set working directory
wd <- "S:/CDSTP_CPRD_25_005368/"
setwd(wd)

# Load in helper functions
source(paste0(wd, "SMI_GLP/Code/1_Data_Extraction/",
              "helper_fns_data_extraction.R"))
source(paste0(wd, "SMI_GLP/Code/1_Data_Extraction/",
              "helper_extract_sql.R"))


# For each codelist, extract relevant medical or product files

for (i in 1:length(code_names)) {
  code_name <- code_names[i]
  med_code <- med_code_vec[i]
  
  # Set input and output paths
  path_input <- paste0("SMI_GLP/Code_Lists/", code_name, "/")
  path_gold <- "GOLD/"
  path_aurum <- c("Aurum_1/", "Aurum_2/", "Aurum_3/")
  path_output <- paste0("SMI_GLP/Data/Extraction_Files/", code_name, "/")
  
  # Create output directory if it doesn't already exist
  if (!dir.exists(path_output)) {
    dir.create(file.path(path_output))
  }
  
  print(paste0("Extracting data for ", code_name, "..."))
  
  # Extract medcode if med_code = TRUE, otherwise extract prodcode
  if (med_code) { # medcode
    
    # Additional paths
    path_extract_gold_clin <- paste0(wd, path_gold, "Clinical/")
    path_extract_gold_test <- paste0(wd, path_gold, "Test/")
    path_extract_gold_ref <- paste0(wd, path_gold, "Referral/")
    path_extract_aurum_obs <- paste0(wd, path_aurum, "Observation/")
    
    extract_patients_medcode(wd = wd, path_input = path_input, path_gold = path_gold, 
                             path_aurum = path_aurum, code_name = code_name,
                             path_extract_gold_clin = path_extract_gold_clin,
                             path_extract_gold_test = path_extract_gold_test,
                             path_extract_gold_ref = path_extract_gold_ref,
                             path_extract_aurum_obs = path_extract_aurum_obs,
                             save_rdata = TRUE)
    
  } else { # prodcode
    
    # Additional paths
    path_extract_gold_ther <- paste0(wd, path_gold, "Therapy/")
    path_extract_aurum_drug <- paste0(wd, path_aurum, "DrugIssue/")
    path_lookups_gold <- "Look ups/GOLD_Lookups_2025_12/"
    path_lookups_aurum <- "Look ups/202512_Lookups_CPRDAurum/"
    
    extract_patients_prodcode(wd = wd, path_input = path_input, path_gold = path_gold, 
                              path_aurum = path_aurum, code_name = code_name,
                              path_extract_gold_ther = path_extract_gold_ther,
                              path_extract_aurum_drug = path_extract_aurum_drug,
                              path_lookups_gold = path_lookups_gold,
                              path_lookups_aurum = path_lookups_aurum, 
                              save_rdata = TRUE)
  }
  

}
