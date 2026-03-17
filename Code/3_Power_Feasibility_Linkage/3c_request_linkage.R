# ====================================================================
# Request data linkage
# Author: SM Wu
# Date Created: 2026/03/03
# Date Updated: 2026/03/03
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
library(dbplyr)  # dplyr w/ SQL

# ============= Set up directories ===============================

# ### For running in Data Safe Haven
# Set working directory
wd <- "S:/CDSTP_CPRD_25_005368/"
setwd(wd)

# Set input and output paths
path_input <- paste0("SMI_GLP/Data/Extraction_Files/")
path_output <- paste0("SMI_GLP/Data/Extraction_Files/Linkage/")
path_gold <- "GOLD/"
path_aurum <- c("Aurum_1/", "Aurum_2/", "Aurum_3/")
path_linkage_gold <- "Linkage/October_2025_Source_GOLD/"
path_linkage_aurum <- "Linkage/January_2026_Source_Aurum/"


    # # DuckDB in-memory connection
    # connection <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    # 
    # # Allow spilling to local disk after hitting memory limit
    # DBI::dbExecute(connection, "PRAGMA memory_limit = '35GB';")
    # spill_dir <- "N:/Temp/duckdb_spill"
    # dir.create(spill_dir, showWarnings = FALSE, recursive = TRUE)
    # DBI::dbExecute(connection, sprintf("PRAGMA temp_directory='%s';", spill_dir))
    # 
    # # Set up progress bar
    # DBI::dbExecute(connection, "SET enable_progress_bar = true;")
    # DBI::dbExecute(connection, "SET enable_progress_bar_print = true;")


# ============= Load data ===============================

# Read in patient files from GOLD and Aurum
# Patient files contain: patid, gender, date of birth, marital, registration date, etc.

# GOLD patient
gold_pat_raw <- read.table(
  file = paste0(path_gold, "Patient/25_005368_SW_Extract_Patient_001.txt"),
  header = TRUE, fill = TRUE, sep = "\t", quote = "", 
  colClasses = c(patid = "character"))

# Aurum patient and practice
aurum_pat_list <- list() # patient
for (i in 1:length(path_aurum)) {
  aurum_pat_list[[i]] <- read.table(
    file = paste0(path_aurum[i], "Patient/25_005368_SW_Extract_Patient_001.txt"),
    header = TRUE, fill = TRUE, sep = "\t", quote = "", 
    colClasses = c(patid = "character"))
}
aurum_pat_raw <- do.call(rbind, aurum_pat_list)
rm(aurum_pat_list)

gold_pat <- gold_pat_raw
aurum_pat <- aurum_pat_raw

# Read in the linkage eligibility files
# Gold. 5 GB
gold_linkage <- read.table(
  file = paste0(path_linkage_gold, "GOLD_enhanced_eligibility_October_2025.txt"),
  header = TRUE, fill = TRUE, sep = ",", quote = "")
# Aurum. 9 GB
aurum_linkage <- read.table(
  file = paste0(path_linkage_aurum, "Aurum_enhanced_eligibility_January_2026.txt"),
  header = TRUE, fill = TRUE, sep = "\t", quote = "")

# ================= Get list of source population patids ======================================
# Note: This is the source population of any patients with T2DM or antidiabetic codes in the 
# study period, prior to applying any inclusion/exclusion criteria

# Generate practice ID for GOLD using the last five digits of the patient ID
gold_pat$pracid <- substring(gold_pat$patid, nchar(gold_pat$patid) - 4)

# Add in variable denoting dataset database
gold_pat$database <- "Gold"
aurum_pat$database <- "Aurum"

# Keep only the necessary columns
gold_pat <- gold_pat %>%
  select(pracid, patid, database)

# Extra aurum column to keep: emis_ddate
aurum_pat <- aurum_pat %>%
  select(pracid, patid, database)


### Subset linkage eligibility to HES APC, LSOA, and ONS
# Exploratory
table(gold_linkage$hes_apc_e, useNA = "always")
table(gold_linkage$ons_death_e, useNA = "always")
table(gold_linkage$lsoa_e, useNA = "always")

table(aurum_linkage$hes_apc_e, useNA = "always")
table(aurum_linkage$ons_death_e, useNA = "always")
table(aurum_linkage$lsoa_e, useNA = "always")

# Gold: 8,675,868 patients with linkage. Removed 2.2 million
gold_linkage_subset <- gold_linkage %>%
  filter(hes_apc_e == 1 & ons_death_e == 1 & lsoa_e == 1)

# Aurum: 53,805,276 patients with linkage. Removed 4.8 million
aurum_linkage_subset <- aurum_linkage %>%
  filter(hes_apc_e == 1 & ons_death_e == 1 & lsoa_e == 1)


### Filter T2DM patient population to those with linkage data

# Gold: 330,407 patients remaining. Removed 574,473 (64%)
gold_pat_linkage <- gold_pat %>%
  filter(patid %in% gold_linkage_subset$patid)

# Aurum: 2,676,671 patients remaining. Removed 119,957 (4%)
aurum_pat_linkage <- aurum_pat %>%
  filter(patid %in% aurum_linkage_subset$patid)


### Save list of patients for linkage
gold_pat_linkage_patids <- gold_pat_linkage %>% 
  select(patid) %>%
  left_join(gold_linkage_subset %>% 
              mutate(patid = as.character(patid)) %>%
              select(patid, hes_apc_e, ons_death_e, lsoa_e),
            by = "patid")
aurum_pat_linkage_patids <- aurum_pat_linkage %>% 
  select(patid) %>%
  left_join(aurum_linkage_subset %>% 
              mutate(patid = as.character(patid)) %>%
              select(patid, hes_apc_e, ons_death_e, lsoa_e),
            by = "patid")

length(unique(gold_pat_linkage_patids$patid))
length(unique(aurum_pat_linkage_patids$patid))
summary(gold_pat_linkage_patids)
summary(aurum_pat_linkage_patids)


protocol_number <- "25_005368"
organisation_name <- "UCL"
chunk_size <- 400000  # num patients per file

# Gold
n_gold <- length(unique(gold_pat_linkage_patids$patid))
num_gold_chunks <- ceiling(n_gold / chunk_size)
for (i in 1:num_gold_chunks) {
  start_id <- (1 + chunk_size * (i - 1))
  end_id <- min(i * chunk_size, n_gold)
  
  gold_chunk <- gold_pat_linkage_patids[start_id:end_id, ]
  
  write.table(gold_chunk, 
              file = paste0(path_output, protocol_number, "_", organisation_name, 
                            "_patientlist", "_Gold_", i, ".txt"),
              row.names = FALSE)
}


# Aurum
n_aurum <- length(unique(aurum_pat_linkage_patids$patid))
num_aurum_chunks <- ceiling(n_aurum / chunk_size)

for (i in 1:num_aurum_chunks) {
  start_id <- (1 + chunk_size * (i - 1))
  end_id <- min(i * chunk_size, n_aurum)
  
  aurum_chunk <- aurum_pat_linkage_patids[start_id:end_id, ]
  
  write.table(aurum_chunk, 
              file = paste0(path_output, protocol_number, "_", organisation_name, 
                            "_patientlist", "_Aurum_", i, ".txt"),
              row.names = FALSE)
}


