# ==============================================================================
# Extract patients with T2DM diagnoses using code lists (SQL Version)
# Author: SM Wu
# Date Created: 2026/01/05
# Date Updated: 2026/01/19
# 
# Details:
# 1) Set up and read in code lists
# 2) Read in CPRD GOLD data
# 3) Read in CPRD Aurum data
# 4) Combine GOLD and Aurum and create data files
#
# Inputs:
# 1) ~/SMI_GLP/Code_Lists/T2Diabetes/Aurum_T2Diabetes_codelist_20250929.txt: Updated Aurum T2DM code list
# 2) ~/SMI_GLP/Code_Lists/T2Diabetes/Gold_T2Diabetes_codelist_20250929.txt: Updated GOLD T2DM code list
# 3) ~/SMI_GLP/Code/1_Data_Extraction/helper_fns_data_extraction.R: Helper functions
# 4) ~/GOLD/ Clinical, Test, and Referral files
# 5) ~/Aurum/ Observation files
# 
# Intermediate outputs:
# 1) ~/SMI_GLP/Data/Extraction_Files/pat_t2dm_gold.RData: GOLD patient files for T2DM diagnosis
# 2) ~/SMI_GLP/Data/Extraction_Files/pat_t2dm_aurum.RData: Aurum patient files for T2DM diagnosis
# 
# Final Outputs:
# 1) ~/SMI_GLP/Data/Extraction_Files/pat_t2dm_comb.RData: Combined GOLD and Aurum patient files for T2DM diagnosis

# ==============================================================================


# ================= 1) Set up and read in code lists ===========================

# Clear memory
rm(list = ls())

# Input arguments
earliest_date <- "1900-01-01"
latest_date <- "2025-06-01"
code_name <- "T2Diabetes"

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

# # ### For running locally
# # Set working directory
# wd <- "/Volumes/ritd-ag-project-rd00qv-jfhay18/" # VPN connection
# setwd(wd)
# 
# # Set input and output paths
# path_input <- paste0("Stephanie/SMI_GLP/Code_Lists/", code_name, "/")
# path_gold <- "2023 CPRD/GOLD/"
# path_aurum <- "2023 CPRD/Aurum/"
# path_output <- paste0("Stephanie/SMI_GLP/Data/Extraction_Files/", code_name, "/")

# ### For running in Data Safe Haven
# Set working directory
wd <- "S:/CDSTP_CPRD_25_005368/"
setwd(wd)

# Set input and output paths
path_input <- paste0("SMI_GLP/Code_Lists/", code_name, "/")
path_gold <- "GOLD/"
path_aurum <- c("Aurum_1/", "Aurum_2/", "Aurum_3/")
path_output <- paste0("SMI_GLP/Data/Extraction_Files/", code_name, "/")

# Create output directory if it doesn't already exist
if (!dir.exists(path_output)) {
  dir.create(file.path(path_output))
}

# Additional paths

path_extract_gold_clin <- paste0(wd, path_gold, "Clinical/")
path_extract_gold_test <- paste0(wd, path_gold, "Test/")
path_extract_gold_ref <- paste0(wd, path_gold, "Referral/")
path_extract_aurum_obs <- paste0(wd, path_aurum, "Observation/")

# Globs for DuckDB
gold_clin_glob <- file.path(path_extract_gold_clin, "*.txt")
gold_test_glob <- file.path(path_extract_gold_test, "*.txt")
gold_ref_glob  <- file.path(path_extract_gold_ref,  "*.txt")
aurum_obs_glob <- file.path(path_extract_aurum_obs, "*.txt")

# Load in helper functions
source(paste0(wd, "SMI_GLP/Code/1_Data_Extraction/",
              "helper_fns_data_extraction.R"))


## Read in final code lists used to define the CPRD data extraction

# GOLD code list
gold_file_name <- list.files(path = paste0(wd, path_input),
                             pattern = paste0("^Gold_", code_name, "_codelist"))
# Check date
gold_file_name
codelist_gold <- read_delim(
  file = paste0(wd, path_input, gold_file_name), 
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character()),  trim_ws = TRUE) %>%
  select(medcode, term) %>%
  filter(medcode != 0)


# AURUM code list
aurum_file_name <- list.files(path = paste0(wd, path_input),
                              pattern = paste0("^Aurum_", code_name, "_codelist"))
# Check date
aurum_file_name
codelist_aurum <- read_delim(
  file = paste0(wd, path_input, aurum_file_name), 
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character()), trim_ws = TRUE)


# ================= 2) Use SQL for extraction ==================================
# DuckDB in-memory connection
connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# Set up progress bar
DBI::dbExecute(connection, "SET enable_progress_bar = true;")
DBI::dbExecute(connection, "SET enable_progress_bar_print = true;")
# Start timer
start_time <- Sys.time()

# Write code lists
gold_codes_table  <- paste0(code_name, "_gold_codes")
aurum_codes_table <- paste0(code_name, "_aurum_codes")
dbWriteTable(connection, gold_codes_table,  codelist_gold,  overwrite = TRUE)
dbWriteTable(connection, aurum_codes_table, codelist_aurum, overwrite = TRUE)

# Check tables
dbListTables(connection)

# Sanity check: column names
# GOLD Clinical 
gold_clin_files <- list.files(path = path_extract_gold_clin,
                              pattern = "\\.txt$", full.names = TRUE)
header_line <- readLines(gold_clin_files[1], n = 1, warn = FALSE)
strsplit(header_line, "\t", fixed = TRUE)[[1]]

# Aurum Observation
aurum_obs_files <- list.files(path = path_extract_aurum_obs[1],
                              pattern = "\\.txt$", full.names = TRUE)
header_line <- readLines(aurum_obs_files[1], n = 1, warn = FALSE)
strsplit(header_line, "\t", fixed = TRUE)[[1]]

# How many files
length(list.files(path = path_extract_gold_clin, pattern = "\\.txt$", 
                  full.names = TRUE))
length(list.files(path = path_extract_gold_test, pattern = "\\.txt$", 
                  full.names = TRUE))
length(list.files(path = path_extract_gold_ref, pattern = "\\.txt$", 
                  full.names = TRUE))
for (i in 1:length(path_extract_aurum_obs)) {
  print(length(list.files(path = path_extract_aurum_obs[i], pattern = "\\.txt$", 
                    full.names = TRUE)))
}


# ================= 2) Read in CPRD GOLD data ==================================

# Progress bar (base R)
steps <- c(
  "GOLD Clinical Raw",
  "GOLD Test Raw",
  "GOLD Referral Raw",
  "Combine GOLD Raw",
  "AURUM Observation Raw",
  "Combine AURUM Raw",
  "Clean GOLD",
  "Clean AURUM",
  "Combine Clean Gold and Aurum"
)
pb <- txtProgressBar(min = 0, max = length(steps), style = 3)
i <- 0
tick <- function(msg) { i <<- i + 1; setTxtProgressBar(pb, i); cat("\n", msg, "\n", sep="") }


# GOLD CLINICAL

gold_extract_sql(connection = connection, 
                 out_table = "gold_clin_raw", 
                 files_glob = gold_clin_glob,
                 gold_codes_table = gold_codes_table, 
                 source_label = "Clinical",
                 code_kind = "medcode")
tick("Completed GOLD Clinical extract.")
# Runtime check
Sys.time() - start_time

# GOLD TEST
gold_extract_sql(connection = connection, 
                 out_table = "gold_test_raw", 
                 files_glob = gold_test_glob,
                 gold_codes_table = gold_codes_table, 
                 source_label = "Test",
                 code_kind = "medcode")
tick("Completed GOLD Test extract.")
# Runtime check
Sys.time() - start_time

# GOLD REFERRAL
gold_extract_sql(connection = connection, 
                 out_table = "gold_ref_raw", 
                 files_glob = gold_ref_glob,
                 gold_codes_table = gold_codes_table, 
                 source_label = "Referral",
                 code_kind = "medcode")
tick("Completed GOLD Referral extract.")
# Runtime check
Sys.time() - start_time

# MERGE ALL GOLD FILES TOGETHER

DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE pat_gold_raw AS
  SELECT * FROM gold_clin_raw
  UNION ALL SELECT * FROM gold_test_raw
  UNION ALL SELECT * FROM gold_ref_raw;
")

# Count number of patients in Gold before transforming dates
gold_raw_by_source <- DBI::dbGetQuery(connection, "
  SELECT source,
         COUNT(*) AS n_rows,
         COUNT(DISTINCT patid) AS n_patids
  FROM pat_gold_raw
  GROUP BY source
  ORDER BY source;
")
print(gold_raw_by_source)

# Save raw extracted patient files matching code list conditions as parquet
out_gold_raw_parquet <- paste0(path_output, "pat_gold_raw.parquet")
dbExecute(connection, 
          sprintf("COPY pat_gold_raw TO '%s' (FORMAT parquet);", 
                  out_gold_raw_parquet))

tick("Combined GOLD Raw")
# Runtime check
Sys.time() - start_time

# ================= 3) Read in CPRD Aurum data ==================================

# AURUM OBSERVATION

# Number of Aurum folders
num_folders <- length(aurum_obs_glob)
# Initialize output table names and destinations for multiple folders
aurum_out_table_names <- character(length(aurum_obs_glob))
aurum_out_save_names <- character(length(aurum_obs_glob))

for (i in 1:num_folders) {
  
  out_tbl <- paste0("aurum_obs_raw_", i)
  
  aurum_extract_sql(connection = connection, 
                    out_table = out_tbl, 
                    files_glob = aurum_obs_glob[i],
                    aurum_codes_table = aurum_codes_table, 
                    source_label = "Observation",
                    code_kind = "medcodeid")
  
  
  # Save partial extracted patient files parquet 
  out_aurum_raw_parquet_i <- paste0(path_output, "pat_aurum_raw_", i, ".parquet")
  DBI::dbExecute(connection, sprintf(
    "COPY %s TO '%s' (FORMAT parquet);", 
    as.character(DBI::dbQuoteIdentifier(connection, out_tbl)),
    out_aurum_raw_parquet_i
  ))
  
  aurum_out_table_names[i] <- out_tbl
  aurum_out_save_names[i] <- out_aurum_raw_parquet_i
  
  tick(paste0("Completed Aurum Observation extract part ", i))
  
  # Drop the per-folder table from duckdb to free memory
  DBI::dbExecute(connection, sprintf(
    "DROP TABLE IF EXISTS %s;",
    as.character(DBI::dbQuoteIdentifier(connection, out_tbl))
  ))
}


# Runtime check
Sys.time() - start_time

# Read all exported Parquet files back as one view/table inside duckDB (does not load all into R)
# Merge all Aurum files from separate folders into one table
parquet_glob <- paste0(path_output, "pat_aurum_raw_*.parquet")
DBI::dbExecute(connection, sprintf("
  CREATE OR REPLACE TABLE pat_aurum_raw AS
  SELECT * FROM read_parquet('%s');
", parquet_glob))

# Count number of patients in Aurum before transforming dates
aurum_raw_by_source <- DBI::dbGetQuery(connection, "
  SELECT source,
         COUNT(*) AS n_rows,
         COUNT(DISTINCT patid) AS n_patids
  FROM pat_aurum_raw
  GROUP BY source
  ORDER BY source;
")
print(aurum_raw_by_source)

# Save raw extracted patient files matching code list conditions as parquet
out_aurum_raw_parquet <- paste0(path_output, "pat_aurum_raw.parquet")
dbExecute(connection, 
          sprintf("COPY pat_aurum_raw TO '%s' (FORMAT parquet);", 
                  out_aurum_raw_parquet))

tick("Combined Aurum Raw")
# Runtime check
Sys.time() - start_time

# ================= 4) Combine GOLD and Aurum and create data files ============


## Clean dates and filter out those with invalid dates

# Gold
transform_dates_sql(connection = connection, 
                    in_table = "pat_gold_raw", 
                    out_table = "pat_gold_clean",
                    earliest_date = earliest_date,
                    latest_date = latest_date,
                    code_kind = "medcode")
tick("Cleaned GOLD")
# Runtime check
Sys.time() - start_time

# Aurum
transform_dates_sql(connection = connection, 
                    in_table = "pat_aurum_raw", 
                    out_table = "pat_aurum_clean",
                    earliest_date = earliest_date,
                    latest_date = latest_date,
                    code_kind = "medcode")
tick("Cleaned Aurum")
# Runtime check
Sys.time() - start_time

# Rearrange columns, add Gold and Aurum identifiers to patid, and drop duplicates
# Note: output is number of rows in newly created table
DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE pat_gold_final AS
  SELECT DISTINCT
    patid || '-G' AS patid,
    medcode, term, eventdate, sysdate, constype, consid, database, source
  FROM pat_gold_clean;
")

DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE pat_aurum_final AS
  SELECT DISTINCT
    patid || '-A' AS patid,
    medcode, term, eventdate, sysdate, constype, consid, database, source
  FROM pat_aurum_clean;
")

# Combine GOLD and Aurum extracted patient files
DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE pat_comb_final AS
  SELECT DISTINCT * FROM (
    SELECT 
      medcode, term, patid, eventdate, sysdate, constype, consid, database, source
    FROM pat_gold_final
    UNION ALL
    SELECT 
      medcode, term, patid, eventdate, sysdate, constype, consid, database, source
    FROM pat_aurum_final
  ) x;
")

## Number of unique patients with condition

# Gold
cat("\nGold rows / patients:\n")
print(dbGetQuery(
  connection, 
  "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_gold_final;"))

# Aurum
cat("\nAURUM rows / patients:\n")
print(dbGetQuery(
  connection, 
  "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_aurum_final;"))

# Total
cat("\nTotal rows / patients:\n")
print(dbGetQuery(
  connection, 
  "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_comb_final;"))


## Save patient data for GOLD and Aurum as parquet file
out_comb_final_parquet <- paste0(path_output, "pat_comb_final.parquet")
dbExecute(connection, 
          sprintf("COPY pat_comb_final TO '%s' (FORMAT parquet);", 
                  out_comb_final_parquet))
tick("Combined Cleaned GOLD and Aurum")
# Runtime check
Sys.time() - start_time

# Close the progress bar
close(pb)
# Check how much memory was used
gc()
# Check tables
dbListTables(connection)

# # Optional: pull into R and save as .RData (but this can be huge file)
parquet_file <- paste0(path_output, "pat_comb_final.parquet")
pat_comb_final <- DBI::dbGetQuery(connection, sprintf(
  "SELECT * FROM read_parquet('%s');", 
  parquet_file))
save(pat_comb_final,
     file = paste0(path_output, "pat_comb_final.RData"))
# rm(pat_comb_final_df)

### Remove data and shut down connection
dbDisconnect(connection, shutdown = TRUE)
cat("\nDone.\n")

