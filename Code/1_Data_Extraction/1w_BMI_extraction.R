# ==============================================================================
# Extract patients with BMI and Weight information using code lists (SQL Version)
# Author: SM Wu
# Date Created: 2026/06/01
# Date Updated: 2026/06/01
# 
# Details:
# 1) Set up and read in code lists
# 2) Read in CPRD GOLD data
# 3) Read in CPRD Aurum data
# 4) Transform dates
# 5) Add in lookup information
# 6) Combine GOLD and Aurum and create data files
#   GOLD Additional contains measurement values.
#   GOLD Clinical contains event dates and links to Additional via adid.
#
# Inputs:
# 1) ~/SMI_GLP/Code_Lists/Glucose/Aurum_BMI_codelist_20260206.txt: Updated Aurum glucose code list
# 2) ~/SMI_GLP/Code_Lists/Glucose/Gold_BMI_codelist_20260206.txt: Updated GOLD glucose code list
# 3) ~/SMI_GLP/Code/1_Data_Extraction/helper_fns_data_extraction.R: Helper functions
# 4) ~/GOLD/ Clinical, Test, and Referral files
# 5) ~/Aurum/ Observation files
# 
# Final Outputs:
# 1) ~/SMI_GLP/Data/Extraction_Files/pat_bmi_comb.RData: Combined GOLD and Aurum patient files for glucose diagnosis

# ==============================================================================


# ================= 1) Set up and read in code lists ===========================

# Clear memory
rm(list = ls())
gc()

# Input arguments
earliest_date <- "1900-01-01"
latest_date <- "2025-06-01"
code_name <- "BMI"

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
path_extract_gold_add <- paste0(wd, path_gold, "Additional/")
path_extract_aurum_obs <- paste0(wd, path_aurum, "Observation/")
path_lookups_gold <- "Lookups/202506_Lookups_GOLD2025_09/"
path_lookups_aurum <- "Lookups/202506_Lookups_CPRDAurum/"

# Globs for DuckDB
gold_clin_glob <- file.path(path_extract_gold_clin, "*.txt")
gold_test_glob <- file.path(path_extract_gold_test, "*.txt")
gold_ref_glob  <- file.path(path_extract_gold_ref,  "*.txt")
gold_add_glob <- file.path(path_extract_gold_add, "*.txt")
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

# ----------------------------
# Helper functions

sql_quote_vec <- function(x) {
  paste0("'", gsub("'", "''", normalizePath(x, winslash = "/", mustWork = TRUE)), "'", collapse = ", ")
}

find_txt_files <- function(directory) {
  files <- list.files(directory, pattern = "\\.txt$", full.names = TRUE)
  
  if (length(files) == 0) {
    stop("No .txt files found in: ", directory)
  }
  
  files
}

duckdb_read_csv_sql <- function(files) {
  # all_varchar = true makes ingestion robust to mixed CPRD field types.
  # We cast only the columns needed downstream.
  paste0(
    "read_csv_auto([", sql_quote_vec(files), "], ",
    "delim='\\t', ",
    "header=true, ",
    "all_varchar=true, ",
    "ignore_errors=true, ",
    "union_by_name=true)"
  )
}

# ================= 2) Use SQL for extraction ==================================
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

# Quick peek
DBI::dbGetQuery(connection, "DESCRIBE gold_clin_raw") 
DBI::dbGetQuery(connection, "DESCRIBE gold_test_raw") 
DBI::dbGetQuery(connection, "DESCRIBE gold_ref_raw") 

# MERGE ALL GOLD FILES TOGETHER

DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE pat_gold_raw AS
  SELECT 
    medcode, term, patid, eventdate, sysdate, constype, consid, database, source,
    enttype, adid, data1, data2, data3 
  FROM (
    SELECT
      medcode, term, patid, eventdate, sysdate, constype, consid, database, source, 
      enttype, adid,
      NULL::VARCHAR AS data1,
      NULL::VARCHAR AS data2,
      NULL::VARCHAR AS data3
    FROM gold_clin_raw
    
    UNION ALL
    
    SELECT
      medcode, term, patid, eventdate, sysdate, constype, consid, database, source,
      enttype, 
      NULL::VARCHAR AS adid,
      data1, data2, data3
    FROM gold_test_raw
    
    UNION ALL
    
    SELECT
      medcode, term, patid, eventdate, sysdate, constype, consid, database, 
      'Referral' AS source,
      NULL::VARCHAR AS enttype,
      NULL::VARCHAR AS adid,
      NULL::VARCHAR AS data1,
      NULL::VARCHAR AS data2,
      NULL::VARCHAR AS data3
    FROM gold_ref_raw
  );
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
  
  sql <- sprintf("
    -- Read Aurum raw files
    
    CREATE OR REPLACE TEMP VIEW aurum_rawfile AS
    SELECT * FROM read_csv('%1$s',
      delim='\\t', 
      header=true, 
      all_varchar=true,   -- all columns as characters
      null_padding=true,  -- pad shorter rows
      strict_mode=false,  -- tolerate irregular rows
      quote='', escape=''
    );
    
    
    -- Create out_tbl, keeping only rows matching code list medcode (from code_tbl)
    -- Attach term from code list
    -- Also add in database identifier and source type
    
    CREATE OR REPLACE TABLE %2$s AS
    SELECT
      trim(a.medcodeid) AS medcode,
      c.term          AS term,
      a.patid         AS patid,
      a.obsdate       AS eventdate,   -- keep as TEXT for now
      a.enterdate     AS sysdate,     -- keep as TEXT for now
      a.obstypeid     AS constype,
      a.value         AS value,
      a.numunitid     AS numunitid,
      'Aurum'         AS database,
      '%3$s'          AS source
    FROM aurum_rawfile a
    INNER JOIN %4$s c
      ON trim(a.medcodeid) = trim(c.medcodeid);
  ", 
                 aurum_obs_glob[i], 
                 out_tbl, 
                 "Observation", 
                 aurum_codes_table
  )
  DBI::dbExecute(connection, sql)
  
  
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

# Drop tables to free up memory
DBI::dbExecute(connection, "
  DROP TABLE IF EXISTS gold_clin_raw;
  DROP TABLE IF EXISTS gold_test_raw;
  DROP TABLE IF EXISTS gold_ref_raw;
  DROP TABLE IF EXISTS aurum_obs_raw;
")

tick("Combined Aurum Raw")
# Runtime check
Sys.time() - start_time


# ================= 4) Transform dates =========================================

# # Read in data if necessary
# out_gold_raw_parquet <- paste0(path_output, "pat_gold_raw.parquet")
# DBI::dbExecute(connection, sprintf("
#   CREATE OR REPLACE TABLE pat_gold_raw AS
#   SELECT * FROM read_parquet('%s');
# ", out_gold_raw_parquet))
# out_aurum_raw_parquet <- paste0(path_output, "pat_aurum_raw.parquet")
# DBI::dbExecute(connection, sprintf("
#   CREATE OR REPLACE TABLE pat_aurum_raw AS
#   SELECT * FROM read_parquet('%s');
# ", out_aurum_raw_parquet))

## Clean dates and filter out those with invalid dates

# Gold
transform_dates_sql_medcode2(connection = connection, 
                             in_table = "pat_gold_raw", 
                             out_table = "pat_gold_clean",
                             earliest_date = earliest_date,
                             latest_date = latest_date)

tick("Cleaned GOLD")
# Runtime check
Sys.time() - start_time

# Aurum
transform_dates_sql_medcode2(connection = connection, 
                             in_table = "pat_aurum_raw", 
                             out_table = "pat_aurum_clean",
                             earliest_date = earliest_date,
                             latest_date = latest_date)
tick("Cleaned Aurum")
# Runtime check
Sys.time() - start_time

# Drop tables to free up memory
DBI::dbExecute(connection, "
  DROP TABLE IF EXISTS pat_gold_raw;
  DROP TABLE IF EXISTS pat_aurum_raw;
")

# Rearrange columns, add Gold and Aurum identifiers to patid, and drop duplicates
# Note: output is number of rows in newly created table
DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE pat_gold_final AS
  SELECT DISTINCT
    patid || '-G' AS patid,
    medcode, term, eventdate, sysdate, constype, consid, database, source, 
    enttype, adid, data1, data2, data3,
  FROM pat_gold_clean;
")

DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE pat_aurum_final AS
  SELECT DISTINCT
    patid || '-A' AS patid,
    medcode, term, eventdate, sysdate, constype, database, source,
    value, numunitid,
  FROM pat_aurum_clean;
")

# Drop tables to free up memory
DBI::dbExecute(connection, "
  DROP TABLE IF EXISTS pat_gold_clean;
  DROP TABLE IF EXISTS pat_aurum_clean;
")

# ================= 5) Add in look up information ==============================

# Lookup file paths
f_entity <- paste0(path_lookups_gold, "entity.txt")
f_sed <- paste0(path_lookups_gold, "TXTFILES/SED.txt") # medical entry
f_tqu <- paste0(path_lookups_gold, "TXTFILES/TQU.txt")  # test qualifier
f_sum <- paste0(path_lookups_gold, "TXTFILES/SUM.txt")  # specimen unit of measure

f_obstype <- paste0(path_lookups_aurum, "ObsType.txt") # observation type
f_numunit <- paste0(path_lookups_aurum, "NumUnit.txt") # unit of measurement

## GOLD

sql_lookup_g <- sprintf("
  -- Read in look up files
  -- Entity
  CREATE OR REPLACE TEMP VIEW entity AS
  SELECT *
  FROM read_csv('%1$s', 
    delim='\\t', 
    header=true, 
    all_varchar=true,   -- all columns as characters
    null_padding=true,  -- pad shorter rows
    strict_mode=false,  -- tolerate irregular rows
    quote='', escape=''
  );
  
  -- SED
  CREATE OR REPLACE TEMP VIEW sed AS
  SELECT *
  FROM read_csv('%2$s', 
    delim='\\t', 
    header=true, 
    all_varchar=true,   -- all columns as characters
    null_padding=true,  -- pad shorter rows
    strict_mode=false,  -- tolerate irregular rows
    quote='', escape=''
  );
  
  -- TQU
  CREATE OR REPLACE TEMP VIEW tqu AS
  SELECT *
  FROM read_csv('%3$s', 
    delim='\\t', 
    header=true, 
    all_varchar=true,   -- all columns as characters
    null_padding=true,  -- pad shorter rows
    strict_mode=false,  -- tolerate irregular rows
    quote='', escape=''
  );
  
  -- SUM
  CREATE OR REPLACE TEMP VIEW sum AS
  SELECT *
  FROM read_csv('%4$s', 
    delim='\\t', 
    header=true, 
    all_varchar=true,   -- all columns as characters
    null_padding=true,  -- pad shorter rows
    strict_mode=false,  -- tolerate irregular rows
    quote='', escape=''
  );
  
  -- Standardise field names and add in look up information
  CREATE OR REPLACE TABLE pat_gold_lookup AS
  SELECT
    -- choose final columns to exclude/rename from lookup files
    -- entity columns: description (renamed to 'enttype_description), category
    -- sed columns: Medical Entry (renamed to 'constype')
    -- tqu columns: Test Qualifier (renaed to 'test_qualifier')
    -- sum columns: Specimen Unit of Measure (renamed to 'unit')
    g.* EXCLUDE (constype, enttype, data1, data3, adid),
    g.data2 AS value,
    e.description AS enttype_description,
    e.category,
    sed.\"Medical Entry\" AS constype,
    tqu.\"Test Qualifier\" AS test_qualifier,
    sum.\"Specimen Unit of Measure\" AS unit
    
  -- Merge in lookup information (note that all columns are available for merging)
  FROM pat_gold_final g
  LEFT JOIN entity e ON g.enttype = e.enttype
  LEFT JOIN sed ON g.constype = sed.Code
  LEFT JOIN tqu ON g.data1 = tqu.Code
  LEFT JOIN sum ON g.data3 = sum.Code;
", f_entity, f_sed, f_tqu, f_sum)

# Execute SQL code for lookups
DBI::dbExecute(connection, sql_lookup_g)


## AURUM

sql_lookup_a <- sprintf("
  -- Read in look up files
  -- Common dosages
  CREATE OR REPLACE TEMP VIEW obstype AS
  SELECT *
  FROM read_csv('%1$s', 
    delim='\\t', 
    header=true, 
    all_varchar=true,   -- all columns as characters
    null_padding=true,  -- pad shorter rows
    strict_mode=false,  -- tolerate irregular rows
    quote='', escape=''
  );
  
  -- Quantity unit
  CREATE OR REPLACE TEMP VIEW numunit AS
  SELECT *
  FROM read_csv('%2$s', 
    delim='\\t', 
    header=true, 
    all_varchar=true,   -- all columns as characters
    null_padding=true,  -- pad shorter rows
    strict_mode=false,  -- tolerate irregular rows
    quote='', escape=''
  );
  
  -- Add in look up information
  CREATE OR REPLACE TABLE pat_aurum_lookup AS
  SELECT
    -- choose final columns to exclude/rename from lookup files
    -- obstype columns: Description (renamed to 'constype')
    -- numunit columns: Description (renamed to 'unit')
    a.* EXCLUDE (constype, numunitid),
    o.Description AS constype,
    n.Description AS unit
    
  -- Merge in lookup information (note that all columns are available for merging)
  FROM pat_aurum_final a
  LEFT JOIN obstype o ON a.constype = o.obstypeid
  LEFT JOIN numunit n ON a.numunitid = n.numunitid;
", f_obstype, f_numunit)

# Execute SQL code for lookups
DBI::dbExecute(connection, sql_lookup_a)

Sys.time() - start_time

# Check tables
dbListTables(connection)

# Drop tables to free up memory
DBI::dbExecute(connection, "
  DROP TABLE IF EXISTS pat_gold_final;
  DROP TABLE IF EXISTS pat_aurum_final;
")


# ================= 6) Combine GOLD and Aurum and create data files ============

# Combine GOLD and Aurum extracted patient files
DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE pat_comb_final AS
  SELECT DISTINCT * FROM (
    SELECT 
      medcode, term, patid, eventdate, sysdate, constype, consid, database, source,
      data2, value, enttype_description, category, test_qualifier, unit
    FROM pat_gold_lookup
    UNION ALL BY NAME
    SELECT 
      medcode, term, patid, eventdate, sysdate, constype, database, source,
      value, unit
    FROM pat_aurum_lookup
  ) x;
")

## Number of unique patients with condition

# Gold
cat("\nGold rows / patients:\n")
print(dbGetQuery(
  connection, 
  "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_gold_lookup;"))

# Aurum
cat("\nAURUM rows / patients:\n")
print(dbGetQuery(
  connection, 
  "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_aurum_lookup;"))

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



# ================= 7) GOLD needs extraction of Additional files ============

# ----------------------------
# Read GOLD Additional data
# enttype == 13: weight/BMI additional data
#   data1 = weight
#   data3 = recorded BMI

additional_files <- find_txt_files(path_extract_gold_add)
additional_scan <- duckdb_read_csv_sql(additional_files)

dbExecute(connection, paste0("
  CREATE OR REPLACE TEMP TABLE additional_13 AS
  SELECT
    CONCAT(patid, '-G') AS patid,
    adid,
    CONCAT(CONCAT(patid, '-G'), '-', adid) AS patad,
    TRY_CAST(enttype AS INTEGER) AS enttype,
    data1,
    data2,
    data3
  FROM ", additional_scan, "
  WHERE TRY_CAST(enttype AS INTEGER) IN (13)
"))

# 13,064,596
message("Additional rows with enttype 13: ",
        dbGetQuery(connection, 
                   "SELECT COUNT(*) AS n FROM additional_13")$n)

# 877,045
message("Patients with Additional enttype 13: ",
        dbGetQuery(connection, 
                   "SELECT COUNT(DISTINCT patid) AS n FROM additional_13")$n)


# ============================================================
# Create Clinical date table
# ============================================================
# We use Clinical only to recover dates and medcodes for Additional rows.
#
# Date logic:
#   1. Parse eventdate as dd/mm/YYYY.
#   2. Treat dates <= 1900-01-01 as missing.
#   3. If multiple Clinical rows have the same patad, keep the earliest
#      available eventdate. This is deterministic and avoids row multiplication.

clinical_files <- find_txt_files(path_extract_gold_clin)
clinical_scan <- duckdb_read_csv_sql(clinical_files)

dbExecute(connection, paste0("
  CREATE OR REPLACE TEMP TABLE clinical_dates_raw AS
  SELECT
    CONCAT(patid, '-G') AS patid,
    adid,
    CONCAT(CONCAT(patid, '-G'), '-', adid) AS patad,
    medcode,
    CASE
      WHEN TRY_STRPTIME(eventdate, '%d/%m/%Y')::DATE <= DATE '1900-01-01' THEN NULL
      ELSE TRY_STRPTIME(eventdate, '%d/%m/%Y')::DATE
    END AS eventdate
  FROM ", clinical_scan, "
  WHERE
    TRY_CAST(enttype AS INTEGER) IN (13)
    AND adid IS NOT NULL
    AND adid <> ''
"))

dbExecute(connection, "
  CREATE OR REPLACE TEMP TABLE clinical_dates AS
  SELECT
    patad,
    ANY_VALUE(patid) AS patid,
    ANY_VALUE(adid) AS adid,
    ANY_VALUE(medcode) AS medcode,
    MIN(eventdate) AS eventdate,
  FROM clinical_dates_raw
  GROUP BY patad
")

# 13,065,809
message("Clinical date rows after de-duplication: ",
        dbGetQuery(connection, 
                   "SELECT COUNT(*) AS n FROM clinical_dates")$n)

# ============================================================
# Merge dates into Additional
# ============================================================
# LEFT JOIN is deliberate:
#   Additional is the source of the measurement values.
#   Keeping all candidate Additional rows allows explicit QC of missing dates.

dbExecute(connection, "
  CREATE OR REPLACE TEMP TABLE additional_dated AS
  SELECT
    a.patid,
    a.adid,
    a.patad,
    a.enttype,
    a.data1,
    a.data2,
    a.data3,
    c.medcode,
    c.eventdate
  FROM additional_13 a
  LEFT JOIN clinical_dates c
    ON a.patad = c.patad
")

# 13,064,596
message("Additional rows after date merge: ",
        dbGetQuery(connection, 
                   "SELECT COUNT(*) AS n FROM additional_dated")$n)

# 4678
message("Rows missing eventdate after merge: ",
        dbGetQuery(connection, 
                   "SELECT SUM(CASE WHEN eventdate IS NULL THEN 1 ELSE 0 END) AS n FROM additional_dated")$n)


# ============================================================
# Recorded BMI
# ============================================================

dbExecute(connection, glue::glue_sql("
  CREATE OR REPLACE TEMP TABLE pat_gold_additional_bmi AS
  SELECT
    patid,
    TRY_CAST(data3 AS DOUBLE) AS value,
    eventdate
  FROM additional_dated
  WHERE
    enttype = 13
    AND eventdate IS NOT NULL
", .con = connection))

# 13,059,918
message("Recorded GOLD BMI rows: ",
        dbGetQuery(connection, 
                   "SELECT COUNT(*) AS n FROM pat_gold_additional_bmi")$n)

# 877,026
message("Patients with recorded GOLD BMI: ",
        dbGetQuery(connection, 
                   "SELECT COUNT(DISTINCT patid) AS n FROM pat_gold_additional_bmi")$n)

# Check table
dbGetQuery(connection, "
           SELECT * FROM pat_gold_additional_bmi LIMIT 10")
# dbGetQuery(connection, "
#            SELECT Type, COUNT(*) AS n
#            FROM pat_gold_additional_bmi
#            GROUP BY Type
# ")

# ============================================================
# Weight
# ============================================================

dbExecute(connection, glue::glue_sql("
  CREATE OR REPLACE TEMP TABLE pat_gold_additional_weight AS
  SELECT
    patid,
    TRY_CAST(data1 AS DOUBLE) AS weight_kg,
    eventdate
  FROM additional_dated
  WHERE
    enttype = 13
    AND eventdate IS NOT NULL
", .con = connection))

# 13,059,918
message("GOLD weight rows: ",
        dbGetQuery(connection, 
                   "SELECT COUNT(*) AS n FROM pat_gold_additional_weight")$n)

# 877,026
message("Patients with GOLD weight: ",
        dbGetQuery(connection, 
                   "SELECT COUNT(DISTINCT patid) AS n FROM pat_gold_additional_weight")$n)

# Check table
dbGetQuery(connection, "
           SELECT * FROM pat_gold_additional_weight LIMIT 10")

# ============================================================
# Final GOLD BMI and Weight datasets
# ============================================================

### BMI
# Pull from duckdb
pat_gold_additional_bmi <- dbGetQuery(connection, "
  SELECT * FROM pat_gold_additional_bmi
  ORDER BY patid, eventdate, value
")
# Remove NAs
pat_gold_additional_bmi <- pat_gold_additional_bmi %>%
  filter(!is.na(value))

# Check
summary(pat_gold_additional_bmi$value)
nrow(pat_gold_additional_bmi) # 12,712,478 records
length(unique(pat_gold_additional_bmi$patid)) # 850,612 patients

# Save
save(pat_gold_additional_bmi,
     file = paste0(path_output, "pat_gold_additional_bmi.RData"))

### Weight
# Pull from duckdb
pat_gold_additional_weight <- dbGetQuery(connection, "
  SELECT * FROM pat_gold_additional_weight
  ORDER BY patid, eventdate, weight_kg
")
# Remove NAs
pat_gold_additional_weight <- pat_gold_additional_weight %>%
  filter(!is.na(weight_kg))

# Check
summary(pat_gold_additional_weight$weight_kg)
nrow(pat_gold_additional_weight) # 13,059,473 records
length(unique(pat_gold_additional_weight$patid)) # 877,025 patients

# Save
save(pat_gold_additional_weight,
     file = paste0(path_output, "pat_gold_additional_weight.RData"))





### Remove data and shut down connection
dbDisconnect(connection, shutdown = TRUE)
cat("\nDone.\n")

