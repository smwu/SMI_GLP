# ==============================================================================
# Extract patients with HbA1c lab information using code lists (SQL Version)
# Author: SM Wu
# Date Created: 2026/01/19
# Date Updated: 2026/02/02
# 
# Details:
# Note: This processes Gold and Aurum in chunks due to large file size.
# 1) Set up and read in code lists
# 2) Read in CPRD GOLD data
# 3) Read in CPRD Aurum data
# 4) Add in lookup information
# 5) Combine GOLD and Aurum and create data files
#
# Inputs:
# 1) ~/SMI_GLP/Code_Lists/HbA1c/Aurum_HbA1c_codelist_20250929.txt: Updated Aurum hba1c code list
# 2) ~/SMI_GLP/Code_Lists/HbA1c/Gold_HbA1c_codelist_20250929.txt: Updated GOLD hba1c code list
# 3) ~/SMI_GLP/Code/1_Data_Extraction/helper_fns_data_extraction.R: Helper functions
# 4) ~/GOLD/ Clinical, Test, and Referral files
# 5) ~/Aurum/ Observation files
# 
# Intermediate outputs:
# 1) ~/SMI_GLP/Data/Extraction_Files/pat_hba1c_gold.RData: GOLD patient files for hba1c diagnosis
# 2) ~/SMI_GLP/Data/Extraction_Files/pat_hba1c_aurum.RData: Aurum patient files for hba1c diagnosis
# 
# Final Outputs:
# 1) ~/SMI_GLP/Data/Extraction_Files/pat_hba1c_comb.RData: Combined GOLD and Aurum patient files for hba1c diagnosis

# ==============================================================================


# ================= 1) Set up and read in code lists ===========================

# Clear memory
rm(list = ls())
gc()

# Input arguments
earliest_date <- "1900-01-01"
latest_date <- "2025-06-01"
code_name <- "Antidiabetics"

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

path_extract_gold_ther <- paste0(wd, path_gold, "Therapy/")
path_extract_aurum_drug <- paste0(wd, path_aurum, "DrugIssue/")
path_lookups_gold <- "Lookups/202506_Lookups_GOLD2025_09/"
path_lookups_aurum <- "Lookups/202506_Lookups_CPRDAurum/"

# Globs for DuckDB
gold_ther_glob <- file.path(path_extract_gold_ther, "*.txt")
aurum_drug_glob <- file.path(path_extract_aurum_drug, "*.txt")

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
  col_types = cols(prodcode = col_character()),  trim_ws = TRUE)


# AURUM code list
aurum_file_name <- list.files(path = paste0(wd, path_input),
                              pattern = paste0("^Aurum_", code_name, "_codelist"))
# Check date
aurum_file_name
codelist_aurum <- read_delim(
  file = paste0(wd, path_input, aurum_file_name), 
  delim = "\t", escape_double = FALSE, 
  col_types = cols(prodcodeid = col_character(),
                   BNFChapter = col_character()), trim_ws = TRUE)

# Start timer
start_time <- Sys.time()

# ================= 2) Use SQL for extraction ==================================

# DuckDB in-memory connection
connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# Allow spilling to local disk after hitting memory limit
DBI::dbExecute(connection, "PRAGMA memory_limit = '40GB';")
spill_dir <- "N:/Temp/duckdb_spill"
dir.create(spill_dir, showWarnings = FALSE, recursive = TRUE)
DBI::dbExecute(connection, sprintf("PRAGMA temp_directory='%s';", spill_dir))

# Set up progress bar
DBI::dbExecute(connection, "SET enable_progress_bar = true;")
DBI::dbExecute(connection, "SET enable_progress_bar_print = true;")


# Write code lists
gold_codes_table  <- paste0(code_name, "_gold_codes")
aurum_codes_table <- paste0(code_name, "_aurum_codes")
dbWriteTable(connection, gold_codes_table,  codelist_gold,  overwrite = TRUE)
dbWriteTable(connection, aurum_codes_table, codelist_aurum, overwrite = TRUE)

# Check tables
dbListTables(connection)

# Sanity check: column names
# GOLD Therapy 
gold_ther_files <- list.files(path = path_extract_gold_ther,
                              pattern = "\\.txt$", full.names = TRUE)
header_line <- readLines(gold_ther_files[1], n = 1, warn = FALSE)
strsplit(header_line, "\t", fixed = TRUE)[[1]]

# Aurum DrugIssue
aurum_drug_files <- list.files(path = path_extract_aurum_drug[1],
                              pattern = "\\.txt$", full.names = TRUE)
header_line <- readLines(aurum_drug_files[1], n = 1, warn = FALSE)
strsplit(header_line, "\t", fixed = TRUE)[[1]]

# How many files
length(list.files(path = path_extract_gold_ther, pattern = "\\.txt$", 
                  full.names = TRUE))
for (i in 1:length(path_extract_aurum_drug)) {
  print(length(list.files(path = path_extract_aurum_drug[i], pattern = "\\.txt$", 
                          full.names = TRUE)))
}

### Remove data and shut down connection
dbDisconnect(connection, shutdown = TRUE)

# ================= PROCESS GOLD DATASET ============================================

# Repeat for each folder with data
for (i in 1:length(path_gold)) {
  path_gold_i <- path_gold[i]
  gold_ther_glob_i <- gold_ther_glob[i]
  
  
  # ===== Restart duckdb connection 
  connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  # Allow spilling to local disk after hitting memory limit
  DBI::dbExecute(connection, "PRAGMA memory_limit = '40GB';")
  spill_dir <- "N:/Temp/duckdb_spill"
  dir.create(spill_dir, showWarnings = FALSE, recursive = TRUE)
  DBI::dbExecute(connection, sprintf("PRAGMA temp_directory='%s';", spill_dir))
  # Set up progress bar
  DBI::dbExecute(connection, "SET enable_progress_bar = true;")
  DBI::dbExecute(connection, "SET enable_progress_bar_print = true;")
  
  # Write code lists
  gold_codes_table  <- paste0(code_name, "_gold_codes")
  dbWriteTable(connection, gold_codes_table,  codelist_gold,  overwrite = TRUE)

  # ===== Read in CPRD GOLD data 
  
  # GOLD THERAPY
  
  gold_extract_sql(connection = connection, 
                   out_table = "gold_ther_raw", 
                   files_glob = gold_ther_glob_i,
                   gold_codes_table = gold_codes_table, 
                   source_label = "Therapy",
                   code_kind = "prodcode")
  print("Completed GOLD Therapy extract.")
  # Runtime check
  Sys.time() - start_time
  
  # Quick peek
  DBI::dbGetQuery(connection, "DESCRIBE gold_ther_raw") 
  
  # MERGE ALL GOLD FILES TOGETHER
  
  DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE pat_gold_raw AS
  SELECT 
    prodcode, productname, 
    patid, eventdate, sysdate, database, source, 
    formulation, route, ingredient, strength, 
    Antidiabetic AS antidiabetic, 
    \"group\" AS group_name,  -- rename to avoid parser error
    bnfcode, dosageid, qty, numdays, numpacks, packtype
  FROM gold_ther_raw;
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
  
  # Drop tables to free up memory
  DBI::dbExecute(connection, "
  DROP TABLE IF EXISTS gold_ther_raw;
  ")
  gc()
  
  # Save raw extracted patient files matching code list conditions as parquet
  out_gold_raw_parquet_i <- paste0(path_output, "pat_gold_raw_", i, ".parquet")
  DBI::dbExecute(connection, sprintf(
    "COPY pat_gold_raw TO '%s' (FORMAT parquet);", 
    out_gold_raw_parquet_i
  ))
  
  print("Combined GOLD Raw")
  # Runtime check
  Sys.time() - start_time
  
  # # Read in pat_gold_raw if necessary. 30 GB
  # DBI::dbExecute(connection, sprintf("
  #   CREATE OR REPLACE TABLE pat_gold_raw AS
  #   SELECT 
  #     prodcode, productname, 
  #     patid, eventdate, sysdate, database, source, 
  #     formulation, route, ingredient, strength, antidiabetic, group_name, 
  #     bnfcode, dosageid, qty, numdays, numpacks, packtype
  #   FROM read_parquet('%s');
  # ", out_gold_raw_parquet_i))
  
  
  # ========= Tranform Gold dates 
  
  ## Clean dates and filter out those with invalid dates
  
  # Gold
  transform_dates_sql_medcode2(connection = connection, 
                               in_table = "pat_gold_raw", 
                               out_table = "pat_gold_clean",
                               earliest_date = earliest_date,
                               latest_date = latest_date)
  
  # Drop tables to free up memory
  DBI::dbExecute(connection, "
  DROP TABLE IF EXISTS pat_gold_raw;
  ")
  
  print("Cleaned GOLD")
  # Runtime check
  Sys.time() - start_time
  
  # Rearrange columns, add Gold identifiers to patid, and drop duplicates
  # Note: output is number of rows in newly created table
  # Slow runtime
  # NOTE: Had to remove the following columns for memory space:
  #   source, formulation, route, ingredient, strength,  
  DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE pat_gold_transform AS
  SELECT DISTINCT
    patid || '-G' AS patid,
    prodcode, productname, eventdate, sysdate, database, 
    bnfcode, dosageid, qty, numdays, numpacks, packtype,
    antidiabetic, group_name 
  FROM pat_gold_clean;
  ")
    
  cat("\nGold rows / patients:\n")
  print(dbGetQuery(
    connection, 
    "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_gold_transform;"))
  
  # Drop tables to free up memory
  DBI::dbExecute(connection, "
  DROP TABLE IF EXISTS pat_gold_clean;
  ")
  gc()  
  
  # ======== Add in Gold look up information 
  
  # Lookup file paths
  f_common_dosages_g <- paste0(path_lookups_gold, "common_dosages.txt")
  f_bnfcodes <- paste0(path_lookups_gold, "bnfcodes.txt") # medical entry
  f_packtype <- paste0(path_lookups_gold, "packtype.txt")  # test qualifier
  
  ## GOLD
  
  sql_lookup_g <- sprintf("
  -- Read in look up files
  -- common dosages
  CREATE OR REPLACE TEMP VIEW common_dosages_g AS
  SELECT *
  FROM read_csv('%1$s', 
    delim='\\t', 
    header=true, 
    all_varchar=true,   -- all columns as characters
    null_padding=true,  -- pad shorter rows
    strict_mode=false,  -- tolerate irregular rows
    quote='', escape=''
  );
  
  -- bnfcodes
  CREATE OR REPLACE TEMP VIEW bnfcodes AS
  SELECT *
  FROM read_csv('%2$s', 
    delim='\\t', 
    header=true, 
    all_varchar=true,   -- all columns as characters
    null_padding=true,  -- pad shorter rows
    strict_mode=false,  -- tolerate irregular rows
    quote='', escape=''
  );
  
  -- packtype
  CREATE OR REPLACE TEMP VIEW packtype AS
  SELECT *
  FROM read_csv('%3$s', 
    delim='\\t', 
    header=true, 
    all_varchar=true,   -- all columns as characters
    null_padding=true,  -- pad shorter rows
    strict_mode=false,  -- tolerate irregular rows
    quote='', escape=''
  );
  
  -- Standardise field names  and add in look up information
  CREATE OR REPLACE TABLE pat_gold_lookup AS
  SELECT
    -- choose final columns to exclude/rename from lookup files
    -- cdg columns: dosage_text, daily_dose, dose_number, dose_unit, dose_frequency, 
    --    dose_interval, choice_of_dose, dose_max_average, change_dose, dose_duration 
    -- b columns: bnf
    -- pt columns: packtype descriptions (renamed to packtype)
    g.*,
    
    cdg.* EXCLUDE (dosageid),
    b.* EXCLUDE (bnfcode),
    CAST(b.bnf AS VARCHAR) AS bnf,
    pt.* EXCLUDE (packtype, packtype_desc),
    pt.packtype_desc AS packtype
    
  -- Merge in lookup information (note that all columns are available for merging)
  FROM pat_gold_transform g
  LEFT JOIN common_dosages_g cdg ON g.dosageid = cdg.dosageid
  LEFT JOIN bnfcodes b ON g.bnfcode = b.bnfcode
  LEFT JOIN packtype pt ON g.packtype = pt.packtype;
  ", f_common_dosages_g, f_bnfcodes, f_packtype)
    
  # Execute SQL code for lookups
  DBI::dbExecute(connection, sql_lookup_g)
  
  # Drop tables to free up memory
  DBI::dbExecute(connection, "
  DROP TABLE IF EXISTS pat_gold_transform;
  ")
  
  # Finalize GOLD extracted patient files
  DBI::dbExecute(connection, "
    CREATE OR REPLACE TABLE pat_gold_final AS
    SELECT 
      prodcode, productname, patid, eventdate, sysdate, database, antidiabetic, group_name,
      qty, numdays, numpacks, packtype, bnf,
      dosage_text, daily_dose, dose_number, dose_unit, dose_frequency, 
      dose_interval, choice_of_dose, dose_max_average, change_dose, dose_duration 
    FROM pat_gold_lookup
  ")
  
  ## Number of unique patients with condition
  
  # Gold
  cat("\nGold rows / patients:\n")
  print(dbGetQuery(
    connection, 
    "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_gold_final;"))
  
  ## Save patient data for Gold as parquet file
  out_gold_final_parquet_i <- paste0(path_output, "pat_gold_final_", i, ".parquet")
  dbExecute(connection, 
            sprintf("COPY pat_gold_final TO '%s' (FORMAT parquet);", 
                    out_gold_final_parquet_i))
  print("Combined Cleaned Gold")
  
  
  # Check tables
  dbListTables(connection)
  
  # Disconnect and wipe the spillover folder
  dbDisconnect(connection, shutdown = TRUE)
  unlink(spill_dir, recursive = TRUE, force = TRUE)
  # Check how much memory was used
  gc()
  
  print(paste0("Gold ", i, " completed!"))
  # Runtime check
  Sys.time() - start_time
  
}




# ================= PROCESS AURUM DATASET ========================================


# Repeat for each folder with data
for (i in 1:length(path_aurum)) {
  path_aurum_i <- path_aurum[i]
  aurum_drug_glob_i <- aurum_drug_glob[i]
  
  # ===== Restart duckdb connection 
  connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  # Allow spilling to local disk after hitting memory limit
  DBI::dbExecute(connection, "PRAGMA memory_limit = '40GB';")
  spill_dir <- "N:/Temp/duckdb_spill"
  dir.create(spill_dir, showWarnings = FALSE, recursive = TRUE)
  DBI::dbExecute(connection, sprintf("PRAGMA temp_directory='%s';", spill_dir))
  # Set up progress bar
  DBI::dbExecute(connection, "SET enable_progress_bar = true;")
  DBI::dbExecute(connection, "SET enable_progress_bar_print = true;")
  
  # Write code lists
  aurum_codes_table <- paste0(code_name, "_aurum_codes")
  dbWriteTable(connection, aurum_codes_table, codelist_aurum, overwrite = TRUE)

  # ====== Read in CPRD Aurum data 
  
  # AURUM DRUGISSUE
  
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
    
    
    -- Create out_tbl aurum_drug_raw, keeping only rows matching code list medcode (from code_tbl)
    -- Attach term from code list
    -- Also add in database identifier and source type
    
    CREATE OR REPLACE TABLE %2$s AS
    SELECT
      trim(a.prodcodeid) AS prodcode,
      c.productname          AS productname,
      a.patid         AS patid,
      a.pracid        AS pracid,
      a.issuedate     AS eventdate,   -- keep as TEXT for now
      a.enterdate     AS sysdate,     -- keep as TEXT for now
      'Aurum'         AS database,
      '%3$s'          AS source,
      c.formulation   AS formulation,
      c.route         AS route,
      c.ingredient    AS ingredient,
      c.strength      AS strength,
      c.Antidiabetic  AS antidiabetic,
      c.\"group\"     AS group_name,  -- to avoid parser error
      c.BNFChapter    AS BNFChapter,
      a.dosageid      AS dosageid,
      a.quantunitid   AS quantunitid,
      a.quantity      AS qty,
      a.duration      AS numdays
    FROM aurum_rawfile a
    INNER JOIN %4$s c
      ON trim(a.prodcodeid) = trim(c.prodcodeid);
  ", 
                 aurum_drug_glob_i, 
                 "aurum_drug_raw", 
                 "DrugIssue", 
                 aurum_codes_table
  )
  DBI::dbExecute(connection, sql)
  
  # Save extracted patient files parquet 
  out_aurum_raw_parquet_i <- paste0(path_output, "pat_aurum_raw_", i, ".parquet")
  DBI::dbExecute(connection, sprintf(
    "COPY aurum_drug_raw TO '%s' (FORMAT parquet);", 
    out_aurum_raw_parquet_i
  ))
  
  print(paste0("Completed Aurum DrugIssue extract part ", i))
  
  # Runtime check
  Sys.time() - start_time
  
  # MERGE ALL Aurum FILES TOGETHER
  
  DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE pat_aurum_raw AS
  SELECT 
    prodcode, productname, patid, eventdate, sysdate, database, 
    BNFChapter, dosageid, quantunitid, qty, numdays,
    pracid, source, formulation, route, ingredient, strength, 
    antidiabetic, group_name,
  FROM aurum_drug_raw;
  ")
  
  # Count number of patients in Aurum before transforming dates
  aurum_raw_by_source <- DBI::dbGetQuery(connection, "
    SELECT database,
           COUNT(*) AS n_rows,
           COUNT(DISTINCT patid) AS n_patids
    FROM pat_aurum_raw
    GROUP BY database
    ORDER BY database;
  ")
  print(aurum_raw_by_source)
  
  # Drop tables to free up memory
  DBI::dbExecute(connection, "
    DROP TABLE IF EXISTS aurum_drug_raw;
  ")
  gc()
  
  # Save raw extracted patient files matching code list conditions as parquet
  out_aurum_raw_parquet_i <- paste0(path_output, "pat_aurum_raw_", i, ".parquet")
  DBI::dbExecute(connection, sprintf(
    "COPY pat_aurum_raw TO '%s' (FORMAT parquet);", 
    out_aurum_raw_parquet_i
  ))
  
  print("Combined Aurum Raw")
  # Runtime check
  Sys.time() - start_time
  
  # # Read in pat_aurum_raw if necessary
  # DBI::dbExecute(connection, sprintf("
  #   CREATE OR REPLACE TABLE pat_aurum_raw AS
  #   SELECT
  #      prodcode, productname, patid, eventdate, sysdate, database,
  #      BNFChapter, dosageid, quantunitid, qty, numdays,
  #      pracid, source, formulation, route, ingredient, strength,
  #      antidiabetic, group_name,
  #   FROM read_parquet('%s');
  # ", out_aurum_raw_parquet_i))
  
  
  # ========= Tranform Aurum dates 
  
  # Aurum
  transform_dates_sql_medcode2(connection = connection, 
                               in_table = "pat_aurum_raw", 
                               out_table = "pat_aurum_clean",
                               earliest_date = earliest_date,
                               latest_date = latest_date)
  print("Cleaned Aurum")
  # Runtime check
  Sys.time() - start_time
  
  # Drop tables to free up memory
  DBI::dbExecute(connection, "
    DROP TABLE IF EXISTS pat_aurum_raw;
  ")
  
  # Rearrange columns, add Aurum identifiers to patid, and drop duplicates
  # Note: output is number of rows in newly created table
  # NOTE: Had to remove the following columns for memory space:
  #   source, formulation, route, ingredient, strength 
  DBI::dbExecute(connection, "
    CREATE OR REPLACE TABLE pat_aurum_transform AS
    SELECT DISTINCT
      patid || '-A' AS patid,
      prodcode, productname, pracid, eventdate, sysdate, database, 
      BNFChapter, dosageid, quantunitid, qty, numdays,
      antidiabetic, group_name 
    FROM pat_aurum_clean;
  ")
  
  cat("\nAurum rows / patients:\n")
  print(dbGetQuery(
    connection, 
    "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_aurum_transform;"))
  
  # Drop tables to free up memory
  DBI::dbExecute(connection, "
    DROP TABLE IF EXISTS pat_aurum_clean;
  ")
  gc()
  
  # ======== Add in Aurum look up information 
  
  f_common_dosages_a <- paste0(path_lookups_aurum, "common_dosages.txt") # observation type
  f_quantunit <- paste0(path_lookups_aurum, "QuantUnit.txt") # unit of measurement
  
  ## AURUM
  
  sql_lookup_a <- sprintf("
    -- Read in look up files
    -- Common dosages
    CREATE OR REPLACE TEMP VIEW common_dosages_a AS
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
    CREATE OR REPLACE TEMP VIEW quantunit AS
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
      -- select all pat_aurum_transform columns
      a.*,
      a.BNFChapter AS bnf,
      
      -- choose final columns to exclude/rename from lookup files
      -- cda columns: dosage_text, daily_dose, dose_number, dose_unit, dose_frequency, 
      --    dose_interval, choice_of_dose, dose_max_average, change_dose, dose_duration 
      -- qu columns: packtype descriptions (renamed to packtype)
      cda.* EXCLUDE (dosageid),
      qu.* EXCLUDE (quantunitid),
      qu.Description AS packtype
      
    -- Merge in lookup information (note that all columns are available for merging)
    FROM pat_aurum_transform a
    LEFT JOIN common_dosages_a cda ON a.dosageid = cda.dosageid
    LEFT JOIN quantunit qu ON a.quantunitid = qu.quantunitid;
  ", f_common_dosages_a, f_quantunit)
  
  # Execute SQL code for lookups
  DBI::dbExecute(connection, sql_lookup_a)
  
  # Check tables
  dbListTables(connection)
  
  # Drop tables to free up memory
  DBI::dbExecute(connection, "
    DROP TABLE IF EXISTS pat_aurum_transform;
  ")
  
  # Finalize Aurum extracted patient files
  DBI::dbExecute(connection, "
    CREATE OR REPLACE TABLE pat_aurum_final AS
    SELECT 
      prodcode, productname, patid, eventdate, sysdate, database, antidiabetic, group_name, 
      qty, numdays, pracid, packtype, bnf,
      dosage_text, daily_dose, dose_number, dose_unit, dose_frequency, 
      dose_interval, choice_of_dose, dose_max_average, change_dose, dose_duration 
    FROM pat_aurum_lookup
  ")
  
  # Drop tables to free up memory
  DBI::dbExecute(connection, "
    DROP TABLE IF EXISTS pat_aurum_lookup;
  ")
  
  ## Number of unique patients with condition
  
  # Aurum
  cat("\nAURUM rows / patients:\n")
  print(dbGetQuery(
    connection, 
    "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_aurum_final;"))
  
  ## Save patient data for Aurum as parquet file
  out_aurum_final_parquet_i <- paste0(path_output, "pat_aurum_final_", i, ".parquet")
  dbExecute(connection, 
            sprintf("COPY pat_aurum_final TO '%s' (FORMAT parquet);", 
                    out_aurum_final_parquet_i))
  print("Combined Cleaned Aurum")
  
  gc()
  
  # Check tables
  dbListTables(connection)
  
  # Disconnect and wipe the spillover folder
  dbDisconnect(connection, shutdown = TRUE)
  closeAllConnections()
  unlink(spill_dir, recursive = TRUE, force = TRUE)
  
  print(paste0("Aurum ", i, " completed!"))
  # Runtime check
  Sys.time() - start_time

}


# ================= Combine GOLD and Aurum (memory-permitting) =====================
## DO NOT RUN FOR ANTIDIABETICS (NOT ENOUGHT MEMORY)

# # ===== Restart duckdb connection 
# connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
# # Allow spilling to local disk after hitting memory limit
# DBI::dbExecute(connection, "PRAGMA memory_limit = '40GB';")
# spill_dir <- "N:/Temp/duckdb_spill"
# dir.create(spill_dir, showWarnings = FALSE, recursive = TRUE)
# DBI::dbExecute(connection, sprintf("PRAGMA temp_directory='%s';", spill_dir))
# 
# 
# # Combine Gold files
# 
# # Read all exported Parquet files back as one view/table inside duckDB (does not load all into R)
# # Merge all Aurum files from separate folders into one table
# # NOTE: Had to remove the following columns for memory space:
# #   pracid, source, formulation, route, ingredient, strength, antidiabetic, group_name 
# parquet_glob <- paste0(path_output, "pat_gold_final_*.parquet")
# DBI::dbExecute(connection, sprintf("
#     CREATE OR REPLACE TABLE pat_gold_final AS
#     SELECT * FROM read_parquet('%s');
#   ", parquet_glob))
# 
# # Save raw extracted patient files matching code list conditions as parquet
# out_gold_final_parquet <- paste0(path_output, "pat_gold_final.parquet")
# dbExecute(connection, 
#           sprintf("COPY pat_gold_final TO '%s' (FORMAT parquet);", 
#                   out_gold_final_parquet))
# 
# # Combine Aurum files
# 
# # Read all exported Parquet files back as one view/table inside duckDB (does not load all into R)
# # Merge all Aurum files from separate folders into one table
# parquet_glob <- paste0(path_output, "pat_aurum_final_*.parquet")
# DBI::dbExecute(connection, sprintf("
#     CREATE OR REPLACE TABLE pat_aurum_final AS
#     SELECT * FROM read_parquet('%s');
#   ", parquet_glob))
# 
# 
# # Save raw extracted patient files matching code list conditions as parquet
# out_aurum_final_parquet <- paste0(path_output, "pat_aurum_final.parquet")
# dbExecute(connection, 
#           sprintf("COPY pat_aurum_final TO '%s' (FORMAT parquet);", 
#                   out_aurum_final_parquet))
# 
# 
# # Combine GOLD and Aurum extracted patient files
# DBI::dbExecute(connection, "
#   CREATE OR REPLACE TABLE pat_comb_final AS
#   SELECT DISTINCT * FROM (
#     SELECT 
#       prodcode, productname, patid, eventdate, sysdate, database, source,
#       formulation, route, ingredient, strength, antidiabetic, group_name,
#       qty, numdays, numpacks, packtype, bnf,
#       dosage_text, daily_dose, dose_number, dose_unit, dose_frequency, 
#       dose_interval, choice_of_dose, dose_max_average, change_dose, dose_duration 
#     FROM pat_gold_lookup
#     UNION ALL BY NAME
#     SELECT 
#       prodcode, productname, patid, eventdate, sysdate, database, source,
#       formulation, route, ingredient, strength, antidiabetic, group_name, 
#       qty, numdays, pracid, packtype, bnf,
#       dosage_text, daily_dose, dose_number, dose_unit, dose_frequency, 
#       dose_interval, choice_of_dose, dose_max_average, change_dose, dose_duration 
#     FROM pat_aurum_final
#   ) x;
# ")
# 
# ## Number of unique patients with condition
# 
# # Gold
# cat("\nGold rows / patients:\n")
# print(dbGetQuery(
#   connection, 
#   "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_gold_final;"))
# 
# # Aurum
# cat("\nAURUM rows / patients:\n")
# print(dbGetQuery(
#   connection, 
#   "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_aurum_final;"))
# 
# # Total
# cat("\nTotal rows / patients:\n")
# print(dbGetQuery(
#   connection, 
#   "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_comb_final;"))
# 
# 
# ## Save patient data for GOLD and Aurum as parquet file
# out_comb_final_parquet <- paste0(path_output, "pat_comb_final.parquet")
# dbExecute(connection, 
#           sprintf("COPY pat_comb_final TO '%s' (FORMAT parquet);", 
#                   out_comb_final_parquet))
# print("Combined Cleaned GOLD and Aurum")
# # Runtime check
# Sys.time() - start_time
# 
# 
# # Check how much memory was used
# gc()
# # Check tables
# dbListTables(connection)
# 
# # # Optional: pull into R and save as .RData (but this can be huge file)
# parquet_file <- paste0(path_output, "pat_comb_final.parquet")
# pat_comb_final <- DBI::dbGetQuery(connection, sprintf(
#   "SELECT * FROM read_parquet('%s');", 
#   parquet_file))
# save(pat_comb_final,
#      file = paste0(path_output, "pat_comb_final.RData"))
# # rm(pat_comb_final_df)
# 
# ### Remove data and shut down connection
# dbDisconnect(connection, shutdown = TRUE)
# cat("\nDone.\n")
# 
