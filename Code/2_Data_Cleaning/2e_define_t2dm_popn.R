# ====================================================================
# Create T2DM population
# Author: SM Wu
# Date Created: 2026/02/10
# Date Updated: 2026/02/24
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
path_output <- paste0("SMI_GLP/Data/Cleaning_Files/")
path_gold <- "GOLD/"
path_aurum <- c("Aurum_1/", "Aurum_2/", "Aurum_3/")


# DuckDB in-memory connection
connection <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# Allow spilling to local disk after hitting memory limit
DBI::dbExecute(connection, "PRAGMA memory_limit = '35GB';")
spill_dir <- "N:/Temp/duckdb_spill"
dir.create(spill_dir, showWarnings = FALSE, recursive = TRUE)
DBI::dbExecute(connection, sprintf("PRAGMA temp_directory='%s';", spill_dir))

# Set up progress bar
DBI::dbExecute(connection, "SET enable_progress_bar = true;")
DBI::dbExecute(connection, "SET enable_progress_bar_print = true;")

# Read in cohort of patients meeting age and registration criteria
load(paste0(wd, path_output, "cohort_demog.Rdata")) # created in 2a_clean_cohort_demog.R


# ============= Restrictions based on elevated lab test ===============================

### Read in HbA1c and Glucose
load(paste0(wd, path_output, "hba1c_clean_elevated.RData"))  # 5 GB
load(paste0(wd, path_output, "glucose_clean_elevated.RData"))  # 1 GB

# Convert to data.table
setDT(hba1c_elevated)
setDT(glucose_elevated)

# Filter to records from patients meeting age and registration criteria
hba1c_elevated <- hba1c_elevated[patid %in% cohort_demog$patid]
glucose_elevated <- glucose_elevated[patid %in% cohort_demog$patid]

# Stack lab data together
lab_elevated <- rbindlist(list(hba1c_elevated, glucose_elevated), use.names = TRUE, fill = TRUE)

# Number of elevated lab tests per patient
lab_pat_counts <- lab_elevated[, .(n_entries = .N), by = patid]

# Number of patients with at least 1 elevated lab test: 2,550,811
patid_lab <- unique(lab_pat_counts$patid)
length(patid_lab)

# Number of patients with at least 2 elevated lab tests: 2,409,841
lab_pat_counts[n_entries >= 2, uniqueN(patid)]

## HbA1c only
# Number of elevated lab tests per patient: HbA1c
lab_pat_counts_hba1c <- hba1c_elevated[, .(n_entries = .N), by = patid]
# Number of patients with at least 1 elevated hba1c test: 2,491,285
uniqueN(lab_pat_counts_hba1c$patid)
# Number of patients with at least 2 elevated hba1c tests: 2,318,555
lab_pat_counts_hba1c[n_entries >= 2, uniqueN(patid)]

# Upload patients with at least 1 elevated lab test to duckdb as a small table
DBI::dbWriteTable(connection,
  "patid_lab",
  data.frame(patid = patid_lab),
  overwrite = TRUE
)

# Restrict cohort to those with elevated lab test
# Removed 396,425. 2,550,811 patients remaining
cohort_demog <- cohort_demog %>%
  filter(patid %in% patid_lab)

rm(hba1c_elevated, glucose_elevated, lab_pat_counts, lab_pat_counts_hba1c,
   lab_elevated)
gc()

  # # Convert lab results to dataframe
  # lab_elevated_df <- as.data.frame(lab_elevated)
  # 
  # # Filter lab records to those that are not before birth and not after 
  # # death, de-registration, date of last collection, or end of linkage
  # lab_elevated <- lab_elevated %>%
  #   left_join(cohort_demog %>% select(patid, yob, regenddate, deathdate, lcd), 
  #             by = "patid", relationship = "many-to-one") %>%
  #   # Set date of birth to Jan 1 of year of birth
  #   mutate(yob_day = as.Date(paste0(yob, "-01-01"), format = "%Y-%m-%d")) %>%
  #   # Remove records before birth: 0
  #   filter(eventdate >= yob_day) %>%
  #   # Remove records after death/de-registration/last collection: 8143
  #   filter(eventdate <= pmin(regenddate, deathdate, lcd, na.rm = TRUE))


# ============= Filter antidiabetic records to cohort ===============================

### Read in antidiabetic records

antidiab_file_names <-  c("pat_gold_final_1", "pat_aurum_final_1", "pat_aurum_final_2", "pat_aurum_final_3")
extraction_files_antidiab <- paste0(path_input, "Antidiabetics/", antidiab_file_names, ".parquet")

# Stack together all of the Gold and Aurum antidiabetic files
# 373,563,370 records from 3,218,404 patients
merge_sql <- paste(sprintf(
  "SELECT 
    patid, prodcode, productname, antidiabetic, eventdate, sysdate, database
  FROM read_parquet('%s')",
  extraction_files_antidiab), 
  collapse = "\n UNION ALL\n"
)

dbExecute(connection, sprintf(
  "CREATE OR REPLACE TABLE pat_antidiab_final AS\n%s", merge_sql))

# Quick peek
DBI::dbGetQuery(connection, "DESCRIBE pat_antidiab_final") 

# Filter to patients in the cohort
DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE pat_antidiab_cohort AS
  SELECT 
    a.patid, a.prodcode, a.productname, a.antidiabetic, a.eventdate, a.sysdate, a.database
  FROM pat_antidiab_final a
  WHERE EXISTS (
    SELECT 1
    FROM patid_lab p
    WHERE p.patid = a.patid
  )
")

# Check number of patients in cohort: 321,367,776 records from 2,200,909 patients
print(DBI::dbGetQuery(connection, "
  SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_antidiab_cohort;
"))

dbListTables(connection)

# Drop tables to free up memory
DBI::dbExecute(connection, "
  DROP TABLE IF EXISTS pat_antidiab_final;
")

gc()

# Export cohort table to parquet
out_cohort_parquet <- paste0(path_output, "pat_antidiab_cohort.parquet")
dbExecute(connection,
          sprintf("COPY pat_antidiab_cohort TO '%s' (FORMAT parquet);",
                  out_cohort_parquet))
# 
# # Read in cohort table
# dbExecute(connection, sprintf("
#   CREATE OR REPLACE TABLE pat_antidiab_cohort AS 
#   SELECT 
#     patid, prodcode, productname, antidiabetic, eventdate
#   FROM read_parquet('%s');
# ", out_cohort_parquet))

# # Create dbyplr table
# antidiab_tbl <- tbl(connection, "pat_antidiab_final")
# 
# # Filter to patients in the cohort
# antidiab_tbl <- antidiab_tbl %>%
#   filter(patid %in% patid_lab)


# ============= Exclude potential T1DM from antidiabetic records =======================================

## Remove patients with only insulin codes

# Get list of patients with only insulin records: 126,537 patients
dbExecute(connection, sprintf("
  CREATE OR REPLACE TABLE insulin_only_patids AS
  SELECT 
    patid 
  FROM pat_antidiab_cohort
  GROUP BY patid
  HAVING SUM(CASE WHEN antidiabetic <> 'Insulin' THEN 1 ELSE 0 END) = 0;
"))

# Filter antidiabetic records excluding insulin-only patids
dbExecute(connection, sprintf("
  CREATE OR REPLACE TABLE pat_antidiab_no_t1dm AS
  SELECT 
    a.patid, a.prodcode, a.productname, a.antidiabetic, a.eventdate
  FROM pat_antidiab_cohort a
  WHERE NOT EXISTS (
    SELECT 1
    FROM insulin_only_patids p
    WHERE p.patid = a.patid
  );
"))

## Checks
# How many insulin-only patients: 126,537
print(dbGetQuery(connection, "
  SELECT COUNT(*) AS n_insulin_only_patids
  FROM insulin_only_patids;
"))

# Check insulin-only patients do not have non-insulin antidiabetic records
# Value should be 0
print(dbGetQuery(connection, "
  SELECT COUNT(*) AS non_insulin_rows_among_insulin_only
  FROM pat_antidiab_cohort a
  INNER JOIN insulin_only_patids p
    ON a.patid = p.patid
  where a.antidiabetic <> 'Insulin';
"))

# Count how many patients are remaining after filtering out probably T1DM
# 301,556,685 records from 2,053,360 patients
# Note: this is only patients with antidiabetic records. For full number of 
# patients, need to combine with T2DM medcode patients
print(DBI::dbGetQuery(connection, "
  SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_antidiab_no_t1dm;
"))

# Drop tables to free up memory
DBI::dbExecute(connection, "
  DROP TABLE IF EXISTS pat_antidiab_cohort;
  DROP TABLE IF EXISTS insulin_only_patids
")

gc()

# Export filtered table to parquet
out_no_t1dm_parquet <- paste0(path_output, "pat_antidiab_no_t1dm.parquet")
dbExecute(connection, 
          sprintf("COPY pat_antidiab_no_t1dm TO '%s' (FORMAT parquet);", 
                  out_no_t1dm_parquet))

# # Read in filtered antidiabetic table
# dbExecute(connection, sprintf("
#   CREATE OR REPLACE TABLE pat_antidiab_no_t1dm AS
#   SELECT
#     patid, prodcode, productname, antidiabetic, eventdate
#   FROM read_parquet('%s');
# ", out_no_t1dm_parquet))

# # Drop tables to free up memory
# DBI::dbExecute(connection, "
#   DROP TABLE IF EXISTS pat_antidiab_no_t1dm;
# ")
# gc()


# ============= Exclude gestational diabetes from antidiabetic records =======================================

# TO ADD



# ============= Filter T2DM records to cohort and excluding potential T1DM ============================

# Upload patients with at least 1 elevated lab test to duckdb as a small table
DBI::dbWriteTable(connection,
                  "patid_lab",
                  data.frame(patid = patid_lab),
                  overwrite = TRUE
)

# Read in antidiabetics cohort table to help filter out T1DM
out_cohort_parquet <- paste0(path_output, "pat_antidiab_cohort.parquet")
dbExecute(connection, sprintf("
  CREATE OR REPLACE TABLE pat_antidiab_cohort AS
  SELECT
    patid, prodcode, productname, antidiabetic, eventdate
  FROM read_parquet('%s');
", out_cohort_parquet))

# Get list of patients with only insulin records (probable T1DM): 126,537 patients
dbExecute(connection, sprintf("
  CREATE OR REPLACE TABLE insulin_only_patids AS
  SELECT 
    patid 
  FROM pat_antidiab_cohort
  GROUP BY patid
  HAVING SUM(CASE WHEN antidiabetic <> 'Insulin' THEN 1 ELSE 0 END) = 0;
"))


### Read in T2DM records

extraction_files_t2dm <- paste0(path_input, "T2Diabetes/", "pat_comb_final.parquet")
dbExecute(connection, sprintf(
  "CREATE OR REPLACE TABLE pat_t2dm_final AS
  SELECT
    patid, medcode, term, eventdate, sysdate, database
  FROM read_parquet('%s');",
  extraction_files_t2dm
))
# Quick peek
DBI::dbGetQuery(connection, "DESCRIBE pat_t2dm_final") 

# Filter to patients in the cohort
DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE pat_t2dm_cohort AS
  SELECT 
    a.patid, a.medcode, a.term, a.eventdate, a.sysdate, a.database
  FROM pat_t2dm_final a
  WHERE EXISTS (
    SELECT 1
    FROM patid_lab p
    WHERE p.patid = a.patid
  )
")

# Check number of patients in cohort: 42,367,812 records from 2,478,038 patients
print(DBI::dbGetQuery(connection, "
  SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_t2dm_cohort;
"))

dbListTables(connection)

# Drop tables to free up memory
DBI::dbExecute(connection, "
  DROP TABLE IF EXISTS pat_t2dm_final;
")

gc()


## Remove patients with only insulin codes
dbExecute(connection, sprintf("
  CREATE OR REPLACE TABLE pat_t2dm_no_t1dm AS
  SELECT 
    a.patid, a.medcode, a.term, a.eventdate, a.sysdate, a.database
  FROM pat_t2dm_cohort a
  WHERE NOT EXISTS (
    SELECT 1
    FROM insulin_only_patids p
    WHERE p.patid = a.patid
  );
"))


# Count how many patients are remaining after filtering out probably T1DM
# 41,179,761 records from 2,371,065 patients
# Note: this is only patients with T2DM records. For full number of 
# patients, need to combine with antidiabetic prodcode patients
print(DBI::dbGetQuery(connection, "
  SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_t2dm_no_t1dm;
"))

# Drop tables to free up memory
DBI::dbExecute(connection, "
  DROP TABLE IF EXISTS pat_antidiab_cohort;
  DROP TABLE IF EXISTS patid_lab;
  DROP TABLE IF EXISTS insulin_only_patids;
  DROP TABLE IF EXISTS pat_t2dm_cohort;
")

gc()

# Export filtered table to parquet
out_t2dm_no_t1dm_parquet <- paste0(path_output, "pat_t2dm_no_t1dm.parquet")
dbExecute(connection, 
          sprintf("COPY pat_t2dm_no_t1dm TO '%s' (FORMAT parquet);", 
                  out_t2dm_no_t1dm_parquet))


# # Create dbplyr table
# t2dm_tbl <- tbl(connection, "pat_t2dm_final")
# 
# # Filter to patients in the cohort
# t2dm_tbl <- t2dm_tbl %>%
#   filter(patid %in% patid_lab)



# ============= Calculate total number of patients from T2DM and antidiabetic codes ============================

# Create a table of unique patids after filtering from T2DM and antidiabetic codes: 2,402,119 patients
# - in_medcode: 1 if patid appears in pat_t2dm_no_t1dm
# - in_prodcode: 1 if patid appears in pat_antidiab_no_t1dm
# - t2dm_type: 'medcode_only', 'prodcode_only', or 'both'
dbExecute(connection, "
  CREATE OR REPLACE TABLE patid_sources AS
  WITH 
    a AS (SELECT DISTINCT patid FROM pat_t2dm_no_t1dm),
    b AS (SELECT DISTINCT patid FROM pat_antidiab_no_t1dm)
  SELECT
    COALESCE(a.patid, b.patid) AS patid,
    CASE WHEN a.patid IS NOT NULL THEN 1 ELSE 0 END AS in_medcode,
    CASE WHEN b.patid IS NOT NULL THEN 1 ELSE 0 END AS in_prodcode,
    CASE
      WHEN a.patid IS NOT NULL AND b.patid IS NOT NULL THEN 'both'
      WHEN a.patid IS NOT NULL THEN 'medcode_only'
      WHEN b.patid IS NOT NULL THEN 'prodcode_only'
    END AS t2dm_type
  FROM a
  FULL OUTER JOIN b
    ON a.patid = b.patid;
")

# Get counts by t2dm_type: 2,022,306 both; 348,759 medcode only; 31,054 prodcode only
dbGetQuery(connection, "
  SELECT t2dm_type, COUNT(*) AS n_patids
  FROM patid_sources
  GROUP BY t2dm_type
  ORDER BY t2dm_type;
")

# Save list of patient IDs for study population
out_study_pop_ids <- paste0(path_output, "study_pop_patids.parquet")
dbExecute(connection, 
          sprintf("COPY patid_sources TO '%s' (FORMAT parquet);", 
                  out_study_pop_ids))

# Disconnect and wipe the spillover folder
dbDisconnect(connection, shutdown = TRUE)
closeAllConnections()
unlink(spill_dir, recursive = TRUE, force = TRUE)
gc()


# ================= Miscellaneous old code ===============================================

# # Combine T2DM and antidiabetics
# dbExecute(connection, sprintf(
#   "CREATE OR REPLACE TABLE pat_comb_final AS
#   SELECT
#     patid, eventdate, database,
#     medcode AS termcode,
#     term AS term
#   FROM pat_t2dm_cohort
#   UNION ALL
#   SELECT
#     patid, eventdate, database,
#     prodcode AS termcode,
#     productname AS term
#   FROM pat_antidiab_no_t1dm;
#   "))
# 
# # Save combined file
# out_parquet <- paste0(path_output, "pat_t2dm_antidiab_no_t1dm.parquet")
# dbExecute(connection,
#           sprintf("COPY pat_comb_final TO '%s' (FORMAT parquet);",
#                   out_parquet))
# 
# # Number of unique patients across t2dm and antidiab: 
# # Before removing T1DM and filtering to cohort: 422,397,882 records from 3,703,508 patients
# cat("\nTotal rows / patients:\n")
# print(dbGetQuery(
#   connection,
#   "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_comb_final;"))
# 
# # By Gold/Aurum
# print(dbGetQuery(
#   connection,
#   "SELECT database,
#     COUNT(*) AS n_rows,
#     COUNT(DISTINCT patid) AS n_pats
#   FROM pat_comb_final
#   GROUP BY database;"))
# 
# 
# 
# signal_table <- "pat_antidiab_final"
# signal_date_col <- "eventdate"
# signal_id_col <- "prodcode"
# signal_term_col <- "productname"
# 
# # Create helper function
# build_t2dm_lab_confirmed <- function(connection, 
#                                      signal_table = "t2dm_codes", signal_date_col = "eventdate", 
#                                      signal_id_col = "medcode", signal_term_col = "term",
#                                      hba1c_table = "hba1c_high", glucose_table = "glucose_high", 
#                                      lab_date_col = "eventdate", 
#                                      exclude_metformin = FALSE, metformin_prod_col = "prodcode",
#                                      require_min_labs = 1L, window_days = 365L, output_patids_only = TRUE) {
#   # Define table with med/product codes
#   sig <- tbl(connection, signal_table)
#   
#   # If excluding metformin-only, need to identify patients who only diabetes meds are metformin
#   if (exclude_metformin) {
#     # Need prodcode table
#     if(signal_id_col != "prodcode") {
#       stop("exclude_metformin = TRUE requires signal_id_col = 'prodcode'")
#     }
#     
#     sig_no_met <- sig %>%
#       filter(antidiabetic != "Metformin")
#   }
#   
#   # Get first medcode/prodcode date per patient = index date
#   
#   
# }
# 
# 
# # Number of patients excluding metformin. Memory: 3 GB
# df_no_met <- sig %>%
#   filter(antidiabetic != "Metformin") %>%
#   collect()
# # Number of non-metformin records
# nrow(df_no_met)
# # Number of patients with at least one non-metformin record
# length(unique(df_no_met$patid))
# 


