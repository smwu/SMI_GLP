# ====================================================================
# Create T2DM population
# Author: SM Wu
# Date Created: 2026/02/10
# Date Updated: 2026/02/24
# 
# Details: Create final analytic T2DM cohort by applying inclusion and 
# exclusion criteria based on age, registration, lab thresholds, T1DM, 
# and gestational diabetes 
# 
# Inputs:
#   1) SMI_GLP/Data/Extraction_Files/
#   2) SMI_GLP/Code_Lists/Pregnancy/
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
path_codelist <- paste0("SMI_GLP/Code_Lists/Pregnancy/")
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

# Initialize consort numbers
consort <- list()
consort[["num_registration"]] <- length(unique(cohort_demog$patid))


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
consort[["num_lab_elevated"]] <- length(patid_lab)

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

# Save patients with lab test to parquet
out_lab_parquet <- paste0(path_output, "patid_lab.parquet")
dbExecute(connection,
          sprintf("COPY patid_lab TO '%s' (FORMAT parquet);",
                  out_lab_parquet))

# Restrict cohort to those with elevated lab test
# Removed 396,425. 2,550,811 patients remaining
cohort_demog <- cohort_demog %>%
  filter(patid %in% patid_lab)

# Upload cohort demographics to duckdb as a table
DBI::dbWriteTable(connection, "cohort_demog", cohort_demog,
                  overwrite = TRUE)

rm(hba1c_elevated, glucose_elevated, lab_pat_counts, lab_pat_counts_hba1c,
   lab_elevated, cohort_demog)
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

# Get list of patients with only insulin records: 130,050 patients
dbExecute(connection, sprintf("
  CREATE OR REPLACE TABLE insulin_only_patids AS
  SELECT 
    patid 
  FROM pat_antidiab_cohort
  GROUP BY patid
  HAVING SUM(CASE WHEN antidiabetic <> 'Insulin' THEN 1 ELSE 0 END) = 0;
"))

# Store insulin only patids in R
pat_antidiab_insulin_only_patids <- dbGetQuery(connection, "SELECT patid FROM insulin_only_patids")

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
# How many insulin-only patients: 130,050 (previously 126,537)
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
# 302,739,962 records from 2,070,859 patients (old: 301,556,685 records from 2,053,360 patients)
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

# 2,070,859 unique patients w/ antidiabetic prescriptions
# DBI::dbGetQuery(connection, "
#   SELECT COUNT(DISTINCT patid) FROM pat_antidiab_no_t1dm AS n_no_t1dm;
# ")

# # Drop tables to free up memory
# DBI::dbExecute(connection, "
#   DROP TABLE IF EXISTS pat_antidiab_no_t1dm;
# ")
# gc()


# ============= Exclude gestational diabetes from antidiabetic records =======================================

# Filter out possible gestational-only diabetes using pregnancy-related codes
# 1) Infer pregnancy episode windows from pregnancy codes, merging overlapping windows
# 2) Exclude patients whose diabetes evidence occurs only within pregnancy episode windows

# Read in extracted pregnancy data
preg_path <- paste0(path_input, "Pregnancy/pat_comb_final.parquet")
dbExecute(connection, sprintf("
  CREATE OR REPLACE TABLE preg_table AS
  SELECT * FROM read_parquet('%s');
", preg_path))

# Peek
dbGetQuery(connection, "DESCRIBE preg_table")

dbListTables(connection)

# Read in pregnancy code lists to obtain pregnancy period groupings
# GOLD code list
gold_file_name <- list.files(path = paste0(wd, path_codelist),
                             pattern = paste0("^Gold_Pregnancy_codelist"))
# Check date
gold_file_name
codelist_gold <- read_delim(
  file = paste0(wd, path_codelist, gold_file_name), 
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character()),  trim_ws = TRUE)
# Convert weeks to early, late, postnatal, unspecified
codelist_gold <- codelist_gold %>%
  mutate(period = case_when(
    as.numeric(period) <= 20 ~ "early pregnancy",
    as.numeric(period) >= 21 & as.numeric(period) <= 40 ~ "late pregnancy",
    as.numeric(period) >= 41 ~ "postnatal",
    .default = period
  ))
table(codelist_gold$period)

# AURUM code list
aurum_file_name <- list.files(path = paste0(wd, path_codelist),
                              pattern = paste0("^Aurum_Pregnancy_codelist"))
# Check date
aurum_file_name
codelist_aurum <- read_delim(
  file = paste0(wd, path_codelist, aurum_file_name), 
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character()), trim_ws = TRUE)
# Convert weeks to early, late, postnatal, unspecified
codelist_aurum <- codelist_aurum %>%
  mutate(period = case_when(
    as.numeric(period) <= 20 ~ "early pregnancy",
    as.numeric(period) >= 21 & as.numeric(period) <= 40 ~ "late pregnancy",
    as.numeric(period) >= 41 ~ "postnatal",
    .default = period
  ))
table(codelist_aurum$period)

# Upload coadlists to duckdb
dbWriteTable(connection, "codelist_gold",  codelist_gold,  overwrite = TRUE)
dbWriteTable(connection, "codelist_aurum", codelist_aurum, overwrite = TRUE)

# Add in the pregnancy period groupings to the pregnancy data table
DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE preg_table_with_period AS
  SELECT
    p.*,
    CASE 
      WHEN p.database = 'Gold' THEN g.period
      -- 'high risk pregnancy due to recurrent pregnancy loss' in Aurum
      WHEN p.medcode = '8042451000006119' THEN 'unspecified'
      WHEN p.database = 'Aurum' THEN a.period
      ELSE 'unspecified'
    END AS period
  FROM preg_table p
  LEFT JOIN codelist_gold g
    ON p.medcode = g.medcode
  LEFT JOIN codelist_aurum a
    ON p.medcode = a.medcodeid;
")
# Check values
DBI::dbGetQuery(connection, "
  SELECT period, COUNT(*) AS n_rows
  FROM preg_table_with_period
  GROUP BY period
  ORDER BY n_rows DESC;
")
DBI::dbGetQuery(connection, "
  SELECT database, COUNT(*) AS n_rows
  FROM preg_table_with_period
  GROUP BY database
  ORDER BY n_rows DESC;
")
DBI::dbGetQuery(connection, "
  SELECT * 
  FROM preg_table_with_period
  WHERE period IS NULL
  LIMIT 10;
")

# Drop tables to free up memory
DBI::dbExecute(connection, "
  DROP TABLE IF EXISTS preg_table;
")
gc()


# Create approximate pregnancy/puerperium time windows from pregnancy-related codes
# (early, late, postnatal, or unspecified) by expanding each code date into a
# stage-specific start/end interval, then merges overlapping/nearby
# intervals into patient-level pregnancy episodes so diabetes evidence can be 
# classified as occurring only during pregnancy/puerperium vs outside those periods
# Total duration: 42 weeks

# Early: 0-20 weeks, midpoint of 10 weeks
early_start_back_days <- 70 # backdate ~10 weeks
early_end_forward_days <- 224 # extend ~32 weeks

# Late: 21-40 weeks, midpoint of 30
late_start_back_days <- 210 # backdate ~30 weeks
late_end_forward_days <- 84 # extend ~12 weeks

# Postnatal: delivery to 2 weeks post. Usually gestational diabetes ends immediately after birth
postnatal_start_back_days <- 280 # backdate ~40 weeks
postnatal_end_forward_days <- 14 # puerperium extend ~2 weeks

# Unspecified: backdate and extend 21 weeks
unspec_start_back_days <- 147
unspec_end_forward_days <- 147


sql_preg_windows <- sprintf("
  CREATE OR REPLACE TABLE preg_windows AS
  SELECT
    patid,
    CAST(eventdate AS DATE) AS code_date,
    period,
    CASE
      WHEN period = 'early' THEN CAST(eventdate AS DATE) - INTERVAL '%d days'
      WHEN period = 'late' THEN CAST(eventdate AS DATE) - INTERVAL '%d days'
      WHEN period = 'postnatal' THEN CAST(eventdate AS DATE) - INTERVAL '%d days'
      WHEN period = 'unspecified' THEN CAST(eventdate AS DATE) - INTERVAL '%d days'
      ELSE NULL
    END AS win_start,
    CASE
      WHEN period = 'early' THEN CAST(eventdate AS DATE) - INTERVAL '%d days'
      WHEN period = 'late' THEN CAST(eventdate AS DATE) - INTERVAL '%d days'
      WHEN period = 'postnatal' THEN CAST(eventdate AS DATE) - INTERVAL '%d days'
      WHEN period = 'unspecified' THEN CAST(eventdate AS DATE) - INTERVAL '%d days'
      ELSE NULL
    END AS win_end
  FROM preg_table_with_period
", early_start_back_days, late_start_back_days, postnatal_start_back_days, unspec_start_back_days,
   early_end_forward_days, late_end_forward_days, postnatal_end_forward_days, unspec_end_forward_days
)

DBI::dbExecute(connection, sql_preg_windows)
DBI::dbExecute(connection, "CREATE INDEX IF NOT EXISTS idx_preg_windows_patid ON preg_windows(patid);")

# Export pregnancy windows table to parquet
out_preg_windows_parquet <- paste0(path_output, "preg_windows.parquet")
dbExecute(connection, 
          sprintf("COPY preg_windows TO '%s' (FORMAT parquet);", 
                  out_preg_windows_parquet))


# Identify patients with antidiabetic prescriptions outside all pregnancy windows
DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE patids_antidiab_non_preg AS
  SELECT DISTINCT p.patid
  FROM pat_antidiab_no_t1dm p
  WHERE NOT EXISTS (
    SELECT 1
    FROM preg_windows w
    WHERE w.patid = p.patid
      AND p.eventdate BETWEEN w.win_start AND w.win_end
  );
")
DBI::dbExecute(connection, "CREATE INDEX IF NOT EXISTS idx_antidiab_non_preg_patid ON patids_antidiab_non_preg(patid);")

# Restrict to patients with antidiabetic prescriptions outside all pregnancy windows
sql_filter <- sprintf("
  CREATE OR REPLACE TABLE pat_antidiab_no_t1dm_no_gest AS
  SELECT c.*
  FROM pat_antidiab_no_t1dm c
  WHERE EXISTS (
    SELECT 1
    FROM patids_antidiab_non_preg x
    WHERE x.patid = c.patid
  );
")
DBI::dbExecute(connection, sql_filter)

# 2,068,234 after gestational filtering. 2,070,859 before
DBI::dbGetQuery(connection, "
  SELECT
    (SELECT COUNT(DISTINCT patid) FROM pat_antidiab_no_t1dm) AS n_before,
    (SELECT COUNT(DISTINCT patid) FROM pat_antidiab_no_t1dm_no_gest) AS n_after,
    (SELECT COUNT(*) FROM preg_windows) AS n_preg_windows;
")

# Export filtered table to parquet. 302,727,096 records from 2,068,234 patients
out_no_t1dm_no_gest_parquet <- paste0(path_output, "pat_antidiab_no_t1dm_no_gest.parquet")
dbExecute(connection, 
          sprintf("COPY pat_antidiab_no_t1dm_no_gest TO '%s' (FORMAT parquet);", 
                  out_no_t1dm_no_gest_parquet))

# # Drop tables to free up memory
# dbListTables(connection)
# DBI::dbExecute(connection, "
#   DROP TABLE IF EXISTS codelist_aurum;
#   DROP TABLE IF EXISTS codelist_gold;
#   DROP TABLE IF EXISTS preg_table_with_period;
#   DROP TABLE IF EXISTS patids_antidiab_non_preg;
#   DROP TABLE IF EXISTS preg_windows;
#   DROP TABLE IF EXISTS pat_antidiab_no_t1dm;
# ")
# gc()

dbDisconnect(connection, shutdown = TRUE)
closeAllConnections()
unlink(spill_dir, recursive = TRUE, force = TRUE)
gc()

# # Read in filtered antidiabetic table
# dbExecute(connection, sprintf("
#   CREATE OR REPLACE TABLE pat_antidiab_no_t1dm_no_gest AS
#   SELECT
#     patid, prodcode, productname, antidiabetic, eventdate
#   FROM read_parquet('%s');
# ", out_no_t1dm_no_gest_parquet))



# ============= Filter T2DM records to cohort and excluding potential T1DM or gestational DM ============================

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

### Read in antidiabetics cohort table to get list of insulin-only patients

out_cohort_parquet <- paste0(path_output, "pat_antidiab_cohort.parquet")
dbExecute(connection, sprintf("
  CREATE OR REPLACE TABLE pat_antidiab_cohort AS
  SELECT
    patid, prodcode, productname, antidiabetic, eventdate
  FROM read_parquet('%s');
", out_cohort_parquet))

# Get list of patients with only insulin records (probable T1DM): 130,050 patients
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

## Get those with at least 1 lab test
out_lab_parquet <- paste0(path_output, "patid_lab.parquet")
dbExecute(connection,
          sprintf("CREATE OR REPLACE TABLE patid_lab AS
                  SELECT * FROM read_parquet('%s');",
                  out_lab_parquet))

# Filter to patients in the cohort passing lab thresholds
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

# Check number of patients in cohort
# 42,602,801 records from 2,499,531 patients (old: 42,367,812 records from 2,478,038 patients)
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
# 41,388,353 records from 2,389,444 patients (old: 41,179,761 records from 2,371,065 patients)
# Note: this is only patients with T2DM records. For full number of 
# patients, need to combine with antidiabetic prodcode patients
print(DBI::dbGetQuery(connection, "
  SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_t2dm_no_t1dm;
"))

# Export filtered table to parquet
out_t2dm_no_t1dm_parquet <- paste0(path_output, "pat_t2dm_no_t1dm.parquet")
dbExecute(connection, 
          sprintf("COPY pat_t2dm_no_t1dm TO '%s' (FORMAT parquet);", 
                  out_t2dm_no_t1dm_parquet))

# Drop tables to free up memory
DBI::dbExecute(connection, "
  DROP TABLE IF EXISTS pat_t2dm_cohort;
")
gc()

## Remove patients with possible gestational diabetes (T2DM codes only during periods of pregnancy)

# Read in pregnancy windows table
out_preg_windows_parquet <- paste0(path_output, "preg_windows.parquet")
dbExecute(connection, sprintf("
  CREATE OR REPLACE TABLE preg_windows AS
  SELECT * FROM read_parquet('%s');
", out_preg_windows_parquet))


# Identify patients with T2DM outside all pregnancy windows
DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE patids_t2dm_non_preg AS
  SELECT DISTINCT p.patid
  FROM pat_t2dm_no_t1dm p
  WHERE NOT EXISTS (
    SELECT 1
    FROM preg_windows w
    WHERE w.patid = p.patid
      AND p.eventdate BETWEEN w.win_start AND w.win_end
  );
")
DBI::dbExecute(connection, "CREATE INDEX IF NOT EXISTS idx_t2dm_non_preg_patid ON patids_t2dm_non_preg(patid);")

# Restrict to patients with T2DM outside all pregnancy windows
sql_filter <- sprintf("
  CREATE OR REPLACE TABLE pat_t2dm_no_t1dm_no_gest AS
  SELECT c.*
  FROM pat_t2dm_no_t1dm c
  WHERE EXISTS (
    SELECT 1
    FROM patids_t2dm_non_preg x
    WHERE x.patid = c.patid
  );
")
DBI::dbExecute(connection, sql_filter)

# Count how many patients are remaining after filtering out probable gestational diabetes
# 41,383,638 records from 2,387,215 patients
# Note: this is only patients with T2DM records. For full number of 
# patients, need to combine with antidiabetic prodcode patients
print(DBI::dbGetQuery(connection, "
  SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_t2dm_no_t1dm_no_gest;
"))


# Drop tables to free up memory
DBI::dbExecute(connection, "
  DROP TABLE IF EXISTS pat_antidiab_cohort;
  DROP TABLE IF EXISTS patid_lab;
  DROP TABLE IF EXISTS insulin_only_patids;
  DROP TABLE IF EXISTS pat_t2dm_cohort;
  DROP TABLE IF EXISTS pat_t2dm_no_t1dm;
  DROP TABLE IF EXISTS patids_t2dm_non_preg
")

gc()

# Export filtered table to parquet
out_t2dm_no_t1dm_no_gest_parquet <- paste0(path_output, "pat_t2dm_no_t1dm_no_gest.parquet")
dbExecute(connection, 
          sprintf("COPY pat_t2dm_no_t1dm_no_gest TO '%s' (FORMAT parquet);", 
                  out_t2dm_no_t1dm_no_gest_parquet))

# # Read in t2dm medcodes after removing T1DM and gestational diabetes
# dbExecute(connection, sprintf("
#   CREATE OR REPLACE TABLE pat_t2dm_no_t1dm_no_gest AS
#   SELECT * FROM read_parquet('%s');
# ", out_t2dm_no_t1dm_no_gest_parquet))


# ============= Calculate total number of patients from T2DM and antidiabetic codes ============================

# Read in  pat_antidiab_no_t1dm and pat_t2dm_no_t1dm
dbExecute(connection, sprintf("
  CREATE OR REPLACE TABLE pat_antidiab_no_t1dm AS
  SELECT * FROM read_parquet('%s');
", paste0(path_output, "pat_antidiab_no_t1dm.parquet")))
dbExecute(connection, sprintf("
  CREATE OR REPLACE TABLE pat_t2dm_no_t1dm AS
  SELECT * FROM read_parquet('%s');
", paste0(path_output, "pat_t2dm_no_t1dm.parquet")))

### Number of unique patients after filtering T1DM, using both medcodes and antidiabetic codes

# Create a table of unique patids after filtering from T2DM and antidiabetic codes:
# 2,418,373 patients (old: 2,402,119)
# - in_medcode: 1 if patid appears in pat_t2dm_no_t1dm
# - in_prodcode: 1 if patid appears in pat_antidiab_no_t1dm
# - t2dm_type: 'medcode_only', 'prodcode_only', or 'both'
dbExecute(connection, "
  CREATE OR REPLACE TABLE patid_no_t1dm_sources AS
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

# Store patids of patients with no t1dm in R
pat_all_no_t1dm_patids <- dbGetQuery(connection, "SELECT patid FROM patid_no_t1dm_sources")

# Number of patients without probably t1dm
consort[["num_no_t1dm"]] <- length(unique(pat_all_no_t1dm_patids$patid))

# Drop tables to free up memory
DBI::dbExecute(connection, "
  DROP TABLE IF EXISTS pat_antidiab_no_t1dm;
  DROP TABLE IF EXISTS pat_t2dm_no_t1dm;
")

gc()


### Read in antidiabetic prescriptions after removing T1DM and gestational diabetes

out_no_t1dm_no_gest_parquet <- paste0(path_output, "pat_antidiab_no_t1dm_no_gest.parquet")
dbExecute(connection, sprintf("
  CREATE OR REPLACE TABLE pat_antidiab_no_t1dm_no_gest AS
  SELECT
    patid, prodcode, productname, antidiabetic, eventdate
  FROM read_parquet('%s');
", out_no_t1dm_no_gest_parquet))

# Create a table of unique patids after filtering from T2DM and antidiabetic codes:
# 2,418,373 patients (old: 2,402,119)
# - in_medcode: 1 if patid appears in pat_t2dm_no_t1dm_no_gest
# - in_prodcode: 1 if patid appears in pat_antidiab_no_t1dm_no_gest
# - t2dm_type: 'medcode_only', 'prodcode_only', or 'both'
dbExecute(connection, "
  CREATE OR REPLACE TABLE patid_sources AS
  WITH 
    a AS (SELECT DISTINCT patid FROM pat_t2dm_no_t1dm_no_gest),
    b AS (SELECT DISTINCT patid FROM pat_antidiab_no_t1dm_no_gest)
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

# Store patids of patients excluding gestational diabetes in R
pat_all_no_t1dm_no_gest_patids <- dbGetQuery(connection, "SELECT patid FROM patid_sources")

# Number of patients excluding gestational diabetes
consort[["num_no_gest"]] <- length(unique(pat_all_no_t1dm_no_gest_patids$patid))

# Get counts by t2dm_type
# 2,037,076 both, 350,139 medcode only, 31,158 prodcode only
# Old: 2,022,306 both; 348,759 medcode only; 31,054 prodcode only
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

# See consort values
consort
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
# 
# # Restrict to records after birth and before death: 5812 records dropped. 
# # 40 GB
# dbExecute(connection, "
#   CREATE OR REPLACE TABLE pat_antidiab_restrict AS
#   SELECT
#     p.patid, p.prodcode, p.productname, p.antidiabetic, p.eventdate, p.sysdate, p.database
#   FROM pat_antidiab_final AS p
#   INNER JOIN cohort_demog as C
#     ON p.patid = c.patid
#   WHERE EXTRACT(YEAR FROM p.eventdate) >= c.yob
#     AND (c.deathdate IS NULL OR p.eventdate <= c.deathdate)
# ")
# 
# # Drop tables to free up memory
# DBI::dbExecute(connection, "
#   DROP TABLE IF EXISTS pat_antidiab_final;
#   DROP TABLE IF EXISTS cohort_demog;
# ")
# gc()
