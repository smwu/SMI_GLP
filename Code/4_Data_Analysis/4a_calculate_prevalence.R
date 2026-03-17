# ====================================================================
# Calculate prevalence of GLP-1RA prescriptions over time
# Author: SM Wu
# Date Created: 2026/02/27
# Date Updated: 2026/02/27
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
path_input <- paste0("SMI_GLP/Data/Cleaning_Files/")
path_input_extract <- paste0("SMI_GLP/Data/Extraction_Files/")
path_input_codelist <- paste0("SMI_GLP/Code_Lists/Antidiabetics/")
path_output <- paste0("SMI_GLP/Data/Outputs/")

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


# =============== Calculate number of SMI patients taking GLP-1RAs ===========================================

### Process T2DM study population and SMI patient data

# Read in list of patient IDs for T2DM study population
out_study_pop_ids <- paste0(path_input, "study_pop_patids.parquet")
dbExecute(connection, sprintf("
  CREATE OR REPLACE TABLE patid_sources AS
  SELECT * FROM read_parquet('%s');
;", out_study_pop_ids))


# Read in extracted SMI and depression files
load(paste0(path_input_extract, "pat_smi_comb.RData"))
load(paste0(path_input_extract, "pat_depr_comb.RData"))

# Combine SMI and depression
pat_smid_comb <- bind_rows(pat_smi_comb, pat_depr_comb)

# Get unique number of patients with SMI
# Excluding depression: 99,388
length(unique(pat_smi_comb$patid))
# Including depression: 1,220,654
length(unique(pat_smid_comb$patid))

# Upload patients with SMI to duckdb as a small table
DBI::dbWriteTable(connection,
                  "pat_smid_comb",
                  pat_smid_comb,
                  overwrite = TRUE
)


### Process GLP-1RA data 

# Read in extracted GLP-1RA files. Memory: 2.3 GB
load(paste0(path_input_extract, "pat_glp_comb.RData"))
table(pat_glp_comb$`GLP-1RA`)

# Read in antidiabetic code lists
gold_code_list <- read_delim(
  file = paste0(path_input_codelist, "Gold_Antidiabetics_codelist_20251027.txt"), 
  delim = "\t", escape_double = FALSE, 
  col_types = cols(prodcode = col_character()),  trim_ws = TRUE) 
table(gold_code_list$group)
table(gold_code_list %>% filter(group %in% c("GLP-1RAs", "GLP-1RAs, Insulin")) %>% select(Antidiabetic))

aurum_code_list <- read_delim(
  file = paste0(path_input_codelist, "Aurum_Antidiabetics_codelist_20251027.txt"), 
  delim = "\t", escape_double = FALSE, 
  col_types = cols(prodcodeid = col_character()),  trim_ws = TRUE) 
table(aurum_code_list$group)
table(aurum_code_list %>% filter(group %in% c("GLP-1RAs", "GLP-1RAs, Insulin")) %>% select(Antidiabetic))

# Get names of antidiabetics considered under the GLP-1RA category
glp_names <- sort(c(union(unique(gold_code_list %>% 
                                     filter(group %in% c("GLP-1RAs", "GLP-1RAs, Insulin")) %>% 
                                     select(Antidiabetic)),
                   unique(aurum_code_list %>% 
                            filter(group %in% c("GLP-1RAs", "GLP-1RAs, Insulin")) %>% 
                            select(Antidiabetic))))[[1]])
glp_names

### Number of SMI patients taking any antidiabetics

# Read in antidiabetic prescriptions after population filtering
out_no_t1dm_parquet <- paste0(path_input, "pat_antidiab_no_t1dm.parquet")
dbExecute(connection, sprintf("
  CREATE OR REPLACE TABLE pat_antidiab_no_t1dm AS
  SELECT
    patid, prodcode, productname, antidiabetic, eventdate
  FROM read_parquet('%s');
", out_no_t1dm_parquet))


# Restrict to those with SMI (including depression)
DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE pat_antidiab_smid AS
  SELECT 
    a.patid, a.prodcode, a.productname, a.antidiabetic, a.eventdate
  FROM pat_antidiab_no_t1dm a
  WHERE EXISTS (
    SELECT 1
    FROM pat_smid_comb p
    WHERE p.patid = a.patid
  )
")

# Restrict to those with SMI (excluding depression)
DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE pat_antidiab_smi AS
  SELECT 
    a.patid, a.prodcode, a.productname, a.antidiabetic, a.eventdate
  FROM pat_antidiab_no_t1dm a
  WHERE EXISTS (
    SELECT 1
    FROM pat_smi_comb p
    WHERE p.patid = a.patid
  )
")


# Get number of patients w/ SMI taking antidiabetics: 
# Excluding depression: 11,180,794 records from 57,782 patients
print(DBI::dbGetQuery(connection, "
  SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_antidiab_smi;
"))
# Including depression: 117,710,607 records from 705,187 patients
print(DBI::dbGetQuery(connection, "
  SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_antidiab_smid;
"))


### Number of SMI patients taking GLP-1RAs

# Restrict to GLP prescriptions (case-insensitive string-contains with ILIKE)
# First create matching clause composed of all glp-1ra antidiabetic names
match_clause <- paste(
  sprintf("antidiabetic ILIKE '%%%s%%'", glp_names),
  collapse = " OR "
)
# Then filter antidiabetics to these glp-1ra prescriptions
dbExecute(connection, sprintf("
  CREATE OR REPLACE TABLE pat_glp AS
  SELECT 
    patid, prodcode, productname, antidiabetic, eventdate
  FROM pat_antidiab_no_t1dm
  WHERE %s
", match_clause))

# Quick peek
dbGetQuery(connection, "
  SELECT * FROM pat_glp
  LIMIT 10
")

# Get number of patients taking GLP-1RAs: 7,274,746 records from 242,695 patients
print(DBI::dbGetQuery(connection, "
  SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_glp;
"))

# Restrict to those with SMI (excluding depression)
DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE pat_glp_smi AS
  SELECT 
    a.patid, a.prodcode, a.productname, a.antidiabetic, a.eventdate
  FROM pat_glp a
  WHERE EXISTS (
    SELECT 1
    FROM pat_smi_comb p
    WHERE p.patid = a.patid
  )
")

# Restrict to those with SMI (including depression)
DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE pat_glp_smid AS
  SELECT 
    a.patid, a.prodcode, a.productname, a.antidiabetic, a.eventdate
  FROM pat_glp a
  WHERE EXISTS (
    SELECT 1
    FROM pat_smid_comb p
    WHERE p.patid = a.patid
  )
")

# Get number of patients w/ SMI taking GLP-1RAs: 
# Excluding depression: 207,404 records from 6641 patients
print(DBI::dbGetQuery(connection, "
  SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_glp_smi;
"))
# Including depression: 3,469,516 records from 112,037 patients
print(DBI::dbGetQuery(connection, "
  SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_glp_smid;
"))





# # Read in cohort of patients meeting age and registration criteria
# load(paste0(wd, path_output, "cohort_demog.Rdata"))



# Disconnect and wipe the spillover folder
dbDisconnect(connection, shutdown = TRUE)
closeAllConnections()
unlink(spill_dir, recursive = TRUE, force = TRUE)
gc()
