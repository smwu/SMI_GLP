# ==============================================================================
# Clean T2DM diagnosis and antidiabetic prescriptions among T2DM study cohort
# Author: SM Wu
# Date Created: 2026/03/30
# Date Updated: 2026/03/30
# 
# Details:
# 1) Setup and restrict cohort demographics to final T2DM cohort
# 2) Get date of T2DM diagnosis and update start of follow-up 
# 3) Partition and save antidiabetic prescriptions
#
# Inputs:
# 1) SMI_GLP/Data/Cleaning_Files/cohort_demog.RData: Patient demographic data
# 2) SMI_GLP/Data/Cleaning_Files/pat_t2dm_no_t1dm_no_gest.parquet: T2DM medical records
# 3) SMI_GLP/Data/Cleaning_Files/pat_antidiab_no_t1dm_no_gest.parquet: Antidiabetic medical records
# 4) SMI_GLP/Code_List/Antidiabetics: Gold and Aurum antidiabetics code lists
# 
# Final Outputs:
# 1) SMI_GLP/Data/Cleaned_Data/cohort_demog_cleaned.RData: Cleaned cohort data 
#    including T2DM diagnosis date and updated start to follow-up
# 2) SMI_GLP/Data/Cleaned_Data/cohort_demog_cleaned_incident.RData: Cleaned cohort data 
#    filtered to those with incident T2DM during the study period
# 3) SMI_GLP/Data/Cleaned_Data/glp1ras.parquet: Antidiabetic prescriptions for glp1ras. 
#    Similar files exist for dpp4is, metformin, sglt2is, sulfonylureas, other, insulin

# ==============================================================================

# Clear memory
rm(list = ls())
gc()

# Packages
library(dplyr)
library(gtsummary)
library(lubridate)
library(readr)
library(forcats)
library(data.table) # fast cleaning
library(tidylog) # track filtering numbers
library(DBI)     # database interface
library(duckdb)  # connect to SQL
library(dbplyr)  # dplyr w/ SQL
library(arrow)   # read in parquet files


# ========== 1) Setup and restrict cohort demographics to final T2DM cohort =====


# Set working directory
wd <- "S:/CDSTP_CPRD_25_005368/"
setwd(wd)

# Set input and output paths
path_input <- paste0("SMI_GLP/Data/Cleaning_Files/")
path_codelist <- paste0("SMI_GLP/Code_Lists/Antidiabetics/")
path_output <- paste0("SMI_GLP/Data/Cleaned_Data/")

# Set up DuckDB in-memory connection
connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
# Allow spilling to local disk after hitting memory limit
DBI::dbExecute(connection, "PRAGMA memory_limit = '40GB';")
spill_dir <- "N:/Temp/duckdb_spill"
dir.create(spill_dir, showWarnings = FALSE, recursive = TRUE)
DBI::dbExecute(connection, sprintf("PRAGMA temp_directory='%s';", spill_dir))
# Set up progress bar
DBI::dbExecute(connection, "SET enable_progress_bar = true;")
DBI::dbExecute(connection, "SET enable_progress_bar_print = true;")
# Set to 1 thread and set seed for reproducible RNG
dbExecute(connection, "SET threads=1;")
dbExecute(connection, "SELECT setseed(0.42);")


# Read in final T2DM cohort patids 
study_pop_patids <- DBI::dbGetQuery(connection, sprintf(
  "SELECT * FROM read_parquet('%s');", 
  paste0(path_input, "study_pop_patids.parquet")))

# Read in cohort of patients meeting age and registration criteria
load(paste0(wd, path_input, "cohort_demog.Rdata")) # created in 2a_clean_cohort_demog.R

# Restrict cohort to those in final T2DM cohort: 2,418,373 patients
cohort_demog_cleaned <- cohort_demog %>%
  filter(patid %in% study_pop_patids$patid)
# Convert to data.table for faster wrangling
setDT(cohort_demog_cleaned)

# Save memory
rm(cohort_demog)


# ========== 2) Get date of T2DM diagnosis and update start of follow-up =====

### Read in T2DM medical records containing index date as data.table: 4 GB
# 41,383,638 records from 2,387,215 patients (note: doesn't include antidiab-only patients)
pat_t2dm <- as.data.table(arrow::read_parquet(
  paste0(path_input, "pat_t2dm_no_t1dm_no_gest.parquet")))
length(unique(pat_t2dm$patid))

# Add in patient registration and birth info, expanding to entire T2DM cohort (31,158 antidiab-only)
# 41,414,796 records from 2,418,373 patients
pat_comb_final <- pat_t2dm[
  cohort_demog_cleaned[, !"acceptable"],
  on = .(patid)
]
length(unique(pat_comb_final$patid))

# Restrict to records after birth and before death: 5812 records dropped. 
# 41,408,984 remaining from 2,418,328 patients (45 dropped)
pat_comb_final <- pat_comb_final[(is.na(eventdate) | year(eventdate) >= yob) & 
                                   (is.na(eventdate) | is.na(deathdate) | eventdate <= deathdate)]
length(unique(pat_comb_final$patid))

# Write birth and death information to duckdb
pat_yob <- unique(as.data.table(pat_comb_final[, .(patid, yob, deathdate)]))
dbWriteTable(connection, "pat_yob", pat_yob, temporary = TRUE, overwrite = TRUE)

# Save memory
rm(pat_t2dm)
rm(pat_yob)
gc()


# Get first T2DM diagnosis per patient
# Setorder puts earliest date first, then medcode/term used to break ties if same day
# Filter to one row per patient 
setorder(pat_comb_final, patid, eventdate, medcode, term) 
dx_first <- unique(pat_comb_final, by = "patid")[
  , .(patid, medcode, term, dx_date = eventdate)]

# Get list of patients for whom medication information is needed
dx_patids <- data.table(patid = unique(dx_first$patid))
# Add to duckdb
dbWriteTable(connection, "dx_patids", dx_patids, temporary = TRUE, overwrite = TRUE)
# Save memory
rm(dx_patids)
gc()

# # Save dx_first
# save(dx_first, file = paste0(path_input, "dx_first_t2dm.RData"))
# load(paste0(path_input, "dx_first_t2dm.RData"))


### Read in antidiabetic records 

# Make paths SQL friendly
out_no_t1dm_no_gest_parquet <- paste0(path_input, "pat_antidiab_no_t1dm_no_gest.parquet")
antidiab_path <- paste0("list_value(", 
                        paste(shQuote(out_no_t1dm_no_gest_parquet), collapse = ", "), ")")

# Get first antidiabetic prescription date using SQL, excluding records before birth or after death

pat_antidiab <- dbGetQuery(connection, sprintf("
  SELECT 
    a.patid, 
    MIN(CAST(a.eventdate AS DATE)) as antidiab_date
  FROM read_parquet(%s) a
  JOIN dx_patids p USING(patid)
  JOIN pat_yob y USING(patid)
  WHERE a.eventdate IS NOT NULL
    AND CAST(a.eventdate AS DATE) >= MAKE_DATE(y.yob, 1, 1)
    AND (y.deathdate IS NULL OR CAST(a.eventdate AS DATE) <= CAST(y.deathdate AS DATE))
  GROUP BY a.patid
", antidiab_path))


# Convert antidiabetics data to data.table and ensure dates
antidiab_first <- as.data.table(pat_antidiab)[
  , drug_date := as.IDate(antidiab_date, format = "%Y-%m-%d")
]
# Set keys for easy matching by patid
setkey(antidiab_first, patid)

# Combine into medcode-based dx_first data table
dx_first <- antidiab_first[dx_first, on = "patid"]

# Get drug source
dx_first[, drug_source := "antidiabetic"]

# Compute index date as earlier of diagnosis date and drug date
dx_first[, index_date := pmin(dx_date, drug_date, na.rm = TRUE)]
dx_first[is.infinite(as.numeric(index_date)), index_date := as.IDate(NA)]

# Get index date source type
dx_first[
  , index_source := fifelse(
    !is.na(index_date) & index_date == dx_date, "medcode",
    fifelse(!is.na(index_date) & !is.na(drug_date) & index_date == drug_date, drug_source, 
            NA_character_)
  )
]

# Check no NAs in source type
table(dx_first$index_source, useNA = "always")
prop.table(table(dx_first$index_source, useNA = "always"))

# Final t2dm diagnosis date table
t2dm_dx_date <- dx_first[, .(patid, medcode, term, index_date, index_source)]
head(t2dm_dx_date)


### Add t2dm_dx_date to T2DM patient demographic data: 2,418,328 patients

cohort_demog_dx_date <- cohort_demog_cleaned %>% select(-acceptable) %>%
  inner_join(t2dm_dx_date, by = join_by(patid)) %>%
  mutate(index_date = as.Date(index_date, "%Y-%m-%d"),
         index_year = year(index_date))

# Make sure index_year is after birth and before death: none removed
cohort_demog_dx_date <- cohort_demog_dx_date %>%
  filter(index_year >= yob) %>%
  filter(is.na(deathdate) | index_year <= year(deathdate))

hist(cohort_demog_dx_date$index_year, main = "Histogram of Index Years for T2DM", xlab = "Index Year",
     breaks = seq(min(cohort_demog_dx_date$index_year)-1, max(cohort_demog_dx_date$index_year)))
abline(v = 2005, col = "blue")
summary(cohort_demog_dx_date$index_year)


### Update start of follow-up and duration of follow-up

# Define start of study period: Jan 1, 2005
# Define end of study period: May 5, 2025, since we are using June 2025 release
study_start_date <- as.Date("2005-01-01", format = "%Y-%m-%d")
study_end_date <- as.Date("2025-05-05", format = "%Y-%m-%d")

# Follow-up start date: latest of date of valid research quality post-registration, 
# year turned 18, start of study period, and date of T2DM diagnosis
cohort_demog_dx_date$startfollow <- pmax(cohort_demog_dx_date$date_valid, 
                                         cohort_demog_dx_date$year18, 
                                         study_start_date, 
                                         cohort_demog_dx_date$index_date, na.rm=TRUE)


# Update days within follow-up period
cohort_demog_dx_date$daysfollow <- as.numeric(
  cohort_demog_dx_date$endfollow - cohort_demog_dx_date$startfollow)

# How many were not in the follow-up period: Aurum 229,179, Gold 88,829
sum(aurum_pat$daysfollow <= 0)
sum(gold_pat$daysfollow <= 0)


# Create a flag for incident T2DM during follow-up
cohort_demog_dx_date <- cohort_demog_dx_date %>%
  mutate(incident_t2dm = ifelse(index_date >= startfollow, 1, 0))


# # Save final T2DM cohort with diagnosis dates included
# save(cohort_demog_dx_date, file = paste0(path_output, "cohort_demog_cleaned.RData"))



# How many have first T2DM diagnosis during follow-up: 1,254,862
# 1,163,463 (48%) removed
cohort_demog_dx_date_incident <- cohort_demog_dx_date %>%
  filter(incident_t2dm == 1)

# Total number with incident T2DM during study period: 1,254,862 (948,689 Aurum and 306,173 Gold)
length(unique(cohort_demog_dx_date_incident$patid))
table(cohort_demog_dx_date_incident$database)

hist(cohort_demog_dx_date_incident$index_year, main = "Histogram of Index Years for Incident T2DM Cases", 
     xlab = "Index Year",
     breaks = seq(min(cohort_demog_dx_date_incident$index_year)-1, max(cohort_demog_dx_date_incident$index_year)))

# # Save T2DM incident case cohort satisfying age and registration criteria
# save(cohort_demog_dx_date_incident, file = paste0(path_output, "cohort_demog_cleaned_incident.RData"))


# Check tables
dbListTables(connection)

# Save memory
rm(antidiab_first, cohort_demog_cleaned, cohort_demog_dx_date, cohort_demog_dx_date_incident, dx_first, 
   pat_antidiab, pat_comb_final, study_pop_patids, t2dm_dx_date)
gc()


# ========== 3) Partition and save antidiabetic prescriptions ============================================

# Read in antidiabetic prescriptions 
out_no_t1dm_no_gest_parquet <- paste0(path_input, "pat_antidiab_no_t1dm_no_gest.parquet")
dbExecute(connection, sprintf("
  CREATE OR REPLACE TABLE pat_antidiab_no_t1dm_no_gest AS
  SELECT * FROM read_parquet('%s');
", out_no_t1dm_no_gest_parquet))

### Read in antidiabetic groups using code lists

# GOLD code list
code_name <- "Antidiabetics"
gold_file_name <- list.files(path = paste0(wd, path_codelist),
                             pattern = paste0("^Gold_", code_name, "_codelist"))
# Check date
gold_file_name
codelist_gold <- read_delim(
  file = paste0(wd, path_codelist, gold_file_name), 
  delim = "\t", escape_double = FALSE, 
  col_types = cols(prodcode = col_character()),  trim_ws = TRUE)

# AURUM code list
aurum_file_name <- list.files(path = paste0(wd, path_codelist),
                              pattern = paste0("^Aurum_", code_name, "_codelist"))
# Check date
aurum_file_name
codelist_aurum <- read_delim(
  file = paste0(wd, path_codelist, aurum_file_name), 
  delim = "\t", escape_double = FALSE, 
  col_types = cols(prodcodeid = col_character(),
                   BNFChapter = col_character()), trim_ws = TRUE)

# Get mapping from antidiabetic to group
antidiab_group <- rbind(codelist_gold %>% select(Antidiabetic, group),
                        codelist_aurum %>% select(Antidiabetic, group)) %>% distinct() %>%
  rename(antidiabetic = Antidiabetic, antidiab_group = group)
antidiab_group
# Add to duckdb
dbWriteTable(connection, "antidiab_group", antidiab_group, temporary = TRUE, overwrite = TRUE)

antidiab_group_list <- c("GLP-1RAs", "DPP-4is", "SGLT-2is", "Sulfonylureas", 
                         "Metformin", "Insulin", "Other")


# Save all prescriptions for each antidiabetic group

for (antidiab_group in antidiab_group_list) {
  # Convert name to lower-case and remove hyphens
  file_stub <- tolower(gsub("[^A-Za-z0-9]+", "", antidiab_group))
  out_file <- paste0(path_output, paste0(file_stub), ".parquet")
  
  # Escape single quotes for SSQL
  antidiab_group_sql <- gsub("'", "''", antidiab_group, fixed = TRUE)
  
  sql <- sprintf("
    COPY (
      SELECT 
        p.*,
        g.antidiab_group
      FROM pat_antidiab_no_t1dm_no_gest p
      LEFT JOIN antidiab_group AS g
        USING (antidiabetic)
      WHERE antidiab_group ILIKE '%%%s%%'
    ) TO '%s' (FORMAT parquet)
  ", antidiab_group_sql, out_file)
  
  dbExecute(connection, sql)
}


# Disconnect and wipe the spillover folder
dbDisconnect(connection, shutdown = TRUE)
closeAllConnections()
unlink(spill_dir, recursive = TRUE, force = TRUE)
gc()
