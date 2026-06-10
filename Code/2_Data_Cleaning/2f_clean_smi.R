# ==============================================================================
# Clean SMI diagnoses among patients in T2DM study cohort
# Author: SM Wu
# Date Created: 2026/03/30
# Date Updated: 2026/05/26
# 
# Details:
# 1) Read in SMI extracted medical files for those in T2DM cohort. Depression 
#    restricted to those with at least one antidepressant medication.
# 2) Get SMI medcode diagnosis date and SMI type. Add future diagnosis dates 
#    for SMI hierarchy.
# 3) Add in psychotropic prescriptions to get diagnosis date, defined as 
#    earliest of medcode or prodcode.
# 4) Add SMI diagnosis information to T2DM patient demographic data. Set to 
#    NA for those without SMI.
#
# Inputs:
# 1) SMI_GLP/Data/Cleaned_Data/cohort_demog_cleaned.RData: Cleaned cohort data 
# 2) SMI_GLP/Data/Extraction_Files/pat_smi_comb.RData: SMI medical records
# 3) SMI_GLP/Data/Extraction_Files/pat_depr_comb.RData: Depression medical records
# 4) SMI_GLP/Data/Extraction_Files/Antidepressants/pat_comb_final.RData: Antidepressant prescription records
# 5) SMI_GLP/Data/Extraction_Files/Antipsychotics/pat_comb_final.RData: Antipsychotic prescription records
# 6) SMI_GLP/Data/Extraction_Files/Mood_Stabilisers/pat_comb_final.RData: Mood stabiliser prescription records
# 
# Intermediate Outputs:
# 1) SMI_GLP/Data/Cleaning_Files/smi_diagnoses.RData: SMI diagnoses for all patients, 
#    including later diagnostic update dates
# 
# Final Outputs:
# 1) SMI_GLP/Data/Cleaned_Data/pat_antipsychotics.RData: Cleaned antipsychotic prescriptions for those in the T2DM cohort 
# 2) SMI_GLP/Data/Cleaned_Data/pat_antidepressants.RData: Cleaned antidepressant prescriptions for those in the T2DM cohort 
# 3) SMI_GLP/Data/Cleaned_Data/pat_mood_stabilisers.RData: Cleaned mood stabiliser prescriptions for those in the T2DM cohort 
# 4) SMI_GLP/Data/Cleaned_Data/smi_dx_date.RDataa: Final SMI diagnosis date table
# 5) SMI_GLP/Data/Cleaned_Data/cohort_demog_cleaned_with_smi.RData: Final T2DM cohort with SMI information 


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
library(data.table)
library(tidylog)
library(DBI)     # database interface
library(duckdb)  # connect to SQL
library(dbplyr)  # dplyr w/ SQL

# ============= 1) Read in SMID for those in T2DM cohort. Require AD for depression ===============================

# Set working directory
wd <- "S:/CDSTP_CPRD_25_005368/"
setwd(wd)

# Set input and output paths
path_input <- paste0("SMI_GLP/Data/Extraction_Files/")
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


# Read in cohort of patients including diagnosis date
# Loads as 'cohort_demog_dx_date'
load(paste0(path_output, "cohort_demog_cleaned.RData")) # created in 2e_finalize_dx_date_antidiab_dedup.R
setDT(cohort_demog_dx_date)

### Read in SMI data and convert to data.table for wrangling

# Read in SMI
load(paste0(path_input, "pat_smi_comb.RData"))
pat_smi_comb <- pat_smi_comb %>%
  rename(term = readterm) %>%
  select(patid, database, medcode, term, eventdate, sysdate, group)
length(unique(pat_smi_comb$patid)) # 101,836

# Read in depression
load(paste0(path_input, "pat_depr_comb.RData"))
pat_depr_comb$group <- "depression"
length(unique(pat_depr_comb$patid)) # 1,214,409


# For depression, restrict to those with at least one medication prescription
# Load antidepressants: 18 GB
load(paste0(path_input, "Antidepressants/pat_comb_final.RData"))
pat_ad_comb <- as.data.table(pat_comb_final) # 118,599,367
rm(pat_comb_final)
gc()
# Add in yob information and restrict to T2DM cohort: 
# 82,445,573 antidepressant records from 1,145,138 patients
pat_ad_comb <- cohort_demog_dx_date[, c("patid", "yob", "deathdate")][
  pat_ad_comb,
  on = .(patid),
  nomatch = NULL
]
gc()
# Restrict to records after birth and before death: 
# 82,384,667 recrods from 1,145,089 patients (92 patients dropped)
pat_ad_comb <- pat_ad_comb[(is.na(eventdate) | year(eventdate) >= yob) & 
                           (is.na(eventdate) | is.na(deathdate) | eventdate <= deathdate)]
ad_ids <- unique(pat_ad_comb$patid)
length(ad_ids)
# Drop unnecessary columns
pat_ad_comb[, c("yob", "deathdate") := NULL]
gc()

# Restrict depression individuals to those who ever had an AD prescription
pat_depr_comb <- pat_depr_comb[patid %in% ad_ids]
# 617,897 (527,192 (46.0%) patients dropped)
length(unique(pat_depr_comb$patid))

# SMI medcodes including depression
pat_smid <- as.data.table(rbind(pat_smi_comb, pat_depr_comb))
# Save memory
rm(pat_smi_comb, pat_depr_comb)
gc()


# ====== 2) Get SMI medcode diagnosis date and SMI type ==================================

# Diagnosis date is set to earliest of medcode or prodcode

### Get SMI medcode earliest date

# Add in patient demog and t2dm dx info for those w/ SMI
# 5,235,420 records from 644,490 patients
# (Old: 5,756,454 records from 811,905 patients if include mild depr)
# (Old: 8,270,435 records from 1,220,654 patients if include those not in t2dm cohort)
pat_comb_final <- cohort_demog_dx_date[, -c("medcode", "term", "database")][
  pat_smid,
  on = .(patid),
  nomatch = NULL
]
length(unique(pat_comb_final$patid))

# Restrict to records after birth and before death: 513 records dropped. 
# 5,234,938 records from 644,401 patients (95 patients dropped)
pat_comb_final <- pat_comb_final[(is.na(eventdate) | year(eventdate) >= yob) & 
                                   (is.na(eventdate) | is.na(deathdate) | eventdate <= deathdate)]
length(unique(pat_comb_final$patid))

# Write birth and death information to duckdb
pat_yob <- unique(as.data.table(pat_comb_final[, .(patid, yob, deathdate)]))
dbWriteTable(connection, "pat_yob", pat_yob, temporary = TRUE, overwrite = TRUE)

# Save memory
rm(pat_smid)
rm(pat_yob)
gc()

### Get hierarchy of SMI diagnoses if multiple diagnoses

# Get earliest dates for each patient and SMI type
# If two SMI types occur on same earliest date, break ties by hierarchy
priority <- c("schizophrenia", "bipolar", "other psychosis", "depression")
dx0 <- pat_comb_final[, c("patid", "medcode", "term", "eventdate", "group")]
dx0[, group := factor(group, levels = priority)]

# Earliest diagnosis for each patient
# Breaks ties by date, group, medcode, then term. Filter to 1 row per patient
first_dx <- dx0[order(patid, eventdate, group, medcode, term), .SD[1L], by = patid][
  , .(patid, dx_group = group, dx_date = eventdate, dx_medcode = medcode, dx_term = term)
]

# Wide patient-level table of first diagnosis dates by disorder
dx0 <- dx0[, .(eventdate = min(eventdate)), by = .(patid, group)]
dxw <- dcast(dx0, patid ~ group, value.var = "eventdate")
setnames(dxw, 
         old = c("schizophrenia", "bipolar", "other psychosis", "depression"),
         new = c("schiz_date", "bpd_date", "psych_date", "depr_date"),
         skip_absent = TRUE)

# Initialize dataframe of all initial diagnoses and updates based on SMI hierarchy
result <- first_dx[dxw, on = "patid"]

# Apply SMI priority hierarchy
# Adding in later schizophrenia diagnosis to schiz_date
result[, date_schiz := fcase(
  dx_group %chin% c("bipolar", "other psychosis", "depression") &
    !is.na(schiz_date) & schiz_date > dx_date, schiz_date, 
  default = as.Date(NA)
)]
# Adding in later bipolar diagnosis to bpd_date, if before schiz_date
result[, date_bpd := fcase(
  dx_group %chin% c("other psychosis", "depression") &
    !is.na(bpd_date) & bpd_date > dx_date & 
    (is.na(date_schiz) | bpd_date < schiz_date), 
  bpd_date, 
  default = as.Date(NA)
)]
# Adding in later psychosis diagnosis to psych_date, if before bpd_date and schiz_date
result[, date_psych := fcase(
  dx_group %chin% c("depression") &
    !is.na(psych_date) & psych_date > dx_date & 
    (is.na(date_schiz) | psych_date < schiz_date) & 
    (is.na(date_bpd) | psych_date < bpd_date), 
  psych_date, 
  default = as.Date(NA)
)]

smi_diagnoses <- result[, .(
  patid, dx_group, dx_date, dx_medcode, dx_term, date_schiz, date_bpd, date_psych
)]

# # Save SMI diagnoses for all patients, including later diagnostic update dates
save(smi_diagnoses, file = paste0(wd, "SMI_GLP/Data/Cleaning_Files/smi_diagnoses.RData"))

# # Load in SMI diagnoses as 'smi_diagnoses'
# load(paste0(wd, "SMI_GLP/Data/Cleaning_Files/smi_diagnoses.RData"))

# Save memory
rm(result, dx0, dxw, first_dx, pat_comb_final)
gc()

# Write SMI patients to duckdb
# columns of 'dx_first': patid, dx_group, dx_date, dx_medcode, dx_term, date_schiz, date_bpd, date_psych
dx_first <- smi_diagnoses
# Get list of patients for whom medication information is needed
dx_patids <- data.table(patid = unique(dx_first$patid))
# Add to duckdb
dbWriteTable(connection, "dx_patids", dx_patids, temporary = TRUE, overwrite = TRUE)
# Save memory
rm(dx_patids, smi_diagnoses)
gc()


# ====== 3) Add in psychotropic prescriptions to get diagnosis date ==================================

### Get earliest psychotropic date

# Read in antipsychotics: 4 GB
load(paste0(path_input, "Antipsychotics/pat_comb_final.RData"))
pat_ap_comb <- as.data.table(pat_comb_final)
rm(pat_comb_final)
# Add in yob information and restrict to T2DM cohort: 
# 16,880,136 records from 669,992 patients
pat_ap_comb <- cohort_demog_dx_date[, c("patid", "yob", "deathdate")][
  pat_ap_comb,
  on = .(patid),
  nomatch = NULL
]
gc()
# Restrict to records after birth and before death: 
# 16,868,581 records from 669,857 patients (135 patients dropped)
pat_ap_comb <- pat_ap_comb[(is.na(eventdate) | year(eventdate) >= yob) & 
                             (is.na(eventdate) | is.na(deathdate) | eventdate <= deathdate)]
# Drop unnecessary columns
pat_ap_comb[, c("yob", "deathdate") := NULL]
gc()


# Read in mood stabilisers: 2 GB
load(paste0(path_input, "Mood_Stabilisers/pat_comb_final.RData"))
pat_ms_comb <- as.data.table(pat_comb_final)
rm(pat_comb_final)
# Add in yob information and restrict to T2DM cohort: 
# 9,430,795 records from 106,642 patients
pat_ms_comb <- cohort_demog_dx_date[, c("patid", "yob", "deathdate")][
  pat_ms_comb,
  on = .(patid),
  nomatch = NULL
]
gc()
# Restrict to records after birth and before death: 
# 9,424,344 records from 106,636 patients (6 patients dropped)
pat_ms_comb <- pat_ms_comb[(is.na(eventdate) | year(eventdate) >= yob) & 
                             (is.na(eventdate) | is.na(deathdate) | eventdate <= deathdate)]
# Drop unnecessary columns
pat_ms_comb[, c("yob", "deathdate") := NULL]
gc()


# # Save psychotropic prescriptions with invalid records dropped, restricted to 
# # those in the T2DM cohort
save(pat_ap_comb, file = paste0(path_output, "pat_antipsychotics.RData"))
save(pat_ms_comb, file = paste0(path_output, "pat_mood_stabilisers.RData"))
save(pat_ad_comb, file = paste0(path_output, "pat_antidepressants.RData"))


### Get first drug date per patient
ap_first <- pat_ap_comb[!is.na(eventdate),
                          .(ap_date = min(eventdate)),
                          by = patid]
ms_first <- pat_ms_comb[!is.na(eventdate),
                          .(ms_date = min(eventdate)),
                          by = patid]
ad_first <- pat_ad_comb[!is.na(eventdate),
                        .(ad_date = min(eventdate)),
                        by = patid]
setkey(ap_first, patid)
setkey(ms_first, patid)
setkey(ad_first, patid)

# # Save memory
# rm(pat_ap_comb, pat_ms_comb, pat_ad_comb)
# gc()


# Bring in first drug dates for patients in dx_first 
dx_first <- ap_first[dx_first, on = "patid"]
dx_first <- ms_first[dx_first, on = "patid"]
dx_first[, ap_ms_date := pmin(ap_date, ms_date, na.rm = TRUE)]
dx_first <- ad_first[dx_first, on = "patid"]

# Compute drug index date per rules: AP/MS for SMI, AD for depr
dx_first[
  , drug_date := fifelse(
    dx_group %chin% c("schizophrenia", "bipolar", "other psychosis"), ap_ms_date,
    fifelse(
      dx_group == "depression", ad_date, 
      as.Date(NA)
    )
  )
]
# Set Inf edge case to NA if both ap_date and ms_date are missing
dx_first[is.infinite(as.numeric(drug_date)), drug_date := as.Date(NA)]

# Get drug source
# Options: AD (antidepressant), AP (antipsychotic), MS (mood stabiliser), 
#   AP and MS (antipsychotic and mood stabiliser on the same date)
dx_first[,
         drug_source := fifelse(
           dx_group %chin% c("schizophrenia", "bipolar", "other psychosis") & 
             !is.na(drug_date) & !is.na(ap_date) & !is.na(ms_date) &
             ap_date == drug_date & ms_date == drug_date, "AP and MS",
           fifelse(
             dx_group %chin% c("schizophrenia", "bipolar", "other psychosis") & 
               !is.na(drug_date) & !is.na(ap_date) & ap_date == drug_date, "AP",
             fifelse(dx_group %chin% c("schizophrenia", "bipolar", "other psychosis") & 
                       !is.na(drug_date) & !is.na(ms_date) & ms_date == drug_date, "MS", 
                     fifelse(dx_group == "depression" & !is.na(drug_date) & 
                               !is.na(ad_date) & ad_date == drug_date, "AD",
                             NA_character_)
             )
           )
         )]

# Compute index date as earlier of diagnosis date and drug date
dx_first[, index_date := pmin(dx_date, drug_date, na.rm = TRUE)]
dx_first[is.infinite(as.numeric(index_date)), index_date := as.Date(NA)]

# Get index date source type
# Options: "medcode", "AD", "AP", "MS", "AP and "MS"
dx_first[
  , index_source := fifelse(
    !is.na(index_date) & index_date == dx_date, "medcode",
    fifelse(!is.na(index_date) & !is.na(drug_date) & index_date == drug_date, drug_source, 
            NA_character_)
  )
]

# Check no NAs in source type
table(dx_first$index_source, useNA = "always")


# Final SMID diagnosis date table
# Columns:
#   patid 
#   index_date: diagnosis date (earliest of medcode and prescription) 
#   index_source: diagnosis source, either medcode or drug prescription (AD, AP, MS, AP and MS)
#   dx_group: SMID subtype (schizophrenia, bipolar, other psychosis, depression)
#   dx_date, dx_medcode, dx_term: date, medcode, and term for earliest SMI medcode
#   date_schiz, date_bpd, date_psych: date of later schizophrenia, bipolar, and psychosis 
#     medcodes for hierarchy (set to NA if only initial medcode is needed)
smi_dx_date <- dx_first[, .(patid, index_date, index_source, dx_group, 
                            dx_date, dx_medcode, dx_term, date_schiz, date_bpd, date_psych)]
head(smi_dx_date)
# 644,401 SMI patients
length(unique(smi_dx_date$patid))

# Histogram of SMI index years
smi_dx_date$index_year <- year(smi_dx_date$index_date)
hist(smi_dx_date$index_year, main = "Histogram of SMI Index Years", xlab = "Index Year",
     breaks = seq(min(smi_dx_date$index_year)-1, max(smi_dx_date$index_year)))
abline(v = 2005, col = "blue")
summary(smi_dx_date$index_year)


# # Save final SMI diagnosis date table
save(smi_dx_date, file = paste0(wd, path_output, "smi_dx_date.RData"))

# Save memory
rm(pat_ad_comb, pat_ap_comb, pat_ms_comb, dx_first, ap_first, ms_first)
gc()

# Number of individuals with each SMI subtype
table(smi_dx_date$dx_group)
prop.table(table(smi_dx_date$dx_group))

# Type of diagnosis: medcode or prodcode
table(smi_dx_date$index_source)
prop.table(table(smi_dx_date$index_source))


# ====== 4) Add SMI diagnosis information to T2DM patient demographic data ==================================

### 2,151,230 patients. Those without SMI have NAs for those columns

# Format SMI data for T2DM cohort
smi_dx_date_format <- smi_dx_date %>%
  rename(smi_dx_date = index_date, smi_group = dx_group, smi_dx_source = index_source) %>%
  select(patid, smi_dx_date, smi_group, smi_dx_source, date_schiz, date_bpd, date_psych)

# Merge in SMI info into T2DM cohort
cohort_demog_dx_date_smi <- cohort_demog_dx_date %>% 
  left_join(smi_dx_date_format, by = join_by(patid))
# Check proportion of patients with SMI (30% with SMI)
prop.table(table(cohort_demog_dx_date_smi$smi_group, useNA = "always"))


# Make sure index_year is after birth and before death: none removed
cohort_demog_dx_date_smi <- cohort_demog_dx_date_smi %>%
  filter(is.na(smi_dx_date) | year(smi_dx_date) >= yob) %>%
  filter(is.na(smi_dx_date) | is.na(deathdate) | year(smi_dx_date) <= year(deathdate))


# # Save T2DM cohort with SMI information 
save(cohort_demog_dx_date_smi, file = paste0(wd, path_output, "cohort_demog_cleaned_with_smi.RData"))


# If SMI diagnosis year is prior to registration year, flag as prevalent SMI (around 42% of SMI)
temp <- cohort_demog_dx_date_smi %>%
  mutate(diag_before_reg = ifelse(smi_dx_date < regstartdate, 1, 0),
         diag_reg_diff = year(smi_dx_date) - year(regstartdate))
table(temp$diag_before_reg)
prop.table(table(temp$diag_before_reg))
summary(temp$diag_reg_diff)



# Disconnect and wipe the spillover folder
dbDisconnect(connection, shutdown = TRUE)
closeAllConnections()
unlink(spill_dir, recursive = TRUE, force = TRUE)
gc()
