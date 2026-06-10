# ==============================================================================
# Add ethnicity for patients in T2DM study cohort
# Author: SM Wu
# Date Created: 2026/06/01
# Date Updated: 2026/06/02
# 
# Details:
# 1) Read in T2DM cohort w/ SMI information
# 2) Add in ethnicity
# 3) Recategorize and assign one per patient
# 4) Add in HES ethnicity as supplement and save data
#
# Inputs:
# 1) SMI_GLP/Data/Cleaned_Data/cohort_demog_cleaned_with_smi.RData: Final T2DM cohort with ethnicity information 
# 2) SMI_GLP/Code_Lists/Ethnicity/Aurum_Ethnicity_codelist_20260601.txt: Aurum ethnicity codelist
# 3) SMI_GLP/Code_Lists/Ethnicity/Gold_Ethnicity_codelist_20260601.txt: Gold ethnicity codelist
# 4) Linkage/Aurum linked/HES APC/25_005368_Aurum_hes_patient.txt: Aurum HES APC patient data
# 5) Linkage/Gold linked/HES APC/25_005368_Gold_hes_patient.txt: Gold HES APC patient data
# 
# Intermediate Outputs:
# 1) SMI_GLP/Data/Cleaning_Files/cohort_ethnicity.RData: Full ethnicity data for cohort
# 
# Final Outputs:
# 1) SMI_GLP/Data/Cleaned_Data/cohort_demog_cleaned_with_smi_eth.RData: Final T2DM cohort with ethnicity added 


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

# ============= 1) Read in T2DM cohort w/ SMI information ===============================

# Set working directory
wd <- "S:/CDSTP_CPRD_25_005368/"
setwd(wd)

# Set input and output paths
path_input <- paste0("SMI_GLP/Data/Extraction_Files/Ethnicity/")
path_codelist <- paste0("SMI_GLP/Code_Lists/Ethnicity/")
path_output <- paste0("SMI_GLP/Data/Cleaned_Data/")
path_linkage_gold <- paste0("Linkage/Gold_linked/")
path_linkage_aurum <- paste0("Linkage/Aurum_linked/")


# Read in cohort of patients including diagnosis date and smi
# Loads as 'cohort_demog_dx_date_smi'
load(paste0(path_output, "cohort_demog_cleaned_with_smi.RData")) # created in 2f_clean_smi.R
length(unique(cohort_demog_dx_date_smi$patid)) # 2,151,230

# Read in ethnicity codelists
aurum_eth_codelist <- read_delim(
  paste0(wd, path_codelist, "Aurum_Ethnicity_codelist_20260601.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character()),
  trim_ws = TRUE) 
gold_eth_codelist <- read_delim(
  paste0(wd, path_codelist, "Gold_Ethnicity_codelist_20260601.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character()),
  trim_ws = TRUE) 

# Create combined codelist
comb_eth_codelist <- aurum_eth_codelist %>%
  rename(medcode = medcodeid) %>%
  select(medcode, term, ethnic_group, ethnic_group_num) %>%
  bind_rows(gold_eth_codelist %>% 
              select(medcode, term, ethnic_group, ethnic_group_num)) %>%
  distinct()

# ============= 2) Add in ethnicity ===============================

# GP ethnicity categories:
# 16-category                     5-category                 HES 11-category    
## 1. British                     1. White                   1. White
## 2. Irish                       1. White                   1. White
## 3. Other White                 1. White                   1. White
## 4. White and Black Caribbean   4. Mixed                   10. Mixed
## 5. White and Black African     4. Mixed                   10. Mixed    
## 6. White and Asian             4. Mixed                   10. Mixed    
## 7. Other Mixed                 4. Mixed                   10. Mixed     
## 8. Indian                      3. Asian                   5. Indian
## 9. Pakistani                   3. Asian                   6. Pakistani
## 10. Bangladeshi                3. Asian                   7. Bangladeshi
## 11. Other Asian                3. Asian                   8. Other Asian
## 12. Caribbean                  2. Black                   2. Black Caribbean
## 13. African                    2. Black                   3. Black African
## 14. Other Black                2. Black                   4. Black Other
## 15. Chinese                    3. Asian                   9. Chinese
## 16. Other Ethnic group         5. Other                   11. Other
## 17. Unknown                    6. Unknown                 12. Unknown


### Read in ethnicity data and convert to data.table for wrangling

# Read in ethnicity
load(paste0(path_input, "pat_comb_final.RData"))

# Add in categories from codelist
pat_ethnicity <- pat_comb_final %>%
  left_join(comb_eth_codelist, by = join_by(medcode, term))
length(unique(pat_ethnicity$patid)) # 2,969,156

# Filter ethnicity records to patients in T2DM cohort
# 2,969,712 (37%) dropped; 5,155,029 remaining
pat_ethnicity <- pat_ethnicity %>%
  filter(patid %in% cohort_demog_dx_date_smi$patid) %>%
  arrange(patid, eventdate) %>%
  select(patid, medcode, term, eventdate, ethnic_group, ethnic_group_num)

# Remove records before birth or after death or after end of follow-up
# 1,757,196 patient with ethnicity records
pat_ethnicity <- pat_ethnicity %>%
  left_join(cohort_demog_dx_date_smi %>% select(patid, yob, endfollow)) %>%
  filter((year(eventdate) >= yob) & (eventdate <= endfollow))
length(unique(pat_ethnicity$patid))

# ============= 3) Recategorize and assign one per patient ===============================

# Recategorize into 5 categories: 
# 1=White, 2=Black, 3=Asian, 4=Mixed, 5=Other, 6=Unknown
pat_ethnicity_recat <- pat_ethnicity %>%
  mutate(eth_cat = case_match(ethnic_group_num,
                               c(1, 2, 3) ~ "White",
                               c(12, 13, 14) ~ "Black",
                               c(8, 9, 10, 11, 15) ~ "Asian",
                               c(4, 5, 6, 7) ~ "Mixed",
                               16 ~ "Other",
                               17 ~ "Unknown",
                               TRUE ~ "Unknown"
  ),
  eth_cat = factor(eth_cat, 
                   levels = c("White", "Black", "Asian", "Mixed", "Other", "Unknown")))
table(pat_ethnicity_recat$eth_cat, useNA = "always")
prop.table(table(pat_ethnicity_recat$eth_cat, useNA = "always"))

## For each patient, choose the most common ethnicity, or the most recent 
## if there is a tie

# For each patient and recategorised ethnicity type, get number of that type
# and date of most recent code
pat_ethnicity_counts <- pat_ethnicity_recat %>%
  filter(eth_cat != "Unknown") %>%   # remove Unknown codes
  group_by(patid, eth_cat) %>%
  summarise(eth_code_count = n(),                                                     
            latest_date_per_cat = max(eventdate, na.rm=TRUE)) %>%
  group_by(patid) %>%
  mutate(num_eth = n()) %>%
  ungroup()

# Split into patients w/ single ethnicity code vs multiple
# 1,679,881 w/ single ethnicity
# (1,493,408 w/ single ethnicity if original 16-cat)
pat_single <- pat_ethnicity_counts %>% 
  filter(num_eth == 1)
# 82,391 w/ multiple
# (263,788 w/ multiple if original 16-cat)
pat_multiple <- pat_ethnicity_counts %>% 
  filter(num_eth > 1)
length(unique(pat_multiple$patid))

# For each patient w/ multiple ethnicities categories, only keep 
# categories with most counts
pat_multiple_max <- pat_multiple %>%
  group_by(patid) %>%
  filter(eth_code_count == max(eth_code_count, na.rm=TRUE))

# If more than one category with most counts, keep the most recent one
pat_multiple_max_recent <- pat_multiple_max %>%
  group_by(patid) %>%
  filter(latest_date_per_cat == max(latest_date_per_cat, na.rm=TRUE))

# If still multiple, choose the first one recorded
pat_multiple_max_recent_unique <- pat_multiple_max_recent %>%
  group_by(patid) %>%
  filter(row_number() == 1)


# Combine single w/ processed multiple
pat_ethnicity_unique <- pat_single %>% 
  select(patid, eth_cat) %>%
  bind_rows(pat_multiple_max_recent_unique %>% select(patid, eth_cat)) %>%
  rename(eth_cat_cprd = eth_cat)

# Add ethnicity information into cohort
# 397,958 patients with no ethnicity codes 
cohort_eth <- cohort_demog_dx_date_smi %>%
  left_join(pat_ethnicity_unique, by = "patid")

table(cohort_eth$eth_cat_cprd, useNA = "always")
prop.table(table(cohort_eth$eth_cat_cprd, useNA = "always"))


# ============= 4) Add in HES ethnicity as supplement and save data ===============================

# Read in HES Patient records
hes_aurum <- read.table(paste0(wd, path_linkage_aurum, "HES APC/25_005368_Aurum_hes_patient.txt"),
                        header = TRUE, quote = "", fill = TRUE, sep = "\t",
                        colClasses = c("patid" = "character"))
hes_aurum <- hes_aurum %>%
  mutate(patid = paste0(patid, "-A"))
hes_gold <- read.table(paste0(wd, path_linkage_gold, "HES APC/25_005368_Gold_hes_patient.txt"),
                        header = TRUE, quote = "", fill = TRUE, sep = "\t",
                        colClasses = c("patid" = "character"))
hes_gold <- hes_gold %>%
  mutate(patid = paste0(patid, "-G"))

# Combine HES Aurum and Gold and investigate ethnicity counts
hes_comb <- hes_aurum %>% bind_rows(hes_gold)
table(hes_comb$gen_ethnicity, useNA = "always")


# HES ethnicity coding:
# 11-category                     5-category
## 1	White                       1 White
## 2	Black Caribbean             2 Black
## 3	Black African               2 Black
## 4	Black Other                 2 Black
## 5	Indian                      3 Asian
## 6	Pakistani                   3 Asian
## 7	Bangladeshi                 3 Asian
## 8	Other Asian                 3 Asian
## 9	Chinese                     3 Asian
## 10	Mixed                       4 Mixed
## 11	Other                       5 Other

# Recategorize into 5 categories: 
# 1=White, 2=Black, 3=Asian, 4=Mixed, 5=Other, 6=Unknown
hes_comb_eth <- hes_comb %>%
  mutate(eth_cat_hes = case_match(gen_ethnicity,
                              "White" ~ "White",
                              c("Bl_Carib", "Bl_Afric", "Bl_Other") ~ "Black",
                              c("Indian", "Pakistani", "Bangladesi", "Oth_Asian", "Chinese") ~ "Asian",
                              "Mixed" ~ "Mixed",
                              "Other" ~ "Other",
                              "Unknown" ~ "Unknown",
                              .default = "Unknown"),
         eth_cat_hes = factor(eth_cat_hes, levels = c("White", "Black", "Asian", 
                                                "Mixed", "Other", "Unknown")))
table(hes_comb_eth$eth_cat_hes, useNA = "always")
prop.table(table(hes_comb_eth$eth_cat_hes, useNA = "always"))


# Use CPRD ethnicity, but supplement with HES when missing
# 1,563,761 patients in cohort also have HES ethnicity data
cohort_eth <- cohort_eth %>%
  left_join(hes_comb_eth %>% select(patid, eth_cat_hes), by = "patid")

cohort_eth <- cohort_eth %>%
  # Set "Unknown" to NA
  mutate(eth_cat_cprd = fct_recode(eth_cat_cprd, NULL = "Unknown"),
         eth_cat_hes = fct_recode(eth_cat_hes, NULL = "Unknown"),
         # Create ethnicity variable, prioritizing CPRD
        eth_cat = coalesce(eth_cat_cprd, eth_cat_hes),
        # Markers of ethnicity source
        from_cprd = ifelse(is.na(eth_cat_cprd), 0, 1),
        from_hes = ifelse(is.na(eth_cat_cprd) & !is.na(eth_cat_hes), 1, 0),
        # Flag mismatches between CPRD and HES
        mismatch = !is.na(eth_cat_cprd) & !is.na(eth_cat_hes) & eth_cat_cprd != eth_cat_hes)

table(cohort_eth$eth_cat, useNA = "always")
prop.table(table(cohort_eth$eth_cat, useNA = "always"))

table(cohort_eth$from_cprd) # 1,753,272 from CPRD
table(cohort_eth$from_hes) # 186,983 from HES
mismatch <- cohort_eth %>%  # 76,113 w/ mismatch
  filter(mismatch)

# # Save comprehensive ethnicity information
# save(cohort_eth, file = paste0(wd, "SMI_GLP/Data/Cleaning_Files/cohort_ethnicity.RData"))


# Finalize ethnicity information for T2DM cohort, dropping unnecessary columns
cohort_demog_dx_date_smi_eth <- cohort_eth %>%
  select(-c(gen_ethnicity, eth_cat_cprd, eth_cat_hes, from_cprd, from_hes, mismatch))


# Save T2DM cohort with ethnicity information 
save(cohort_demog_dx_date_smi_eth, 
     file = paste0(wd, path_output, "cohort_demog_cleaned_with_smi_eth.RData"))


