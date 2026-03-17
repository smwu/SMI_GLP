# Generate code list for glucose 
# Author: S Picton & S Wu
# Date created: 2026/01/27
# Date updated: 2026/02/06

# 
# Details:
# 1) Set up and load data
# 2) Search for new relevant med codes
# 3) Create updated code lists
# 4) Adjust formatting for extraction
#
# Inputs:
# 1) Code_Lists/MASTER_Lists/CPRD_Aurum_Medical_14Oct2025.txt: Aurum medical master code list
# 2) Code_Lists/MASTER_Lists/CPRD_GOLD_Medical_14Oct2025.txt: GOLD medical master code list
# 3) Code_Lists/Glucose/Old/Aurum_Glucose_codelist_20240723_Alvin.txt : Old Aurum Glucose code list 
# 4) Code_Lists/Glucose/Old/Gold_Glucose_codelist_20240723_Alvin.txt : Old Gold Glucose code list 
# 
# Final Outputs:

# 1) Code_Lists/Glucose/Aurum_Glucose_codelist_20260127.txt : Updated Aurum glucose code list 
# 2) Code_Lists/Glucose/Gold_Glucose_codelist_20260127.txt : Updated Gold glucose code list 



# ================= 1) Set up and load data ====================================

# Clear memory
rm(list = ls())

# Packages
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(writexl)


#  If working in Data Safe Haven - manually install packages using Artifactory:
#   bit, bit64, cli, crayon, dplyr, generics, glue, hms,
#   lifecycle, magrittr, pillar, pkgconfig, purrr, R6, readr, rlang, stringi, 
#   stringr, tibble, tidyr, tidyselect, tzdb, utf8, vctrs, vroom, withr, writexl

# ### For running locally
# # Set working directory
# wd <- "/Volumes/ritd-ag-project-rd00qv-jfhay18/" # VPN connection
# # wd <- "//live.rd.ucl.ac.uk/ritd-ag-project-rd00qv-jfhay18/" #Desktop@UCL
# setwd(wd)
# 
# # Set input and output paths
# path_input <- "Code_Lists/"
# path_output <- "Code_Lists/Glucose/"


### For running in Data Safe Haven
# Set working directory
wd <- "S:/CDSTP_CPRD_25_005368/" 
setwd(wd)

# Set input and output paths
path_input <- "SMI_GLP/Code_Lists/"
path_output <- "SMI_GLP/Code_Lists/Glucose/"

## Load data

# Read in Aurum medical dictionary
cprd_aurum_medical_raw <- 
  read_delim(
    paste0(wd, path_input, "MASTER_Lists/CPRD_Aurum_Medical_14Oct2025.txt"), 
    delim = "\t", escape_double = FALSE, 
    col_types = cols(MedCodeId = col_character(), 
                     OriginalReadCode = col_character(), 
                     CleansedReadCode = col_character(), 
                     SnomedCTConceptId = col_character(), 
                     SnomedCTDescriptionId = col_character()), 
    trim_ws = TRUE)
cprd_aurum_medical <- cprd_aurum_medical_raw %>%
  select(-Release) %>%
  rename(term = Term, medcodeid = MedCodeId) %>%
  mutate(term = str_to_lower(term))

# Read in Gold medical dictionary
cprd_gold_medical_raw <- 
  read_delim(
    paste0(wd, path_input, "MASTER_Lists/CPRD_GOLD_Medical_14Oct2025.txt"), 
    delim = "\t", escape_double = FALSE, 
    col_types = cols(medcode = col_character(), 
                     readcode = col_character()), 
    trim_ws = TRUE) 
cprd_gold_medical <- cprd_gold_medical_raw %>%
  rename(term = readterm) %>%
  mutate(term = str_to_lower(term))

# Read in old Glucose code list from 2024/07/23, setting all col types to character
# Aurum
aurum_glucose_old <- read_delim(
  paste0(wd, path_input, "Glucose/Old/Aurum_Glucose_codelist_20240723_Alvin.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character()), trim_ws = TRUE)

# Gold
gold_glucose_old <- read_delim(
  paste0(wd, path_input, "Glucose/Old/Gold_Glucose_codelist_20240723_Alvin.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character()), trim_ws = TRUE)


# ================= 2) Search for new relevant med codes =======================

# Aurum

aurum_glucose <- cprd_aurum_medical %>%
  # Inclusion - Glucose related terms-  blood tests, urine tests, elevated blood glucose codes 
  
  filter(grepl("(?i)glucose|Urine clinitest|blood sugar|Sugar - urine test|Sugars in urine|Faecal sugars|Urine sugar charts|Urine screen for sugar|
  |Diabetic monitoring|diabetic assessment|diabetic review|pre-bed sugar|pre-breakfast sugar|pre-evening meal sugar| pre-lunch sugar|
  |hyperglyceridaemia|[D]Hypergylcaemia|[X]Hyperglycaemia|Hyperglycaemia|Severe hyperglycaemia due to diabetes mellitus|Acute hyperglycaemia|Diabetic severe hyperglycaemia|Chronic hyperglycaemia|diabetic blood test|Diabetes mellitus screen|Screening for diabetes|test for diabetes", term)) %>% 
               
  # Exclusion - Not relevant glucose terms  - Excluded pregnancy, surgical terms, pre-diabetes terms
  filter(!grepl("(?i)6-phosphate|syndrome|Deficiency|intravenous|adverse|intake|gbo|cranial|cotransporter|CSF:glucose normal|CSF: glucose decreased|CSF: glucose increased|Postpancreatectomy|CSF(cerebrospinal fluid)
glucose level| Dialysis fluid glucose level|Ascitic fluid glucose level|Non-diabetic|Abnormal glucose tolerance test during pregnancy|Abnormal glucose
tolerance test in the puerperium|puerperium|CSF glucose|Diabetes mellitus insulin-glucose infusion in acute myocardial infarction|cerebrospinal|csf|[v]screening|glucose-galactose|pet study|dialysis|positron|fluorodeoxyglucose|
Diabetes mellitus screening|Malabsorption of glucose - galactose|Glucose galactose intolerance|Preg.+|[D]Stress induced|lignocaine|renal threshold|
Diabetes mellitus screening risk|Impaired glucose tolerance|enteral feeding|interstitial|Impaired glucose regulation|Provision|IGT|sarstedt|request|lidocaine|family history|pet ct|carboplatin|previous abnormality of glucose tolerance|potential abnormality of glucose tolerance|decreased glucose level|normal glucose level|glucose challenge test|low glucose diet|blood glucose testing strips|	
blood glucose meters|blood glucose testing kit|glucose measurement by monitoring device|measurement of glucose 1 hour after glucose challenge|glucose tolerance test, antenatal|blood glucose management|subcutaneous glucose sensor|
home-use/point-of-care blood glucose|two hours after glucose dose|2h post dose glucose|screening not done|neonatal non-ketotic hyperglycinaemia|infantile non-ketotic hyperglycinaemia|	
glucose gel|glucose tolerance test|blood glucose tolerance|Hyperglycinaemia|glucose monitoring equipment|declined|referral|monitoring stopped|discussion|palmdoc|contour|freestyle|glucomen|checking accuracy|charts|glucose load test|test strips|gestational", term)) 
  
  

#Gold - 

gold_glucose <- cprd_gold_medical %>%
#Inclusion - Glucose related terms
filter(grepl("(?i)glucose|Urine clinitest|blood sugar|Sugar - urine test|Sugars in urine|Faecal sugars|Urine sugar charts|Urine screen for sugar|
  |Diabetic monitoring|diabetic assessment|diabetic review|pre-bed sugar|pre-breakfast sugar|pre-evening meal sugar| pre-lunch sugar|
  |hyperglyceridaemia|Hypergylcaemia|[D]Hyperglycaemia|[X]Hyperglycaemia, unspecified|diabetic blood test|Diabetes mellitus screen|Screening for diabetes|test for diabetes|Chronic hyperglycaemia", term)) %>%
  
#Exclusion criteria - pregnancy, non diabetic conditions, pre-diabetic terms

  filter(!grepl("(?i)csf|tolerance|phosphatase|6-phosphate|provision|ascitic|referral|charts|myocardial infarct|gtt|
interstitial|pancreatic|pregnancy|transporter|Hyperglycinaemia|galactose|neonatal|malabsorption|dialysis", term))


  
  
## Comparing with older codelists

# New codes not in old list
new_aurum <- aurum_glucose %>%
  filter(!medcodeid %in% aurum_glucose_old$medcodeid)

new_gold <- gold_glucose %>%
  filter(!medcode %in% gold_glucose_old$medcode)

# Old codes not in new old list
miss_new_aurum <- aurum_glucose_old %>%
  filter(!medcodeid %in% aurum_glucose$medcodeid)



miss_new_gold <- gold_glucose_old %>%
  filter(!medcode %in% gold_glucose$medcode)

## 

# ================= 3) Create updated code lists ===============================

# Create updated code lists

# Aurum
glucose_codelist_aurum_new <- aurum_glucose

# Gold
glucose_codelist_gold_new <- gold_glucose

# Save updated code lists

write.table(glucose_codelist_aurum_new,
            file = paste0(wd, path_output, "Aurum_Glucose_codelist_20260206.txt"),
            sep = "\t", row.names = FALSE)

write.table(glucose_codelist_gold_new,
            file = paste0(wd, path_output, "Gold_Glucose_codelist_20260206.txt"),
            sep = "\t", row.names = FALSE)

# Combine Aurum and GOLD updated code lists
temp_aurum <- glucose_codelist_aurum_new %>%
  rename(medcode = medcodeid, readcode = CleansedReadCode) %>%
  select(medcode, term)
temp_gold <- glucose_codelist_gold_new %>%
  select(medcode, term)
temp_both <- rbind(temp_aurum, temp_gold)
aurum_gold_glucose_new <- temp_both %>% distinct()

# Save lists of new combined codelist into one .xlsx file

library(writexl)

write_xlsx(aurum_gold_glucose_new,
           file = paste0(wd, path_output, "Aurum_Gold_Glucose_codelist_20260206.xlsx")

# # Combine Aurum and GOLD into one file with a column specifying database
# glucose_codelist_aurum_new$database <- "Aurum"
# glucose_codelist_gold_new$database <- "Gold"
# glucose_codelist_aurum_gold_new <- rbind(
#   glucose_codelist_aurum_new %>% 
#     rename(medcode = medcodeid) %>%
#     select(medcode, term, database), 
#   glucose_codelist_gold_new %>%#     select(medcode, term, database))
# 
# # Save combined code list
# write.table(glucose_codelist_aurum_gold_new,
#             file = paste0(wd, path_output, "Aurum_Gold_Glucose_codelist_20260206.txt"),
#             sep = "\t", row.names = FALSE)
           
           
glucose_codelist_aurum_new$database <- "Aurum"
glucose_codelist_gold_new$database <- "Gold"
glucose_codelist_aurum_gold_new <- rbind(
glucose_codelist_aurum_new %>% 
      rename(medcode = medcodeid) %>%
       select(medcode, term, database), 
        glucose_codelist_gold_new %>%
               select(medcode, term, database))
           
write.table(glucose_codelist_aurum_gold_new,
                       file = paste0(wd, path_output, "Aurum_Gold_Glucose_codelist_20260206.txt"),
                       sep = "\t", row.names = FALSE)

