# ==============================================================================
# Generate code lists for Hypertension
# Authors: SM Wu & S Picton
# Date Created: 2026/04/08
# Date Updated: 2026/04/15
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
# 3) Code_Lists/Hypertension/Old/Aurum_Hypertension_20240228_Alvin.txt : Old Aurum Hypertension code list 
# 4) Code_Lists/Hypertension/Old/Gold_Hypertension_20240228_Alvin.txt : Old Gold Hypertension code list 
# 
# Final Outputs:
# 1) Code_Lists/Hypertension/Aurum_Hypertension_code_list_20260408.txt : Updated Aurum Hypertension code list 
# 2) Code_Lists/Hypertension/Gold_Hypertension_code_list_20260408.txt : Updated Gold Hypertension code list 
# 3) Code_Lists/Hypertension/Aurum_Gold_Hypertension_code_list_20260408.txt : Updated combined Aurum and Gold Hypertension code list 


# ================= 1) Set up and load data ====================================

# Clear memory
rm(list = ls())

# Packages
library(readr)
library(dplyr)
library(stringr)
library(tidyr)


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
# path_output <- "Code_Lists/Hypertension/"

### For running in Data Safe Haven

# Set working directory
wd <- "S:/CDSTP_CPRD_25_005368/" 
setwd(wd)

# Set input and output paths
path_input <- "SMI_GLP/Code_Lists/"
path_output <- "SMI_GLP/Code_Lists/Hypertension/"


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


# Read in old Hypertension code list from 2024/02/28, setting all col types to character

# Aurum

aurum_hypertension_old <- read_delim(
  paste0(wd, path_input, "Hypertension/Old/Aurum_Hypertension_20240228_Alvin.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character()), trim_ws = TRUE)

# Gold

gold_hypertension_old <- read_delim(
  paste0(wd, path_input, "Hypertension/Old/Gold_Hypertension_20240228_Alvin.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character()), trim_ws = TRUE)

# ================= 2) Search for new relevant med codes =======================

# Aurum

aurum_hypertension <- cprd_aurum_medical %>%
  
  # Inclusion - Hypertension related terms,  
  filter(grepl(paste0("(?i)hypertension|hypertensive|high blood pressure|high diastolic|high systolic"), 
               term)) %>%




# Exclusion - Unrelated terms 
  
  
 filter(!grepl(paste0("(?i)pulmonary|intracranial|intracranial|ocular|ocular",
                      
                      
 # Relating to pregnancy 
 
 "pregnancy|pregnant|obstetric|maternal|pre-eclampsia|gestational|preg.|preg/childb/puerp","maternal", 
 
 
 # Relating to family history 
 
 "family history|fh:|no fh:|no significant family history|	
no family history|	
no family history|no family history of|family history|family history of hypertension|family", 
 
 # Relating to children 
 
 "child|paediatric|infant|infancy|newborn|fetus|neonate|neonatal","neonatal effect",
 
 # Negation 
 
 "no history of|no h/o:|not to have|resolved|resolved",
 
 # Process of care 
 
 
 "has-bled|abnormal renal|chads2|cha2ds2-vasc|screening"),
 
 term, perl = TRUE)) %>% 
 
 # Filter out family history codes
 
 filter(!grepl("family history:[-_ ]*hypertension", term, ignore.case = TRUE, perl =  TRUE))  %>%

filter(!grepl("family history[-_ ]*hypertension", term, ignore.case = TRUE, perl =  TRUE)) %>%
  
  # Filter out neonatal codes 
  
filter(!grepl("neonatal[-_ ]*hypertension", term, ignore.case = TRUE, perl =  TRUE)) %>% 
filter(!grepl("neonatal[-_ ]*hypertensive", term, ignore.case = TRUE, perl =  TRUE)) %>%

# Filter out pulmonary hypertension codes

filter(!grepl("pulmonary[-_ ]*hypertension", term, ignore.case = TRUE, perl =  TRUE))  



# Gold

gold_hypertension <- cprd_gold_medical %>%
  
  # Inclusion - Hypertension related terms,  
  filter(grepl(paste0("(?i)hypertension|hypertensive|high blood pressure|high diastolic|high systolic"), 
               term)) %>%
  
  
  
  
  # Exclusion - Unrelated terms 
  
  
  filter(!grepl(paste0("(?i)pulmonary|intracranial|intracranial|ocular|ocular",
                       
                       
 # Relating to pregnancy 
                       
"pregnancy|pregnant|obstetric|maternal|pre-eclampsia|gestational|preg.|preg/childb/puerp","maternal", 
                       
                       
 # Relating to family history 
                       
 "family history|fh:|no fh:|no significant family history|	
no family history|	
no family history|no family history of|family history|family history of hypertension|family", 
                       
# Relating to children 
                       
"child|paediatric|infant|infancy|newborn|fetus|neonate|neonatal","neonatal effect",
                       
# Negation 
                       
"no history of|no h/o:|not to have|resolved|resolved",
                       
# Process of care 
                       
                       
 "has-bled|abnormal renal|chads2|cha2ds2-vasc|screening|screen"),
                
                term, perl = TRUE)) %>% 
  
  # Filter out family history codes
  
  filter(!grepl("family history:[-_ ]*hypertension", term, ignore.case = TRUE, perl =  TRUE))  %>%
  
  filter(!grepl("family history[-_ ]*hypertension", term, ignore.case = TRUE, perl =  TRUE)) %>%
  
  # Filter out neonatal codes 
  
  filter(!grepl("neonatal[-_ ]*hypertension", term, ignore.case = TRUE, perl =  TRUE)) %>% 
  filter(!grepl("neonatal[-_ ]*hypertensive", term, ignore.case = TRUE, perl =  TRUE)) %>%
  
  # Filter out pulmonary hypertension codes
  
  filter(!grepl("pulmonary[-_ ]*hypertension", term, ignore.case = TRUE, perl =  TRUE))  


## Comparing with older codelists

# New codes not in old list

new_aurum <- aurum_hypertension %>%
  filter(!medcodeid %in% aurum_hypertension_old$medcodeid)

new_gold <- gold_hypertension %>%
  filter(!medcode %in% gold_hypertension_old$medcode)

# Old codes not in new old list
miss_new_aurum <- aurum_hypertension_old %>%
  filter(!medcodeid %in% aurum_hypertension$medcodeid)



miss_new_gold <- gold_hypertension_old %>%
  filter(!medcode %in% gold_hypertension$medcode)


## # ================= 3) Create updated code lists ===============================

# Create updated code lists

# Aurum
hypertension_codelist_aurum_new <- aurum_hypertension

# Gold
hypertension_codelist_gold_new <- gold_hypertension

# Save updated code lists

write.table(hypertension_codelist_aurum_new,
            file = paste0(wd, path_output, "Aurum_Hypertension_codelist_20260408.txt"),
            sep = "\t", row.names = FALSE)

write.table(hypertension_codelist_gold_new,
            file = paste0(wd, path_output, "Gold_Hypertension_codelist_20260408.txt"),
            sep = "\t", row.names = FALSE)

# Combine Aurum and GOLD updated code lists

temp_aurum <- hypertension_codelist_aurum_new %>%
  rename(medcode = medcodeid, readcode = CleansedReadCode) %>%
  select(medcode, term)

temp_gold <- hypertension_codelist_gold_new %>%
  select(medcode, term)

temp_both <- rbind(temp_aurum, temp_gold)

aurum_gold_hypertension_new <- temp_both %>% distinct()




# # Combine Aurum and GOLD into one file with a column specifying database

   hypertension_codelist_aurum_new$database <- "Aurum"
   hypertension_codelist_gold_new$database <- "Gold"
   hypertension_codelist_aurum_gold_new <- rbind(
   hypertension_codelist_aurum_new %>% 
               rename(medcode = medcodeid) %>%
               select(medcode, term, database), 
             hypertension_codelist_gold_new %>%
               select(medcode, term, database))
           
# 
 # # Save combined code list
           write.table(hypertension_codelist_aurum_gold_new,
                       file = paste0(wd, path_output, "Aurum_Gold_Hypertension_codelist_20260408.txt"),
                       sep = "\t", row.names = FALSE)
           
