# Generate code list for angina
# Author: S Picton & S Wu
# Date created: 2026/06/02
# Date updated: 2026/006/02

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
# 3) 

# Final Outputs:

# 1) Code_Lists/Angina/Aurum_Angina_20260602.txt : Aurum Angina codelist 
# 2) Code_Lists/Angina/Gold_Angina_20260602.txt : Gold Angina codelist 



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
#
# # Set working directory
#
#  wd <- "/Volumes/ritd-ag-project-rd00qv-jfhay18/" # VPN connection
#  wd <- "//live.rd.ucl.ac.uk/ritd-ag-project-rd00qv-jfhay18/" #Desktop@UCL
# setwd(wd)
#
# # Set input and output paths
#
# path_input <- "Code_Lists/"
# path_output <- "Code_Lists/Angina/

### For running in Data Safe Haven

# Set working directory

wd <- "S:/CDSTP_CPRD_25_005368/" 
setwd(wd)

# Set input and output paths

path_input <- "SMI_GLP/Code_Lists/"
path_output <- "SMI_GLP/Code_Lists/Angina/"

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


# ================= 2) Search for new relevant med codes =======================

# Aurum

aurum_angina <- cprd_aurum_medical %>%
  # Inclusion - angina related terms 
  
  filter(grepl(paste0("(?i) 
  
  angina|angina"),
               
               term, ignore.case = TRUE, perl = TRUE)) %>% 
  
  
  # Exclusion
  
  filter(!grepl(paste0("(?i)
  
  # Unrelated
  
 herpangina|herpangina|angina bullosa hemorrhagica",
                       "angina bullosa haemorrhagica|angina bullosa haemorrhagica",

  # Process of care
  
  "grading|score|adverse reaction|classification|grading|angina grading",
  
  # FH
  
  "fh angina|fh:|fh|family history:|family history: angina",
  
  # Negations
  
  "possible|treadmill stress test negative for angina pectoris|angina pectoris not detected on treadmill stress test"), 
  
  term, ignore.case = TRUE, perl = TRUE)) %>%
  
  # Unrelated angina 
  
  
  
  filter(!grepl("ludwig's[-_ ]*angina", term, ignore.case = TRUE, perl =  TRUE)) %>% 
  
  filter(!grepl("ludwig[-_ ]*angina", term, ignore.case = TRUE, perl =  TRUE)) %>%
    
    filter(!grepl("vincent's[-_ ]*angina", term, ignore.case = TRUE, perl =  TRUE)) %>% 
    
  filter(!grepl("abdominal[-_ ]*angina", term, ignore.case = TRUE, perl =  TRUE)) %>%
  
  
  filter(!grepl("streptococcal[-_ ]*angina", term, ignore.case = TRUE, perl =  TRUE)) %>%
  
  
  filter(!grepl("angina[-_ ]*bullosa[-_ ]*hemorrhagica", term, ignore.case = TRUE, perl = TRUE)) %>% 
filter(!grepl("angina[-_ ]*bullosa[-_ ]*haemorrhagica", term, ignore.case = TRUE, perl = TRUE)) %>%
  filter(!grepl("angina[-_ ]*abdominal", term, ignore.case = TRUE, perl = TRUE)) 


# Gold

gold_angina <- cprd_gold_medical %>%
  
  # Inclusion - angina related terms 
  
  filter(grepl(paste0("(?i) 
  
  angina|angina"),
               
               term, ignore.case = TRUE, perl = TRUE)) %>% 
  
  
  # Exclusion
  
  filter(!grepl(paste0("(?i)
  
  # Unrelated
  
 herpangina|herpangina|angina bullosa hemorrhagica",
                       "angina bullosa haemorrhagica|angina bullosa haemorrhagica",
                       
                       # Process of care
                       
                       "grading|score|adverse reaction|classification|grading|angina grading",
                       
                       # FH
                       
                       "fh angina|fh:|fh|family history:|family history: angina",
                       
                       # Negations
                       
                       "possible|treadmill stress test negative for angina pectoris|angina pectoris not detected on treadmill stress test"), 
                
                term, ignore.case = TRUE, perl = TRUE)) %>%
  
  # Unrelated angina 
  
  
  
  filter(!grepl("ludwig's[-_ ]*angina", term, ignore.case = TRUE, perl =  TRUE)) %>% 
  
  filter(!grepl("ludwig[-_ ]*angina", term, ignore.case = TRUE, perl =  TRUE)) %>%
  
  filter(!grepl("vincent's[-_ ]*angina", term, ignore.case = TRUE, perl =  TRUE)) %>% 
  
  filter(!grepl("abdominal[-_ ]*angina", term, ignore.case = TRUE, perl =  TRUE)) %>%
  
  
  filter(!grepl("streptococcal[-_ ]*angina", term, ignore.case = TRUE, perl =  TRUE)) %>%
  
  
  filter(!grepl("angina[-_ ]*bullosa[-_ ]*hemorrhagica", term, ignore.case = TRUE, perl = TRUE)) %>% 
  filter(!grepl("angina[-_ ]*bullosa[-_ ]*haemorrhagica", term, ignore.case = TRUE, perl = TRUE)) %>%
filter(!grepl("angina[-_ ]*abdominal", term, ignore.case = TRUE, perl = TRUE)) 
  

# ================= 3) Create updated code lists ===============================

# No previous code lists for comparison 

# Save updated code lists

write.table(aurum_angina,
            file = paste0(wd, path_output, "Aurum_Angina_codelist_20260602.txt"),
            sep = "\t", row.names = FALSE)

write.table(gold_angina,
            file = paste0(wd, path_output, "Gold_Angina_codelist_20260602.txt"),
            sep = "\t", row.names = FALSE)
