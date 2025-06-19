# ==============================================================================
# Generate code lists for Depression diagnoses
# Author: SM Wu
# Date Created: 2025/06/13
# Date Updated: 2025/06/16
# 
# Details:
# 1) Set up and load data
# 2) Search for new relevant med codes
# 3) Create updated code lists
#
# Inputs:
# 1) Code_Lists/MASTER_Lists/CPRD_Aurum_Product_10Feb2025.txt: Aurum product master code list
# 2) Code_Lists/MASTER_Lists/CPRD_GOLD_Product_23Feb2025.txt: GOLD product master code list
# 3) OLD_Code_Lists/Depression/Aurum_Depression_Naomi.txt: Old Aurum depression code list
# 4) OLD_Code_Lists/Depression/Gold_Depression_Naomi.txt: Old GOLD depression code list
# 
# Intermediate outputs:
# 1) Code_Lists/Depression/Aurum_other_codes.csv: Potential codes to add for Aurum depression
# 2) Code_Lists/Depression/Gold_other_codes.csv: Potential codes to add for Gold depression
# 
# Final Outputs:
# 1) Code_Lists/Depression/Aurum_Depression_codelist_20250614.txt: Updated Aurum depression code list
# 2) Code_Lists/Depression/Gold_Depression_codelist_20250614.txt: Updated GOLD depression code list

# ==============================================================================


# ================= 1) Set up and load data ====================================

# Clear memory
rm(list = ls())

# Packages
library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# Set working directory
wd <- "/Volumes/ritd-ag-project-rd00qv-jfhay18/Stephanie/SMI_GLP/" # VPN connection
# wd <- "//live.rd.ucl.ac.uk/ritd-ag-project-rd00qv-jfhay18/Stephanie/SMI_GLP/" #Desktop@UCL
setwd(wd)

# Set input and output paths
path_input <- "Code_Lists/"
path_output <- "Code_Lists/Depression/"


## Load data

# Read in Aurum medical dictionary
cprd_aurum_medical <- 
  read_delim(
    paste0(wd, path_input, "MASTER_Lists/CPRD_Aurum_Medical_10Feb2025.txt"), 
    delim = "\t", escape_double = FALSE, 
    col_types = cols(MedCodeId = col_character(), 
                     Observations = col_skip(), OriginalReadCode = col_skip(), 
                     CleansedReadCode = col_skip(), SnomedCTConceptId = col_skip(), 
                     SnomedCTDescriptionId = col_skip(), 
                     Release = col_skip(), EmisCodeCategoryId = col_skip()), 
    trim_ws = TRUE) %>%
  rename(term = Term, medcodeid = MedCodeId) %>%
  mutate(term = str_to_lower(term))

# Read in Gold medical dictionary
cprd_gold_medical <- 
  read_delim(
    paste0(wd, path_input, "MASTER_Lists/CPRD_GOLD_Medical_23Feb2025.txt"), 
    delim = "\t", escape_double = FALSE, 
    col_types = cols(medcode = col_character(), 
                     readcode = col_skip()), 
    trim_ws = TRUE) %>%
  rename(term = readterm) %>%
  mutate(term = str_to_lower(term))

# Read in old Depression code list from 2024/03/28, setting all col types to character
# Aurum
depr_codelist_aurum_old <- read_delim(
  paste0(wd, path_input, "OLD_Code_Lists/Depression/Aurum_Depression_Naomi.txt"),
  delim = "\t", escape_double = FALSE, col_types = "c", trim_ws = TRUE)
# Gold
depr_codelist_gold_old <- read_delim(
  paste0(wd, path_input, "OLD_Code_Lists/Depression/Gold_Depression_Naomi.txt"),
  delim = "\t", escape_double = FALSE, col_types = "c", trim_ws = TRUE)


# ================= 2) Search for new relevant med codes =======================

# Aurum

aurum_depr <- cprd_aurum_medical %>%
  # Inclusions
  filter(grepl("(?i)depress|seasonal affect}", 
               term)) %>%
  # Exclusions: family history, other disorders, substance, no symptom, treatment
  filter(!grepl(paste0(
    "(?i)fh:|family history|child|infant|maternal history|family|member|relative|",
    "mother|sex|newborn|fetus|maternal|relation|social|occupation|management|",
    "therap|psychosoc|manicurist|counselling|referral|behavioural|behavioral|",
    "psychoanalytic|play|gong|concussion|psychotherap|paranoid personality|",
    "signposting|language|training|assessment|intervention|bone|fracture|skull|",
    "muscle|tongue|st segment|depressor|pr depression|vaso|ventricular|motion|",
    "st depression|s-t depression|bereavement|sleep|complain|structure|review|qof|",
    "score|scale|caused|provision|information|sign|screen|level|improvement|",
    "sedative|seds|bh dis due|behav dis due|solvents|criminal|drg use|drug|",
    "substance|second|conduct|condition|axis|coc|amph|overdose|canna|tort|cell|",
    "senil|dementia|organic|disease|synthesis|epilep|partum|natal|meno|hyd|",
    "psychoticism|disturbance|alcohol|puer|infective|somatic|organism|product|",
    "bipolar|manic-depress|manic depress|schizophren|poison|adverse|allergy|",
    "mood disorder|adjustment disorder|respiratory|anxiety|pseudocyesis|",
    "dysmorph|object|nail|spirit|disab|environment|pros|surgical|candida|",
    "antidepress|anti-depress|psychotropic|suspect|monitor|educ|follow up|absent|",
    "antipsychotic|neurosis|stimulant|induced|mood affective|asper|",
    "psychotic disorder|no past history of|anxiol|no history|feelings|",
    "removed from|no evidence"), term))

# Subset to terms not already included in old Depression code list
aurum_new_depr <- aurum_depr %>%
  filter(!(medcodeid %in% depr_codelist_aurum_old$medcodeid))

# Which terms were in the old list but are not included in the new list
aurum_depr_miss_from_new <- depr_codelist_aurum_old %>%
  filter(!(medcodeid %in% aurum_depr))


# Gold

gold_depr <- cprd_gold_medical %>%
  # Inclusions
  filter(grepl("(?i)depress|seasonal affect}", 
               term)) %>%
  # Exclusions: family history, other disorders, substance, no symptom, treatment
  filter(!grepl(paste0(
    "(?i)fh:|family history|child|infant|maternal history|family|member|relative|",
    "mother|sex|newborn|fetus|maternal|relation|social|occupation|management|",
    "therap|psychosoc|manicurist|counselling|referral|behavioural|behavioral|",
    "psychoanalytic|play|gong|concussion|psychotherap|paranoid personality|",
    "signposting|language|training|assessment|intervention|bone|fracture|skull|",
    "muscle|tongue|st segment|depressor|pr depression|vaso|ventricular|motion|",
    "st depression|s-t depression|bereavement|sleep|complain|structure|review|qof|",
    "score|scale|caused|provision|information|sign|screen|level|improvement|",
    "sedative|seds|bh dis due|behav dis due|solvents|criminal|drg use|drug|",
    "substance|second|conduct|condition|axis|coc|amph|overdose|canna|tort|cell|",
    "senil|dementia|organic|disease|synthesis|epilep|partum|natal|meno|hyd|",
    "psychoticism|disturbance|alcohol|puer|infective|somatic|organism|product|",
    "bipolar|manic-depress|manic depress|schizophren|poison|adverse|allergy|",
    "mood disorder|adjustment disorder|respiratory|anxiety|pseudocyesis|",
    "dysmorph|object|nail|spirit|disab|environment|pros|surgical|candida|",
    "antidepress|anti-depress|psychotropic|suspect|monitor|educ|follow up|absent|",
    "antipsychotic|neurosis|stimulant|induced|mood affective|asper|",
    "psychotic disorder|no past history of|anxiol|no history|feelings|",
    "removed from|no evidence"), term))

# Subset to terms not already included in old Depression code list
gold_new_depr <- gold_depr %>%
  filter(!(medcode %in% depr_codelist_gold_old$medcode))

# Which terms were in the old list but are not included in the new list
gold_depr_miss_from_new <- depr_codelist_gold_old %>%
  filter(!(medcode %in% gold_depr))


# # Save lists of potential codes to add for Aurum and GOLD
# write.csv(aurum_new_depr, file = paste0(wd, path_output, "Aurum_Depression_other_codes.csv"))
# write.csv(gold_new_depr, file = paste0(wd, path_output, "Gold_Depression_other_codes.csv"))


# ================= 3) Create updated code lists ===============================

# Create updated code lists

# Aurum
depr_codelist_aurum_new <- aurum_depr
# Gold
depr_codelist_gold_new <- gold_depr

# # Save updated code lists
# write.table(depr_codelist_aurum_new,
#             file = paste0(wd, path_output, "Aurum_Depression_codelist_20250614.txt"),
#             sep = "\t", row.names = FALSE)
# 
# write.table(depr_codelist_gold_new,
#             file = paste0(wd, path_output, "Gold_Depression_codelist_20250614.txt"),
#             sep = "\t", row.names = FALSE)



