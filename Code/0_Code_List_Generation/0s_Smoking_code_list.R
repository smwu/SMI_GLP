# ==============================================================================
# Generate code lists for Smoking (current or former smoker)
# Author: SM Wu
# Date Created: 2025/11/11
# Date Updated: 2025/11/11
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
# 3) Code_Lists/Smoking/Old/Aurum_Smoking_codelist_20240227_Alvin.txt: Old Aurum Smoking code list
# 4) Code_Lists/Smoking/Old/Gold_Smoking_codelist_20240227_Alvin.txt: Old GOLD Smoking code list
# 
# Intermediate outputs:
# 1) Code_Lists/Smoking/Aurum_other_codes.csv: Potential codes to add for Aurum Smoking
# 2) Code_Lists/Smoking/Gold_other_codes.csv: Potential codes to add for Gold Smoking
# 
# Final Outputs:
# 1) Code_Lists/Smoking/Aurum_Gold_Smoking_new_codes_20250819.txt: Newly added Smoking codes for Aurum and GOLD
# 2) Code_Lists/Smoking/Aurum_Smoking_codelist_20251027.txt: Updated Aurum Smoking code list
# 3) Code_Lists/Smoking/Gold_Smoking_codelist_20251027.txt: Updated GOLD Smoking code list
# 4) Code_Lists/Smoking/Aurum_Gold_Smoking_codelist_20251027.txt: Updated Aurum and GOLD Smoking code list
# 5) Code_Lists/Smoking/Aurum_Smoking_codelist_medcode_20251027.txt: Aurum Smoking comma-separated medcodes only
# 6) Code_Lists/Smoking/Aurum_Smoking_codelist_processed_20251027.txt: Aurum Smoking reformatted for CPRD extraction
# 7) Code_Lists/Smoking/Gold_Smoking_codelist_medcode_20251027.txt: GOLD Smoking comma-separated medcodes only
# 8) Code_Lists/Smoking/Gold_Smoking_codelist_processed_20251027.txt: GOLD Smoking reformatted for CPRD extraction

# ==============================================================================


# ================= 1) Set up and load data ====================================

# Clear memory
rm(list = ls())

# Packages
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(openxlsx)

# Set working directory
wd <- "/Volumes/ritd-ag-project-rd00qv-jfhay18/Stephanie/SMI_GLP/" # VPN connection
# wd <- "//live.rd.ucl.ac.uk/ritd-ag-project-rd00qv-jfhay18/Stephanie/SMI_GLP/" #Desktop@UCL
setwd(wd)

# Set input and output paths
path_input <- "Code_Lists/"
path_output <- "Code_Lists/Smoking/"


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

# Read in old Smoking code list from 2023/07/25, setting all col types to character
# Aurum
smoking_codelist_aurum_old <- read_delim(
  paste0(wd, path_input, "Smoking/Old/Aurum_Gold_Smoking_codelist_20230725_Alvin.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character()), trim_ws = TRUE)
# Gold
smoking_codelist_gold_old <- smoking_codelist_aurum_old


# ================= 2) Search for new relevant med codes =======================

# Aurum

aurum_smoking <- cprd_aurum_medical %>%
  # Inclusions: smoking, tobacco, bupropion (for nicotine withdrawal)
  filter(grepl(paste0("(?i)smoking|smoker|smokes|smoked|roll-up|roll up|",
               "tobacco|ex-tobacco|ex-smoker|stop-smoking|stopped smoking|",
               "bupropion refused|bupropion contraindicated|anti-smoking|",
               "cigar|cigarette|nicotine|Smokers"), 
               term)) %>%
  # Exclusions: smoke from burning, smoke inhalation
  filter(!grepl(paste0(
    # Burning, smoke inhalation, occupational
    "(?i)burn|smoke inhalation|accident caused by|exposure to|factory|industry|",
    "processor|occupation|smoking-pipe maker|exposed to tobacco smoke|",
    "environmental tobacco|cigarette maker|tobacco dust|",
    # Family/caregiver
    "fh:|family history|child|infant|maternal|paternal|family|member|relative|",
    "father|mother|carer|newborn|pregnancy|parental|smoker in home|",
    "smoker in household|smokefree home|",
    # Counselling and care
    "leaflet|advice on|assessment of tobacco use|provision of written information|",
    # e-cigarette, vaping, drug, non-specific
    "electronic cigarette|e-cigarette|vaper without nicotine|^cigarette$|",
    "drops unextinguished cigarettes|hides lit cigarettes in pockets|",
    # measurement
    "smoking status|waking time|date of|total time|plcom2012|age at|",
    "smoking scale|other smoking information|screen|screening|pack years|",
    "socio-economic classification|smoking assessment|exacerbation frequency index|",
    # allergies
    "allergy|asthma|poisoning|",
    # Other 
    "teeth|heart beat|copd|chronic obstructive pulmonary|amblyopia|",
    "processed cheese|staining of finger|",
    # Negations
    "never used|non-smoker|not a|passive|consumption nil|never smoked|",
    "never chewed|tobacco usage unknown|possibly untrue|does not use|",
    "excepted from smoking quality indicators|exception reporting|", 
    "smoking consumption unknown|does not chew tobacco"), 
    term, perl = TRUE))

# Create smoking status column
aurum_smoking$smoking_status <- "current or former"

# Which terms were in the old list but are not included in the new list,
# filtering to those terms in the code dictionary
aurum_smoking_miss_from_new <- smoking_codelist_aurum_old %>%
  filter(!(medcodeid %in% aurum_smoking$medcodeid)) %>% 
  filter(medcodeid %in% cprd_aurum_medical$medcodeid)

# See which terms were not included in old Smoking code list
aurum_new_smoking <- aurum_smoking %>%
  filter(!(medcodeid %in% smoking_codelist_aurum_old$medcodeid))


# Get non-smoker codes
aurum_non_smoker <- cprd_aurum_medical %>%
  # Inclusions: non-smoker
  filter(grepl(paste0("(?i)non-smoker|never smoked|",
                      "non smoker|never used tobacco"), 
               term)) %>%
  # Exclusions: smoke from burning, smoke inhalation
  filter(!grepl(paste0(
    # Family/caregiver
    "fh:|family history|child|infant|maternal|paternal|family|member|relative|",
    "father|mother|carer|newborn|pregnancy|parental|smoker in home|",
    "smoker in household|smokefree home"),
    term, perl = TRUE))

# Create smoking status column
aurum_non_smoker$smoking_status <- "non-smoker"



# Gold

gold_smoking <- cprd_gold_medical %>%
  # Inclusions: smoking, tobacco, bupropion (for nicotine withdrawal)
  filter(grepl(paste0("(?i)smoking|smoker|smokes|smoked|roll-up|roll up|",
                      "tobacco|ex-tobacco|ex-smoker|stop-smoking|stopped smoking|",
                      "bupropion refused|bupropion contraindicated|anti-smoking|",
                      "cigar|cigarette|nicotine|Smokers"), 
               term)) %>%
  # Exclusions: smoke from burning, smoke inhalation
  filter(!grepl(paste0(
    # Burning, smoke inhalation, occupational
    "(?i)burn|smoke inhalation|accident caused by|exposure to|factory|industry|",
    "processor|occupation|smoking-pipe maker|exposed to tobacco smoke|",
    "environmental tobacco|cigarette maker|tobacco dust|",
    # Family/caregiver
    "fh:|family history|child|infant|maternal|paternal|family|member|relative|",
    "father|mother|carer|newborn|pregnancy|parental|smoker in home|",
    "smoker in household|smokefree home|",
    # Counselling and care
    "leaflet|advice on|assessment of tobacco use|provision of written information|",
    # e-cigarette, vaping, drug, non-specific
    "electronic cigarette|e-cigarette|vaper without nicotine|^cigarette$|",
    "drops unextinguished cigarettes|hides lit cigarettes in pockets|",
    # measurement
    "smoking status|waking time|date of|total time|plcom2012|age at|",
    "smoking scale|other smoking information|screen|screening|pack years|",
    "socio-economic classification|smoking assessment|exacerbation frequency index|",
    # allergies
    "allergy|asthma|poisoning|",
    # Other 
    "teeth|heart beat|copd|chronic obstructive pulmonary|amblyopia|",
    "processed cheese|staining of finger|",
    # Negations
    "never used|non-smoker|not a|passive|consumption nil|never smoked|",
    "never chewed|tobacco usage unknown|possibly untrue|does not use|",
    "excepted from smoking quality indicators|exception reporting|", 
    "smoking consumption unknown|does not chew tobacco"), 
    term, perl = TRUE))

# Create smoking status column
gold_smoking$smoking_status <- "current or former"

# Which terms were in the old list but are not included in the new list,
# filtering to those terms in the code dictionary
gold_smoking_miss_from_new <- smoking_codelist_gold_old %>%
  filter(!(medcode %in% gold_smoking$medcode)) %>% 
  filter(medcode %in% cprd_gold_medical$medcode)

# Which terms are not included in old Smoking code list
gold_new_smoking <- gold_smoking %>%
  filter(!(medcode %in% smoking_codelist_gold_old$medcode))

# Get non-smoker codes
gold_non_smoker <- cprd_gold_medical %>%
  # Inclusions: non-smoker
  filter(grepl(paste0("(?i)non-smoker|never smoked|",
                      "non smoker|never used tobacco"), 
               term)) %>%
  # Exclusions: smoke from burning, smoke inhalation
  filter(!grepl(paste0(
    # Family/caregiver
    "fh:|family history|child|infant|maternal|paternal|family|member|relative|",
    "father|mother|carer|newborn|pregnancy|parental|smoker in home|",
    "smoker in household|smokefree home"),
    term, perl = TRUE))

# Create smoking status column
gold_non_smoker$smoking_status <- "non-smoker"



# Combine Aurum and GOLD new Smoking codes
aurum_gold_smoking_new_codes <- list(
  Aurum = aurum_new_smoking,
  Gold = gold_new_smoking)
# # Save lists of newly added codes into one .xlsx file on separate tabs
# write.xlsx(aurum_gold_smoking_new_codes,
#            file = paste0(wd, path_output, "Aurum_Gold_Smoking_new_codes_11Nov2025.xlsx"),
#            overwrite = TRUE)


# ================= 3) Create updated code lists ===============================

# Create updated code lists

# Aurum
smoking_codelist_aurum_new <- bind_rows(aurum_smoking, aurum_non_smoker)
# Gold
smoking_codelist_gold_new <- bind_rows(gold_smoking, gold_non_smoker)

# # Save updated code lists
# write.table(smoking_codelist_aurum_new,
#             file = paste0(wd, path_output, "Aurum_Smoking_codelist_20251111.txt"),
#             sep = "\t", row.names = FALSE)
# 
# write.table(smoking_codelist_gold_new,
#             file = paste0(wd, path_output, "Gold_Smoking_codelist_20251111.txt"),
#             sep = "\t", row.names = FALSE)

