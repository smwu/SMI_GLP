# ==============================================================================
# Generate code lists for Depression diagnoses
# Author: SM Wu
# Date Created: 2025/06/13
# Date Updated: 2025/07/25
# 
# Details:
# 1) Set up and load data
# 2) Search for new relevant med codes
# 3) Create updated code lists
# 4) Combine depression into SMI code lists
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
# 1) Code_Lists/Depression/Aurum_Depression_codelist_20250725.txt: Updated Aurum depression code list
# 2) Code_Lists/Depression/Gold_Depression_codelist_202507225.txt: Updated GOLD depression code list
# 3) Code_Lists/Depression/Aurum_Gold_Depression_new_codes_20250725.txt: Newly added depression codes for Aurum and GOLD
# 4) Code_Lists/Depression/Aurum_SMI_Depression_codelist_20250725.txt: Aurum SMI (including depression) code list
# 5) Code_Lists/Depression/Gold_SMI_Depression_codelist_20250725.txt: GOLD SMI (including depression) code list

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
cprd_aurum_medical_raw <- 
  read_delim(
    paste0(wd, path_input, "MASTER_Lists/CPRD_Aurum_Medical_10Feb2025.txt"), 
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
    paste0(wd, path_input, "MASTER_Lists/CPRD_GOLD_Medical_23Feb2025.txt"), 
    delim = "\t", escape_double = FALSE, 
    col_types = cols(medcode = col_character(), 
                     readcode = col_character()), 
    trim_ws = TRUE) 
cprd_gold_medical <- cprd_gold_medical_raw %>%
  rename(term = readterm) %>%
  mutate(term = str_to_lower(term))

# Read in old Depression code list from 2024/03/28, setting all col types to character
# Aurum
depr_codelist_aurum_old <- read_delim(
  paste0(wd, path_input, "OLD_Code_Lists/Depression/Aurum_Depression_Naomi.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character(), 
                   SnomedCTConceptId = col_character()), trim_ws = TRUE)
# Gold
depr_codelist_gold_old <- read_delim(
  paste0(wd, path_input, "OLD_Code_Lists/Depression/Gold_Depression_Naomi.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character(), 
                   readcode = col_character()), trim_ws = TRUE)


# ================= 2) Search for new relevant med codes =======================

# Aurum

aurum_depr <- cprd_aurum_medical %>%
  # Inclusions: referrals, management, monitoring, review, signposting,
  # geriatric depression scale 9-15 points, phq9 10-27 total score
  filter(grepl("(?i)depress|affective disorder|major depr ep", 
               term)) %>%
  # Exclusions: family history, other disorders, substance, no symptom, treatment,
  # mild depression (if not recurrent)
  filter(!grepl(paste0(
    # Family history
    "(?i)fh:|family history|child|infant|maternal history|family|member|relative|",
    # Maternal and pregnancy-related
    "mother|sex|newborn|fetus|maternal|puer|relation|partum|natal|meno|",
    # Counselling
    "therap|psychosoc|manicurist|counselling|behavioural|behavioral|educ|",
    "follow up|absent|social|occupation|",
    # Other disorders
    "psychoanalytic|play|gong|concussion|psychotherap|paranoid personality|asper|",
    "depressive personality|seasonal|postviral depr|postoperative depr|dysmorph|",
    # Other non-related medical 
    "language|training|assessment|intervention|bone|fracture|skull|induced|",
    "muscle|tongue|st segment|depressor|pr depression|vaso|ventricular|motion|",
    "st depression|s-t depression|bereavement|sleep|complain|structure|qof|disab|",
    "environment|pros|surgical|candida|respiratory|pseudocyesis|nail|object|",
    # Measurement without diagnosis
    "beck|ipos|caused|provision|information|sign|screen|level|improvement|",
    "keele|phq9 score|grading|geriatric depression score|score$|",
    "\\bscale\\b(?!\\s*-\\s*(9|1[0-5]) points)|",
    # Substance-related
    "sedative|seds|bh dis due|behav dis due|solvents|criminal|drg use|drug|",
    "substance|second|conduct|condition|axis|coc|amph|overdose|canna|tort|cell|",
    "disturbance|alcohol|infective|organism|product|adverse|allergy|stimulant|",
    "spirit|poison|",
    # Organic
    "senil|dementia|organic|disease|synthesis|epilep|hyd|",
    # Other SMI conditions
    "bipolar|bipol|manic-depress|manic depress|schizophren|schizoaffective|",
    "psychoticism|mood disorder|psychotic disorder|adjustment disorder|",
    "mild anxiety|mild major depress|single major depressive episode, mild|",
    "major depression, mild|mild depress|",
    # Medications,
    "antipsychotic|antidepress|anti-depress|psychotropic|",
    # Negations
    "removed from|no past history of|anxiol|no history|feelings|no evidence"), 
    term, perl = TRUE))

# Which terms were in the old list but are not included in the new list,
# filtering to those terms in the code dictionary
aurum_depr_miss_from_new <- depr_codelist_aurum_old %>%
  filter(!(medcodeid %in% aurum_depr$medcodeid)) %>% 
  filter(medcodeid %in% cprd_aurum_medical$medcodeid)


# Removing specific terms due to SNOMED mapping issues
exclusions <- c()
aurum_depr <- aurum_depr %>%
  filter(!(medcodeid %in% exclusions))

# Subset to terms not already included in old Depression code list
aurum_new_depr <- aurum_depr %>%
  filter(!(medcodeid %in% depr_codelist_aurum_old$medcodeid))


# Gold

gold_depr <- cprd_gold_medical %>%
  # Inclusions: referrals, management, monitoring, review, signposting,
  # geriatric depression scale 9-15 points, phq9 10-27 total score
  filter(grepl("(?i)depress|affective disorder|major depr ep", 
               term)) %>%
  # Exclusions: family history, other disorders, substance, no symptom, treatment,
  # mild depression (if not recurrent)
  filter(!grepl(paste0(
    # Family history
    "(?i)fh:|family history|child|infant|maternal history|family|member|relative|",
    # Maternal and pregnancy-related
    "mother|sex|newborn|fetus|maternal|puer|relation|partum|natal|meno|",
    # Counselling
    "therap|psychosoc|manicurist|counselling|behavioural|behavioral|educ|",
    "follow up|absent|social|occupation|",
    # Other disorders
    "psychoanalytic|play|gong|concussion|psychotherap|paranoid personality|asper|",
    "depressive personality|seasonal|postviral depr|postoperative depr|dysmorph|",
    # Other non-related medical 
    "language|training|assessment|intervention|bone|fracture|skull|induced|",
    "muscle|tongue|st segment|depressor|pr depression|vaso|ventricular|motion|",
    "st depression|s-t depression|bereavement|sleep|complain|structure|qof|disab|",
    "environment|pros|surgical|candida|respiratory|pseudocyesis|nail|object|",
    # Measurement without diagnosis
    "beck|ipos|caused|provision|information|sign|screen|level|improvement|",
    "keele|phq9 score|grading|geriatric depression score|score$|",
    "\\bscale\\b(?!\\s*-\\s*(9|1[0-5]) points)|",
    # Substance-related
    "sedative|seds|bh dis due|behav dis due|solvents|criminal|drg use|drug|",
    "substance|second|conduct|condition|axis|coc|amph|overdose|canna|tort|cell|",
    "disturbance|alcohol|infective|organism|product|adverse|allergy|stimulant|",
    "spirit|poison|",
    # Organic
    "senil|dementia|organic|disease|synthesis|epilep|hyd|",
    # Other SMI conditions
    "bipolar|bipol|manic-depress|manic depress|schizophren|schizoaffective|",
    "psychoticism|mood disorder|psychotic disorder|adjustment disorder|",
    "mild anxiety|mild major depress|single major depressive episode, mild|",
    "major depression, mild|mild depress|",
    # Medications,
    "antipsychotic|antidepress|anti-depress|psychotropic|",
    # Negations
    "removed from|no past history of|anxiol|no history|feelings|no evidence"), 
    term, perl = TRUE))


# Which terms were in the old list but are not included in the new list,
# filtering to those terms in the code dictionary
gold_depr_miss_from_new <- depr_codelist_gold_old %>%
  filter(!(medcode %in% gold_depr$medcode)) %>% 
  filter(medcode %in% cprd_gold_medical$medcode)

# Removing specific terms due to SNOMED mapping issues
gold_depr <- gold_depr %>%
  filter(!(medcode %in% exclusions))

# Subset to terms not already included in old Depression code list
gold_new_depr <- gold_depr %>%
  filter(!(medcode %in% depr_codelist_gold_old$medcode))


# Combine Aurum and GOLD new Depression codes
aurum_gold_depr_new_codes <- list(
  Aurum = aurum_new_depr,
  Gold = gold_new_depr)
# # Save lists of newly added codes into one .xlsx file on separate tabs
# write.xlsx(aurum_gold_depr_new_codes,
#            file = paste0(wd, path_output, "Aurum_Gold_Depression_new_codes_25Jul2025.xlsx"),
#            overwrite = TRUE)



### OLDER CODE [DO NOT RUN]

# # Save lists of potential codes to add for Aurum and GOLD
# # These were last reviewed by JH on Jul 23, 2025
# write.csv(aurum_new_depr, file = paste0(wd, path_output, "Aurum_Depression_other_codes.csv"))
# write.csv(gold_new_depr, file = paste0(wd, path_output, "Gold_Depression_other_codes.csv"))

# # Any terms in older lists but not in searched terms
# aurum_gold_depr_miss_codes <- list(
#   Aurum = aurum_depr_miss_from_new,
#   Gold = gold_depr_miss_from_new)
# 
# # Save lists of missing codes into one .xlsx file on separate tabs
# # These were last reviewed by JH on Jul 23, 2025
# write.xlsx(aurum_gold_depr_miss_codes,
#            file = paste0(wd, path_output, 
#                          "Aurum_Gold_Depression_missing_codes_14Jul2025.xlsx"),
#            overwrite = TRUE)


# ================= 3) Create updated code lists ===============================

# Create updated code lists

# Aurum
depr_codelist_aurum_new <- aurum_depr
# Gold
depr_codelist_gold_new <- gold_depr

# # Save updated code lists
# write.table(depr_codelist_aurum_new,
#             file = paste0(wd, path_output, "Aurum_Depression_codelist_20250725.txt"),
#             sep = "\t", row.names = FALSE)
# 
# write.table(depr_codelist_gold_new,
#             file = paste0(wd, path_output, "Gold_Depression_codelist_20250725.txt"),
#             sep = "\t", row.names = FALSE)


# ================= 4) Combine depression into SMI code lists ==================

# Read in depression code lists
depr_codelist_aurum_new <- read_delim(
  paste0(wd, path_output, "Aurum_Depression_codelist_20250725.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character(), OriginalReadCode = col_skip(), 
                   CleansedReadCode = col_skip(), Observations = col_skip(),
                   SnomedCTConceptId = col_skip(), 
                   SnomedCTDescriptionId = col_skip(),
                   EmisCodeCategoryId = col_skip()), 
  trim_ws = TRUE)

depr_codelist_gold_new <- read_delim(
  paste0(wd, path_output, "Gold_Depression_codelist_20250725.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character(), readcode = col_skip(), 
                   clinicalevents = col_skip(), immunisationevents = col_skip(), 
                   referralevents = col_skip(), testevents = col_skip(), 
                   databaserelease = col_skip()), 
  trim_ws = TRUE)

# Read in SMI code lists
smi_codelist_aurum_new <- read_delim(
  paste0(wd, "Code_Lists/SMI/Aurum_SMI_codelist_20250725.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character(), 
                   TermSNOMED = col_skip(), TermEMIS = col_skip(), 
                   SNOMED = col_skip(), OriginalReadCode = col_skip(), 
                   CleansedReadCode = col_skip(), Observations = col_skip()), 
  trim_ws = TRUE) %>%
  rename(term = TermRead, group = Group)

smi_codelist_gold_new <- read_delim(
  paste0(wd, "Code_Lists/SMI/Gold_SMI_codelist_20250725.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character(), 
                   OriginalReadCode = col_skip(), 
                   CleansedReadCode = col_skip(), clinicalevents = col_skip(), 
                   immunisationevents = col_skip(), referralevents = col_skip(), 
                   testevents = col_skip(), databaserelease = col_skip()), 
  trim_ws = TRUE) %>%
  rename(term = Term, group = Group)

# Added group = "depression"
depr_codelist_aurum_new <- depr_codelist_aurum_new %>%
  mutate(group = "depression")
depr_codelist_gold_new <- depr_codelist_gold_new %>%
  mutate(group = "depression")

# Combine depression into the SMI code lists
smi_depr_codelist_aurum <- rbind(smi_codelist_aurum_new, depr_codelist_aurum_new)
smi_depr_codelist_gold <- rbind(smi_codelist_gold_new, depr_codelist_gold_new)

# # Save SMI (including depression) code lists
# write.table(smi_depr_codelist_aurum,
#             file = paste0(wd, path_output, 
#                           "Aurum_SMI_Depression_codelist_20250725.txt"),
#             sep = "\t", row.names = FALSE)
# 
# write.table(smi_depr_codelist_gold,
#             file = paste0(wd, path_output, 
#                           "Gold_SMI_Depression_codelist_20250725.txt"),
#             sep = "\t", row.names = FALSE)

