# ==============================================================================
# Generate code lists for SMI diagnoses
# Author: SM Wu
# Date Created: 2025/06/11
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
# 3) Code_Lists/MASTER_Lists/Business_Rules_Combined_Change_Log_QOF%2B2024-25_v49.1.xlsm: QOF v49.1 clusters
# 4) OLD_Code_Lists/SMI/Aurum_SMI_codelist_21032024_Alvin.txt: Old Aurum SMI code list
# 5) OLD_Code_Lists/SMI/Gold_SMI_codelist_21032024_Alvin.txt: Old GOLD SMI code list
# 
# Intermediate outputs:
# 1) Code_Lists/SMI/Aurum_other_codes.csv: Potential codes to add for Aurum SMI
# 2) Code_Lists/SMI/Gold_other_codes.csv: Potential codes to add for Gold SMI
# 
# Final Outputs:
# 1) Code_Lists/SMI/Aurum_SMI_codelist_20250613.txt: Updated Aurum SMI code list
# 2) Code_Lists/SMI/Gold_SMI_codelist_20250613.txt: Updated GOLD SMI code list

# ==============================================================================


# ================= 1) Set up and load data ====================================

# Clear memory
rm(list = ls())

# Packages
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(readxl)

# Set working directory
wd <- "/Volumes/ritd-ag-project-rd00qv-jfhay18/Stephanie/SMI_GLP/" # VPN connection
# wd <- "//live.rd.ucl.ac.uk/ritd-ag-project-rd00qv-jfhay18/Stephanie/SMI_GLP/" #Desktop@UCL
setwd(wd)

# Set input and output paths
path_input <- "Code_Lists/"
path_output <- "Code_Lists/SMI/"

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

# Read in QOF clusters to get incentivized SMI codes
qof <- readxl::read_excel(
  paste0(wd, path_input, 
         "MASTER_Lists/Business_Rules_Combined_Change_Log_QOF%2B2024-25_v49.1.xlsm"),
  sheet = "Expanded Cluster List", skip = 14)
# Get unique cluster types
qof_unique <- qof %>% 
  select(`Cluster ID`, `Cluster description`) %>% 
  distinct()
# Subset to SMI clusters
qof_smi <- qof %>%
  filter(`Cluster ID` == "MH_COD")

# Read in old SMI code list from 2024/03/21, setting all col types to character
# Aurum
smi_codelist_aurum_old <- read_delim(
  paste0(wd, path_input, "OLD_Code_Lists/SMI/Aurum_SMI_codelist_21032024_Alvin.txt"),
  delim = "\t", escape_double = FALSE, col_types = "c", trim_ws = TRUE)
# Gold
smi_codelist_gold_old <- read_delim(
  paste0(wd, path_input, "OLD_Code_Lists/SMI/Gold_SMI_codelist_21032024_Alvin.txt"),
  delim = "\t", escape_double = FALSE, col_types = "c", trim_ws = TRUE)


## Sanity check
# # Naomi's code lists
# smi_codelist_aurum_naomi <- read_delim(
#   paste0(wd, path_input, "OLD_Code_Lists/SMI/Aurum_SMI_Naomi.txt"),
#   delim = "\t", escape_double = FALSE, col_types = "c", trim_ws = TRUE)
# 
# # Check between Alvin's and Naomi's lists: none missing
# miss_from_alvin <- smi_codelist_aurum_naomi %>%
#   filter(!(medcodeid %in% smi_codelist_aurum_naomi$medcodeid))
# miss_from_naomi <- smi_codelist_aurum_old %>%
#   filter(!(medcodeid %in% smi_codelist_aurum_old$medcodeid))


# ================= 2) Search for new relevant med codes =======================

# Aurum

aurum_smi <- cprd_aurum_medical %>%
  # Inclusions
  filter(grepl("(?i)schizo|psychos|psychot|manic|lithium|delusion|paranoi|bipolar", 
               term)) %>%
  # Exclusions: family history, other disorders, substance, no symptom, treatment
  filter(!grepl(paste0(
    "(?i)fh:|family history|child|infant|maternal history|family|member|relative|",
    "mother|paranoid personality disorder|schizoid personality disorder|sex|",
    "hypomanic personality disorder|",
    "therap|psychosoc|manicurist|counselling|referral|behavioural|honos|",
    "signposting|language|training|assessment|intervention|hip replacement|",
    "score|scale|caused|provision|information|sign|",
    "behavioral|sedative|seds|bh dis due|behav dis due|solvents|criminal|drg use|",
    "senil|dementia|organic|korsakov|korsakoff|presbyophrenic|partum|puer|",
    "postnatal|org.|schizoid character|insight|monitoring|character|parkinson|",
    "alzheimer|dysmorph|pros|surgical|major depress|depression|depressive|",
    "substance|drug|alcoh|stimulant|amph|stimulant|coc|opioid|cannab|inhalant|",
    "nonpsychotic disorders|lithium|antipsychotic|psychotropic|convalescence|",
    "poison|adverse|allergy|psychosomatic|epilep|disintegrative|dwarfism|",
    "no evidene of psychosis|face caras|psychotic symptoms absent|no delusion|",
    "no paranoid|no hypomanic|jealousy|non|without psychotic|w'out psychotic|",
    "no evidence"), term))

# Subset to terms not already included in old SMI code list
aurum_new_smi <- aurum_smi %>%
  filter(!(medcodeid %in% smi_codelist_aurum_old$medcodeid))


# Gold

gold_smi <- cprd_gold_medical %>%
  # Inclusions
  filter(grepl("(?i)schizo|psychos|psychot|manic|lithium|delusion|paranoi|bipolar", 
               term)) %>%
  # Exclusions: family history, other disorders, substance, no symptom, treatment
  filter(!grepl(paste0(
    "(?i)fh:|family history|child|infant|maternal history|family|member|relative|",
    "mother|paranoid personality disorder|schizoid personality disorder|sex|",
    "hypomanic personality disorder|",
    "therap|psychosoc|manicurist|counselling|referral|behavioural|honos|",
    "signposting|language|training|assessment|intervention|hip replacement|",
    "score|scale|caused|provision|information|sign|",
    "behavioral|sedative|seds|bh dis due|behav dis due|solvents|criminal|drg use|",
    "senil|dementia|organic|korsakov|korsakoff|presbyophrenic|partum|puer|",
    "postnatal|org.|schizoid character|insight|monitoring|character|parkinson|",
    "alzheimer|dysmorph|pros|surgical|major depress|depression|depressive|",
    "substance|drug|alcoh|stimulant|amph|stimulant|coc|opioid|cannab|inhalant|",
    "nonpsychotic disorders|lithium|antipsychotic|psychotropic|convalescence|",
    "poison|adverse|allergy|psychosomatic|epilep|disintegrative|dwarfism|",
    "no evidence of psychosis|face caras|psychotic symptoms absent|no delusion|",
    "no paranoid|no hypomanic|jealousy|non|without psychotic|w'out psychotic|",
    "no evidence"), term))

# Subset to terms not already included in old SMI code list
gold_new_smi <- gold_smi %>%
  filter(!(medcode %in% smi_codelist_gold_old$medcode))

# Any terms in QOF but not in searched terms
add_from_qof <- qof_smi %>%
  filter(!(`SNOMED concept ID` %in% aurum_smi$medcodeid))
add_from_qof_gold <- qof_smi %>%
  filter(!(`SNOMED concept ID` %in% gold_smi$medcode))

# # Save lists of potential codes to add for Aurum and GOLD
# write.csv(aurum_new_smi, file = paste0(wd, path_output, "Aurum_SMI_other_codes.csv"))
# write.csv(gold_new_smi, file = paste0(wd, path_output, "Gold_SMI_other_codes.csv"))


# ================= 3) Create updated code lists ===============================

# Create updated code lists

# Aurum
smi_codelist_aurum_new <- smi_codelist_aurum_old
# Gold
smi_codelist_gold_new <- smi_codelist_gold_old

# # Save updated code lists
# write.table(smi_codelist_aurum_new, 
#             file = paste0(wd, path_output, "Aurum_SMI_codelist_20250613.txt"),
#             sep = "\t", row.names = FALSE)
# 
# write.table(smi_codelist_gold_new, 
#             file = paste0(wd, path_output, "Gold_SMI_codelist_20250613.txt"),
#             sep = "\t", row.names = FALSE)



