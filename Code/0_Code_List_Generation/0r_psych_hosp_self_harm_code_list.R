# ==============================================================================
# Generate code lists for psychiatric hospitalisation and self-harm 
# Author: SM Wu
# Date Created: 2025/09/29
# Date Updated: 2025/09/30
# 
# Details:
# 1) Set up and load data
# 2) Subset to relevant codes and create code list
#
# Inputs:
# 1) Code_Lists/MASTER_Lists/icd102019syst_codes.txt: ICD-10 codes from WHO
# 
# Final Outputs:
# 1) Code_Lists/Psych_Hosp_Self_Harm/psych_hosp_self_harm_codelist_20250929.txt: 
#    Updated psychiatric hospitalisation and self-harm ICD code list
# 2) Code_Lists/Psych_Hosp_Self_Harm/outcome_codes_29Sept2025.xlsx: 
#    Updated outcome ICD code lists including MACE and psych hosp/self-harm

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
library(openxlsx)
library(writexl)

# Set working directory
wd <- "/Volumes/ritd-ag-project-rd00qv-jfhay18/Stephanie/SMI_GLP/" # VPN connection
# wd <- "//live.rd.ucl.ac.uk/ritd-ag-project-rd00qv-jfhay18/Stephanie/SMI_GLP/" #Desktop@UCL
setwd(wd)

# Set input and output paths
path_input <- "Code_Lists/"
path_output <- "Code_Lists/Psych_Hosp_Self_Harm/"

## Load data

# Read in ICD-10 dictionary, obtained from https://icdcdn.who.int/icd10/index.html 
icd10_raw <- read_delim(
  paste0(wd, path_input, "MASTER_Lists/icd102019syst_codes.txt"),
  delim = ";", escape_double = FALSE, col_names = FALSE,
  trim_ws = TRUE)
colnames(icd10_raw) <- c("hier_level", "tree_node", "terminal_type", 
                         "chapter", "3chars", "no_dagger", "no_aster", "no_dot",
                         "title")
# Subset to relevant columns
icd10 <- icd10_raw %>% 
  select(chapter, no_dagger, title) %>%
  rename(code = no_dagger)


# ================= 2) Subset to relevant codes and create code list ===========

# Subset to relevant codes
psych_icd <- icd10 %>%
  filter(grepl("(?i)F", code)) # psychiatric diagnosis codes
# Specify code must be primary diagnosis
psych_icd$primary_only <- TRUE
# Specify type
psych_icd$type <- "psych_primary"

# Add in mental health symptom or observation codes
mh_symptom_icd <- icd10 %>%
  filter(grepl(
    paste0("(?i)R41|", # Disorientation, amnesia
           "R44|", # Hallucinations
           "R45|", # Emotional state
           "R46|", # Appearance and behaviour
           "Z00.4|", # General psychiatric examination, not elsewhere classified
           "Z03.2|", # Observation for suspected mental and behavioural disorders
           "Z13.3|",  # Special screening examination for mental and behavioural disorders
           "Z73"), # Problems related to life-management difficulty
    code))
# Code must be primary diagnosis
mh_symptom_icd$primary_only <- TRUE
# Specify type
mh_symptom_icd$type <- "mh_symptom"

# Add in self-harm codes
self_harm_icd <- icd10 %>% 
  filter(grepl(
    paste0("(?i)X6|", # Intentional self-poisoning
           "X7|", # Intentional self-harm
           "X81|X82|X83|X84|", # Intentional self-harm
           "Y87.0"), #  Sequelae of intentional self-harm
    code))
# Code can be primary or secondary diagnosis
self_harm_icd$primary_only <- FALSE
# Specify type
self_harm_icd$type <- "self_harm"


# Combine ICD codes
psych_hosp_icd <- rbind(psych_icd, mh_symptom_icd, self_harm_icd)
  
# Specify type for group heading codes that contain .-
psych_hosp_icd <- psych_hosp_icd %>%
  mutate(type = case_when(
    grepl("\\.-", code) ~ "group_heading",
    .default = type))

# # Save updated code lists
# write.table(psych_hosp_icd,
#             file = paste0(wd, path_output, 
#                           "psych_hosp_self_harm_codelist_20250929.txt"),
#             sep = "\t", row.names = FALSE)


# ================= 3) Combine outcome code lists ==============================


# Read in MACE ICD code list
mace_icd <- read_delim(
  paste0(wd, path_input, "MACE/MACE_codelist_20250929.txt"),
  delim = "\t", escape_double = FALSE, 
  trim_ws = TRUE)

# Combine MACE and psychiatric hospitalisation/self-harm codes into one file
outcome_codes <- list(
  MACE = mace_icd,
  psych_hosp_self_harm = psych_hosp_icd)

# # Save lists of outcome codes into one .xlsx file on separate tabs
# write.xlsx(outcome_codes,
#            file = paste0(wd, path_output, "outcome_codes_29Sept2025.xlsx"),
#            overwrite = TRUE)
