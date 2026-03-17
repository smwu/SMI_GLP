# -----------------------
# Hypertension code list
# -----------------------
# CPRD 2023 data
# Last run: 28/02/24

# DERIVE COHORT

# Clear memory
rm(list = ls())

# Packages
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Set correct file path
path <- "//live.rd.ucl.ac.uk" #Desktop@UCL
path <- "/Volumes/" # VPN connection

# Set working directory
setwd(paste0(path, "/ritd-ag-project-rd00qv-jfhay18/Alvin"))

# Import medical dictionaries
CPRDAurumMedical <- read_delim("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/PhD project/Data sources/CPRD/Documentation/CPRD-2023/CPRDAurumMedical.txt", 
                               delim = "\t", escape_double = FALSE, 
                               col_types = cols(MedCodeId = col_character(), 
                                                Observations = col_skip(), OriginalReadCode = col_skip(), 
                                                CleansedReadCode = col_skip(), SnomedCTConceptId = col_skip(), 
                                                SnomedCTDescriptionId = col_skip(), 
                                                Release = col_skip(), EmisCodeCategoryId = col_skip()), 
                               trim_ws = TRUE) %>%
  rename(term = Term, medcodeid = MedCodeId) %>%
  mutate(term = str_to_lower(term))

CPRDGoldMedical <- read_delim("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/PhD project/Data sources/CPRD/Documentation/CPRD-2023/CPRDGoldMedical.txt", 
                              delim = "\t", escape_double = FALSE, 
                              col_types = cols(medcode = col_character(), 
                                               readcode = col_skip()), trim_ws = TRUE) %>%
  rename(term = desc) %>%
  mutate(term = str_to_lower(term))

# Define code lists
# Codes relating to HTN in pregnancy removed as likely more transient (consistent with handling of gestational diabetes)

# Aurum

aurum_hypertension <- CPRDAurumMedical %>%
  filter(grepl("(?i)hypertension|hypertensive|high blood pressure|high systolic|high diastolic", term)) %>%
  filter(!grepl("(?i)fh:|family history|child|infant|maternal history|absent|member|score|no evidence|hypertension resolved|borderline|caused by drug|secondary to drug|
                |neonatal|risk|screen|white coat hypertension|newborn|fetus|pregnancy|without diagnosis of hypertension", term) | medcodeid == "1992471000006116")

# Gold

gold_hypertension <- CPRDGoldMedical %>%
  filter(grepl("(?i)hypertension|hypertensive|high blood pressure|high systolic|high diastolic", term)) %>%
  filter(!grepl("(?i)fh:|family history|child|infant|maternal history|absent|member|score|no evidence|hypertension resolved|borderline|caused by drug|secondary to drug|
                |neonatal|risk|screen|white coat hypertension|newborn|fetus|pregnancy|without diagnosis of hypertension", term))


# save as text file
write.table(gold_hypertension, file = "Code lists/Hypertension/hypertension_gold_280224.txt",
            sep = "\t", row.names = FALSE)

write.table(aurum_hypertension, file = "Code lists/Hypertension/hypertension_aurum_280224.txt",
            sep = "\t", row.names = FALSE)

remove(gold_hypertension, aurum_hypertension, CPRDGoldMedical, CPRDAurumMedical)
