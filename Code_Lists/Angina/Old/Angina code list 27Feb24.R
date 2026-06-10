# -----------------------
# Angina code list
# -----------------------
# CPRD 2023 data
# Last run: 27/02/24

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


# Aurum

aurum_angina <- CPRDAurumMedical %>%
  filter(grepl("(?i)angina|cardiac syndrome x", term)) %>%
  filter(!grepl("(?i)fh:|family history|fh angina|bullosa|herpangina|score|grade|grading|streptococcal|ludwig's|vincent's|infarct|adverse reaction|canadian", term))

# Gold

gold_angina <- CPRDGoldMedical %>%
  filter(grepl("(?i)angina|cardiac syndrome x", term)) %>%
  filter(!grepl("(?i)fh:|family history|fh angina|bullosa|herpangina|score|grade|grading|streptococcal|ludwig's|vincent's|infarct|adverse reaction|canadian", term))


# save as text file
write.table(gold_angina, file = "Code lists/Angina/angina_gold_270224.txt",
            sep = "\t", row.names = FALSE)

write.table(aurum_angina, file = "Code lists/Angina/angina_aurum_270224.txt",
            sep = "\t", row.names = FALSE)

remove(gold_angina, aurum_angina, CPRDGoldMedical, CPRDAurumMedical)
