# -----------------------
# RenalDisease code list
# -----------------------
# CPRD 2023 data
# Last run: 23/04/24

## This code list has been designed by using ICD-10 terms in the N17-N19 chapter +  transplant

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

# Aurum

aurum_renaldisease <- CPRDAurumMedical %>%
  filter(grepl("(?i)disease|failure|transplant|chronic|insufficiency|impair", term) & grepl("(?i)renal|kidney|nephrotic", term) |
           
           grepl("(?i)ckd|acute kidney|chronic uraemia|chronic diffuse glomerulonephritis|chronic glomerulonephritis|nephrotic syndrome|
                 |dialysis|hemofiltration|renal replacement|kidney recipient", term)) %>%
  
  filter(!grepl("(?i)fh:|family history|child|infant|maternal history|pregnancy|absent|screening test|member|no h/o|suspected|neonatal|score|
                |negative|assay|assessment|risk|warning|study group|egfr|estimated|adrenal|cyclodialysis|iridodialysis|retinal|
                |notification|level|test request|resolved", term))

# Gold

gold_renaldisease <- CPRDGoldMedical %>%
  filter(grepl("(?i)disease|failure|transplant|chronic|insufficiency|impair", term) & grepl("(?i)renal|kidney|nephrotic", term) |
           
           grepl("(?i)ckd|acute kidney|chronic uraemia|chronic diffuse glomerulonephritis|chronic glomerulonephritis|nephrotic syndrome|
                 |dialysis|hemofiltration|renal replacement|kidney recipient", term)) %>%
  
  filter(!grepl("(?i)fh:|family history|child|infant|maternal history|pregnancy|absent|screening test|member|no h/o|suspected|neonatal|score|
                |negative|assay|assessment|risk|warning|study group|egfr|estimated|adrenal|cyclodialysis|iridodialysis|retinal|
                |notification|level|test request|resolved", term))

# save as text file
write.table(gold_renaldisease, file = "Code lists/Renal disease/renaldisease_gold_230424.txt",
            sep = "\t", row.names = FALSE)

write.table(aurum_renaldisease, file = "Code lists/Renal disease/renaldisease_aurum_230424.txt",
            sep = "\t", row.names = FALSE)

remove(gold_renaldisease, aurum_renaldisease, CPRDGoldMedical, CPRDAurumMedical)
