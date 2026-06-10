# -----------------------
# Myocardial infarction code list
# -----------------------
# CPRD 2023 data
# Last run: 22/04/24

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


# Define codes
aurum_mi <- CPRDAurumMedical %>%
  filter(grepl("myocardial infarct|myocard infarct|heart attack|post infarct|acute q wave|acute q-wave|acute non-q wave|stemi |anterolateral infarction|anteroseptal infarction|comp fol ac mi|acut myocardal infarctn|
               |inferolateral infarction|inferoposterial infarction|subendocardial infarction|septal infarction|inferoposterior infarction|atrial infarction|lateral infarction|
               |myocard infarction|myocar infarction|anteroapical infarction|attack - heart|papillary muscle infarction", term) | medcodeid == "1790731000006117") %>%
  filter(!grepl("fh:|fh |family history|prevention|risk|score|no history|negative|scale|no myocardial infarction|not|suspected|anxiety", term))



gold_mi <- CPRDGoldMedical %>%
  filter(grepl("myocardial infarct|myocard infarct|heart attack|post infarct|acute q wave|acute q-wave|acute non-q wave|stemi |anterolateral infarction|anteroseptal infarction|comp fol ac mi|acut myocardal infarctn|
               |inferolateral infarction|inferoposterial infarction|subendocardial infarction|septal infarction|inferoposterior infarction|atrial infarction|lateral infarction|
               |myocard infarction|myocar infarction|anteroapical infarction|attack - heart|papillary muscle infarction", term)) %>%
  filter(!grepl("fh:|fh |family history|prevention|risk|score|no history|negative|scale|no myocardial infarction|not|suspected|anxiety", term))


# save as text files
write.table(gold_mi, file = "Code lists/Myocardial infarction/myocardialinfarction_gold_220424.txt",
            sep = "\t", row.names = FALSE)

write.table(aurum_mi, file = "Code lists/Myocardial infarction/myocardialinfarction_aurum_220424.txt",
            sep = "\t", row.names = FALSE)

remove(CPRDGoldMedical, CPRDAurumMedical, gold_mi, aurum_mi)
