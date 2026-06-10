# -----------------------
# Stroke code list
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

# Import Naomi's latest neuro code list (medically reviewed by Ella)

stroke <- read_delim("Code lists/Dementia/NeuroPara from Naomi 23.01.24.txt", 
                                            delim = "\t", escape_double = FALSE, 
                                            col_types = cols(...4 = col_skip()), 
                                            trim_ws = TRUE) %>%
  filter(Umbrella == "Cerebrovascular disease") %>%
  mutate(term = str_replace_all(term, '""', '')) 

stroke_aurum <- stroke %>%
  inner_join(CPRDAurumMedical, by = "term", relationship = "many-to-many") %>%
  filter(!grepl("suspected|newborn", term)) %>%
  distinct()

stroke_gold <- stroke %>%
  inner_join(CPRDGoldMedical, by = "term", relationship = "many-to-many") %>%
  filter(!grepl("suspected|newborn", term)) %>%
  distinct()

remove(stroke)

# Check if there are any additional codes worth including from Elixhauser and Charlson

# Aurum

elix_aurum <- read_delim("Code lists/Comorbidity scores/AurumElixhauser_UCLpharmacoepi_060323.txt", 
                                                      delim = "\t", escape_double = FALSE, 
                                                      col_types = cols(medcodeid = col_character(), 
                                                                       snomedctconceptid = col_skip(), snomedctdescriptionid = col_skip()), 
                                                      trim_ws = TRUE)

charl_aurum <- read_delim("Code lists/Comorbidity scores/AurumCharlson_UCLpharmacoepi_060323.txt", 
                                                  delim = "\t", escape_double = FALSE, 
                                                  col_types = cols(medcodeid = col_character(), 
                                                                   originalreadcode = col_skip(), snomedctconceptid = col_skip(), 
                                                                   snomedctdescriptionid = col_skip(), 
                                                                   readcode = col_skip(), score = col_skip()), 
                                                  trim_ws = TRUE)

elix_aurum_stroke <- elix_aurum %>%
  filter(grepl("(?i)stroke|cerebrovascular accident|transient ischaemic", term)) %>%
  filter(!grepl("(?i)suspected|strokelike|stroke-like", term)) %>%
  rename(elix_term = term) %>%
  select(medcodeid, elix_term) %>%
  anti_join(stroke_aurum, by = "medcodeid") %>%
  rename(term = elix_term)

charl_aurum_stroke <- charl_aurum %>%
  filter(condition == "Cerebrovascular disease")  %>%
  filter(!grepl("(?i)suspected|strokelike|stroke-like", term)) %>%
  rename(charl_term = term) %>%
  select(medcodeid, charl_term) %>%
  anti_join(stroke_aurum, by = "medcodeid") %>%
  rename(term = charl_term)

aurum_stroke_additional_codes <- elix_aurum_stroke %>%
  bind_rows(charl_aurum_stroke) %>%
  distinct() %>%
  mutate(Umbrella = "Stroke")

stroke_aurum <- stroke_aurum %>%
  bind_rows(aurum_stroke_additional_codes) %>%
  distinct()

# check if there are any obvious codes remaining in the code browser

aurum_browser_stroke_codes <- CPRDAurumMedical %>%
  filter(grepl("stroke|cerebrovascular disease|cerebrovascular accident|cva|cerebral infarct|cerebral isch|transient ischaemic attack|subarachnoid h|intracranial haemorrhage|intracranial hemorrhage|
               |intracerebral haemorrhage|intracerebral hemorrhage|brainstem haemorrhage|brainstem hemorrhage|brain stem hemorrhage|brain stem haemorrhage|thalamic haemorrhage|thalamic hemorrhage|
               |subcortical haemorrhage|subcortical hemorrhage|intraventricular haemorrhage|intraventricular hemorrhage|basilar artery occluded|carotid artery occluded|vertebral artery occluded|
               |thrombosis of left|thrombosis of right|embolus of left|embolus of right|h/o: tia|cerebral a. occlusion nos|
               |occlusion of left|occlusion of right", term))

crosscheck <- aurum_browser_stroke_codes %>%
  anti_join(stroke_aurum, by = "medcodeid") %>%
  filter(!grepl("fh:|family history|heat stroke|sun|prevention|risk|score|no h/o|negative|scale|bcva|cvag|impending|written|mcvay|neonatal|fetus|iliac vein|fallopian|birth|stroke-like|strokelike|suspected|femoral|retinal", term)) %>%
  mutate(Umbrella = "Stroke")

stroke_aurum <- stroke_aurum %>%
  bind_rows(crosscheck) %>%
  filter(!grepl("annual stroke/cva blood test|fetus|score|atrial appendage|popliteal artery|palsy", term)) %>%
  distinct()

remove(elix_aurum_stroke, charl_aurum_stroke, aurum_stroke_additional_codes, elix_aurum, charl_aurum, crosscheck, aurum_browser_stroke_codes)

# Gold

elix_gold <- read_delim("Code lists/Comorbidity scores/GoldElixhauser_UCLpharmacoepi_060323.txt", 
                                                   delim = "\t", escape_double = FALSE, 
                                                   col_types = cols(medcode = col_character(), 
                                                                    readcode = col_skip()), trim_ws = TRUE) %>%
  rename(term = readterm)

charl_gold <- read_delim("Code lists/Comorbidity scores/GoldCharlson_UCLpharmacoepi_060323.txt", 
                                                 delim = "\t", escape_double = FALSE, 
                                                 col_types = cols(medcode = col_character(), 
                                                                  readcode = col_skip(), score = col_skip()), 
                                                 trim_ws = TRUE) %>%
  rename(term = readterm)

elix_gold_stroke <- elix_gold %>%
  filter(grepl("(?i)stroke|cerebrovascular accident|transient ischaemic", term)) %>%
  filter(!grepl("(?i)suspected|strokelike|stroke-like", term)) %>%
  rename(elix_term = term) %>%
  select(medcode, elix_term) %>%
  anti_join(stroke_gold, by = "medcode") %>%
  rename(term = elix_term)

charl_gold_stroke <- charl_gold %>%
  filter(condition == "Cerebrovascular disease")  %>%
  filter(!grepl("(?i)suspected|strokelike|stroke-like", term)) %>%
  rename(charl_term = term) %>%
  select(medcode, charl_term) %>%
  anti_join(stroke_gold, by = "medcode") %>%
  filter(!grepl("parkinsonism", charl_term)) %>%
  rename(term = charl_term)

gold_stroke_additional_codes <- elix_gold_stroke %>%
  bind_rows(charl_gold_stroke) %>%
  distinct() %>%
  mutate(Umbrella = "Stroke")

stroke_gold <- stroke_gold %>%
  bind_rows(gold_stroke_additional_codes) %>%
  distinct()

# check if there are any obvious codes remaining in the code browser

gold_browser_stroke_codes <- CPRDGoldMedical %>%
  filter(grepl("stroke|cerebrovascular disease|cerebrovascular accident|cva|cerebral infarct|cerebral isch|transient ischaemic attack|subarachnoid h|intracranial haemorrhage|intracranial hemorrhage|
               |intracerebral haemorrhage|intracerebral hemorrhage|brainstem haemorrhage|brainstem hemorrhage|brain stem hemorrhage|brain stem haemorrhage|thalamic haemorrhage|thalamic hemorrhage|
               |subcortical haemorrhage|subcortical hemorrhage|intraventricular haemorrhage|intraventricular hemorrhage|basilar artery occluded|carotid artery occluded|vertebral artery occluded|
               |thrombosis of left|thrombosis of right|embolus of left|embolus of right|h/o: tia|cerebral a. occlusion nos|
               |occlusion of left|occlusion of right", term))

crosscheck2 <- gold_browser_stroke_codes %>%
  anti_join(stroke_gold, by = "medcode") %>%
  filter(!grepl("fh:|family history|heat stroke|sun|prevention|risk|score|no h/o|negative|scale|bcva|cvag|impending|written|mcvay|neonatal|fetus|iliac vein|fallopian|birth|stroke-like|strokelike|suspected|femoral|retinal", term)) %>%
  mutate(Umbrella = "Stroke")

stroke_gold <- stroke_gold %>%
  bind_rows(crosscheck2) %>%
  filter(!grepl("annual stroke/cva blood test|fetus|score|atrial appendage|popliteal artery|palsy", term)) %>%
  distinct()

remove(elix_gold_stroke, charl_gold_stroke, gold_stroke_additional_codes, elix_gold, charl_gold, crosscheck2, gold_browser_stroke_codes)

# save as text file
write.table(stroke_gold, file = "Code lists/Stroke/stroke_gold_220424.txt",
            sep = "\t", row.names = FALSE)

write.table(stroke_aurum, file = "Code lists/Stroke/stroke_aurum_220424.txt",
            sep = "\t", row.names = FALSE)

remove(stroke_gold, stroke_aurum, CPRDGoldMedical, CPRDAurumMedical)
