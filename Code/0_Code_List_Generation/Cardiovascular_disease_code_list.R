# Generate code list for cardiovascular disease   
# Author: S Picton & S Wu
# Date created: 2026/05/27
# Date updated: 2026/05/27

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
# 3) 

# Final Outputs:

# 1) Code_Lists/Cardiovascular_Disease/Aurum_Cardiovascular_Disease_20260527.txt : Updated Aurum Cardiovascular disease code list
# 2) Code_Lists/Cardiovascular_Disease/Gold_Cardiovascular_Disease_20260527.txt : Updated Gold Cardiovascular disease code list 




# ================= 1) Set up and load data ====================================

# Clear memory
rm(list = ls())

# Packages
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(writexl)


#  If working in Data Safe Haven - manually install packages using Artifactory:
#   bit, bit64, cli, crayon, dplyr, generics, glue, hms,
#   lifecycle, magrittr, pillar, pkgconfig, purrr, R6, readr, rlang, stringi, 
#   stringr, tibble, tidyr, tidyselect, tzdb, utf8, vctrs, vroom, withr, writexl

# ### For running locally
#
# # Set working directory
#
#  wd <- "/Volumes/ritd-ag-project-rd00qv-jfhay18/" # VPN connection
#  wd <- "//live.rd.ucl.ac.uk/ritd-ag-project-rd00qv-jfhay18/" #Desktop@UCL
# setwd(wd)
#
# # Set input and output paths
#
# path_input <- "Code_Lists/"
# path_output <- "Code_Lists/Cardiovascular_Disease/

### For running in Data Safe Haven

# Set working directory

wd <- "S:/CDSTP_CPRD_25_005368/" 
setwd(wd)

# Set input and output paths

path_input <- "SMI_GLP/Code_Lists/"
path_output <- "SMI_GLP/Code_Lists/Cardiovascular_Disease/"

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


# Read in old CVD code lists, setting all col types to character
# Upload old code lists to DSH 

# Aurum

cardiovasculardisease_aurum_old <- read_delim(
  paste0(wd, path_input, " "),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character()), trim_ws = TRUE)



# Gold

cardiovasculardisease_gold_old <- read_delim(
  paste0(wd, path_input, " "),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character()), trim_ws = TRUE)



# ================= 2) Search for new relevant med codes =======================

# Aurum

aurum_cardiovasculardisease <- cprd_aurum_medical %>%
  # Inclusion - cardiovascular disease related terms 
  
  filter(grepl(paste0("(?i)  
  
  # Ischaemic heart disease 
  
  
  ischaemic heart disease|ischaemic heart disease|cardiovascular disease|CVD|myocardial infarction|
  angina|angina|ischemic heart disease|acute coronary syndrome|heart disease|cardiac disease|
                      coronary artery disease|coronary artery disease|heart bypass|percutaneous coronary intervention|coronary artery bypass",
  
  # Heart failure 
  
  "heart failure|heart failure|congestive cardiac failure|CCF|HF|diastolic heart failure|systolic heart failure",
  
  
  # Other heart disease 
  
  
  "atrial flutter|atrial fibrillation|arrhythmia|cardiomyopathy|valvular heart disease|aortic stenosis|
  aortic sclerosis|mitral regurgitation|mitral stenosis|tricuspid regurgitation|tricuspid stenosis|atrial stenosis|atrial regurgitation",
  
  # Peripheral vascular disease
  
  "peripheral vascular disease|limb ischaemia|ischaemic limb|peripheral vascular disease",
  
  
 # Stroke / cerebrovascular disease 
 
 
   "stroke|ischaemic stroke|haemorrhagic stroke|stroke|cerebrovascular"),
  
   term, ignore.case = TRUE, perl = TRUE)) %>%

# Exclusion 

filter(!grepl(paste0("(?i)
   
   # Unrelated to CVD
   
   	
   normal sinus arrhythmia", "normal sinus arrhythmia|vincent's|	
no anti-cancer treatment|abnormal appendix|oxygen therapy|herpangina|ecg: sinus arrhythmia|
                     high fibre diet|high fibre diet|crutchfield|hemofiltration|bothfirst|pitchfork|hfe|
                     hemochromatosis|hfe|haemochromatosis|crimean|rheumatoid|hypsarrhythmia|peho|hypsarrhythmia|sunstroke|heat stroke|stroke test|
                     stroke volume|stroke volume|juvenile myopathy|mitochondrial myopathy|angina bullosa haemorrhagica|
                     melas|melas|mitoch|kyphoscoliotic|thyrotoxic|rheumatic|gonococcal|duchenne muscular dystrophy|sarcoid|amyloid|carcinoid|mitochondrial cardiomyopathy",
                     
                     
   # Pregnancy
   
   "puerperal|puerperal|puerperium",
   
   # Family history 
   
   "family history|FH:|no fh:|fh|no family history|family history of|family history:|	
family history:",
   
   # Children
   
   "fetal|neonatal|fetal|fetal arrhythmia|paediatric arrhythmia|paediatric|paediatric arterial ischaemic stroke",
   
   # Negations
   
   
   "normal|no myocardial|not required|no ventricular arrhythmia|negative|test negative|excluded|
   not indicated|not indicated|not to have|more time needed|more time needed to decide on heart failure status|
   atrial fibrillation not detected|atrial fibrillation not detected|no h/o: cva/stroke|resolved",
   
   # Process of care 
   
   "recently performed administration|recently performed administration|assessment declined|
   group declined|review declined|monitoring declined|plan declined|programme declined|	
declined to register|declined to register|group declined|risk score|score|screening|risk|
   canadian cardiovascular society|canadian cardiovascular society|grading|screen|education|
   	primary prevention of|primary prevention of|suspected|qof|prevention|no anti-cancer treatment|
   opportunistic|opportunistic|adverse reaction|173-hf|hfq|watchful|exception|exception|3d study|study|new york heart assoc classification heart failure symptoms|blood test"), 
   
 term, ignore.case = TRUE, perl = TRUE)) %>%


# Filter out unrelated angina (not CVD related)

filter(!grepl("ludwig's[-_ ]*angina", term, ignore.case = TRUE, perl =  TRUE)) %>% 

filter(!grepl("ludwig[-_ ]*angina", term, ignore.case = TRUE, perl =  TRUE)) %>% 
  
filter(!grepl("abdominal[-_ ]*angina", term, ignore.case = TRUE, perl =  TRUE)) %>%
  
  filter(!grepl("angina bullosa haemorrhagica[-_ ]*angina", term, ignore.case = TRUE, perl =  TRUE)) %>%
  
  filter(!grepl("streptococcal[-_ ]*angina", term, ignore.case = TRUE, perl =  TRUE)) %>%
   
  

  # Filter out sinus arrhythmia (normal variant)
  filter(!grepl("sinus[-_ ]*arrhythmia", term, ignore.case = TRUE, perl =  TRUE)) %>% 

# Filter out negations

filter(!grepl("atrial fibrillation not detected", term, ignore.case = TRUE, perl =  TRUE)) %>%
 
  
filter(!grepl("angina pectoris not detected on treadmill stress test", term, ignore.case = TRUE, perl =  TRUE)) %>% 
  
filter(!grepl("no h/o: cva/stroke", term, ignore.case = TRUE, perl =  TRUE)) %>%

filter(!grepl("atrial fibrillation resolved", term, ignore.case = TRUE, perl =  TRUE)) %>%

filter(!grepl("heart failure resolved", term, ignore.case = TRUE, perl =  TRUE))



# Gold

gold_cardiovasculardisease <- cprd_gold_medical %>%
  
  # Inclusion - cardiovascular disease related terms 
  
  
  
   # Inclusion - cardiovascular disease related terms 
  
  filter(grepl(paste0("(?i)  
                      
                      # Ischaemic heart disease 
                      
                      
                      ischaemic heart disease|ischaemic heart disease|cardiovascular disease|CVD|myocardial infarction|
                        angina|angina|ischemic heart disease|acute coronary syndrome|heart disease|cardiac disease|
                        coronary artery disease|coronary artery disease|heart bypass|percutaneous coronary intervention|coronary artery bypass",
  
  # Heart failure 
  
  "heart failure|heart failure|congestive cardiac failure|CCF|HF|diastolic heart failure|systolic heart failure",
  
  
  # Other heart disease 
  
  
  "atrial flutter|atrial fibrillation|arrhythmia|cardiomyopathy|valvular heart disease|aortic stenosis|
                        aortic sclerosis|mitral regurgitation|mitral stenosis|tricuspid regurgitation|tricuspid stenosis|atrial stenosis|atrial regurgitation",
  
  # Peripheral vascular disease
  
  "peripheral vascular disease|limb ischaemia|ischaemic limb|peripheral vascular disease",
  
  
 # Stroke / cerebrovascular disease 
 
 
   "stroke|ischaemic stroke|haemorrhagic stroke|stroke|cerebrovascular"),
  
   term, ignore.case = TRUE, perl = TRUE)) %>%

# Exclusion 

filter(!grepl(paste0("(?i)
                      
                      # Unrelated to CVD
                      
                      
                      normal sinus arrhythmia", "normal sinus arrhythmia|vincent's|	
no anti-cancer treatment|abnormal appendix|oxygen therapy|herpangina|ecg: sinus arrhythmia|
                     high fibre diet|high fibre diet|crutchfield|hemofiltration|bothfirst|pitchfork|hfe|
                     hemochromatosis|hfe|haemochromatosis|crimean|rheumatoid|hypsarrhythmia|peho|hypsarrhythmia|sunstroke|heat stroke|stroke test|
                     stroke volume|stroke volume|juvenile myopathy|mitochondrial myopathy|angina bullosa haemorrhagica|
                     melas|melas|mitoch|kyphoscoliotic|thyrotoxic|rheumatic|gonococcal|duchenne muscular dystrophy|sarcoid|amyloid|carcinoid|mitochondrial cardiomyopathy",
                     
                     
   # Pregnancy
   
   "puerperal|puerperal|puerperium",
   
   # Family history 
   
   "family history|FH:|no fh:|fh|no family history|family history of|family history:|	
family history:",
   
   # Children
   
   "fetal|neonatal|fetal|fetal arrhythmia|paediatric arrhythmia|paediatric|paediatric arterial ischaemic stroke",
   
   # Negations
   
   
   "normal|no myocardial|not required|no ventricular arrhythmia|negative|test negative|excluded|
   not indicated|not indicated|not to have|more time needed|more time needed to decide on heart failure status|
   atrial fibrillation not detected|atrial fibrillation not detected|no h/o: cva/stroke|resolved",
   
   # Process of care 
   
   "recently performed administration|recently performed administration|assessment declined|
   group declined|review declined|monitoring declined|plan declined|programme declined|	
declined to register|declined to register|group declined|risk score|score|screening|risk|
   canadian cardiovascular society|canadian cardiovascular society|grading|screen|education|
   	primary prevention of|primary prevention of|suspected|qof|prevention|no anti-cancer treatment|
   opportunistic|opportunistic|adverse reaction|173-hf|hfq|watchful|exception|exception|3d study|study|new york heart assoc classification heart failure symptoms|blood test"), 
   
 term, ignore.case = TRUE, perl = TRUE)) %>%


# Filter out unrelated angina (not CVD related)

filter(!grepl("ludwig's[-_ ]*angina", term, ignore.case = TRUE, perl =  TRUE)) %>% 

filter(!grepl("ludwig[-_ ]*angina", term, ignore.case = TRUE, perl =  TRUE)) %>% 
  
filter(!grepl("abdominal[-_ ]*angina", term, ignore.case = TRUE, perl =  TRUE)) %>%
  
  filter(!grepl("angina bullosa haemorrhagica[-_ ]*angina", term, ignore.case = TRUE, perl =  TRUE)) %>%
  
  filter(!grepl("streptococcal[-_ ]*angina", term, ignore.case = TRUE, perl =  TRUE)) %>%
   
  

  # Filter out sinus arrhythmia (normal variant)
  filter(!grepl("sinus[-_ ]*arrhythmia", term, ignore.case = TRUE, perl =  TRUE)) %>% 

# Filter out negations

filter(!grepl("atrial fibrillation not detected", term, ignore.case = TRUE, perl =  TRUE)) %>%
 
  
filter(!grepl("angina pectoris not detected on treadmill stress test", term, ignore.case = TRUE, perl =  TRUE)) %>% 
  
filter(!grepl("no h/o: cva/stroke", term, ignore.case = TRUE, perl =  TRUE)) %>%

filter(!grepl("atrial fibrillation resolved", term, ignore.case = TRUE, perl =  TRUE)) %>%

filter(!grepl("heart failure resolved", term, ignore.case = TRUE, perl =  TRUE))

  
  
  
  
  
  
  
## Comparing with older codelists

# New codes not in old list

new_aurum <- aurum_cardiovasculardisease %>%
  filter(!medcodeid %in% aurum_cardiovasculardisease_old$medcodeid)

new_gold <- gold_cardiovasculardisease %>%
  filter(!medcode %in% gold_cardiovasculardisease_old$medcode)

# Old codes not in new old list
miss_new_aurum <- aurum_cardiovasculardisease_old %>%
  filter(!medcodeid %in% aurum_cardiovasculardisease$medcodeid)

miss_new_gold <- gold_cardiovasculardisease_old %>%
  filter(!medcode %in% gold_cardiovasculardisease$medcode)


## # ================= 3) Create updated code lists ===============================

# Create updated code lists

# Aurum

cardiovasculardisease_codelist_aurum_new <- aurum_cardiovasculardisease

# Gold

cardiovasculardisease_codelist_gold_new <- gold_cardiovasculardisease

# Save updated code lists

write.table(cardiovasculardisease_codelist_aurum_new,
            file = paste0(wd, path_output, "Aurum_Cardiovasculardisease_codelist_20260601.txt"),
            sep = "\t", row.names = FALSE)

write.table(cardiovasculardisease_codelist_gold_new,
            file = paste0(wd, path_output, "Gold_Cardiovasculardisease_codelist_20260601.txt"),
            sep = "\t", row.names = FALSE)




