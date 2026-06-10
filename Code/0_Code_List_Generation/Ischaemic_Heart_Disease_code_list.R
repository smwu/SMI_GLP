# Generate code list for ischaemic heart disease (comprising angina, MI, cardiac revascularisation, plus IHD codes)
# Author: S Picton & S Wu
# Date created: 2026/06/02
# Date updated: 2026/06/05

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
# 3) Code_Lists/Angina/Aurum_Angina_codelist_20260602.txt : Aurum Angina code list
# 4) Code_Lists/Angina/Gold_Angina_codelist_20260602.txt : Gold Angina code list 
# 5) Code_Lists/Coronary_Revascularisation_Procedures/Aurum_Coronary_Revascularisation_Procedures_codelist_20260603.txt : Aurum coronary revascularisation code list 
# 6) Code_Lists/Coronary_Revascularisation_Procedures/Gold_Coronary_Revascularisation_Procedures_Codelist_20260603.txt : Gold Coronary revascularisation code list 
# 7) Code_Lists/Myocardial_Infarction/Aurum_Myocardial_Infarct_codelist_20260602.txt : Aurum MI code list 
# 8) Code_Lists/Myocardial_Infarction/Gold_Myocardial_Infarct_codelist_20260602.txt : Gold MI code list 

# Final Outputs:

# 1) Code_Lists/Ischaemic_Heart_Disease/Aurum_Ischaemic_Heart_Disease_20260605.txt : Aurum Ischaemic Heart Disease codelist 
# 2) Code_Lists/Ischaemic_Heart_Disease/Gold_Ischaemic_Heart_Disease_20260605.txt : Gold Ischaemic Heart Disease codelist 
# 3) Code_Lists/Ischaemic_Heart_Disease/Combined_Ischaemic_Heart_Disease_Code_List_20260605.txt : Combined IHD code list


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
# path_output <- "Code_Lists/Ischaemic_Heart_Disease/

### For running in Data Safe Haven

# Set working directory

wd <- "S:/CDSTP_CPRD_25_005368/" 
setwd(wd)

# Set input and output paths

path_input <- "SMI_GLP/Code_Lists/"
path_output <- "SMI_GLP/Code_Lists/Ischaemic_Heart_Disease/"

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


# Read in angina, MI, cardiac revascularisation code lists - setting all col types to character
# Switch medcodeid to medcode for Gold code lists 

# Aurum angina

aurum_angina <- read_delim(
  paste0(wd, path_input, "Angina/Aurum_Angina_codelist_20260602.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character()), trim_ws = TRUE)

# Gold angina

gold_angina <- read_delim(
  paste0(wd, path_input, "Angina/Gold_Angina_codelist_20260602.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character()), trim_ws = TRUE)

# Aurum coronary revascularisation

aurum_coronary_revascularisation <- read_delim(
  paste0(wd, path_input, "Coronary_Revascularisation_Procedures/Aurum_Coronary_Revascularisation_Procedures_codelist_20260603.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character()), trim_ws = TRUE)

# Gold coronary revascularisation

gold_coronary_revascularisation <- read_delim(
  paste0(wd, path_input, "Coronary_Revascularisation_Procedures/Gold_Coronary_Revascularisation_Procedures_Codelist_20260603.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character()), trim_ws = TRUE)

# Aurum MI


aurum_MI <- read_delim(
  paste0(wd, path_input, "Myocardial_Infarction/Aurum_Myocardial_Infarct_codelist_20260602.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character()), trim_ws = TRUE)


# Gold MI

gold_MI <- read_delim(
  paste0(wd, path_input, "Myocardial_Infarction/Gold_Myocardial_Infarct_codelist_20260602.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character()), trim_ws = TRUE)

# ================= 2) Search for new relevant med codes =======================

# Aurum

aurum_ischaemic_heart_disease <- cprd_aurum_medical %>%
  # Inclusion -  ischaemic heart disease
  
  filter(grepl(paste0("(?i)  
  
  # Ischaemic heart disease 
  
  
  ischaemic heart disease|ischaemic heart disease|cardiovascular disease|CVD|ischemic heart disease|acute coronary syndrome|heart disease|cardiac disease|
                      coronary artery disease|coronary artery disease|coronary vessel disease|myocardial ischaemia|coronary microvascular disease|
                      coronary thrombosis|coronary thrombosis|thrombosis - coronary|subendocardial ischaemia|resting ischaemia|
                      coronary sclerosis|coronary sclerosis|ischaemic cardiomyopathy|myocardial ischemia|ischemic myocardial"),
               
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
                       
                       "valvular heart disease|congenital heart disease|beriberi heart disease|hyperkinetic heart disease
                       cyanotic heart disease|cyanotic heart disease|hypertensive heart|pulmonary heart|cerebrovascular disease|pulmonary|valvular heart disease|	
acute rh. heart disease|acute rh. heart disease|syphilitic coronary artery disease|congestive heart disease|hypertensive cardiovascular disease", 
                       
                       
                       # Pregnancy
                       
                       "puerperal|puerperal|puerperium|other cardiovascular disease in pregnancy/childb/puerp|other cardiovascular disease in pregnancy/childb/puerp nos|other cardiovascular disease in pregnancy - baby delivered|other cardiovascular disease in pregnancy - baby delivered|
                       cardiac disease in pregnancy|cardiac disease in pregnancy|heart disease during pregnancy|
                       other cardiovascular diseases in pregnancy/childbirth/puerp|other cardiovascular diseases in pregnancy/childbirth/puerp",
                       
                       # Family history 
                       
                       "family history|FH:|no fh:|fh|no family history|family history of|family history:|	
family history:",
                       
                       # Children
                       
                       "fetal|neonatal|fetal|fetal arrhythmia|paediatric arrhythmia|paediatric|paediatric arterial ischaemic stroke|	
myocardial ischaemia of newborn|myocardial ischaemia of newborn",
                       
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
   opportunistic|opportunistic|adverse reaction|173-hf|hfq|watchful|exception|exception|3d study|study|new york heart assoc classification heart failure symptoms|blood test|
                       invite|invitation|exempted|monitor|registration for access to online heart disease self-management application|excepted from"), 
                
                term, ignore.case = TRUE, perl = TRUE))    %>%


# Filter out hyperkinetic and valvular heart disease & other heart disease


filter(!grepl(paste0("(?i)valvular heart disease|hyperkinetic heart disease|acute rh. heart disease|heart disease nos|heart disease nos|other forms of heart disease|other specified heart disease|
                       other ill-defined heart disease nos|other ill-defined heart disease nos|[x]other ill-defined heart diseases|other ill-defined heart disease|
                     	other cardiovascular diseases in pregnancy/childbirth/puerp|other cardiovascular diseases in pregnancy/childbirth/puerp|myocardial ischaemia of newborn|
                     preg.+ cardiovascular disease|preg.+ cardiovascular disease|chronic heart disease|hypertensive cardiovascular disease"),
              term, ignore.case = TRUE, perl = TRUE)) %>%
  
  # Filter out non specific "heart disease/s" codes
  
  filter(!grepl("^heart disease$", term, ignore.case = TRUE)) %>%
  filter(!grepl("^acute heart disease$", term, ignore.case = TRUE)) %>%
  filter(!grepl("^chronic heart disease$", term, ignore.case = TRUE)) %>%
  
  filter(!grepl("^heart diseases$", term, ignore.case = TRUE))

# Gold 

gold_ischaemic_heart_disease <- cprd_gold_medical %>%
  # Inclusion -  ischaemic heart disease
  
  filter(grepl(paste0("(?i)  
  
  # Ischaemic heart disease 
  
  
  ischaemic heart disease|ischaemic heart disease|cardiovascular disease|CVD|ischemic heart disease|acute coronary syndrome|heart disease|cardiac disease|
                      coronary artery disease|coronary artery disease|coronary vessel disease|myocardial ischaemia|coronary microvascular disease|
                      coronary thrombosis|coronary thrombosis|thrombosis - coronary|subendocardial ischaemia|resting ischaemia|
                      coronary sclerosis|coronary sclerosis|ischaemic cardiomyopathy|myocardial ischemia|ischemic myocardial"),
               
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
                       
                       "valvular heart disease|congenital heart disease|beriberi heart disease|hyperkinetic heart disease
                       cyanotic heart disease|cyanotic heart disease|hypertensive heart|pulmonary heart|cerebrovascular disease|pulmonary|valvular heart disease|	
acute rh. heart disease|acute rh. heart disease|syphilitic coronary artery disease|congestive heart disease|hypertensive cardiovascular disease", 
                       
                       
                       # Pregnancy
                       
                       "puerperal|puerperal|puerperium|other cardiovascular disease in pregnancy/childb/puerp|other cardiovascular disease in pregnancy/childb/puerp nos|other cardiovascular disease in pregnancy - baby delivered|other cardiovascular disease in pregnancy - baby delivered|
                       cardiac disease in pregnancy|cardiac disease in pregnancy|heart disease during pregnancy|
                       other cardiovascular diseases in pregnancy/childbirth/puerp|other cardiovascular diseases in pregnancy/childbirth/puerp",
                       
                       # Family history 
                       
                       "family history|FH:|no fh:|fh|no family history|family history of|family history:|	
family history:",
                       
                       # Children
                       
                       "fetal|neonatal|fetal|fetal arrhythmia|paediatric arrhythmia|paediatric|paediatric arterial ischaemic stroke|	
myocardial ischaemia of newborn|myocardial ischaemia of newborn",
                       
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
   opportunistic|opportunistic|adverse reaction|173-hf|hfq|watchful|exception|exception|3d study|study|new york heart assoc classification heart failure symptoms|blood test|
                       invite|invitation|exempted|monitor|registration for access to online heart disease self-management application|excepted from"), 
                
                term, ignore.case = TRUE, perl = TRUE))  %>%
  
  
  # Filter out hyperkinetic and valvular heart disease / other heart disease 
  
  
  filter(!grepl(paste0("(?i)valvular heart disease|hyperkinetic heart disease|acute rh. heart disease|
                       heart disease nos|heart disease nos|other forms of heart disease|other specified heart disease|
                       other ill-defined heart disease nos|other ill-defined heart disease nos|other ill-defined heart disease|	
other cardiovascular diseases in pregnancy/childbirth/puerp|other cardiovascular diseases in pregnancy/childbirth/puerp|myocardial ischaemia of newborn|
                       preg.+ cardiovascular disease|preg.+ cardiovascular disease|chronic heart disease|hypertensive cardiovascular disease"),
  term, ignore.case = TRUE, perl = TRUE)) %>%
  
  # Filter out non specific "heart disease/s" codes
  
  filter(!grepl("^heart disease$", term, ignore.case = TRUE)) %>%
  filter(!grepl("^acute heart disease$", term, ignore.case = TRUE)) %>%
  filter(!grepl("^chronic heart disease$", term, ignore.case = TRUE)) %>%
  
  filter(!grepl("^heart diseases$", term, ignore.case = TRUE))


# Combine angina, MI, coronary revascularisation, IHD into combined table. Keep column headings 





# Combine separate tables into a list 

dfs <- list(
  aurum_angina = aurum_angina,
  aurum_coronary_revascularisation = aurum_coronary_revascularisation,
  aurum_MI = aurum_MI,
  aurum_ischaemic_heart_disease = aurum_ischaemic_heart_disease,
  gold_angina = gold_angina,
  gold_coronary_revascularisation = gold_coronary_revascularisation,
  gold_MI = gold_MI,
  gold_ischaemic_heart_disease = gold_ischaemic_heart_disease
)

#  set all col types to character 

dfs <- lapply(dfs, function(df) {
  mutate(df, across(everything(), as.character)) 
  
})

# Create combined IHD code list into new table 

combined_IHD_codes <- bind_rows(dfs, .id = "source_table")



# ================= 3) Create updated code lists ===============================

 

# Save updated code lists - Aurum IHD, Gold IHD, plus combined angina/coronary revascularisation/MI/IHD 

write.table(aurum_ischaemic_heart_disease,
            file = paste0(wd, path_output, "Aurum_Ischaemic_Heart_Disease_20260605.txt"),
            sep = "\t", row.names = FALSE)

write.table(gold_ischaemic_heart_disease,
            file = paste0(wd, path_output, "Gold_Ischaemic_Heart_Disease_20260605.txt"),
            sep = "\t", row.names = FALSE)

write.table(combined_IHD_codes,
            file = paste0(wd, path_output, "Combined_Ischaemic_Heart_Disease_Code_List_20260605.txt"),
            sep = "\t", row.names = FALSE)
