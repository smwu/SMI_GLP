# Generate code list for dyslipidaemia  
# Author: S Picton & S Wu
# Date created: 2026/04/10
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
# 3) Code_Lists/Dyslipidaemia/Old/Aurum_Dyslipidaemia_20230724_Alvin.txt : Old Aurum dyslipidaemia code list 
# 4) Code_Lists/Dyslipidaemia/Old/Gold_Dyslipidaemia_20230724_Alvin.txt : Old Gold dyslpidaemia code list 
#
# Final Outputs:

# 1) Code_Lists/Dyslipidaemia/Aurum_Dyslipidaemia_codelist_20260515.txt : Updated Aurum dyslipidaemia code list 
# 2) Code_Lists/Dyslipidaemia/Gold_Dyslipidaemia_codelist_20260515.txt : Updated Gold dyslipidaemia code list 

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
# path_output <- "Code_Lists/Dyslipidaemia/"


### For running in Data Safe Haven

# Set working directory

wd <- "S:/CDSTP_CPRD_25_005368/" 
setwd(wd)

# Set input and output paths

path_input <- "SMI_GLP/Code_Lists/"
path_output <- "SMI_GLP/Code_Lists/Dyslipidaemia/"



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


# Read in old Dyslipidaemia code lists, setting all col types to character

# Aurum

dyslipidaemia_aurum_old <- read_delim(
  paste0(wd, path_input, "Dyslipidaemia/Old/Aurum_Dyslipidaemia_20230724_Alvin.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character()), trim_ws = TRUE)



# Gold

dyslipidaemia_gold_old <- read_delim(
  paste0(wd, path_input, "Dyslipidaemia/Old/Gold_Dyslipidaemia_20230724_Alvin.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character()), trim_ws = TRUE)

# ================= 2) Search for new relevant med codes =======================

# Aurum

aurum_dyslipidaemia <- cprd_aurum_medical %>%
  # Inclusion - dyslipidaemia  related terms 
  
  filter(grepl(paste0("(?i)lipidaemia|lipidemia|hypercholesterolaemia|hypertriglycerideamia|raised cholesterol|
                      hypertriglyceridaemia|high cholesterol|xanthoma|Primary hypercholesterolemia|
                      Primary hypertriglyceridaemia|Hypertriglyceridemia|Dyslipidemia|Hypertriglyceridaemia|
                      cholesterol raised|cholesterol very high|cholesterol borderline|
                      |triglycerides borderline|triglycerides raised|cholesterol raised|
                      Serum cholesterol raised|Disorder of cholesterol metabolism|
                      Hyperlipidemia|Low cholesterol diet|Lipids abnormal|
                      Pure hyperglyceridaemia|Cholesterol reduction programme|
                      Hyperlipidemia|Triglyceride-lowering diet|Pure hypercholesterolemia|
                      Mixed hyperlipidemia|Dyslipidemia|Familial hypercholesterolemia",
                      
                      "Lipids abnormal|Lipid lowering|Low density lipoproteinaemia|
                      hyperglyceridaemia|Hypercholesterolemia|cholesterol clinic|
                      Hyperbetalipoproteinaemia|Hypo-beta-lipoproteinaemia|
                      	Pure hyperglyceridaemia|Abetalipoproteinaemia|Chemically induced lipidosis|
                      	lipoproteinaemia|hyperbetalipoproteinaemia|Hypo-alpha-lipoproteinaemia|
                      	hyperlipoproteinaemia|Lipid disorder|Cholesterol-lowering|
                      	Dyslipidemia|High blood cholesterol/triglycerides|
                      	cholesterol clinic|hyperlipoproteinaemia|hyperglyceridaemia|Lipoprotein electrophoresis abnormal|Low density lipoprotein receptor disorder|
                      Dysbetalipoproteinaemia|High density lipoprotein deficiency",
                      "Mixed hyperlipidaemia|LDLR|Adverse reaction to Cholesterol And Simvastatin|
                      Low density lipoprotein receptor mutation|Low density lipoprotein receptor disorder",
                      "raised blood lipids|lipids borderline|lipids high|lipid clinic|Disorder of lipid metabolism|
                      lipid management|Lipid-lowering drug|lipid metabolism|Lipid disorder|Lipid-lowering|
                      hyperlipidaemia|Lipid disorder treatment|Referral to general practitioner for lipid management|
                      Referral to GP - lipid management|Disorder of lipid and lipoprotein metabolism|
                      Dysbetalipoproteinaemia|Low density lipoprotein receptor mutation|
                      High density lipoprotein deficiency",
                      "Hypocholesterolaemia|Hypocholesterolemia|Hypocholesterolaemia|
                      hypoalphalipoproteinaemia|Hypocholesteraemia|Hypoalphalipoproteinaemia|
                      Lipid disorder treatment stopped|Lipoprotein deficiency|Chemically induced lipidosis|
                      High density lipoprotein deficiency|Mixed hyperlipidaemia|
                      Cholesterol embolus syndrome|Referral to GP - lipid management|
                      Chemically induced lipidosis|H/O: raised blood lipids|antihyperlipidaemic|
                      cholesterol reduction program |
                      Atherogenic lipoprotein phenotype|A-beta-lipoproteinaemia|Lipoprotein electroph.abnormal|
                      disorders of lipoprotein metabolism"),
               term, ignore.case = TRUE)) %>% 
     
  
  
  # Exclusion     

  # Not related to dyslipidaemia 
  
  filter(!grepl(paste0("(?i)antiphospholipid|sphingolipidosis|storage disease|lipidoses|glycolipid|
  carcinoma|phospholipid|tumour|androblastoma|lipidosis|	
  adverse reaction|mucolipidosis|storage|intravenous|
  transfer|carcinoma|fibroxanthoma|malignant fibroxanthoma|intralipid|intralipid 20 %|vitlipid|xanthomax|
  adverse reaction to amphotericin|eosinophilic|avene|tear layer|tear deficiency|thealipid|thealipid|fucibet|
                       lipid fractionation",
  
  
  # Related to family history 
  
  "family history|fh:|family history:|family history|	
  family history of",
  
  # Related to children / infants 
  
  "neonatal",
  
  
  # Negations
  
  "not indicated|screening declined|within reference range|not indicated|therapy not indicated|lipids normal",
  
  # Process of care 
  
  "bloods sent|measurement of|fasting blood lipids|plasma lipids|screening|risk assessment|
  test request|screen|lipid level|simon broome|dutch|framingham|lipid panel|lipid profile|mucolipidosis|
  fasting lipid profile|test request|blood sent|lipids level|fasting lipids|",
  "genetic test|assessment for familial hypercholesterolaemia"),
  
  term, perl = TRUE)) %>%
    
  # Filter remaining family history code   
    
  filter(!grepl("no fh of[-_ ]*hypercholesterolaemia", term, ignore.case = TRUE, perl =  TRUE)) 

# Aurum terms to add by medcodeid: Dysbetalipoproteinaemia
aurum_dyslipidaemia <- bind_rows(
  aurum_dyslipidaemia, 
  cprd_aurum_medical %>% filter(medcodeid %in% c("6611941000006113")))


# Gold

gold_dyslipidaemia <- cprd_gold_medical %>%
  
  # Inclusion - dyslipidaemia  related terms 
  
  filter(grepl(paste0("(?i)lipidaemia|lipidemia|hypercholesterolaemia|hypertriglycerideamia|raised cholesterol|
                      hypertriglyceridaemia|high cholesterol|xanthoma|Primary hypercholesterolemia|
                      Primary hypertriglyceridaemia|Hypertriglyceridemia|Dyslipidemia|Hypertriglyceridaemia|
                      cholesterol raised|cholesterol very high|cholesterol borderline|
                      |triglycerides borderline|triglycerides raised|cholesterol raised|
                      Serum cholesterol raised|Disorder of cholesterol metabolism|
                      Hyperlipidemia|Low cholesterol diet|Lipids abnormal|
                      Pure hyperglyceridaemia|Cholesterol reduction programme|
                      Hyperlipidemia|Triglyceride-lowering diet|Pure hypercholesterolemia|
                      Mixed hyperlipidemia|Dyslipidemia|Familial hypercholesterolemia",
                      
                      "Lipids abnormal|Lipid lowering|Low density lipoproteinaemia|
                      hyperglyceridaemia|Hypercholesterolemia|cholesterol clinic|
                      Hyperbetalipoproteinaemia|Hypo-beta-lipoproteinaemia|
                      	Pure hyperglyceridaemia|Abetalipoproteinaemia|Chemically induced lipidosis|
                      	lipoproteinaemia|hyperbetalipoproteinaemia|Hypo-alpha-lipoproteinaemia|
                      	hyperlipoproteinaemia|Lipid disorder|Cholesterol-lowering|
                      	Dyslipidemia|High blood cholesterol/triglycerides|
                      	cholesterol clinic|hyperlipoproteinaemia|hyperglyceridaemia|Lipoprotein electrophoresis abnormal|Low density lipoprotein receptor disorder|
                      Dysbetalipoproteinaemia|High density lipoprotein deficiency",
                      "Mixed hyperlipidaemia|LDLR|Adverse reaction to Cholesterol And Simvastatin|
                      Low density lipoprotein receptor mutation|Low density lipoprotein receptor disorder",
                      "raised blood lipids|lipids borderline|lipids high|lipid clinic|Disorder of lipid metabolism|
                      lipid management|Lipid-lowering drug|lipid metabolism|Lipid disorder|Lipid-lowering|
                      hyperlipidaemia|Lipid disorder treatment|Referral to general practitioner for lipid management|
                      Referral to GP - lipid management|Disorder of lipid and lipoprotein metabolism|
                      Dysbetalipoproteinaemia|Low density lipoprotein receptor mutation|
                      High density lipoprotein deficiency",
                      "Hypocholesterolaemia|Hypocholesterolemia|Hypocholesterolaemia|
                      hypoalphalipoproteinaemia|Hypocholesteraemia|Hypoalphalipoproteinaemia|
                      Lipid disorder treatment stopped|Lipoprotein deficiency|Chemically induced lipidosis|
                      High density lipoprotein deficiency|Mixed hyperlipidaemia|
                      Cholesterol embolus syndrome|Referral to GP - lipid management|
                      Chemically induced lipidosis|H/O: raised blood lipids|antihyperlipidaemic|
                      cholesterol reduction program |
                      Atherogenic lipoprotein phenotype|A-beta-lipoproteinaemia|Lipoprotein electroph.abnormal|
                      disorders of lipoprotein metabolism"),
               term, ignore.case = TRUE)) %>% 
  
  
  
  # Exclusion     
  
  # Not related to dyslipidaemia 
  
  filter(!grepl(paste0("(?i)antiphospholipid|sphingolipidosis|storage disease|lipidoses|glycolipid|
  carcinoma|phospholipid|tumour|androblastoma|lipidosis|	
  adverse reaction|mucolipidosis|storage|intravenous|
  transfer|carcinoma|fibroxanthoma|malignant fibroxanthoma|intralipid|intralipid 20 %|vitlipid|xanthomax|
  adverse reaction to amphotericin|eosinophilic|avene|tear layer|tear deficiency|thealipid|thealipid|fucibet|
                       lipid fractionation",
                       
                       
                       # Related to family history 
                       
                       "family history|fh:|family history:|family history|	
  family history of",
                       
                       # Related to children / infants 
                       
                       "neonatal",
                       
                       
                       # Negations
                       
                       "not indicated|screening declined|within reference range|not indicated|therapy not indicated|lipids normal",
                       
                       # Process of care 
                       
                       "bloods sent|measurement of|fasting blood lipids|plasma lipids|screening|risk assessment|
  test request|screen|lipid level|simon broome|dutch|framingham|lipid panel|lipid profile|mucolipidosis|
  fasting lipid profile|test request|blood sent|lipids level|fasting lipids|",
                       "genetic test|assessment for familial hypercholesterolaemia"),
                
                term, perl = TRUE)) %>%
  
  # Filter remaining family history code   
  
  filter(!grepl("no fh of[-_ ]*hypercholesterolaemia", term, ignore.case = TRUE, perl =  TRUE))                   
               
# Gold terms to add by medcode: Atherogenic lipoprotein phenotype
gold_dyslipidaemia <- bind_rows(gold_dyslipidaemia, 
                                cprd_gold_medical %>% filter(medcode %in% c("58637")))


## Comparing with older codelists

# New codes not in old list

new_aurum <- aurum_dyslipidaemia %>%
  filter(!medcodeid %in% dyslipidaemia_aurum_old$medcodeid)

new_gold <- gold_dyslipidaemia %>%
  filter(!medcode %in% dyslipidaemia_gold_old$medcode)

# Old codes not in new old list

miss_new_aurum <- dyslipidaemia_aurum_old %>%
  filter(!medcodeid %in% aurum_dyslipidaemia$medcodeid)

miss_new_gold <- dyslipidaemia_gold_old %>%
  filter(!medcode %in% gold_dyslipidaemia$medcode)


### ================= 3) Create updated code lists ===============================

# Create updated code lists

# Aurum
dyslipidaemia_codelist_aurum_new <- aurum_dyslipidaemia

# Gold
dyslipidaemia_codelist_gold_new <- gold_dyslipidaemia

# Save updated code lists

write.table(dyslipidaemia_codelist_aurum_new,
            file = paste0(wd, path_output, "Aurum_Dyslipidaemia_codelist_20260515.txt"),
            sep = "\t", row.names = FALSE)

write.table(dyslipidaemia_codelist_gold_new,
            file = paste0(wd, path_output, "Gold_Dyslipidaemia_codelist_20260515.txt"),
            sep = "\t", row.names = FALSE)


