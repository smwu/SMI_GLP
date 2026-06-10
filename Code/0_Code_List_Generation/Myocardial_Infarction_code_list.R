# Generate code list for myocardial infarction 

# Author: S Picton & S Wu
# Date created: 2026/06/02
# Date updated: 2026/06/02

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

# 1) Code_Lists/Myocardial_Infarction/Aurum_Myocardial_Infarction_20260602.txt : Updated Aurum Myocardial Infarction code list
# 2) Code_Lists/Myocardial_Infarction/Gold_Myocardial_Infarction_20260602.txt : Updated Gold Myocardial Infarction code list 




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
# path_output <- "Code_Lists/Myocardial_Infarction/

### For running in Data Safe Haven

# Set working directory

wd <- "S:/CDSTP_CPRD_25_005368/" 
setwd(wd)

# Set input and output paths

path_input <- "SMI_GLP/Code_Lists/"
path_output <- "SMI_GLP/Code_Lists/Myocardial_Infarction/"

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


# No previous code list on DSH - use Exeter Github CPRD code lists for comparison


# ================= 2) Search for new relevant med codes =======================

# Aurum

aurum_myocardial_infarction <- cprd_aurum_medical %>%
  # Inclusion - myocardial infarction related terms 
  
  filter(grepl(paste0("(?i)  
  
  myocardial infarction|myocardial infarction|heart attack|myocardial infarct|cardiac thrombosis|
    coronary artery thrombosis|coronary artery thrombosis|acute stemi|N-STEMI"),
               
               term, ignore.case = TRUE, perl = TRUE)) %>%
  
  # Exclusion 
  
  filter(!grepl(paste0("(?i)
   
   # Unrelated to Myocardial infarction - other organs
   
   hepatic|hepatic infarction|infarction of ovary|infarction of prostate|infarction of spleen|renal infarction|systemic lupus erythematosus|infarction of fallopian|	
systemic sclerosis|systemic sclerosis|cerebellar infarction|infarction of breast|thyroid haemorrhage|lacunar infarction|
                       	
cerebral infarction|cerebral infarction|lymph node infarction|lymph vessel infarction| placental infarction|
                       precerebral|precerebral|brainstem infarct|pulmonary infarct|infarction of basal ganglia|
                       	multi-infarct dementia|multi-infarct dementia|infarct of liver|infarct of lung|multi-infarct state|systemic|	
cerebrl infarctn|cerebrl infarctn|cerebral infarct|infarction of liver|bowel infarction|intestinal infarction|
                       splenic infarction|splenic infarction|	
optic nerve infarction|optic nerve infarction|multiple infarcts|testicular infarction|embolic infarction|
                       anaemic infarct|anaemic infarct|	
infarction of kidney|infarction of kidney|renal infarct|thyroid infarction|infarction - cerebral|	
mesenteric infarction|",
      
# Pregnancy                  
                       	
"placental infarct|placental infarction|placenta infarcted|placental infarct",
   
  

  # Unrelated diseases
  
  "creation of portosystemic shunt nec|systemic bartonellosis|systemic cryptococcosis|myelopathy|glaucoma|
  systemic mycoses|systemic mycoses|agent poisoning|renal disorders|idiopathic livedo reticularis|	
systemic lupus erythematosus|systemic lupus erythematosus|systemic disease|systemic vasculitis|panhypopituitarism|
systemic mycosis|systemic mycosis|systemic pulmonary collateral|	
systemic inflammatory response|systemic inflammatory response|gonococcal|	
periventricular hemorrhagic|periventricular hemorrhagic|systemic mastocytosis|lymphoma|cerebral autosomal dominant|
cerebral palsy|cerebral palsy" ,
  

# FH 

"family history|family history|fh|fh myocardial infarction",
  
  # Children
  
  
  # Negations
  
  "no myocardial infarction|no fh:|ecg: no myocardial infarction|not resulting in myocardial infarction|myocardial infarction aborted",
  
  # Process of care 
  
  "agents|drug reaction| couple and sex therapy|adverse reaction to|fear of having a heart attack|anxiety about having a heart attack|
radioisotope scan for myocardial infarction|radioisotope scan for myocardial infarction"), 
  
  
  term, ignore.case= TRUE, perl = TRUE)) %>%
  
  # Filter out no history of mI
  
  filter(!grepl("no[-_ ]*myocardial infarction", term, ignore.case = TRUE, perl =  TRUE)) %>%
                     
filter(!grepl("not resulting in[-_ ]*myocardial infarction", term, ignore.case = TRUE, perl =  TRUE)) %>%
  
  filter(!grepl(paste0("(?i)myocardial infarction aborted"), term, ignore.case = TRUE, perl = TRUE)) 

# Gold 

gold_myocardial_infarction <- cprd_gold_medical %>% 
  
  # Inclusion - myocardial infarction related terms 
  
  filter(grepl(paste0("(?i)  
  
  myocardial infarction|myocardial infarction|heart attack|myocardial infarct|cardiac thrombosis|
    coronary artery thrombosis|coronary artery thrombosis|acute stemi|N-STEMI"),
               
               term, ignore.case = TRUE, perl = TRUE)) %>%
  
  # Exclusion 
  
  filter(!grepl(paste0("(?i)
   
   # Unrelated to Myocardial infarction - other organs
   
   hepatic|hepatic infarction|infarction of ovary|infarction of prostate|infarction of spleen|renal infarction|systemic lupus erythematosus|infarction of fallopian|	
systemic sclerosis|systemic sclerosis|cerebellar infarction|infarction of breast|thyroid haemorrhage|lacunar infarction|
                       	
cerebral infarction|cerebral infarction|lymph node infarction|lymph vessel infarction| placental infarction|
                       precerebral|precerebral|brainstem infarct|pulmonary infarct|infarction of basal ganglia|
                       	multi-infarct dementia|multi-infarct dementia|infarct of liver|infarct of lung|multi-infarct state|systemic|	
cerebrl infarctn|cerebrl infarctn|cerebral infarct|infarction of liver|bowel infarction|intestinal infarction|
                       splenic infarction|splenic infarction|	
optic nerve infarction|optic nerve infarction|multiple infarcts|testicular infarction|embolic infarction|
                       anaemic infarct|anaemic infarct|	
infarction of kidney|infarction of kidney|renal infarct|thyroid infarction|infarction - cerebral|	
mesenteric infarction|",
                       
                       # Pregnancy                  
                       
                       "placental infarct|placental infarction|placenta infarcted|placental infarct",
                       
                       
                       
                       # Unrelated diseases
                       
                       "creation of portosystemic shunt nec|systemic bartonellosis|systemic cryptococcosis|myelopathy|glaucoma|
  systemic mycoses|systemic mycoses|agent poisoning|renal disorders|idiopathic livedo reticularis|	
systemic lupus erythematosus|systemic lupus erythematosus|systemic disease|systemic vasculitis|panhypopituitarism|
systemic mycosis|systemic mycosis|systemic pulmonary collateral|	
systemic inflammatory response|systemic inflammatory response|gonococcal|	
periventricular hemorrhagic|periventricular hemorrhagic|systemic mastocytosis|lymphoma|cerebral autosomal dominant|
cerebral palsy|cerebral palsy" ,
                       
                       
                       # FH 
                       
                       "family history|family history|fh|fh myocardial infarction",
                       
                       # Children
                       
                       
                       # Negations
                       
                       "no myocardial infarction|no fh:|ecg: no myocardial infarction|not resulting in myocardial infarction|myocardial infarction aborted",
                       
                       # Process of care 
                       
                       "agents|drug reaction| couple and sex therapy|adverse reaction to|fear of having a heart attack|anxiety about having a heart attack|
radioisotope scan for myocardial infarction|radioisotope scan for myocardial infarction"), 
                
                
                term, ignore.case= TRUE, perl = TRUE)) %>%
  
  # Filter out no history of mI
  
  filter(!grepl("no[-_ ]*myocardial infarction", term, ignore.case = TRUE, perl =  TRUE)) %>%
  
  filter(!grepl("not resulting in[-_ ]*myocardial infarction", term, ignore.case = TRUE, perl =  TRUE)) %>%
  
  filter(!grepl(paste0("(?i)myocardial infarction aborted"), term, ignore.case = TRUE, perl = TRUE)) 



# ================= 3) Create updated code lists ===============================

# No previous code lists for comparison 

# Save updated code lists

write.table(aurum_myocardial_infarction,
            file = paste0(wd, path_output, "Aurum_Myocardial_Infarct_codelist_20260602.txt"),
            sep = "\t", row.names = FALSE)

write.table(gold_myocardial_infarction,
            file = paste0(wd, path_output, "Gold_Myocardial_Infarct_codelist_20260602.txt"),
            sep = "\t", row.names = FALSE)
