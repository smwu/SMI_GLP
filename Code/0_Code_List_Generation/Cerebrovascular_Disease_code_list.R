# Generate code list for cerebrovascular disease (composite stroke and other cerebrovascular disease) 
# Author: S Picton & S Wu
# Date created: 2026/06/05
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
# 3) Code_Lists/Cerebrovascular_Disease/Old/Aurum_Cerebrovascular_Disease_codelist_20260604_Naomi.txt : Old Aurum cerebrovascular code list 
# 4) Code_Lists/Cerebrovascular_Disease/Old/Gold_Cerebrovascular_Disease_codelist_20260604_Naomi.txt : Old Gold cerebrovascular code list 




# Final Outputs:

# 1) Code_Lists/Cerebrovascular_Disease/Aurum_Cerebrovascular_Disease_20260605.txt : Aurum Cerebrovascular Disease codelist 
# 2) Code_Lists/Cerebrovascular_Disease/Gold_Cerebrovascular_Disease_20260605.txt : Gold Cerebrovascular Disease codelist 




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
# path_output <- "Code_Lists/Cerebrovascular_Disease/

### For running in Data Safe Haven

# Set working directory

wd <- "S:/CDSTP_CPRD_25_005368/" 
setwd(wd)

# Set input and output paths

path_input <- "SMI_GLP/Code_Lists/"
path_output <- "SMI_GLP/Code_Lists/Cerebrovascular_Disease/"

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



# Read in old cerebrovascular disease code lists, setting all col types to character

# Aurum

cerebrovascular_disease_aurum_old <- read_delim(
  paste0(wd, path_input, "Cerebrovascular_Disease/Old/Aurum_Cerebrovascular_Disease_codelist_20260604_Naomi.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character()), trim_ws = TRUE)



# Gold

cerebrovascular_disease_gold_old <- read_delim(
  paste0(wd, path_input, "Cerebrovascular_Disease/Gold_Cerebrovascular_Disease_20260605.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character()), trim_ws = TRUE)



# ================= 2) Search for new relevant med codes =======================

# Aurum

aurum_cerebrovascular_disease <- cprd_aurum_medical %>%
  # Inclusion -  cerebrovascular disease related terms 
  
  filter(grepl(paste0("(?i)
  
  cerebrovascular disease|cerebrovascular disease|cerebral ischaemia|cerebral ischemia|
  transient ischaemic event| transient ischaemic event|cerebral insufficiency|subarachnoid haemorrhage|
  intracranial haemorrhage|intracranial haemorrhage|intracranial hemorrhage"),
               
               term, ignore.case = TRUE, perl = TRUE)) %>%
  # Exclusions 
  
  filter(!grepl(paste0("(?!)
                       
        # Unrelated terms
        
        
        heat stroke|heat stroke|sunstroke|stroke volume|heatstroke|stroke index|	
diabetes, heart disease and stroke|diabetes, heart disease and stroke|cerebral palsy|mcvay repair of inguinal hernia|
                       mcvay repair of inguinal hernia|mitochond encephalopathy, lact acidosis & strokelike episode|cva tenderness|cva tenderness",
                       
                       # Head injury
                       
                       "head injury|head injury|traumatic cerebral haemorrhage|traumatic intracerebral hemorrhage|
          traumatic intracerebral haemorrhage|traumatic intracerebral haemorrhage|cerebral haemorrhage due to trauma|	
cerebral haemorrhage following injury|cerebral haemorrhage following injury|	
other cerebral haemorrhage following injury nos|cerebral haemorrhage following injury nos|	
subarachnoid haemorrhage following injury|subarachnoid haemorrhage following injury",
                       "subarachnoid haemorrhage following injury|subarachnoid haemorrhage following injury|
                       intracranial hemorrhage following injury|intracranial hemorrhage following injury|
                       intracranial haemorrhage following injury|intracranial haemorrhage following injury",
                       
                       "haemorrhage.*injury|haemorrhage.*injury",
                       
                       # Family history
                       
                       "fh|fh|fh:|family history|family history:", 
                       
                       # Pregnancy
                       
                       "puerperium|puerperium|pregnancy|pregnant|cerebral haemorrhage due to birth trauma|
        	cerebral haemorrhage due to birth injury|cerebral haemorrhage due to birth injury|
        perinatal arterial ischaemic stroke|perinatal arterial ischaemic stroke|cerebral haemorrhage unspecified, due to birth trauma|
        	cerebral haemorrhage unspecified, due to birth trauma|cerebral haemorrhage unspecified, due to birth trauma|
          subarachnoid haemorrhage due to birth injury|subarachnoid haemorrhage due to birth injury|subarachnoid haemorrhage due to birth injury",
                       
                       # Children
                       
                       "infant|infant|child|foetal|fetal|neonatal cerebral ischaemia|intracerebral haemorrhage in foetus or newborn|
        	neonatal stroke|neonatal stroke|cerebral haemorrhage - birth|mitoch myopath|
        	juvenile myopathy,|juvenile myopathy,|juvenile myopathy, encephalopathy, lactic acidosis and stroke|
        mitochondrial myopathy, encephalopathy, lactic acidosis and stroke-like episodes|mitochondrial myopathy, encephalopathy, lactic acidosis and stroke-like episodes|
        neonatal epilepsy due to perinatal stroke|neonatal epilepsy due to perinatal stroke|
        paediatric arterial ischaemic stroke|paediatric arterial ischaemic stroke|perinatal arterial ischaemic stroke of fetus and/or neonate|
        juvenile myopathy, encephalopathy, lactic acidosis, stroke|juvenile myopathy, encephalopathy, lactic acidosis, stroke|
        melas - mitochondrial encephalopathy, lactic acidosis and stroke-like episodes|melas - mitochondrial encephalopathy, lactic acidosis and stroke-like episodes|
        neonatal cerebral haemorrhage|neonatal cerebral haemorrhage|neonatal cerebral haemorrhage",
                       
                       # Negations
                       
                       "no history of|no history of|no h/o:|stroke prevention|stroke test negative|suspected stroke|stroke test negative|suspected cerebrovascular accident|
                       impending cerebral ischaemia|impending cerebral ischaemia")
                
                , term, ignore.case = TRUE, perl = TRUE)) 
                       

