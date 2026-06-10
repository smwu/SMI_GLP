# Generate code list for liver disease   
# Author: S Picton & S Wu
# Date created: 2026/05/18
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
# 3) Code_Lists/Liver_Disease/Old/Aurum_Liver_Disease_20240422_Alvin.txt : Old Aurum liver disease code list 
# 4) Code_Lists/Liver_Disease/Old/Gold_Liver_Disease_20240422_Alvin.txt : Old Gold liver disease code list 

# Final Outputs:

# 1) Code_Lists/Liver_Disease/Aurum_Liver_Disease_20260518.txt : Updated Aurum liver disease code list
# 2) Code_Lists/Liver_Disease/Gold_Liver_Disease_20260518.txt : Updated Gold liver disease code list 

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
# path_output <- "Code_Lists/Liver_Disease/

### For running in Data Safe Haven

# Set working directory

wd <- "S:/CDSTP_CPRD_25_005368/" 
setwd(wd)

# Set input and output paths

path_input <- "SMI_GLP/Code_Lists/"
path_output <- "SMI_GLP/Code_Lists/Liver_Disease/"

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


# Read in old Liver Disease code lists, setting all col types to character

# Aurum

liverdisease_aurum_old <- read_delim(
  paste0(wd, path_input, "Liver_Disease/Old/Aurum_Liver_Disease_20240422_Alvin.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character()), trim_ws = TRUE)



# Gold

liverdisease_gold_old <- read_delim(
  paste0(wd, path_input, "Liver_Disease/Old/Gold_Liver_Disease_20240422_Alvin.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character()), trim_ws = TRUE)


# ================= 2) Search for new relevant med codes =======================

# Aurum

aurum_liverdisease <- cprd_aurum_medical %>%
  # Inclusion - liver disease related terms 
  
  filter(grepl(paste0("(?i)
  
  # Liver disease 
  
  liver disease|cirrhosis|liver fibrosis|chronic hepatitis|hepatitis|
  liver failure|hepatic failure|liver transplant|liver abscess|liver cyst|
  steatohepatitis|liver disorder|liver congestion|fatty liver|phlebitis of portal vein|
  hepatectomy|hepatic fibrosis|liver disease|liver failure|liver necrosis|hepatic sclerosis|",
  "liver hyperplasia|autoimmune hepatitis|infarction of liver|fibrosis of liver|cirrhotic|",
  "hepatic infarction|hepatic coma|hepatoptosis|necrosis of liver|hepatis|sclerosis of liver|",
  "abscess of liver|disease of liver|portal hypertension|hepatorenal syndrome|
  hepatic coma|active hepatitis|hepatic sclerosis|hepatitis|necrosis of liver|hepatic granuloma|
  active hepatitis|abscess of liver|hepatic encephalopathy|hepatorenal failure|disease of the liver|",
  "hepatic congestion|chronic active hepatitis| yellow liver atrophy|autoimmune hepatitis|yellow atrophy|
  hepatic veno-occlusive disease|hepatic veno-occlusive|hepatic abscess|fatty change of liver|	
  hepatic cyst|congestion of liver|hyperplasia of liver|portal hypertensive|chronic liver disease|
  hepatic granuloma|cyst of liver|hepatic cyst|autoimmune liver disease|alcoholic fibrosis and sclerosis of liver|",
                      
  # Transplant / surgical issues
  
  "transplantation of liver|hepatectomy|hemihepatectomy|",
  "transplanted liver|orthotopic transplantation|transplantation of liver|",
  "transplanted liver present|liver transplant planned|transplanted liver|piggy-back liver|piggy back liver"),
               trimws(term), ignore.case = TRUE)) %>% 
  
  
  
  # Exclusion     
  
  # Not related to liver disease  
  
  filter(!grepl(paste0("(?i) 
  
  # Unrelated medical terms
  
  lung|respiratory|lymph node|porta hepatis|",
  
  # Family history 
  
  "fh:|family history|family history:","fh:",
  
  # Children / infants 
  
  "childhood|neonatal jaundice|neonatal|infancy|	
  unborn child|children",
  
  # Relating to pregnancy 
  
  "pregnancy",

  # Process of care 
    
    "assay|antibody level|occupational risk|surface antigen measurement|
    test|requires a course of|screening|igg level|antibody test|antigen test|
    igm level|viral load|immunoblot assay|igg level|nucleic acid detection|
  test request|donor for|test request|virus measurement|booster|detection assay|antigen level|
  dna assay|rna assay|blood components|at risk of|antibody measurement|antibody qualitative|rna assay|
   igm level|globulin given|d serology|immunoglob|outbreak education|check for hepatitis|serum qualitative|
  occupation|igm level|mri of transplanted liver|score$|", 
  
  # Negations 
  
  "not detected|antigen negative|pcr negative|reaction negative|no h/o|not detected|test negative|test|antibody negative|
 core antibody negative|no history of|negative|non-cirrhotic|",
  
  # Vaccinations 

  "vaccination|non-immune|status|immunisation|vaccination|vaccine|adverse reaction|diphtheria|vaccinat|immunization"),
  
  term, ignore.case = TRUE, perl =  TRUE)) %>%
  
  # REMOVE TEST REQUESTS UNLESS POSITIVE 
  
  filter(!grepl("^hepatitis a igm level(?! positive)",
           term, perl = TRUE)) %>%
  filter(!grepl("^hepatitis e igm level(?! positive)",
                term, perl = TRUE)) %>%
  
  filter(!grepl("^hepatitis b core igm level(?! positive)",
                term, perl = TRUE)) %>%
  filter(!grepl("^anti hbc (hepatitis b core) igm(?! positive)",
                term, perl = TRUE)) %>%
  filter(!grepl("^hepatitis a virus antibody igm(?! positive)",
                term, perl = TRUE)) %>%
  filter(!grepl("^hepatitis e virus igm(?! positive)", 
                term, perl = TRUE))  %>%
               
  filter(!grepl("^hbc (hepatitis b core) igm antibody arbitrary concentration in serum(?! positive)",
                              term, perl = TRUE)) %>%
  filter(!grepl("^hev (hepatitis e virus) igm antibody in csf qualitative result (?! positive)",
                term, perl = TRUE)) %>% 
  
  # Remove family history codes
  
  filter(!grepl("(?i)(fh: liver disease|fh: hepatitis)", term, perl = TRUE)) %>%
  
  # Remove administration of hepatitis globulin 
  
  filter(!grepl("(?i)(hepatitis b virus immune globulin)", term, perl = TRUE)) %>% 
  
  # Checked HDRUK phenotype library - antibody terms not included in liver disease codes - remove
  
  
  # "hepatitis c pcr" - again could be negative result "hepatitis b surface antigen" 	
  # "hepatitis b virus surface antigen" "hepatitis d virus antibody" "hepatitis delta antibody" 
  # "hepatitis b e antigen" "antibody to hepatitis c virus" "hepatitis c virus antibody" 
  # "hepatitis c antibody" "hepatitis b virus dna nucleic acid amplification qualitative result"


  filter(!grepl(paste0("(?i)hepatitis b antibody present|hepatitis c pcr|hepatitis b surface antigen|
              hepatitis b virus surface antigen|hepatitis d virus antibody|hepatitis delta antibody|
              hepatitis b e antigen|antibody to hepatitis c virus|hepatitis c virus antibody|
              hepatitis c antibody|hepatitis b virus dna nucleic acid amplification qualitative result|
              hepatitis c virus rna|hepatitis e antigen present|hepatitis ant.radioimmunoassay|
              hepatitis a virus antibody, igg type|antibody to hepatitis be antigen|
              hepatitis b e antibody|hepatitis b virus surface antigen|antibody to hepatitis a virus|
              hepatitis a virus antibody|hepatitis a antibody|hepatitis b core antigen|
              hepatitis delta virus antibody|hepatitis b e antigen|hepatitis c antibody|
              antibody to hepatitis b core antigen|hepatitis b core antibody|
              anbc - hepatitis b core antibody|hepatitis b surface antibody|
              hepatitis a virus antibody, igm type|hepatitis antibody|hepatitis b virus dna assay|
              hepatitis b virus dna|antibody to hepatitis b virus|antibody to hepatitis b|
              hepatitis b virus antibody|hepatitis b antibody|hepatitis be antigen present|
              hepatitis antibody radioimmunoassay|hepatitis c virus genotype determination|
              anti-hepatitis a virus igg|hepatitis c virus core antigen|
              hepatitis b virus surface antigen|hepatitis b virus e protein antigen|
              hepatitis b virus antigen|hbc (hepatitis b core) igm antibody arbitrary concentration in serum|
              hepatitis b virus dna arbitrary concentration by nucleic acid amplification|
              hepatitis c virus rna arbitrary concentration by nucleic acid amplification|
              hcv (hepatitis c) number concentration by nucleic acid amplification|
              hcv core ag (hepatitis c virus core antigen) arbitrary concentration in serum|
              hepatitis d virus rna nucleic acid amplification qualitative result|
              hcv (hepatitis c virus) antibody in oral fluid qualitative result|
              hev (hepatitis e virus) igg antibody in csf qualitative result|
              hepatitis b virus rna nucleic acid amplification qualitative result|
              hev (hepatitis e virus) igm antibody in csf qualitative result|
              hepatitis e virus rna nucleic acid amplification qualitative result|
              hepatitis b virus core total antibody in oral fluid qualitative result|
              hepatitis b virus core igg antibody in oral fluid qualitative result|
              hepatitis c virus rna nucleic acid amplification qualitative result|
              hepatitis b core antigen antibody|hepatitis b virus surface antigen antibody"), term, perl = TRUE)) %>%
              
              
  # REMOVE ANTIBODY CODES
  
  filter(!grepl(paste0("(?i)antibody|igg$|nucleic acid amplification|oral fluid|antigen"),
                   term, perl = TRUE))


# Gold



gold_liverdisease <- cprd_gold_medical %>% 
  # Inclusion - liver disease related terms 
  
  filter(grepl(paste0("(?i)
  
  # Liver disease 
  
  liver disease|cirrhosis|liver fibrosis|chronic hepatitis|hepatitis|
  liver failure|hepatic failure|liver transplant|liver abscess|liver cyst|
  steatohepatitis|liver disorder|liver congestion|fatty liver|phlebitis of portal vein|
  hepatectomy|hepatic fibrosis|liver disease|liver failure|liver necrosis|hepatic sclerosis|",
  "liver hyperplasia|autoimmune hepatitis|infarction of liver|fibrosis of liver|cirrhotic|",
  "hepatic infarction|hepatic coma|hepatoptosis|necrosis of liver|hepatis|sclerosis of liver|",
  "abscess of liver|disease of liver|portal hypertension|hepatorenal syndrome|
  hepatic coma|active hepatitis|hepatic sclerosis|hepatitis|necrosis of liver|hepatic granuloma|
  active hepatitis|abscess of liver|hepatic encephalopathy|hepatorenal failure|disease of the liver|",
  "hepatic congestion|chronic active hepatitis| yellow liver atrophy|autoimmune hepatitis|yellow atrophy|
  hepatic veno-occlusive disease|hepatic veno-occlusive|hepatic abscess|fatty change of liver|	
  hepatic cyst|congestion of liver|hyperplasia of liver|portal hypertensive|chronic liver disease|
  hepatic granuloma|cyst of liver|hepatic cyst|autoimmune liver disease|alcoholic fibrosis and sclerosis of liver|",
                      
  # Transplant / surgical issues
  
  "transplantation of liver|hepatectomy|hemihepatectomy|",
  "transplanted liver|orthotopic transplantation|transplantation of liver|",
  "transplanted liver present|liver transplant planned|transplanted liver|piggy-back liver|piggy back liver"),
               trimws(term), ignore.case = TRUE)) %>% 
  
  
  
  # Exclusion     
  
  # Not related to liver disease  
  
  filter(!grepl(paste0("(?i) 
  
  # Unrelated medical terms
  
  lung|respiratory|lymph node|porta hepatis|",
                       
 # Family history 
 
 "fh:|family history|family history:","fh:",
 
 # Children / infants 
 
 "childhood|neonatal jaundice|neonatal|infancy|	
  unborn child|children",
   
   # Relating to pregnancy 
   
   "pregnancy",
   
   # Process of care 
   
   "assay|antibody level|occupational risk|surface antigen measurement|
    test|requires a course of|screening|igg level|antibody test|antigen test|
    igm level|viral load|immunoblot assay|igg level|nucleic acid detection|
  test request|donor for|test request|virus measurement|booster|detection assay|antigen level|
  dna assay|rna assay|blood components|at risk of|antibody measurement|antibody qualitative|rna assay|
   igm level|globulin given|d serology|immunoglob|outbreak education|check for hepatitis|serum qualitative|
  occupation|igm level|mri of transplanted liver|score$|", 
                       
   # Negations 
   
   "not detected|antigen negative|pcr negative|reaction negative|no h/o|not detected|test negative|test|antibody negative|
 core antibody negative|no history of|negative|non-cirrhotic|",
   
   # Vaccinations 
   
   "vaccination|non-immune|status|immunisation|vaccination|vaccine|adverse reaction|diphtheria|vaccinat|immunization"),
                
                term, ignore.case = TRUE, perl =  TRUE)) %>%
  
  # REMOVE TEST REQUESTS UNLESS POSITIVE 
  
  filter(!grepl("^hepatitis a igm level(?! positive)",
                term, perl = TRUE)) %>%
  filter(!grepl("^hepatitis e igm level(?! positive)",
                term, perl = TRUE)) %>%
  
  filter(!grepl("^hepatitis b core igm level(?! positive)",
                term, perl = TRUE)) %>%
  filter(!grepl("^anti hbc (hepatitis b core) igm(?! positive)",
                term, perl = TRUE)) %>%
  filter(!grepl("^hepatitis a virus antibody igm(?! positive)",
                term, perl = TRUE)) %>%
  filter(!grepl("^hepatitis e virus igm(?! positive)", 
                term, perl = TRUE))  %>%
  
  filter(!grepl("^hbc (hepatitis b core) igm antibody arbitrary concentration in serum(?! positive)",
                term, perl = TRUE)) %>%
  filter(!grepl("^hev (hepatitis e virus) igm antibody in csf qualitative result (?! positive)",
                term, perl = TRUE)) %>% 
  
  # Remove family history codes
  
  filter(!grepl("(?i)(fh: liver disease|fh: hepatitis)", term, perl = TRUE)) %>%
  
  # Remove administration of hepatitis globulin 
  
  filter(!grepl("(?i)(hepatitis b virus immune globulin)", term, perl = TRUE)) %>% 
  
  # Checked HDRUK phenotype library - antibody terms not included in liver disease codes - remove
  
  
  # "hepatitis c pcr" - again could be negative result "hepatitis b surface antigen" 	
  # "hepatitis b virus surface antigen" "hepatitis d virus antibody" "hepatitis delta antibody" 
  # "hepatitis b e antigen" "antibody to hepatitis c virus" "hepatitis c virus antibody" 
  # "hepatitis c antibody" "hepatitis b virus dna nucleic acid amplification qualitative result"
  
  
  filter(!grepl(paste0("(?i)hepatitis b antibody present|hepatitis c pcr|hepatitis b surface antigen|
              hepatitis b virus surface antigen|hepatitis d virus antibody|hepatitis delta antibody|
              hepatitis b e antigen|antibody to hepatitis c virus|hepatitis c virus antibody|
              hepatitis c antibody|hepatitis b virus dna nucleic acid amplification qualitative result|
              hepatitis c virus rna|hepatitis e antigen present|hepatitis ant.radioimmunoassay|
              hepatitis a virus antibody, igg type|antibody to hepatitis be antigen|
              hepatitis b e antibody|hepatitis b virus surface antigen|antibody to hepatitis a virus|
              hepatitis a virus antibody|hepatitis a antibody|hepatitis b core antigen|
              hepatitis delta virus antibody|hepatitis b e antigen|hepatitis c antibody|
              antibody to hepatitis b core antigen|hepatitis b core antibody|
              anbc - hepatitis b core antibody|hepatitis b surface antibody|
              hepatitis a virus antibody, igm type|hepatitis antibody|hepatitis b virus dna assay|
              hepatitis b virus dna|antibody to hepatitis b virus|antibody to hepatitis b|
              hepatitis b virus antibody|hepatitis b antibody|hepatitis be antigen present|
              hepatitis antibody radioimmunoassay|hepatitis c virus genotype determination|
              anti-hepatitis a virus igg|hepatitis c virus core antigen|
              hepatitis b virus surface antigen|hepatitis b virus e protein antigen|
              hepatitis b virus antigen|hbc (hepatitis b core) igm antibody arbitrary concentration in serum|
              hepatitis b virus dna arbitrary concentration by nucleic acid amplification|
              hepatitis c virus rna arbitrary concentration by nucleic acid amplification|
              hcv (hepatitis c) number concentration by nucleic acid amplification|
              hcv core ag (hepatitis c virus core antigen) arbitrary concentration in serum|
              hepatitis d virus rna nucleic acid amplification qualitative result|
              hcv (hepatitis c virus) antibody in oral fluid qualitative result|
              hev (hepatitis e virus) igg antibody in csf qualitative result|
              hepatitis b virus rna nucleic acid amplification qualitative result|
              hev (hepatitis e virus) igm antibody in csf qualitative result|
              hepatitis e virus rna nucleic acid amplification qualitative result|
              hepatitis b virus core total antibody in oral fluid qualitative result|
              hepatitis b virus core igg antibody in oral fluid qualitative result|
              hepatitis c virus rna nucleic acid amplification qualitative result|
              hepatitis b core antigen antibody|hepatitis b virus surface antigen antibody"), term, perl = TRUE)) %>%
  
  
  # REMOVE ANTIBODY CODES
  
  filter(!grepl(paste0("(?i)antibody|igg$|nucleic acid amplification|oral fluid|antigen"),
                term, perl = TRUE))


# New codes not in old list

new_aurum <- aurum_liverdisease %>%
  filter(!medcodeid %in% liverdisease_aurum_old$medcodeid)

new_gold <- gold_liverdisease %>%
  filter(!medcode %in% liverdisease_gold_old$medcode)

# Old codes not in new old list

miss_new_aurum <- liverdisease_aurum_old %>%
  filter(!medcodeid %in% aurum_liverdisease$medcodeid)

miss_new_gold <- liverdisease_gold_old %>%
  filter(!medcode %in% gold_liverdisease$medcode)


# ================= 3) Create updated code lists ===============================

# No previous code lists for comparison 

# Save updated code lists

write.table(aurum_liverdisease,
            file = paste0(wd, path_output, "Aurum_Liver_Disease_codelist_20260522.txt"),
            sep = "\t", row.names = FALSE)

write.table(gold_liverdisease,
            file = paste0(wd, path_output, "Gold_Liver_Disease_codelist_20260522.txt"),
            sep = "\t", row.names = FALSE)

