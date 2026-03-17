# -----------------------
# LiverDisease code list
# -----------------------
# CPRD 2023 data
# Last run: 18/04/24

## This code list has been designed by using ICD-10 terms in the K70-K77 chapter + liver transplant

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

aurum_liverdisease <- CPRDAurumMedical %>%
  filter(grepl("(?i)liver disease|disease of liver|disease of the liver|liver disorder|
               
               alcohol liver disease|alcoholic hepatitis|alcoholic fibrosis|sclerosis of liver|alcoholic cirrhosis of liver|alcoholic hepatic failure|
               
               |toxic liver disease|
               
    |hepatic failure|liver failure|hepatic coma|hepatic encephalopathy|yellow liver atrophy|yellow liver dystrophy|acute yellow atrophy|subacute yellow atrophy|acute non-viral hepatitis|acute non viral hepatitis|acute nonviral hepatitis|
               
               |cirrhosis|cirrhotic|
               
               |pylephlebitis|steatohepatitis|
               
               |Peliosis hepatis|hepatic veno-occlusive|hepatic venoocclusive|hepatorenal syndrome|hepatorenal failure|intrahepatic vascular shunt|hepatoptosis|hepatosplenic schistosomiasis|hepatic granuloma|hepatectomy", term) | 
           
           grepl("(?i)hepatitis", term) & grepl("(?i)chronic|reactive|granulomatous|autoimmune|cytomegaloviral|herpesviral|toxoplasma", term) |
           
           grepl("(?i)fibrosis|sclerosis|fatty|congestion|necrosis|infarction|infarct|angiomatosis|hyperplasia|transplant", term) & grepl("(?i)liver|hepatic", term) |
           
           grepl("(?i)abscess|cyst", term) & grepl("(?i)liver|hepatic", term) |
           
           grepl("(?i)phlebitis", term) & grepl("(?i)portal vein", term) |
           
           grepl("(?i)portal", term) & grepl("(?i)hypertens|htn", term)) %>%
           
  filter(!grepl("(?i)fh:|family history|child|infant|maternal history|pregnancy|absent|screening test|member|no h/o|suspected|neonatal|score|non-cirrhotic|
                |negative|assay|assessment|contact with and exposure to viral hepatitis|baby|adverse reaction|offered|declined|antibody level|risk|
                |notification|level|test request|resolved|without hepatic coma|subhepatic|lung|cardiac cirrhosis|congestive cirrhosis|without mention|obstetric|cystocele", term))


# Gold

gold_liverdisease <- CPRDGoldMedical %>%
  filter(grepl("(?i)liver disease|disease of liver|disease of the liver|liver disorder|

    alcohol liver disease|alcoholic hepatitis|alcoholic fibrosis|sclerosis of liver|alcoholic cirrhosis of liver|alcoholic hepatic failure|
    
    |toxic liver disease|
    
    |hepatic failure|liver failure|hepatic coma|hepatic encephalopathy|yellow liver atrophy|yellow liver dystrophy|acute yellow atrophy|subacute yellow atrophy|acute non-viral hepatitis|acute non viral hepatitis|acute nonviral hepatitis|
    
    |cirrhosis|cirrhotic|
    
    |pylephlebitis|steatohepatitis|
    
    |Peliosis hepatis|hepatic veno-occlusive|hepatic venoocclusive|hepatorenal syndrome|hepatorenal failure|intrahepatic vascular shunt|hepatoptosis|hepatosplenic schistosomiasis|hepatic granuloma|hepatectomy", term) | 
           
           grepl("(?i)hepatitis", term) & grepl("(?i)chronic|reactive|granulomatous|autoimmune|cytomegaloviral|herpesviral|toxoplasma", term) |
           
           grepl("(?i)fibrosis|sclerosis|fatty|congestion|necrosis|infarction|infarct|angiomatosis|hyperplasia|transplant", term) & grepl("(?i)liver|hepatic", term) |
           
           grepl("(?i)abscess|cyst", term) & grepl("(?i)liver|hepatic", term) |
           
           grepl("(?i)phlebitis", term) & grepl("(?i)portal vein", term) |
           
           grepl("(?i)portal", term) & grepl("(?i)hypertens|htn", term)) %>%
           
  filter(!grepl("(?i)fh:|family history|child|infant|maternal history|pregnancy|absent|screening test|member|no h/o|suspected|neonatal|score|non-cirrhotic|
    |negative|assay|assessment|contact with and exposure to viral hepatitis|baby|adverse reaction|offered|declined|antibody level|risk|
                |notification|level|test request|resolved|without hepatic coma|subhepatic|lung|cardiac cirrhosis|congestive cirrhosis|without mention|obstetric|cystocele", term))
  
# save as text file
write.table(gold_liverdisease, file = "Code lists/Liver disease/liverdisease_gold_220424.txt",
            sep = "\t", row.names = FALSE)

write.table(aurum_liverdisease, file = "Code lists/Liver disease/liverdisease_aurum_220424.txt",
            sep = "\t", row.names = FALSE)

remove(gold_liverdisease, aurum_liverdisease, CPRDGoldMedical, CPRDAurumMedical)
