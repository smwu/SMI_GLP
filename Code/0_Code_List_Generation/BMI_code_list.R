# ==============================================================================
# Generate code lists for BMI
# Authors: SM Wu & S Picton
# Date Created: 2026/03/10
# Date Updated: 2026/04/14
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
# 3) Code_Lists/BMI/Old/Aurum_BMI_20230724_Alvin.txt : Old Aurum BMI code list 
# 4) Code_Lists/BMI/Old/Gold_BMI_20230724_Alvin.txt : Old Gold BMI code list 
# 
# Final Outputs:
# 1) Code_Lists/BMI/Aurum_BMI_codelist_20260320.txt : Updated Aurum BMI code list 
# 2) Code_Lists/BMI/Gold_BMI_codelist_20260320.txt : Updated Gold BMI code list
# 3) Code_Lists/BMI/Aurum_Gold_BMI_codelist_20260320.txt : Updated combined Aurum & Gold code list 


# ================= 1) Set up and load data ====================================

# Clear memory
rm(list = ls())

# Packages
library(readr)
library(dplyr)
library(stringr)
library(tidyr)


#  If working in Data Safe Haven - manually install packages using Artifactory:
#   bit, bit64, cli, crayon, dplyr, generics, glue, hms,
#   lifecycle, magrittr, pillar, pkgconfig, purrr, R6, readr, rlang, stringi, 
#   stringr, tibble, tidyr, tidyselect, tzdb, utf8, vctrs, vroom, withr, writexl

# ### For running locally
# # Set working directory
# wd <- "/Volumes/ritd-ag-project-rd00qv-jfhay18/" # VPN connection
# # wd <- "//live.rd.ucl.ac.uk/ritd-ag-project-rd00qv-jfhay18/" #Desktop@UCL
# setwd(wd)
# 
# # Set input and output paths
# path_input <- "Code_Lists/"
# path_output <- "Code_Lists/BMI/"

### For running in Data Safe Haven
# Set working directory
wd <- "S:/CDSTP_CPRD_25_005368/" 
setwd(wd)

# Set input and output paths
path_input <- "SMI_GLP/Code_Lists/"
path_output <- "SMI_GLP/Code_Lists/BMI/"


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


# Read in old BMI code list from 2023/07/24, setting all col types to character
# Aurum
aurum_BMI_old <- read_delim(
  paste0(wd, path_input, "BMI/Old/Aurum_BMI_20230724_Alvin.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character()), trim_ws = TRUE)
  
# Gold
gold_BMI_old <- read_delim(
  paste0(wd, path_input, "BMI/Old/Gold_BMI_20230724_Alvin.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character()), trim_ws = TRUE)


# ================= 2) Search for new relevant med codes =======================

# Aurum

aurum_BMI <- cprd_aurum_medical %>%
  
  # Inclusion - BMI related terms,  
  filter(grepl(paste0("(?i)BMI|weight|obesity|underweight|overweight|obese|height|body mass index"), 
               term)) %>%
  
# Exclusion - Unrelated terms 

                      
filter(!grepl(paste0("(?i)below knee|weightless environment|weightlessness|
weight-bearing|non-weight-bearing|sample|weight-bearing|weight-bear|
post-dialysis|fpc|molecular|gold|heparin|hb|lift weights|
lesions|7pcl|eyelid|7-point|skin lesion|ede-q|skeletal dysplasia|
down's|mri|magnetic",

#  Unrelated height codes


"uterine fundus|fundal|unfit|gravid|fall|jumping|tinetti|	
mid-parental|predicted|down's|footwear|measurement declined|furniture|
heightened|falling|pubis|demispan|sitting|wave|fear|knee|neurodynamic|
mother|perception|uterine|fundus|fundal|qrs|step",

# Relating to family history 

"fh:obesity|family history:|fh:",


# Relating to children 

"baby birth|child|infant|premature|baby|childhood|birthweight|birth|
fetal|circum.|foetal|child protection conference|child weight=|27-30 mth exam.|
national child measurement programme|child overweight|overweight child",

# Negations

"not done|unsuitable for",

# Relating to pregnancy 

"pregnancy|gestation|placental|placenta|preg|postnatal|preg+postnatal|
maternal",

# Process of care


"framingham",

# Relating to eating disorder

"purging|excessive exercise|	
self-induced purging",

# Administrative 


"ede-q|compensation|ac563|see report|sentinel"), 

  term, perl = TRUE)) %>%
  

  
  # Filter out fh code 
  
filter(!grepl("fh:[-_ ]*obesity", term, ignore.case = TRUE, perl =  TRUE))





# Gold 


gold_BMI <- cprd_gold_medical %>%
  
  
# Inclusion - BMI related terms,  
 filter(grepl(paste0("(?i)BMI|weight|obesity|underweight|overweight|obese|height|body mass index"), 
                            term)) %>%
                 
# Exclusion - Unrelated terms 
                 
                 
filter(!grepl(paste0("(?i)below knee|weightless environment|weightlessness|
weight-bearing|non-weight-bearing|sample|weight-bearing|weight-bear|
post-dialysis|fpc|molecular|gold|heparin|hb|lift weights|
lesions|7pcl|eyelid|7-point|skin lesion|ede-q|skeletal dysplasia|
down's|mri|magenetic",

#  Unrelated height codes
                     
                     
"uterine fundus|fundal|unfit|gravid|fall|jumping|tinetti|
mid-parental|predicted|down's|footwear|measurement declined|furniture|
heightened|falling|pubis|demispan|sitting|wave|fear|knee|neurodynamic|
mother|perception|uterine|fundus|fundal|qrs|step|mid-parental",                                      
                                      
# Relating to family history 
                                      
"fh:obesity|family history:|fh:",
                                      
                                      
# Relating to children 
                                    
"baby birth|child|infant|premature|baby|childhood|birthweight|birth|
fetal|circum.|foetal|child protection conference|child weight=|27-30 mth exam.|
national child measurement programme|child overweight|overweight child",
                                      
# Negations
                                      
"not done|unsuitable for",
                                      
# Relating to pregnancy 
                                      
"pregnancy|gestation|placental|placenta|preg|postnatal|preg+postnatal|
maternal",
                                      
# Process of care
                                      
                                      
"framingham",
                                      
# Relating to eating disorder
                                      
"purging|excessive exercise|	
self-induced purging",
                                      
# Administrative 
                                      
                                    
"ede-q|compensation|ac563|see report|sentinel"), 
                               
                               term, perl = TRUE)) %>%
                 
                 
            
                 
# Filter out fh code 
                 
filter(!grepl("fh:[-_ ]*obesity", term, ignore.case = TRUE, perl =  TRUE))         
               
                     
## Comparing with older codelists

# New codes not in old list

new_aurum <- aurum_BMI %>%
  filter(!medcodeid %in% aurum_BMI_old$medcodeid)

new_gold <- gold_BMI %>%
  filter(!medcode %in% gold_BMI_old$medcode)

# Old codes not in new old list

miss_new_aurum <- aurum_BMI_old %>%
  filter(!medcodeid %in% aurum_BMI$medcodeid)

miss_new_gold <- gold_BMI_old %>%
  filter(!medcode %in% gold_BMI$medcode)

               
# ================= 3) Create updated code lists ===============================

# Create updated code lists

# Aurum
BMI_codelist_aurum_new <- aurum_BMI

# Gold
BMI_codelist_gold_new <- gold_BMI

# Save updated code lists

write.table(BMI_codelist_aurum_new,
            file = paste0(wd, path_output, "Aurum_BMI_codelist_20260320.txt"),
            sep = "\t", row.names = FALSE)

write.table(BMI_codelist_gold_new,
            file = paste0(wd, path_output, "Gold_BMI_codelist_20260320.txt"),
            sep = "\t", row.names = FALSE)

# Combine Aurum and GOLD updated code lists

temp_aurum <- BMI_codelist_aurum_new %>%
  rename(medcode = medcodeid, readcode = CleansedReadCode) %>%
  select(medcode, term)

temp_gold <- BMI_codelist_gold_new %>%
  select(medcode, term)

temp_both <- rbind(temp_aurum, temp_gold)

aurum_gold_BMI_new <- temp_both %>% distinct()


# # Combine Aurum and GOLD into one file with a column specifying database


BMI_codelist_aurum_new$database <- "Aurum"
BMI_codelist_gold_new$database <- "Gold"
BMI_codelist_aurum_gold_new <- rbind(
  BMI_codelist_aurum_new %>% 
    rename(medcode = medcodeid) %>%
    select(medcode, term, database), 
  BMI_codelist_gold_new %>%
    select(medcode, term, database))


# # Save combined code list

write.table(BMI_codelist_aurum_gold_new,
            file = paste0(wd, path_output, "Aurum_Gold_BMI_codelist_20260320.txt"),
            sep = "\t", row.names = FALSE)


               
              






        
        
        
        