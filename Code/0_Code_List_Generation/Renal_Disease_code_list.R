# Generate code list for renal disease   
# Author: S Picton & S Wu
# Date created: 2026/05/22
# Date updated: 2026/05/26

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
# 3) Code_Lists/Renal_Disease/Old/Aurum_Renal_Disease_20240423_Alvin.txt : Old Aurum renal disease code list 
# 4) Code_Lists/Renal_Disease/Old/Gold_Renal_Disease_20240423_Alvin.txt : Old Gold renal disease code list

# Final Outputs:

# 1) Code_Lists/Renal_Disease/Aurum_Renal_Disease_20260526.txt : Updated Aurum renal disease code list
# 2) Code_Lists/Renal_Disease/Gold_Renal_Disease_20260526.txt : Updated Gold renal disease code list 



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
# path_output <- "Code_Lists/Renal_Disease/

### For running in Data Safe Haven

# Set working directory

wd <- "S:/CDSTP_CPRD_25_005368/" 
setwd(wd)

# Set input and output paths

path_input <- "SMI_GLP/Code_Lists/"
path_output <- "SMI_GLP/Code_Lists/Renal_Disease/"


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

# Read in old Renal Disease code lists, setting all col types to character

# Aurum

renaldisease_aurum_old <- read_delim(
  paste0(wd, path_input, "Renal_Disease/Old/Aurum_Renal_Disease_20240423_Alvin.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character()), trim_ws = TRUE)



# Gold

renaldisease_gold_old <- read_delim(
  paste0(wd, path_input, "Renal_Disease/Old/Gold_Renal_Disease_20240423_Alvin.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character()), trim_ws = TRUE)



# ================= 2) Search for new relevant med codes =======================

# Aurum

aurum_renaldisease <- cprd_aurum_medical %>%
  # Inclusion - renal disease related terms 
  
  filter(grepl(paste0("(?i)
  
  # Chronic kidney disease
  
  chronic kidney disease|CKD|chronic renal failure|renal impairment|renal disease|renal diseases|
                      dialysis|peritoneal dialysis|chronic kidney disease|dialysis|haemodialysis",
                      "renal failure|renal failure|impaired renal function|renal function impairment|kidney disease|
                      renal bone disease|end stage kidney disease|renal bone disease|renal disorders|renal replacement therapy|	
renal replacement|renal replacement|renal insufficiency|arteriovenous shunt for renal dialysis|renal artery stenosis|
                       ambulatory peritoneal dialysis| ambulatory peritoneal dialysis|
                      kidney and pancreas transplant waiting list|kidney and pancreas transplant waiting list|
                      sterile precautions dur kidney dialys/other perf|sterile precautions dur kidney dialys/other perf|disordrs/kidney+ureter/infects+parasitic diseases|
                      disorders of kidney+ureter|resulting/impaired renal tubular function|resulting/impaired renal tubular function|disorders of kidney+ureter
                      removal of arteriovenous shunt for renal dialysis|removal of arteriovenous shunt for renal dialysis|aspiration of chronic ambulatory peritoneal dialysis catheter|repeat test for CKD confirmatory",
                      
   # Acute kidney injury
   
   "acute kidney injury|AKI|renal injury|kidney injury|kidney disease|kidney failure|acute kidney failure",
   
   # Other kidney disease
   
   "glomerulonephritis|glomerulonephritic|nephritic|nephrotic|polycystic kidney disease|glomerulonephritis|renal cystic disease|
    tubulo-interstitial disorders|cystic kidney disease|renal involvement|fibrocystic kidney|renal vascular disease|chronic uraemia|
   	renal tubulo-interstitial|renal tubulo-interstitial|renal diseases|tubule insufficiency|kidney infection|renal parenchymal disease|
   	cystic disease of kidney|other kidney/urinary diseases|cystic disease of kidney|impairment of renal function|
   	kidney/urinary disease nos|kidney/urinary disease nos|gout due to impairment of renal function|acquired cystic disease associated renal cell carcinoma|	
kidney and ureter disease nos",
   
   # Renal transplant
   
   "renal transplant|kidney transplant|transplanted kidney|kidney recipient|renal transplant| transplantation of kidney|
   allotransplantation of kidney|autotransplant of kidney|allotransplantation of kidney|chronic kidney rejection|transplant kidney|
   transplantation of kidney|pre-transplantation of kidney|transplant of kidney|transplantation of kidney|entire transplant kidney|entire transplant kidney|donor of kidney for transplant"),
   
   term, ignore.case = TRUE, perl = TRUE)) %>% 
  
  
  # Exclusion 
  
  filter(!grepl(paste0("(?i)
   
   # Not related to kidney disease 
   
   
   leukoplakia|aphakia|shaking|kawasaki|malakoplakia|melanoplakia|pseudophakia|making|pakistani|
                         anisakiasis|taking|waking|slovakia|pakistan|
                         pancaking|leaking|akinetic|microphakia|maki|
                         hakim|homemaking|caretaking|akineton|
                         anakinra|dakins|konakion|canakinumab|eakin|fakiod|
                         aphakic|epikeratophakia|akinetic|malacoplakia|
                         breaking|baking|pseudophakic|canakinumab|
                         erythroleukoplakia|akin|undertaking|leukoplakia|spherophakia|anisakiasis|
                       cyclodialysis|iridodialysis|leucoplakia|erythroplakia|anisakis|aphakic|erythroplakia|
                       adrenal|anisakis|pollakiuria|phakic|aphakic|cyclodialysis|citrobacter|carney|adrenal disorders|adrenal|adrenal insufficiency|
                       	corticoadrenal insufficiency|corticoadrenal insufficiency|retinal dialysis|retinal detachment|akis|hakim",   
 
   
   
   # Children
   
   
   
   
   
   # Family history 
   
   "family history of|family history of|family history of chronic renal impairment|family history of chronic renal disease
|family history of renal failure syndrome|family history of renal failure",
   
   # Negations
   
   
   
   "not diagnostic|no family history of|no h/o:|no evidence of|no evidence of chronic kidney disease|not diagnostic|egfr 3 month repeat test for ckd not diagnostic",
   
   # Process of care 
   
   "requested|nurse|requested|at risk of|calculated by abbreviated|risk calculator score|estimated glomerular filtration rate using chronic kidney disease epidemiology collaboration formula
|estimated glomerular filtration rate using chronic kidney disease epidemiology collaboration formula|egfr (estimated glomerular filtration rate) using creatinine chronic kidney disease epidemiology collaboration equation per 1.73 square metres|egfr (estimated glomerular filtration rate) using ckd-epi (chronic kidney disease epidemiology collaboration) formula|
   egfr (estimated glomerular filtration rate) using cystatin c ckd-epi (chronic kidney disease epidemiology collaboration) equation|estimated glomerular filtration rate using creatinine chronic kidney disease epidemiology collaboration equation|
egfr (estimated glomerular filtration rate) using creatinine ckd-epi (chronic kidney disease epidemiology collaboration) equation|	
treatment escalation plan decision|assessment using|risk equation|risk score|estimated glomerular filtration rate using creatinine chronic kidney disease epidemiology collaboration equation per 1.73 square metres|extraneal|qkidney disease risk calculator|complement component 3 nephritic factor level"),
   
   term, ignore.case = TRUE, perl = TRUE)) %>%
  
  # Remove eGFR test code unless positive for CKD
  
filter(!grepl("^egfr(?! 3 month repeat test for ckd confirmatory)",
              term, ignore.case = TRUE, perl = TRUE))


# Gold 


gold_renaldisease <- cprd_gold_medical %>%
  
  # Inclusion - renal disease related terms 
  
  filter(grepl(paste0("(?i)
  
  # Chronic kidney disease
  
  chronic kidney disease|CKD|chronic renal failure|renal impairment|renal disease|renal diseases|
                      dialysis|peritoneal dialysis|chronic kidney disease|dialysis|haemodialysis",
                      "renal failure|renal failure|impaired renal function|renal function impairment|kidney disease|
                      renal bone disease|end stage kidney disease|renal bone disease|renal disorders|renal replacement therapy|	
renal replacement|renal replacement|renal insufficiency|arteriovenous shunt for renal dialysis|renal artery stenosis|
                       ambulatory peritoneal dialysis| ambulatory peritoneal dialysis|
                      kidney and pancreas transplant waiting list|kidney and pancreas transplant waiting list|
                      sterile precautions dur kidney dialys/other perf|sterile precautions dur kidney dialys/other perf|disordrs/kidney+ureter/infects+parasitic diseases|
                      disorders of kidney+ureter|resulting/impaired renal tubular function|resulting/impaired renal tubular function|disorders of kidney+ureter
                      removal of arteriovenous shunt for renal dialysis|removal of arteriovenous shunt for renal dialysis|aspiration of chronic ambulatory peritoneal dialysis catheter|repeat test for CKD confirmatory",
                      
                      # Acute kidney injury
                      
                      "acute kidney injury|AKI|renal injury|kidney injury|kidney disease|kidney failure|acute kidney failure",
                      
                      # Other kidney disease
                      
                      "glomerulonephritis|glomerulonephritic|nephritic|nephrotic|polycystic kidney disease|glomerulonephritis|renal cystic disease|
    tubulo-interstitial disorders|cystic kidney disease|renal involvement|fibrocystic kidney|renal vascular disease|chronic uraemia|
   	renal tubulo-interstitial|renal tubulo-interstitial|renal diseases|tubule insufficiency|kidney infection|renal parenchymal disease|
   	cystic disease of kidney|other kidney/urinary diseases|cystic disease of kidney|impairment of renal function|
   	kidney/urinary disease nos|kidney/urinary disease nos|gout due to impairment of renal function|acquired cystic disease associated renal cell carcinoma|kidney and ureter disease nos",
                      
                      # Renal transplant
                      
                      "renal transplant|kidney transplant|transplanted kidney|kidney recipient|renal transplant| transplantation of kidney|
   allotransplantation of kidney|autotransplant of kidney|allotransplantation of kidney|chronic kidney rejection|transplant kidney|
   transplantation of kidney|pre-transplantation of kidney|transplant of kidney|transplantation of kidney|entire transplant kidney|entire transplant kidney|donor of kidney for transplant"),
               
               term, ignore.case = TRUE, perl = TRUE)) %>% 
  
  
  # Exclusion 
  
  filter(!grepl(paste0("(?i)
   
   # Not related to kidney disease 
   
   
   leukoplakia|aphakia|shaking|kawasaki|malakoplakia|melanoplakia|pseudophakia|making|pakistani|
                         anisakiasis|taking|waking|slovakia|pakistan|
                         pancaking|leaking|akinetic|microphakia|maki|
                         hakim|homemaking|caretaking|akineton|
                         anakinra|dakins|konakion|canakinumab|eakin|fakiod|
                         aphakic|epikeratophakia|akinetic|malacoplakia|
                         breaking|baking|pseudophakic|canakinumab|
                         erythroleukoplakia|akin|undertaking|leukoplakia|spherophakia|anisakiasis|
                       cyclodialysis|iridodialysis|leucoplakia|erythroplakia|anisakis|aphakic|erythroplakia|
                       adrenal|anisakis|pollakiuria|phakic|aphakic|cyclodialysis|citrobacter|carney|adrenal disorders|adrenal|adrenal insufficiency|
                       	corticoadrenal insufficiency|corticoadrenal insufficiency|retinal dialysis|retinal detachment|akis|hakim",   
                       
                       
                       
                       # Children
                       
                       
                       
                       
                       
                       # Family history 
                       
                       "family history of|family history of|family history of chronic renal impairment|family history of chronic renal disease
|family history of renal failure syndrome|family history of renal failure",
                       
                       # Negations
                       
                       
                       
                       "not diagnostic|no family history of|no h/o:|no evidence of|no evidence of chronic kidney disease|not diagnostic|egfr 3 month repeat test for ckd not diagnostic",
                       
                       # Process of care 
                       
                       "requested|nurse|requested|at risk of|calculated by abbreviated|risk calculator score|estimated glomerular filtration rate using chronic kidney disease epidemiology collaboration formula
|estimated glomerular filtration rate using chronic kidney disease epidemiology collaboration formula|egfr (estimated glomerular filtration rate) using creatinine chronic kidney disease epidemiology collaboration equation per 1.73 square metres|egfr (estimated glomerular filtration rate) using ckd-epi (chronic kidney disease epidemiology collaboration) formula|
   egfr (estimated glomerular filtration rate) using cystatin c ckd-epi (chronic kidney disease epidemiology collaboration) equation|estimated glomerular filtration rate using creatinine chronic kidney disease epidemiology collaboration equation|
egfr (estimated glomerular filtration rate) using creatinine ckd-epi (chronic kidney disease epidemiology collaboration) equation|	
treatment escalation plan decision|assessment using|risk equation|risk score|estimated glomerular filtration rate using creatinine chronic kidney disease epidemiology collaboration equation per 1.73 square metres|extraneal|qkidney disease risk calculator|complement component 3 nephritic factor level"),
                
                term, ignore.case = TRUE, perl = TRUE)) %>%
  
  # Remove eGFR test code unless positive for CKD
  
  filter(!grepl("^egfr(?! 3 month repeat test for ckd confirmatory)",
                term, ignore.case = TRUE, perl = TRUE))






# New codes not in old list

new_aurum <- aurum_renaldisease %>%
  filter(!medcodeid %in% renaldisease_aurum_old$medcodeid)

new_gold <- gold_renaldisease %>%
  filter(!medcode %in% renaldisease_gold_old$medcode)

# Old codes not in new old list

miss_new_aurum <- renaldisease_aurum_old %>%
  filter(!medcodeid %in% aurum_renaldisease$medcodeid)



miss_new_gold <- renaldisease_gold_old %>%
  filter(!medcode %in% gold_renaldisease$medcode)                      





# ================= 3) Create updated code lists ===============================
  

# Save updated code lists

write.table(aurum_renaldisease,
            file = paste0(wd, path_output, "Aurum_Renal_Disease_codelist_20260526.txt"),
            sep = "\t", row.names = FALSE)

write.table(gold_renaldisease,
            file = paste0(wd, path_output, "Gold_Renal_Disease_codelist_20260526.txt"),
            sep = "\t", row.names = FALSE)
