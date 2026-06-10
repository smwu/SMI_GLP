# Generate code list for coronary revascularisation procedures 

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

# 1) Code_Lists/Coronary_Revascularisation_Procedures/Aurum_Coronary_Revascularisation_Procedures_20260602.txt : Updated Aurum Myocardial Infarction code list
# 2) Code_Lists/Coronary_Revascularisation_Procedures/Gold_Coronary_Revascularisation_Procedures_20260602.txt : Updated Gold Myocardial Infarction code list 




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
# path_output <- "Code_Lists/Coronary_Revascularisation_Procedures/

### For running in Data Safe Haven

# Set working directory

wd <- "S:/CDSTP_CPRD_25_005368/" 
setwd(wd)

# Set input and output paths

path_input <- "SMI_GLP/Code_Lists/"
path_output <- "SMI_GLP/Code_Lists/Coronary_Revascularisation_Procedures/"

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


# Read in old Hypertension code list from 2024/02/28, setting all col types to character

# Aurum

aurum_coronary_revascularisation_old <- read_delim(
  paste0(wd, path_input, "Coronary_Revascularisation/Old/Aurum_Coronary_Revascularisation_codelist_20260326_Exeter.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(MedCodeId = col_character()), trim_ws = TRUE)


# ================= 2) Search for new relevant med codes =======================

# Aurum


aurum_coronary_revascularisation_procedures <- cprd_aurum_medical %>%
  # Inclusion - coronary revascularisation procedures 
  
  filter(grepl(paste0("(?i) 
  
  coronary revascularisation|coronary revascularisation|coronary intervention|PCI|
                      coronary artery bypass|coronary graft|coronary bypass graft|coronary angioplasty|
                      coronary artery operation|coronary artery operation|bypass coronary|coronary artery stent|
                      coronary stent|coronary stent|percutaneous insertion|cardiac revascularisation|cardiac revasculization|
                      percutaneous transluminal|percutaneous transluminal coronary|coronary artery bypass|
                      coronary artery operation|coronary artery repair|coronary endarterectomy|saphenous vein graft|prosthetic bypass|
                      triple bypass|triple bypass|autograft replacement|balloon angioplasty|drug eluting stent|drug-eluting stent|
                      coronary artery bypass graft| coronary artery bypass graft|cabg|aortocoronary bypass|
                      replacement of coronary artery|replacement of coronary artery|bypass of coronary artery|
                      coronary endarterctomy|allograft bypass|drug eluting coronary artery stent|coronary vein graft|
                      coronary bypass|coronary bypass|heart bypass|angioplasty of coronary|bypass of three|Coronary A revasc.|repair of coronary|
                       Endarterectomy of coronary artery|Endarterectomy of coronary artery|Peroperative angioplasty|Aorto-coronary by-pass|
    transluminal atherectomy of coronary|transluminal atherectomy of coronary|Cardiac revascularisation|Coronary artery surgery|
                      bypass aortocoronary|bypassaortocoronary|coronary angioplasty|stents cor art|Percut angiopl coronary artery|
                      coronary A by-pass|coronary A by-pass|Removal of coronary artery obstruction|
                      Percut angiopl coronary artery|Percut angiopl coronary artery|
                      |therapeutic transluminal op on coronary|therapeutic transluminal op on coronary|
                      mammary arteries to coronary|mammary arteries to coronary|Replacement of coronary arteries using multiple methods|
                      LIMA single anastomosis|LIMA single anastomosis|LIMA sequential anastomosis|mammary arteries into coronary|
                      coronary arteries using multiple methods|coronary arteries using multiple methods|RIMA single anastomosis|
                      arteries into coronary arteries|arteries into coronary arteries|RIMA sequential anastomosis|
                      Heart: arterial implant|Heart: arterial implant|Bypass of four or more coronary arteries with prosthesisis|
                      Coronary atherectomy by laser|Coronary atherectomy by laser|thoracic arteries to coronary arteries|
                       implantation of thoracic artery into heart|implantation of thoracic artery into heart|	
Heart: int. mammary A implant|Heart: int. mammary A implant|Bypass of four or more coronary arteries with prosthesis|Heart: aortic branch implant",
    
                      
                      "coronary.*bypass|bypass.*coronary|coronary.*bypass|stent.*coronary|graft.*coronary|
                      coronary.*angioplasty|angioplasty.*coronary|operat.*coronary|coronary.*graft|prosthetic.*coronary|
                      cardiac.*bypass|cardiac*bypass|artery.*coronary arter|graft.*angioplasty|angioplasty.*graft|
                      stent.*coronary artery|coronary artery.*stent|stent.*artery"), 
              
                term, ignore.case = TRUE, perl = TRUE)) %>%


# Exclusion 

filter(!grepl(paste0("(?i)
   
   # Unrelated to coronary revascularisation procedures 
   
   
   pepcid|pepcid|i-pcit|keele taps t|pciol|writtn|pneumatosis|nephrostomy|ureteric|shunt|nephroureterostomy|
                     ureter|ureter|bile duct|tubal prosthesis|gastrostomy|hepatic|atrial appendage|bifurcated|
                     anomalous|anomalous|anomaly|extra-articular ligament|intra-articular ligament",
                     "primary open autograft replacement intra-articular ligament|primary open autograft replacement intra-articular ligament",
                     "revision open autograft replacement intra-articular ligament|revision open autograft replacement intra-articular ligament",
                     
    # Children
    
    "fetus|fetal|fetus|bladder drain to fetus",
    
    
    # Process of care 
    
    "score|score|calcium score|assessment using pci",
    
    # Not related to revascularisation 
    
    "left coronary artery main stem|right coronary artery structure|left anterior descending coronary artery thrombosis|
    congenital coronary artery sclerosis|congenital coronary artery sclerosis|left coronary artery structure|cad|
    	coronary artery occlusion|coronary artery occlusion|coronary artery occluded|coronary artery atheroma|
    anomalous origin|anomalous origin|coronary artery spasm angina|coronary artery and coronary artery, cs|
    fistula to pulmonary artery|fistula to pulmonary artery|muscle bridge|single vessel disease|	
  congenital absence|congenital absence|coronary artery aneurysm|single coronary artery|coronary artery spasm|
    transposition of coronary artery|coronary artery anomaly nos|congenital stricture of coronary artery|
    dissection of coronary artery|dissection of coronary artery|intravascular ultrasound|coronary artery disease|
    perforation of coronary artery|perforation of coronary artery|coronary artery structure|	
left coronary artery main stem|left coronary artery main stem|congenital coronary|coronary artery rupture|
    coronary artery embolism|coronary artery embolism|arteriovenous fistula|aorta|renal artery|pulmonary artery|
     pulmonary vein|pulmonary vein|pulmonary venous pathway|common iliac artery|coeliac artery|external iliac artery|
     common femoral artery|profunda femoris|femoral artery|popliteal artery|infrapopliteal artery|
     crural artery|crural artery|arterial graft|superior vena cava|balloon angioplasty of vein|device|pulmonary collateral|
    	
distention of artery|distention of artery|pericardial drainage tube|femoropopliteal|superior mesenteric artery",
    "carotid|carotid|cerebral|subclavian",
   
    # Negations 
    
    "excluded|excluded|planned"),
                     
              
              term, ignore.case = TRUE, perl = TRUE))  

# Gold


gold_coronary_revascularisation_procedures <- cprd_gold_medical %>%
  # Inclusion - coronary revascularisation procedures 
  
 
filter(grepl(paste0("(?i) 
  
  coronary revascularisation|coronary revascularisation|coronary intervention|PCI|
                      coronary artery bypass|coronary graft|coronary bypass graft|coronary angioplasty|
                      coronary artery operation|coronary artery operation|bypass coronary|coronary artery stent|
                      coronary stent|coronary stent|percutaneous insertion|cardiac revascularisation|cardiac revasculization|
                      percutaneous transluminal|percutaneous transluminal coronary|coronary artery bypass|
                      coronary artery operation|coronary artery repair|coronary endarterectomy|saphenous vein graft|prosthetic bypass|
                      triple bypass|triple bypass|autograft replacement|balloon angioplasty|drug eluting stent|drug-eluting stent|
                      coronary artery bypass graft| coronary artery bypass graft|cabg|aortocoronary bypass|
                      replacement of coronary artery|replacement of coronary artery|bypass of coronary artery|
                      coronary endarterctomy|allograft bypass|drug eluting coronary artery stent|coronary vein graft|
                      coronary bypass|coronary bypass|heart bypass|angioplasty of coronary|bypass of three|Coronary A revasc.|repair of coronary|
                       Endarterectomy of coronary artery|Endarterectomy of coronary artery|Peroperative angioplasty|Aorto-coronary by-pass|
    transluminal atherectomy of coronary|transluminal atherectomy of coronary|Cardiac revascularisation|Coronary artery surgery|
                      bypass aortocoronary|bypassaortocoronary|coronary angioplasty|stents cor art|Percut angiopl coronary artery|
                      coronary A by-pass|coronary A by-pass|Removal of coronary artery obstruction|
                      Percut angiopl coronary artery|Percut angiopl coronary artery|
                      |therapeutic transluminal op on coronary|therapeutic transluminal op on coronary|
                      mammary arteries to coronary|mammary arteries to coronary|Replacement of coronary arteries using multiple methods|
                      LIMA single anastomosis|LIMA single anastomosis|LIMA sequential anastomosis|mammary arteries into coronary|
                      coronary arteries using multiple methods|coronary arteries using multiple methods|RIMA single anastomosis|
                      arteries into coronary arteries|arteries into coronary arteries|RIMA sequential anastomosis|
                      Heart: arterial implant|Heart: arterial implant|Bypass of four or more coronary arteries with prosthesisis|
                      Coronary atherectomy by laser|Coronary atherectomy by laser|thoracic arteries to coronary arteries|
                       implantation of thoracic artery into heart|implantation of thoracic artery into heart|	
Heart: int. mammary A implant|Heart: int. mammary A implant|Bypass of four or more coronary arteries with prosthesis|Heart: aortic branch implant",
                    
                    
                    "coronary.*bypass|bypass.*coronary|coronary.*bypass|stent.*coronary|graft.*coronary|
                      coronary.*angioplasty|angioplasty.*coronary|operat.*coronary|coronary.*graft|prosthetic.*coronary|
                      cardiac.*bypass|cardiac*bypass|artery.*coronary arter|graft.*angioplasty|angioplasty.*graft|
                      stent.*coronary artery|coronary artery.*stent|stent.*artery"), 
             
             term, ignore.case = TRUE, perl = TRUE)) %>%
  
  
  # Exclusion 
  
  filter(!grepl(paste0("(?i)
   
   # Unrelated to coronary revascularisation procedures 
   
   
   pepcid|pepcid|i-pcit|keele taps t|pciol|writtn|pneumatosis|nephrostomy|ureteric|shunt|nephroureterostomy|
                     ureter|ureter|bile duct|tubal prosthesis|gastrostomy|hepatic|atrial appendage|bifurcated|
                     anomalous|anomalous|anomaly|extra-articular ligament|intra-articular ligament",
                       "primary open autograft replacement intra-articular ligament|primary open autograft replacement intra-articular ligament",
                       "revision open autograft replacement intra-articular ligament|revision open autograft replacement intra-articular ligament",
                       
                       # Children
                       
                       "fetus|fetal|fetus|bladder drain to fetus",
                       
                       
                       # Process of care 
                       
                       "score|score|calcium score|assessment using pci",
                       
                       # Not related to revascularisation 
                       
                       "left coronary artery main stem|right coronary artery structure|left anterior descending coronary artery thrombosis|
    congenital coronary artery sclerosis|congenital coronary artery sclerosis|left coronary artery structure|cad|
    	coronary artery occlusion|coronary artery occlusion|coronary artery occluded|coronary artery atheroma|
    anomalous origin|anomalous origin|coronary artery spasm angina|coronary artery and coronary artery, cs|
    fistula to pulmonary artery|fistula to pulmonary artery|muscle bridge|single vessel disease|	
  congenital absence|congenital absence|coronary artery aneurysm|single coronary artery|coronary artery spasm|
    transposition of coronary artery|coronary artery anomaly nos|congenital stricture of coronary artery|
    dissection of coronary artery|dissection of coronary artery|intravascular ultrasound|coronary artery disease|
    perforation of coronary artery|perforation of coronary artery|coronary artery structure|	
left coronary artery main stem|left coronary artery main stem|congenital coronary|coronary artery rupture|
    coronary artery embolism|coronary artery embolism|arteriovenous fistula|aorta|renal artery|pulmonary artery|
     pulmonary vein|pulmonary vein|pulmonary venous pathway|common iliac artery|coeliac artery|external iliac artery|
     common femoral artery|profunda femoris|femoral artery|popliteal artery|infrapopliteal artery|
     crural artery|crural artery|arterial graft|superior vena cava|balloon angioplasty of vein|device|pulmonary collateral|
    	
distention of artery|distention of artery|pericardial drainage tube|femoropopliteal|superior mesenteric artery",
                       "carotid|carotid|cerebral|subclavian",
                       
                       # Negations 
                       
                       "excluded|excluded|planned"),
                
                
                term, ignore.case = TRUE, perl = TRUE))  

  
  
  
  
   

## Comparing with older codelists

# New codes not in old list

new_aurum <- aurum_coronary_revascularisation_procedures %>%
  filter(!medcodeid %in% aurum_coronary_revascularisation_old$MedCodeId)


# Old codes not in new old list
miss_new_aurum <- aurum_coronary_revascularisation_old %>%
  filter(!MedCodeId %in% aurum_coronary_revascularisation_procedures$medcodeid)




# ================= 3) Create updated code lists ===============================


# Save updated code lists

# Aurum


write.table(aurum_coronary_revascularisation_procedures,
            file = paste0(wd, path_output, "Aurum_Coronary_Revascularisation_Procedures_codelist_20260603.txt"),
            sep = "\t", row.names = FALSE)

write.table(gold_coronary_revascularisation_procedures,
            file = paste0(wd, path_output, "Gold_Coronary_Revascularisation_Procedures_Codelist_20260603.txt"),
            sep = "\t", row.names = FALSE)

