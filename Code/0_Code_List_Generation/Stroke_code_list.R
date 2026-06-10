# Generate code list for stroke 
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
# 3) Code_Lists/Stroke/Old/Aurum_Stroke_codelist_20240422_Alvin.txt : Old Aurum stroke list
# 4) Code_Lists/Stroke/Old/Gold_Stroke_codelist_20240422_Alvin.txt : Old Gold stroke list 
# 5) Code_Lists/Stroke/Old/Aurum_Stroke_codelist_20260326_Exeter.txt : Old Exeter aurum stroke list 


# Final Outputs:

# 1) Code_Lists/Stroke/Aurum_Stroke_20260605.txt : Aurum Stroke codelist 
# 2) Code_Lists/Stroke/Gold_Stroke_20260605.txt : Gold Stroke codelist 




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
# path_output <- "Code_Lists/Stroke/

### For running in Data Safe Haven

# Set working directory

wd <- "S:/CDSTP_CPRD_25_005368/" 
setwd(wd)

# Set input and output paths

path_input <- "SMI_GLP/Code_Lists/"
path_output <- "SMI_GLP/Code_Lists/Stroke/"

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



# Read in old stroke code lists, setting all col types to character

# Aurum

stroke_aurum_old <- read_delim(
  paste0(wd, path_input, "Stroke/Old/Aurum_Stroke_codelist_20240422_Alvin.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character()), trim_ws = TRUE)



# Gold

stroke_gold_old <- read_delim(
  paste0(wd, path_input, "Stroke/Old/Gold_Stroke_codelist_20240422_Alvin.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character()), trim_ws = TRUE)


# Exeter Aurum 

stroke_aurum_exeter_old <- read_delim(
  paste0(wd, path_input, "Stroke/Old/Aurum_Stroke_codelist_20260326_Exeter.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(MedCodeId = col_character()), trim_ws = TRUE)


# ================= 2) Search for new relevant med codes =======================

# Aurum

aurum_stroke <- cprd_aurum_medical %>%
  # Inclusion -  stroke related terms 
  
  filter(grepl(paste0("(?i) 
   
   stroke|stroke|cerebral haemorrhage|cerebral haemorrhage|cerebral hemorrhage|
                      cerebrovascular accident|cerebrovascular accident|CVA|cerebral infarction|
                      Cerebral arterial occlusion|Cerebral arterial occlusion|Cerebellar infarction|
                      Cerebral thrombosis|Cerebral thrombosis|lacunar infarction|Infarction - cerebral|
                      Cerebellar haemorrhage|Cerebellar haemorrhage|Infarction of basal ganglia|cerebral artery occlusion|
                      Anterior cerebral artery syndrome|brainstem infarction|basilar artery occlusion|occlusion and stenosis of anterior cerebral artery|
                      cerebrl infarctn|cerebrl infarctn|infarct due unsp occlus/stenos precerebr arteries|cerebral embolism|cerebral embolus|cerebral venous thrombosis|
                      vertebral artery occlusion|vertebral artery occlusion",
                      
                      "cerebral.*infarct|cerebral.*infarct|infarct.*cerebral|occlusion.*cerebral|
                      thrombosis.*cerebral|thrombosis.*cerebral|embolus.*cerebral"),
               
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
other cerebral haemorrhage following injury nos|cerebral haemorrhage following injury nos",
                       
        # Family history
        
        "fh|fh|fh:|family history|family history:", 
        
        # Pregnancy
        
        "puerperium|puerperium|pregnancy|pregnant|cerebral haemorrhage due to birth trauma|
        	cerebral haemorrhage due to birth injury|cerebral haemorrhage due to birth injury|
        perinatal arterial ischaemic stroke|perinatal arterial ischaemic stroke|cerebral haemorrhage unspecified, due to birth trauma|
        	cerebral haemorrhage unspecified, due to birth trauma|cerebral haemorrhage unspecified, due to birth trauma",
        
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
        
        "no history of|no history of|no h/o:|stroke prevention|stroke test negative|suspected stroke|stroke test negative|suspected cerebrovascular accident", 
        
        # Process of care 
        
        "screen|screen|risk|stroke monitoring invite|chads2|stroke(dhds)prevention|stroke scale|
        stroke/transient ischaemic attack monitoring|stroke/transient ischaemic attack monitoring|
        cha2ds2-vasc|cha2ds2-vasc|pass (postural assessment scale for stroke patients) score|
        exception reporting|exception reporting|excepted from stroke|has-bled|
        hypertension, abnormal renal/liver function, stroke, bleeding history or predisposition, labile international normalized ratio, elderly over 65, and drugs/alcohol concomitantly score|hypertension, abnormal renal/liver function, stroke, bleeding history or predisposition, labile international normalized ratio, elderly over 65, and drugs/alcohol concomitantly score|
        	stroke impact scale version|stroke impact scale version|stroke tom|qof stroke|
        pass (postural assessment scale for stroke patients) score|pass (postural assessment scale for stroke patients) score|
        diabetes, heart disease and stroke(dhds)prevention pilot project|diabetes, heart disease and stroke(dhds)prevention pilot project|
        cvag|stroke test|cvag|bcva - best corrected visual acuity|qof (quality and outcomes framework) stroke and transient ischaemic attack quality indicator-related care invitation|
        qof (quality and outcomes framework) stroke and transient ischaemic attack quality indicator-related care invitation using preferred method of communication|	
cva prevention|cva prevention|qof (quality and outcomes framework) stroke and transient ischaemic attack quality indicator-related care invitation|
        hyperten, abnorm renal/liver funct, stroke, bled score|hyperten, abnorm renal/liver funct, stroke, bled score|age, bp, clinical feat, duration, diabetes 2 stroke rsk scre|provision of written information about stroke"),
        
              term, ignore.case = TRUE, perl = TRUE)) 

# Gold

gold_stroke <- cprd_gold_medical %>%

# Inclusion -  stroke related terms 

filter(grepl(paste0("(?i) 
   
   stroke|stroke|cerebral haemorrhage|cerebral haemorrhage|cerebral hemorrhage|
                      cerebrovascular accident|cerebrovascular accident|CVA|cerebral infarction|
                      Cerebral arterial occlusion|Cerebral arterial occlusion|Cerebellar infarction|
                      Cerebral thrombosis|Cerebral thrombosis|lacunar infarction|Infarction - cerebral|
                      Cerebellar haemorrhage|Cerebellar haemorrhage|Infarction of basal ganglia|cerebral artery occlusion|
                      Anterior cerebral artery syndrome|brainstem infarction|basilar artery occlusion|occlusion and stenosis of anterior cerebral artery|
                    cerebrl infarctn|cerebrl infarctn|infarct due unsp occlus/stenos precerebr arteries|cerebral embolism|cerebral embolus|cerebral venous thrombosis|	
vertebral artery occlusion|vertebral artery occlusion",
                    
                    "cerebral.*infarct|cerebral.*infarct|infarct.*cerebral|occlusion.*cerebral|
                    thrombosis.*cerebral|thrombosis.*cerebral|embolus.*cerebral"),
             
             term, ignore.case = TRUE, perl = TRUE)) %>% 
  
  
  # Exclusions 
  
  filter(!grepl(paste0("(?!)
                       
        # Unrelated terms
        
        
        heat stroke|heat stroke|sunstroke|stroke volume|heatstroke|stroke index|	
diabetes, heart disease and stroke|diabetes, heart disease and stroke|cerebral palsy|mcvay repair of inguinal hernia|
                       mcvay repair of inguinal hernia|mitochond encephalopathy, lact acidosis & strokelike episode|cva tenderness",
                       
                       # Head injury
                       
                       "head injury|head injury|traumatic cerebral haemorrhage|traumatic intracerebral hemorrhage|
          traumatic intracerebral haemorrhage|traumatic intracerebral haemorrhage|cerebral haemorrhage due to trauma|	
cerebral haemorrhage following injury|cerebral haemorrhage following injury|	
other cerebral haemorrhage following injury nos|cerebral haemorrhage following injury nos",
                       
                       # Family history
                       
                       "fh|fh|fh:|family history|family history:", 
                       
                       # Pregnancy
                       
                       "puerperium|puerperium|pregnancy|pregnant|cerebral haemorrhage due to birth trauma|
        	cerebral haemorrhage due to birth injury|cerebral haemorrhage due to birth injury|
        perinatal arterial ischaemic stroke|perinatal arterial ischaemic stroke|cerebral haemorrhage unspecified, due to birth trauma|
        	cerebral haemorrhage unspecified, due to birth trauma|cerebral haemorrhage unspecified, due to birth trauma",
                       
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
                       
                       "no history of|no history of|no h/o:|stroke prevention|stroke test negative|suspected stroke|stroke test negative|suspected cerebrovascular accident", 
                       
                       # Process of care 
                       
                       "screen|screen|risk|stroke monitoring invite|chads2|stroke(dhds)prevention|stroke scale|
        stroke/transient ischaemic attack monitoring|stroke/transient ischaemic attack monitoring|
        cha2ds2-vasc|cha2ds2-vasc|pass (postural assessment scale for stroke patients) score|
        exception reporting|exception reporting|excepted from stroke|has-bled|
        hypertension, abnormal renal/liver function, stroke, bleeding history or predisposition, labile international normalized ratio, elderly over 65, and drugs/alcohol concomitantly score|hypertension, abnormal renal/liver function, stroke, bleeding history or predisposition, labile international normalized ratio, elderly over 65, and drugs/alcohol concomitantly score|
        	stroke impact scale version|stroke impact scale version|stroke tom|qof stroke|
        pass (postural assessment scale for stroke patients) score|pass (postural assessment scale for stroke patients) score|
        diabetes, heart disease and stroke(dhds)prevention pilot project|diabetes, heart disease and stroke(dhds)prevention pilot project|
        cvag|stroke test|cvag|bcva - best corrected visual acuity|qof (quality and outcomes framework) stroke and transient ischaemic attack quality indicator-related care invitation|
        qof (quality and outcomes framework) stroke and transient ischaemic attack quality indicator-related care invitation using preferred method of communication|	
cva prevention|cva prevention|qof (quality and outcomes framework) stroke and transient ischaemic attack quality indicator-related care invitation|
                       hyperten, abnorm renal/liver funct, stroke, bled score|hyperten, abnorm renal/liver funct, stroke, bled score|age, bp, clinical feat, duration, diabetes 2 stroke rsk scre|provision of written information about stroke"),
                
                term, ignore.case = TRUE, perl = TRUE)) 



## Comparing with older codelists

# New codes not in old list

# Compared with Exeter code 

new_aurum <- aurum_stroke %>%
  filter(!medcodeid %in% stroke_aurum_exeter_old$MedCodeId)


# Compared with Alvin code 

new_aurum2 <- aurum_stroke %>%
  filter(!medcodeid %in% stroke_aurum_old$medcodeid)

new_gold <- gold_stroke %>%
  filter(!medcode %in% stroke_gold_old$medcode)

# Old codes not in new old list

# Compared with  Exeter

miss_new_aurum <- stroke_aurum_exeter_old %>%
  filter(!MedCodeId %in% aurum_stroke$medcodeid)

# compared with Alvin

miss_new_aurum2 <- stroke_aurum_old %>% 
  filter(!medcodeid %in% aurum_stroke$medcodeid)

miss_new_gold <- stroke_gold_old %>%
  filter(!medcode %in% gold_stroke$medcode)  



# ================= 3) Create updated code lists ===============================




write.table(aurum_stroke,
            file = paste0(wd, path_output, "Aurum_Stroke_Code_List_20260605.txt"),
            sep = "\t", row.names = FALSE)

write.table(gold_stroke,
            file = paste0(wd, path_output, "Gold_Stroke_Code_List_20260605.txt"),
            sep = "\t", row.names = FALSE)
