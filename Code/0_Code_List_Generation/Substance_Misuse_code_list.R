# Generate code list for substance misuse  
# Author: S Picton & S Wu
# Date created: 2026/03/06
# Date updated: 2026/03/06

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
# 3) Code_Lists/Substance_Misuse/Old/Aurum_Substance_Misuse_codelist_Naomi.rdata : Old Aurum substance misuse code list 
# 4) Code_Lists/Substance_Misuse/Old/Gold_Substance_Misuse_codelist_Naomi.rdata : Old Gold substance misuse code list 
#
# Final Outputs:

# 1) Code_Lists/Substance_Misuse/Aurum_Substance_misuse_codelist_20260306.txt Updated Aurum substance misuse code list 
# 2) Code_Lists/Substance_Misuse/Gold_Substance_misuse_codelist_20260306.txt : Updated Gold substance misuse code list 
# 3) Code_Lists/Substance_Misuse/Aurum_Gold_Substance_misuse_codelist_20260306.txt : Combined Aurum & Gold substance misuse code list

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
# # Set working directory
# wd <- "/Volumes/ritd-ag-project-rd00qv-jfhay18/" # VPN connection
# # wd <- "//live.rd.ucl.ac.uk/ritd-ag-project-rd00qv-jfhay18/" #Desktop@UCL
# setwd(wd)
# 
# # Set input and output paths
# path_input <- "Code_Lists/"
# path_output <- "Code_Lists/Substance_Misuse/"


### For running in Data Safe Haven
# Set working directory
wd <- "S:/CDSTP_CPRD_25_005368/" 
setwd(wd)

# Set input and output paths
path_input <- "SMI_GLP/Code_Lists/"
path_output <- "SMI_GLP/Code_Lists/Substance_Misuse/"



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


# Read in old Substance Misuse code lists, setting all col types to character

# Aurum


load("SMI_GLP/Code_Lists/Substance_Misuse/Old/Aurum_Substance_Misuse_codelist_Naomi.rdata")
  
substance_misuse_aurum_old <- AurumDrugAndAlcohol
  
  
# Gold

load("SMI_GLP/Code_Lists/Substance_Misuse/Old/Gold_Substance_Misuse_codelist_Naomi.rdata")

substance_misuse_gold_old <- GoldDrugAndAlcohol


# ================= 2) Search for new relevant med codes =======================

# Aurum

aurum_substance_misuse <- cprd_aurum_medical  %>%
  
  # Inclusion - Substance misuse related terms 
  
  filter(grepl(paste0("(?i)substance misuse|drug user|drug abuse|drug misuse|substance misuse|
  heroin|cocaine use|cocaine dependence|cocaine misuse|cocaine behav|cocaine abuse|
  cocaine drugs|cocaine induced|cocaine intoxication|cocaine withdrawal|
  cocaine-induced|cocaine freebase|cocaine-related|cocaine overdose|cocaine disorder|
  |amphetamine dependence|amphetamine misuse|amphetamine use|amphetamine abuse|
  amphetamine poisoning|poison amphetamine|misuse amphetamine|drugs amphetamine|reaction to amphetamine|
  amphetamine positive|use amphetamine|amphetamine-induced|amphetamine overdose|
  amphetamine or psychostimulant|amphetamine or psychostimulant dependence|
  caused by amphetamine|cannabis|ecstasy|benzodiazepine misuse|
  hallucinogen|solvent misuse|opiate misuse|tranquilliser misuse|
  barbiturate misuse|antidepressant misuse|misuses drugs|cannabinoids|
  addict|injects drugs|illicit drug|methadone use|methadone dependence|methadone overdose|
  methadone misuse|therapy methadone|methadone positive|education methadone|methadone poison|
  adverse methadone|consumption methadone|methadone product|methadone supervised|
  history of methadone|misuse of methadone|methadone misuse|
  urine buprenorphine|drug dependence|substance dependence|
  opioid antagonist|opioid agonist|drug withdrawal|
  drug intoxication|nondependent abuse|drug addiction detoxification therapy|
  mental & behav cocaine|opioids:|hallucinogens:|seds/hypntcs:|solvents:|
  intravenous drug use|substance misuse annual review|cocaine:|opiate:|psychoac subs:|
  psych subs:|opioid dependence|hallucinogen dependence|opioid:|
  glue sniffing|hallucinogen abuse|opioid abuse|heroin misuse|benzodiazepine dependence|
  hallucinogen misuse|barbiturate misuse|anti-depressant misuse|misused drugs|
  substance use|injecting drugs|misuse of diamorphine|
  psych sbs:|psychoa sbs:|psych subs:|abuse of antidepressants|
  sedative dependence|glue sniffing|misuse of drugs|drug dependence|drugs service|
  substance use|syringe exchange|needle exchange|misuse of heroin|misuse of opium|
  misuse of opiates|opioids:|drug dependence|intravenous drug use|
  take home naloxone|mlti drg use/oth|reduced drugs misuse|
  sedative dependence|barbiturate dependence|opioid antagonist therapy|
  drug dependence|referral to community drug dependency team|
  substance use|misuse of opiates|misuse of morphine|
  misuse of dihydrocodeine|misuse of oxycodone|misuse of fentanyl|misuse of codeine|
  misuse of tramadol|misuse of benzodiazepines|misuse of diazepam|misuse of nitrazepam|
  misuse of lorazepam|misuse of temazepam|misuse of oxazepam|misuse of methamphetamine|
  misuse of midazolam|misuse of dexamphetamine|misuse of mdma|misuse of solvents|
  misuse of cocaine|misuse of psiloybin|misuse of amyl nitrate|misuse of ketamine")
               , term)) %>%

  # Exclusion     
filter(!grepl(paste0(

# Not related to substance misuse

"(?!)h/o: respirator dependence|h/o: machine dependence nos|dependence on renal dialysis|
dependence on enabling machine or device|aspirator dependence|respirator dependence|
wheelchair|machine|care provider|iron lung|possum|chamber|prosys independence|
extra sticky|personal independence|independence training|walking stick|
seeing eye dog|promotion of independence|maintaining independence|hemodialysis|
haemodialysis|client independence|ventilator|ventilation|oxygen|dialysis|steroid|
extra dressing",


# Alcohol related terms 

 "alcohol|sadq|substance misuse of alcohol",
  
  
# Smoking related terms
  
"tobacco|fagerstrom|nicotine",

# Family history 

"maternal drug abuse|carer of a person|family history of|child|	
witness to|care provider",
  
# Neonatal / infancy
  
"newborn|neonatal|fetal|foetal",

# Process of care 

"questionnaire score|sadq|screening test|urine level|audit score|
honosca-cr|quantitative screen|northwick park|level of dependence|
honosca-sr|honosca",

# Negations

"declined to give|assessment declined|	
urine methadone negative|	
does not engage in|urine negative|no history of substance misuse|no misuse"), 
term, perl = TRUE)) %>%



# Remove negative urine screening tests

filter(!grepl("^urine test(?! positive)",
              term, perl = TRUE))

# Gold 

gold_substance_misuse <- cprd_gold_medical %>%
  
  # Inclusion - Substance misuse related terms
  
  filter(grepl(paste0("(?i)substance misuse|drug user|drug abuse|drug misuse|substance abuse|
  substance dependence|
  heroin|cocaine use|cocaine dependence|cocaine misuse|cocaine behav|cocaine abuse|
  cocaine drugs|cocaine induced|cocaine intoxication|cocaine withdrawal|
  cocaine-induced|cocaine freebase|cocaine-related|cocaine overdose|cocaine disorder|
  |amphetamine dependence|amphetamine misuse|amphetamine use|amphetamine abuse|
  amphetamine poisoning|poison amphetamine|misuse amphetamine|drugs amphetamine|reaction to amphetamine|
  amphetamine positive|use amphetamine|amphetamine-induced|amphetamine overdose|
  amphetamine or psychostimulant|amphetamine or psychostimulant dependence|
  caused by amphetamine|cannabis|ecstasy|benzodiazepine misuse|cannabinoids|
  hallucinogen|solvent misuse|opiate misuse|tranquilliser misuse|
  barbiturate misuse|antidepressant misuse|misuses drugs|
  addict|injects drugs|illicit drug|methadone use|methadone dependence|methadone overdose|
  methadone misuse|therapy methadone|methadone positive|education methadone|methadone poison|
  adverse methadone|consumption methadone|methadone product|methadone supervised|
  history of methadone|misuse of methadone|methadone misuse|
  urine buprenorphine|drug dependence|
  opioid antagonist|opioid agonist|drug withdrawal|
  drug intoxication|nondependent abuse|drug addiction detoxification therapy|
  mental & behav cocaine|opioids:|hallucinogens:|seds/hypntcs:|substance misuse annual review|
  intravenous drug use|solvents:|cocaine:|opiate:|psychoac subs:| psych subs:|
  misuse of drugs|opioid dependence|barbiturate dependence|sedative dependence|
  glue sniffing|benzodiazepine dependence|hallucinogen abuse|opioid abuse|
  heroin misuse|barbiturate misuse|drug dependence|misused drugs in past|	
glue sniffing dependence|heroin misuse|hallucinogen dependence|anti-depressant misuse|
  addiction - opioids|misuse of prescription only drugs|hallucinogen misuse")
               , term)) %>%
  
  # Exclusion     
  filter(!grepl(paste0(
    
# Not related to substance misuse
    
"(?!)h/o: respirator dependence|h/o: machine dependence nos|dependence on renal dialysis|
dependence on enabling machine or device|aspirator dependence|respirator dependence|
wheelchair|machine|care provider|iron lung|possum|chamber|prosys independence|
extra sticky|personal independence|independence training|walking stick|
seeing eye dog|promotion of independence|maintaining independence|hemodialysis|
haemodialysis|client independence|ventilator|ventilation|oxygen|dialysis|steroid|
extra dressing",
    
    
# Alcohol related terms 
    
    "alcohol|sadq|substance misuse of alcohol",
    

# Smoking related terms
    
    "tobacco|fagerstrom|nicotine",
    
# Family history 
    
    "maternal drug abuse|carer of a person|family history of|child|	
witness to|care provider|witness to adult substance misuse",
    
# Neonatal / infancy
    
    "newborn|neonatal|fetal|foetal",

# Process of care 

"questionnaire score|sadq|screening test|urine level|audit score|
honosca-cr|quantitative screen|northwick park|level of dependence|
honosca-sr|honosca", 

# Negations
    
    "declined to give|assessment declined|	
urine methadone negative|	
does not engage in|urine negative|no history of substance misuse"), 
    term, perl = TRUE)) %>%

# Remove negative urine screening tests
  
  filter(!grepl("^urine test(?! positive)",
                term, perl = TRUE))

## Comparing with older codelists

# New codes not in old list
new_aurum <- aurum_substance_misuse %>%
  filter(!medcodeid %in% substance_misuse_aurum_old$medcodeid)

new_gold <- gold_substance_misuse %>%
  filter(!medcode %in% substance_misuse_gold_old$medcode)

# Old codes not in new old list

miss_new_aurum <- substance_misuse_aurum_old %>%
  filter(!medcodeid %in% aurum_substance_misuse$medcodeid)


miss_new_gold <- substance_misuse_gold_old %>%
  filter(!medcode %in% gold_substance_misuse$medcode)


## # ================= 3) Create updated code lists ===============================

# Create updated code lists

# Aurum
substance_misuse_codelist_aurum_new <- aurum_substance_misuse

# Gold
substance_misuse_codelist_gold_new <- gold_substance_misuse

# Save updated code lists

write.table(substance_misuse_codelist_aurum_new,
            file = paste0(wd, path_output, "Aurum_Substance_misuse_codelist_20260306.txt"),
            sep = "\t", row.names = FALSE)

write.table(substance_misuse_codelist_gold_new,
            file = paste0(wd, path_output, "Gold_Substance_misuse_codelist_20260306.txt"),
            sep = "\t", row.names = FALSE)

# Combine Aurum and GOLD updated code lists
temp_aurum <- substance_misuse_codelist_aurum_new %>%
  rename(medcode = medcodeid, readcode = CleansedReadCode) %>%
  select(medcode, term)
temp_gold <- substance_misuse_codelist_gold_new %>%
  select(medcode, term)
temp_both <- rbind(temp_aurum, temp_gold)
aurum_gold_misuse_misuse_new <- temp_both %>% distinct()


# Save lists of new combined codelist into one .xlsx file



write_xlsx(aurum_gold_substance_misuse_new,
           file = paste0(wd, path_output, "Aurum_Gold_Substance_misuse_codelist_20260306.xlsx")

           
# # Combine Aurum and GOLD into one file with a column specifying database
substance_misuse_codelist_aurum_new$database <- "Aurum"
substance_misuse_codelist_gold_new$database <- "Gold"
substance_misuse_codelist_aurum_gold_new <- rbind(
substance_misuse_codelist_aurum_new %>% 
        rename(medcode = medcodeid) %>%
            select(medcode, term, database), 
          substance_misuse_codelist_gold_new %>%
        select(medcode, term, database))



# # Save combined code list
write.table(substance_misuse_codelist_aurum_gold_new,
            file = paste0(wd, path_output, "Aurum_Gold_Substance_misuse_codelist_20260306.txt"),
            sep = "\t", row.names = FALSE)

