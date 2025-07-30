# ==============================================================================
# Generate code lists for SMI diagnoses
# Author: SM Wu
# Date Created: 2025/06/11
# Date Updated: 2025/07/25
# 
# Details:
# 1) Set up and load data
# 2) Search for new relevant med codes
# 3) Create updated code lists
#
# Inputs:
# 1) Code_Lists/MASTER_Lists/CPRD_Aurum_Product_10Feb2025.txt: Aurum product master code list
# 2) Code_Lists/MASTER_Lists/CPRD_GOLD_Product_23Feb2025.txt: GOLD product master code list
# 3) Code_Lists/MASTER_Lists/Business_Rules_Combined_Change_Log_QOF%2B2024-25_v49.1.xlsm: QOF v49.1 clusters
# 4) OLD_Code_Lists/SMI/Aurum_SMI_codelist_21032024_Alvin.txt: Old Aurum SMI code list
# 5) OLD_Code_Lists/SMI/Gold_SMI_codelist_21032024_Alvin.txt: Old GOLD SMI code list
# 
# Intermediate outputs:
# 1) Code_Lists/SMI/Aurum_other_codes.csv: Potential codes to add for Aurum SMI
# 2) Code_Lists/SMI/Gold_other_codes.csv: Potential codes to add for Gold SMI
# 
# Final Outputs:
# 1) Code_Lists/SMI/Aurum_SMI_codelist_20250725.txt: Updated Aurum SMI code list
# 2) Code_Lists/SMI/Gold_SMI_codelist_20250725.txt: Updated GOLD SMI code list
# 3) Code_Lists/SMI/Aurum_Gold_SMI_new_codes_20250725.txt: Newly added SMI codes for Aurum and GOLD

# ==============================================================================


# ================= 1) Set up and load data ====================================

# Clear memory
rm(list = ls())

# Packages
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(readxl)
library(openxlsx)
library(writexl)

# Set working directory
wd <- "/Volumes/ritd-ag-project-rd00qv-jfhay18/Stephanie/SMI_GLP/" # VPN connection
# wd <- "//live.rd.ucl.ac.uk/ritd-ag-project-rd00qv-jfhay18/Stephanie/SMI_GLP/" #Desktop@UCL
setwd(wd)

# Set input and output paths
path_input <- "Code_Lists/"
path_output <- "Code_Lists/SMI/"

## Load data

# Read in Aurum medical dictionary
cprd_aurum_medical_raw <- 
  read_delim(
    paste0(wd, path_input, "MASTER_Lists/CPRD_Aurum_Medical_10Feb2025.txt"), 
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
    paste0(wd, path_input, "MASTER_Lists/CPRD_GOLD_Medical_23Feb2025.txt"), 
    delim = "\t", escape_double = FALSE, 
    col_types = cols(medcode = col_character(), 
                     readcode = col_character()), 
    trim_ws = TRUE) 
cprd_gold_medical <- cprd_gold_medical_raw %>%
  rename(term = readterm) %>%
  mutate(term = str_to_lower(term))

# Read in QOF clusters to get incentivized SMI codes
qof <- readxl::read_excel(
  paste0(wd, path_input, 
         "MASTER_Lists/Business_Rules_Combined_Change_Log_QOF%2B2024-25_v49.1.xlsm"),
  sheet = "Expanded Cluster List", skip = 14)
# Get unique cluster types
qof_unique <- qof %>% 
  select(`Cluster ID`, `Cluster description`) %>% 
  distinct()
# Subset to SMI clusters
qof_smi <- qof %>%
  filter(`Cluster ID` == "MH_COD")

# Read in old SMI code list from 2024/03/21, setting all col types to character
# Aurum
smi_codelist_aurum_old <- read_delim(
  paste0(wd, path_input, "OLD_Code_Lists/SMI/Aurum_SMI_codelist_21032024_Alvin.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character(), SNOMED = col_character()),
  trim_ws = TRUE) %>%
  # Remove extraneous \\"
  mutate(TermSNOMED = str_replace_all(TermSNOMED, '\\\\\"', ''),
         TermSNOMED = str_replace_all(TermSNOMED, '\\\\', ''),
         TermEMIS = str_replace_all(TermEMIS, '\\\\\"', ''),
         TermEMIS = str_replace_all(TermEMIS, '\\\\', ''),
         TermRead = str_replace_all(TermRead, '\\\\\"', ''),
         TermRead = str_replace_all(TermRead, '\\\\', ''))

# Gold
smi_codelist_gold_old <- read_delim(
  paste0(wd, path_input, "OLD_Code_Lists/SMI/Gold_SMI_codelist_21032024_Alvin.txt"),
  delim = "\t", escape_double = FALSE, col_types = "c", trim_ws = TRUE)


## Sanity check
# # Naomi's code lists
# smi_codelist_aurum_naomi <- read_delim(
#   paste0(wd, path_input, "OLD_Code_Lists/SMI/Aurum_SMI_Naomi.txt"),
#   delim = "\t", escape_double = FALSE, col_types = "c", trim_ws = TRUE)
# 
# # Check between Alvin's and Naomi's lists: none missing
# miss_from_alvin <- smi_codelist_aurum_naomi %>%
#   filter(!(medcodeid %in% smi_codelist_aurum_naomi$medcodeid))
# miss_from_naomi <- smi_codelist_aurum_old %>%
#   filter(!(medcodeid %in% smi_codelist_aurum_old$medcodeid))


# ================= 2) Search for new relevant med codes =======================

# Aurum

# Note: R does not allow variable-length lookbehinds
aurum_smi <- cprd_aurum_medical %>%
  # Inclusions: schizophrenia, bipolar, psychotic
  filter(grepl(paste0(
    "(?i)schizo|psychos|psychot|manic|lithium|delusion|paranoi|bipol|amok|",
    "bouffee delirante|bouffée délirante|capgras|catatonic stupor|mania|",
    "folie a deux|folie à deux|oneirophrenia|paraphrenia|beziehungswahn|",
    "clerambault's|sander's|psychogenic stupor|ideas of reference|",
    "morbid jealousy|thought insertion|mixed affective"), 
    term)) %>%
  # Exclusions: family history, other disorders, substance, no symptom, treatment
  filter(!grepl(paste0(
    # Family history
    "(?i)fh:|family history|child|infant|maternal history|family|member|relative|",
    # Counselling
    "therap|psychosoc|manicurist|counselling|referral|behavioural|honos|",
    "signposting|language|training|assessment|intervention|checklist|monitoring|",
    # Other non-related medical
    "intensive care|hip replacement|pros|surgical|bipolar diathermy|",
    "adverse|allergy|romania|panamanian|",
    # Measurement
    "score|scale|caused|provision|information|sign|",
    # Drug-related
    "behavioral|sedative|seds|bh dis due|behav dis due|solvents|criminal|drg use|",
    "korsakov|korsakoff|substance|drug|alcoh|stimulant|amph|stimulant|coc|opioid|",
    "cannab|inhalant|hallucinogen-induced|oth stims inc caffeine|poison|",
    # Maternal and pregnancy-related
    "mother|partum|puer|postnatal|",
    # Organic
    "senil|dementia|(?<!non[-\\s])\\borganic\\b|org\\.|presbyophrenic|alzheimer|",
    "parkinson|",
    # Other disorders 
    "paranoid personality disorder|schizoid personality disorder|sex|",
    "hypomanic personality disorder|level of psychotism|inventory|major depress|",
    "(?<!schizophrenic[-\\s])(?<!bipolar affective disorder, current episode )",
    "(?<!bipolar disorder, most recent episode )\\bdepression\\b(?!.*\\bbipolar\\b)|",
    "(?<!manic[-\\s])(?<!schizoaffective disorder, )(?<!schizophreniform psychosis, )",
    "(?<!schizoaffective psychosis, )\\bdepressive\\b|schizoid character|",
    "pyro|nymph|klepto|leishmania|hysterical psychosis|subacute infective|",
    "dipsomania|trichotillomania|onychotillomania|pornograph|dwarfism|",
    "secondary psychotic syndrome|symbiotic psychosis|nonpsychotic disorders|",
    "disintegrative|psychosomatic|epilep|character|",
    # Medications
    "lithium|antipsychotic|psychotropic|convalescence|",
    # Negations
    "no evidene of psychosis|face caras|psychotic symptoms absent|no delusion|",
    "nondelusional|no paranoid|no hypomanic|no evidence|nonpsychotic|", 
    "non-psychotic"), 
    term, perl = TRUE))

# Removing specific terms due to SNOMED mapping issues
# exclusions <- c("460273017", "14656", "362781000006116", "31633", "22104", 
#                 "22080", "23963")
exclusions <- c()
aurum_smi <- aurum_smi %>%
  filter(!(medcodeid %in% exclusions))

# Subset to terms not already included in old SMI code list
aurum_new_smi <- aurum_smi %>%
  filter(!(medcodeid %in% smi_codelist_aurum_old$medcodeid))

# Add in classification information and other notes
aurum_new_smi <- aurum_new_smi %>%
  mutate(smi_type = case_when(
    term %in% c("[x]affective psychosis nos", "affective psychosis", 
                "other affective psychosis nos", "affective psychoses",
                "unspecified affective psychoses nos") ~ "other psychosis",
    grepl("schizophrenia-like psychotic", term) ~ "other psychosis",
    grepl("schizophrenia", term) ~ "schizophrenia",
    grepl("schizophrenia reaction", term) ~ "schizophrenia",
    grepl("bipolar", term) ~ "bipolar",
    grepl("manic behav", term) ~ "bipolar",
    .default = "other psychosis"
  ),
  notes = case_when(
    medcodeid %in% c("1807561000006117", "1821481000006114", "1821491000006112", 
                     "1975671000006116", "1975681000006118", "1975691000006115", 
                     "1975711000006117", "1975731000006111", "1975761000006119", 
                     "1975781000006112", "1975821000006118", "1975871000006117", 
                     "1975901000006117", "5247211000006115", "5247261000006117", 
                     "5247401000006117", "5624451000006114", "9326201000006116", 
                     "14472391000006111") ~ "Use for start date if further SMI codes",
    .default = NA_character_
  ))

# Which terms were in the old list but are not included in the new list,
# filtering to those terms in the code dictionary
aurum_smi_miss_from_new <- smi_codelist_aurum_old %>%
  filter(!(medcodeid %in% aurum_smi$medcodeid)) %>% 
  filter(medcodeid %in% cprd_aurum_medical$medcodeid)


# Gold

gold_smi <- cprd_gold_medical %>%
  # Inclusions: schizophrenia, bipolar, psychotic
  filter(grepl(paste0(
    "(?i)schizo|psychos|psychot|manic|lithium|delusion|paranoi|bipol|amok|",
    "bouffee delirante|bouffée délirante|capgras|catatonic stupor|mania|",
    "folie a deux|folie à deux|oneirophrenia|paraphrenia|beziehungswahn|",
    "clerambault's|sander's|psychogenic stupor|ideas of reference|",
    "morbid jealousy|thought insertion|mixed affective"), 
    term)) %>%
  # Exclusions: family history, other disorders, substance, no symptom, treatment
  filter(!grepl(paste0(
    # Family history
    "(?i)fh:|family history|child|infant|maternal history|family|member|relative|",
    # Counselling
    "therap|psychosoc|manicurist|counselling|referral|behavioural|honos|",
    "signposting|language|training|assessment|intervention|checklist|monitoring|",
    # Other non-related medical
    "intensive care|hip replacement|pros|surgical|bipolar diathermy|",
    "adverse|allergy|romania|panamanian|",
    # Measurement
    "score|scale|caused|provision|information|sign|",
    # Drug-related
    "behavioral|sedative|seds|bh dis due|behav dis due|solvents|criminal|drg use|",
    "korsakov|korsakoff|substance|drug|alcoh|stimulant|amph|stimulant|coc|opioid|",
    "cannab|inhalant|hallucinogen-induced|oth stims inc caffeine|poison|",
    # Maternal and pregnancy-related
    "mother|partum|puer|postnatal|",
    # Organic
    "senil|dementia|(?<!non[-\\s])\\borganic\\b|org\\.|presbyophrenic|alzheimer|",
    "parkinson|",
    # Other disorders 
    "paranoid personality disorder|schizoid personality disorder|sex|",
    "hypomanic personality disorder|level of psychotism|inventory|major depress|",
    "(?<!schizophrenic[-\\s])(?<!bipolar affective disorder, current episode )",
    "(?<!bipolar disorder, most recent episode )\\bdepression\\b(?!.*\\bbipolar\\b)|",
    "(?<!manic[-\\s])(?<!schizoaffective disorder, )(?<!schizophreniform psychosis, )",
    "(?<!schizoaffective psychosis, )\\bdepressive\\b|schizoid character|",
    "pyro|nymph|klepto|leishmania|hysterical psychosis|subacute infective|",
    "dipsomania|trichotillomania|onychotillomania|pornograph|dwarfism|",
    "secondary psychotic syndrome|symbiotic psychosis|nonpsychotic disorders|",
    "disintegrative|psychosomatic|epilep|character|",
    # Medications
    "lithium|antipsychotic|psychotropic|convalescence|",
    # Negations
    "no evidene of psychosis|face caras|psychotic symptoms absent|no delusion|",
    "nondelusional|no paranoid|no hypomanic|no evidence|nonpsychotic|", 
    "non-psychotic"), 
    term, perl = TRUE))

# Removing specific terms due to SNOMED mapping issues
gold_smi <- gold_smi %>%
  filter(!(medcode %in% exclusions))

# Subset to terms not already included in old SMI code list
gold_new_smi <- gold_smi %>%
  filter(!(medcode %in% smi_codelist_gold_old$medcode))

# Add in classification information
gold_new_smi <- gold_new_smi %>%
  mutate(smi_type = case_when(
    term %in% c("[x]affective psychosis nos", "affective psychosis", 
                "other affective psychosis nos", "affective psychoses",
                "unspecified affective psychoses nos") ~ "other psychosis",
    grepl("schizophrenia-like psychotic", term) ~ "other psychosis",
    grepl("schizophrenia", term) ~ "schizophrenia",
    grepl("schizophrenia reaction", term) ~ "schizophrenia",
    grepl("bipolar", term) ~ "bipolar",
    grepl("manic behav", term) ~ "bipolar",
    .default = "other psychosis"
  ))

# Which terms were in the old list but are not included in the new list,
# filtering to those terms in the code dictionary
gold_smi_miss_from_new <- smi_codelist_gold_old %>%
  filter(!(medcode %in% gold_smi$medcode)) %>% 
  filter(medcode %in% cprd_gold_medical$medcode)


# Combine Aurum and GOLD new SMI codes
aurum_gold_SMI_new_codes <- list(
  Aurum = aurum_new_smi,
  Gold = gold_new_smi)
# # Save lists of newly added codes into one .xlsx file on separate tabs
# write.xlsx(aurum_gold_SMI_new_codes,
#            file = paste0(wd, path_output, "Aurum_Gold_SMI_new_codes_25Jul2025.xlsx"),
#            overwrite = TRUE)



### OLDER CODE [DO NOT RUN]

# # Which terms were not in the old list
# setdiff(aurum_smi$term, smi_codelist_aurum_old$TermRead)
# # Which terms were in the old list but are not in the current list
# smi_codelist_aurum_old$TermSNOMED[
#   setdiff(smi_codelist_aurum_old$medcodeid, aurum_smi$medcodeid)]

# # Any terms in older lists but not in searched terms
# aurum_gold_smi_miss_codes <- list(
#   Aurum = aurum_smi_miss_from_new,
#   Gold = gold_smi_miss_from_new)
#
# # Save lists of missing codes into one .xlsx file on separate tabs
# # These were last reviewed by JH on Jul 22, 2025
# writexl::write_xlsx(aurum_gold_smi_miss_codes,
#                     path = paste0(wd, path_output, 
#                                   "Aurum_Gold_SMI_missing_codes_22Jul2025.xlsx"))
#
# # Any terms in QOF but not in searched terms
# add_from_qof <- qof_smi %>%
#   filter(!(`SNOMED concept ID` %in% aurum_smi$medcodeid))
# add_from_qof_gold <- qof_smi %>%
#   filter(!(`SNOMED concept ID` %in% gold_smi$medcode))
# 
# # Save lists of potential codes to add for Aurum and GOLD 
# # These were last reviewed by JH on Jul 14, 2025
# write.csv(aurum_new_smi, file = paste0(wd, path_output, "Aurum_SMI_other_codes.csv"))
# write.csv(gold_new_smi, file = paste0(wd, path_output, "Gold_SMI_other_codes.csv"))


# ================= 3) Create updated code lists ===============================

# # Use old code lists
# smi_codelist_aurum_new <- smi_codelist_aurum_old
# smi_codelist_gold_new <- smi_codelist_gold_old


# Create updated code lists

# Aurum
# Merge in information from old code lists
smi_codelist_aurum_new <- aurum_smi %>%
  # Add in the original case-sensitive readcode terms from dictionary
  left_join(cprd_aurum_medical_raw %>% select(MedCodeId, Term), 
            by = join_by("medcodeid" == "MedCodeId")) %>%
  # Add in information on Group and SNOMED description from old code lists
  left_join(smi_codelist_aurum_old, by = "medcodeid") %>%
  # For the read term description, use the dictionary version
  select(-TermRead) %>%
  rename(TermRead = Term) %>%
  # Select variables to keep
  select(medcodeid, TermRead, TermSNOMED, TermEMIS, SNOMED, OriginalReadCode, 
         CleansedReadCode, Group, Observations)
# Fill in missing Group information for newly added SMI codes and fix incorrect 
# Group info
smi_codelist_aurum_new <- smi_codelist_aurum_new %>%
  left_join(aurum_new_smi %>% select(medcodeid, smi_type), 
            by = "medcodeid") %>%
  mutate(Group = coalesce(Group, smi_type),
         Group = case_when(
           medcodeid == "1227584015" ~ "bipolar",  
           # Update cyclic schizophrenia as "schizophrenia"
           medcodeid %in% c("294773015", "376251000006112") ~ "schizophrenia",
           # Update affective psychosis group to "other psychosis"
           medcodeid %in% c("294897018", "294898011", "294902017", 
                            "362781000006116") ~ "other psychosis",
           .default = Group)) %>%
  select(-smi_type)
  

# Gold
smi_codelist_gold_new <- gold_smi %>%
  # Add in the original case-sensitive readcode terms from dictionary
  left_join(cprd_gold_medical_raw %>% select(medcode, readterm), 
            by = join_by("medcode")) %>%
  # Add in information on Group and SNOMED description from old code lists
  left_join(smi_codelist_gold_old, by = "medcode") %>%
  # For the read term description, use the dictionary version
  select(-Term) %>%
  rename(Term = readterm, OriginalReadCode = `Read.code`, 
         CleansedReadCode = readcode) %>%
  # Select variables to keep
  select(medcode, Term, OriginalReadCode, 
         CleansedReadCode, Group, clinicalevents, immunisationevents, 
         referralevents, testevents, databaserelease)
# Fill in missing Group information for newly added SMI codes
smi_codelist_gold_new <- smi_codelist_gold_new %>%
  left_join(gold_new_smi %>% select(medcode, smi_type), 
            by = "medcode") %>%
  mutate(Group = coalesce(Group, smi_type),
         Group = case_when(
           medcode %in% c("22080", "23963") ~ "bipolar",  
           # Update cyclic schizophrenia as "schizophrenia"
           medcode %in% c("104763", "99000") ~ "schizophrenia",
           # Update affective psychosis group to "other psychosis"
           medcode %in% c("31633", "14656", "33425", "41992", 
                          "54607") ~ "other psychosis",
           .default = Group)) %>%
  select(-smi_type)


# # Save updated code lists
# write.table(smi_codelist_aurum_new,
#             file = paste0(wd, path_output, "Aurum_SMI_codelist_20250725.txt"),
#             sep = "\t", row.names = FALSE)
# 
# write.table(smi_codelist_gold_new,
#             file = paste0(wd, path_output, "Gold_SMI_codelist_20250725.txt"),
#             sep = "\t", row.names = FALSE)



