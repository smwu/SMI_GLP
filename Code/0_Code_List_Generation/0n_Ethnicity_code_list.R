# ==============================================================================
# Generate code lists for ethnicity
# Author: SM Wu
# Date Created: 2025/08/01
# Date Updated: 2025/08/01
# 
# Details:
# 1) Set up and load data
# 2) Search for new relevant med codes
# 3) Create updated code lists
#
# Inputs:
# 1) Code_Lists/MASTER_Lists/CPRD_Aurum_Medical_10Feb2025.txt: Aurum medical master code list
# 2) Code_Lists/MASTER_Lists/CPRD_GOLD_Medical_23Feb2025.txt: GOLD medical master code list
# 3) Code_Lists/Ethnicity/Old/Aurum_Ethnicity_codelist_20230724_Alvin.txt: Old Aurum Ethnicity code list
# 4) Code_Lists/Ethnicity/Old/Gold_Ethnicity_codelist_20230724_Alvin.txt: Old GOLD Ethnicity code list
# 
# Intermediate outputs:
# 1) 
# 
# Final Outputs:
# 1) Code_Lists/Ethnicity/Aurum_Ethnicity_codelist_20250725.txt: Updated Aurum Ethnicity code list
# 2) Code_Lists/Ethnicity/Gold_Ethnicity_codelist_20250725.txt: Updated GOLD Ethnicity code list
# 3) Code_Lists/Ethnicity/Aurum_Gold_Ethnicity_new_codes_20250725.txt: Newly added Ethnicity codes for Aurum and GOLD

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


# Set working directory
wd <- "/Volumes/ritd-ag-project-rd00qv-jfhay18/Stephanie/SMI_GLP/" # VPN connection
# wd <- "//live.rd.ucl.ac.uk/ritd-ag-project-rd00qv-jfhay18/Stephanie/SMI_GLP/" #Desktop@UCL
setwd(wd)

# Set input and output paths
path_input <- "Code_Lists/"
path_output <- "Code_Lists/Ethnicity/"

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


# Read in old Ethnicity code list from 2024/03/21, setting all col types to character
# Aurum
ethn_codelist_aurum_old <- read_delim(
  paste0(wd, path_input, "Ethnicity/Old/Aurum_Ethnicity_codelist_20230724_Alvin.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character()),
  trim_ws = TRUE)

# Gold
ethn_codelist_gold_old <- read_delim(
  paste0(wd, path_input, "Ethnicity/Old/Gold_Ethnicity_codelist_20230724_Alvin.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character()),
  trim_ws = TRUE)


# ================= 2) Search for new relevant med codes =======================

# Aurum

# Note: R does not allow variable-length lookbehinds
aurum_ethn <- cprd_aurum_medical %>%
  # Inclusions: language, interpreter
  filter(grepl("(?i)language|interpreter", term)) %>%
  # Exclusions: English language, assessments, disorders, negations, generic
  filter(!grepl(
    paste0("(?i)child|parental|clinic|second|inappropriate|foul|body|further|",
           # assessment and disorders
           "test|difficulty|disorders?|delay|scale|speech|assistant|score|",
           "alphabet|therapy|sign|impairment|nos|referral|therapist|telephone|",
           "measure|verbal language|development|inventory|scheme|remediation|",
           "assess|use language|comprehend|use of language|ability|finding|",
           "provision|barrier|advice|braille|request|analysis|schizophrenic|",
           "exercises|approach|regimes|stimulation|element|program|observation|",
           # negations
           "refus|not needed|not recorded|english|not present|not available|",
           "declined|",
           # generic language entries
           "read$|spoken$|spoken - finding|preferred spoken language|^language$|",
           "language commonly spoken in europe|world languages|written language$|",
           "^interpreter$|first language|language finding|interpreter booked|",
           "indo-european|afro-asiatic|translator/interpreter|presence|",
           "^interpreter needed$|^interpreter present$|^language interpreter$|",
           "^need for interpreter$|^main spoken language$|additional|",
           "^uses language$|other interpreter"), 
    term, perl = TRUE))

# Subset to terms not already included in old Ethnicity code list
aurum_new_ethn <- aurum_ethn %>%
  filter(!(medcodeid %in% ethn_codelist_aurum_old$medcodeid))

    # Add in classification information and other notes
    aurum_new_ethn <- aurum_new_ethn %>%
      mutate(ethn_type = case_when(
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
aurum_ethn_miss_from_new <- ethn_codelist_aurum_old %>%
  filter(!(medcodeid %in% aurum_ethn$medcodeid)) %>% 
  filter(medcodeid %in% cprd_aurum_medical$medcodeid)


# Gold

gold_ethn <- cprd_gold_medical %>%
  # Inclusions: language, interpreter
  filter(grepl("(?i)language|interpreter", term)) %>%
  # Exclusions: English language, assessments, disorders, negations, generic
  filter(!grepl(
    paste0("(?i)child|parental|clinic|second|inappropriate|foul|body|further|",
           # assessment and disorders
           "test|difficulty|disorders?|delay|scale|speech|assistant|score|",
           "alphabet|therapy|sign|impairment|nos|referral|therapist|telephone|",
           "measure|verbal language|development|inventory|scheme|remediation|",
           "assess|use language|comprehend|use of language|ability|finding|",
           "provision|barrier|advice|braille|request|analysis|schizophrenic|",
           "exercises|approach|regimes|stimulation|element|program|observation|",
           # negations
           "refus|not needed|not recorded|english|not present|not available|",
           "declined|",
           # generic language entries
           "read$|spoken$|spoken - finding|preferred spoken language|^language$|",
           "language commonly spoken in europe|world languages|written language$|",
           "^interpreter$|first language|language finding|interpreter booked|",
           "indo-european|afro-asiatic|translator/interpreter|presence|",
           "^interpreter needed$|^interpreter present$|^language interpreter$|",
           "^need for interpreter$|^main spoken language$|additional|",
           "^uses language$|other interpreter"), 
    term, perl = TRUE))

# Subset to terms not already included in old Ethnicity code list
gold_new_ethn <- gold_ethn %>%
  filter(!(medcode %in% ethn_codelist_gold_old$medcode))

    # Add in classification information
    gold_new_ethn <- gold_new_ethn %>%
      mutate(ethn_type = case_when(
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
gold_ethn_miss_from_new <- ethn_codelist_gold_old %>%
  filter(!(medcode %in% gold_ethn$medcode)) %>% 
  filter(medcode %in% cprd_gold_medical$medcode)


# Combine Aurum and GOLD new Ethnicity codes
aurum_gold_SMI_new_codes <- list(
  Aurum = aurum_new_ethn,
  Gold = gold_new_ethn)
# # Save lists of newly added codes into one .xlsx file on separate tabs
# write.xlsx(aurum_gold_SMI_new_codes,
#            file = paste0(wd, path_output, "Aurum_Gold_SMI_new_codes_25Jul2025.xlsx"),
#            overwrite = TRUE)



### OLDER CODE [DO NOT RUN]

# # Which terms were not in the old list
# setdiff(aurum_ethn$term, ethn_codelist_aurum_old$TermRead)
# # Which terms were in the old list but are not in the current list
# ethn_codelist_aurum_old$TermSNOMED[
#   setdiff(ethn_codelist_aurum_old$medcodeid, aurum_ethn$medcodeid)]

# # Any terms in older lists but not in searched terms
# aurum_gold_ethn_miss_codes <- list(
#   Aurum = aurum_ethn_miss_from_new,
#   Gold = gold_ethn_miss_from_new)
#
# # Save lists of missing codes into one .xlsx file on separate tabs
# # These were last reviewed by JH on Jul 22, 2025
# writexl::write_xlsx(aurum_gold_ethn_miss_codes,
#                     path = paste0(wd, path_output, 
#                                   "Aurum_Gold_SMI_missing_codes_22Jul2025.xlsx"))
#
# # Any terms in QOF but not in searched terms
# add_from_qof <- qof_ethn %>%
#   filter(!(`SNOMED concept ID` %in% aurum_ethn$medcodeid))
# add_from_qof_gold <- qof_ethn %>%
#   filter(!(`SNOMED concept ID` %in% gold_ethn$medcode))
# 
# # Save lists of potential codes to add for Aurum and GOLD 
# # These were last reviewed by JH on Jul 14, 2025
# write.csv(aurum_new_ethn, file = paste0(wd, path_output, "Aurum_SMI_other_codes.csv"))
# write.csv(gold_new_ethn, file = paste0(wd, path_output, "Gold_SMI_other_codes.csv"))


# ================= 3) Create updated code lists ===============================

# # Use old code lists
# ethn_codelist_aurum_new <- ethn_codelist_aurum_old
# ethn_codelist_gold_new <- ethn_codelist_gold_old


# Create updated code lists

# Aurum
# Merge in information from old code lists
ethn_codelist_aurum_new <- aurum_ethn %>%
  # Add in the original case-sensitive readcode terms from dictionary
  left_join(cprd_aurum_medical_raw %>% select(MedCodeId, Term), 
            by = join_by("medcodeid" == "MedCodeId")) %>%
  # Add in information on Group and SNOMED description from old code lists
  left_join(ethn_codelist_aurum_old, by = "medcodeid") %>%
  # For the read term description, use the dictionary version
  select(-TermRead) %>%
  rename(TermRead = Term) %>%
  # Select variables to keep
  select(medcodeid, TermRead, TermSNOMED, TermEMIS, SNOMED, OriginalReadCode, 
         CleansedReadCode, Group, Observations)
# Fill in missing Group information for newly added Ethnicity codes and fix incorrect 
# Group info
ethn_codelist_aurum_new <- ethn_codelist_aurum_new %>%
  left_join(aurum_new_ethn %>% select(medcodeid, ethn_type), 
            by = "medcodeid") %>%
  mutate(Group = coalesce(Group, ethn_type),
         Group = case_when(
           medcodeid == "1227584015" ~ "bipolar",  
           # Update cyclic schizophrenia as "schizophrenia"
           medcodeid %in% c("294773015", "376251000006112") ~ "schizophrenia",
           # Update affective psychosis group to "other psychosis"
           medcodeid %in% c("294897018", "294898011", "294902017", 
                            "362781000006116") ~ "other psychosis",
           .default = Group)) %>%
  select(-ethn_type)


# Gold
ethn_codelist_gold_new <- gold_ethn %>%
  # Add in the original case-sensitive readcode terms from dictionary
  left_join(cprd_gold_medical_raw %>% select(medcode, readterm), 
            by = join_by("medcode")) %>%
  # Add in information on Group and SNOMED description from old code lists
  left_join(ethn_codelist_gold_old, by = "medcode") %>%
  # For the read term description, use the dictionary version
  select(-Term) %>%
  rename(Term = readterm, OriginalReadCode = `Read.code`, 
         CleansedReadCode = readcode) %>%
  # Select variables to keep
  select(medcode, Term, OriginalReadCode, 
         CleansedReadCode, Group, clinicalevents, immunisationevents, 
         referralevents, testevents, databaserelease)
# Fill in missing Group information for newly added Ethnicity codes
ethn_codelist_gold_new <- ethn_codelist_gold_new %>%
  left_join(gold_new_ethn %>% select(medcode, ethn_type), 
            by = "medcode") %>%
  mutate(Group = coalesce(Group, ethn_type),
         Group = case_when(
           medcode %in% c("22080", "23963") ~ "bipolar",  
           # Update cyclic schizophrenia as "schizophrenia"
           medcode %in% c("104763", "99000") ~ "schizophrenia",
           # Update affective psychosis group to "other psychosis"
           medcode %in% c("31633", "14656", "33425", "41992", 
                          "54607") ~ "other psychosis",
           .default = Group)) %>%
  select(-ethn_type)


# # Save updated code lists
# write.table(ethn_codelist_aurum_new,
#             file = paste0(wd, path_output, "Aurum_SMI_codelist_20250725.txt"),
#             sep = "\t", row.names = FALSE)
# 
# write.table(ethn_codelist_gold_new,
#             file = paste0(wd, path_output, "Gold_SMI_codelist_20250725.txt"),
#             sep = "\t", row.names = FALSE)



