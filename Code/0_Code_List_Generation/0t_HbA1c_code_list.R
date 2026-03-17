# ==============================================================================
# Generate code lists for HbA1c lab measures
# Authors: SM Wu & S Picton
# Date Created: 2025/12/09
# Date Updated: 2025/12/09
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
# 3) Code_Lists/HbA1c/Old/Aurum_HbA1c_codelist_20240227_Alvin.txt: Old Aurum HbA1c code list
# 4) Code_Lists/HbA1c/Old/Gold_HbA1c_codelist_20240227_Alvin.txt: Old GOLD HbA1c code list
# 
# Final Outputs:
# 1) Code_Lists/HbA1c/Aurum_HbA1c_codelist_20251027.txt: Updated Aurum HbA1c code list
# 2) Code_Lists/HbA1c/Gold_HbA1c_codelist_20251027.txt: Updated GOLD HbA1c code list

# ==============================================================================


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
# path_output <- "Code_Lists/HbA1c/"


### For running in Data Safe Haven
# Set working directory
wd <- "S:/CDSTP_CPRD_25_005368/" 
setwd(wd)

# Set input and output paths
path_input <- "SMI_GLP/Code_Lists/"
path_output <- "SMI_GLP/Code_Lists/HbA1c/"


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


# Read in old HbA1c code list from 2022/07/23, setting all col types to character
# Aurum
aurum_hba1c_old <- read_delim(
  paste0(wd, path_input, "HbA1c/Old/Aurum_HbA1c_codelist_20220723_Alvin.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character()), trim_ws = TRUE)
# Gold
gold_hba1c_old <- read_delim(
  paste0(wd, path_input, "HbA1c/Old/Gold_HbA1c_codelist_20220723_Alvin.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character()), trim_ws = TRUE)


# ================= 2) Search for new relevant med codes =======================

# Aurum

aurum_hba1c <- cprd_aurum_medical %>%
  # Inclusion - Hba1c related terms, including target  
  filter(grepl("(?i)a1c|glycosylated|glycated|HbA1|Haemoglobin A1", 
               term)) %>%
  # Exclusion - Not diagnostic Hba1c terms - administrative terms (request/provision)
  filter(!grepl(paste0("(?i)request|provision"), term))



# Gold

gold_hba1c <- cprd_gold_medical %>%
  # Inclusion - Hba1c related terms, including target 
  filter(grepl("(?i)a1c|glycosylated|glycated|HbA1|Haemoglobin A1", 
               term)) %>%
  # Exclusion - administrative terms - e.g request, provision of information 
  filter(!grepl(paste0("(?i)request|provision"), term))




## Comparing with older codelists

# New codes not in old list
new_aurum <- aurum_hba1c %>%
  filter(!medcodeid %in% aurum_hba1c_old$medcodeid)
new_gold <- gold_hba1c %>%
  filter(!medcode %in% gold_hba1c_old$medcode)

# Old codes not in new old list
miss_new_aurum <- aurum_hba1c_old %>%
  filter(!medcodeid %in% aurum_hba1c$medcodeid)
miss_new_gold <- gold_hba1c_old %>%
  filter(!medcode %in% gold_hba1c$medcode)


# ================= 3) Create updated code lists ===============================

# Create updated code lists

# Aurum
hba1c_codelist_aurum_new <- aurum_hba1c
# Gold
hba1c_codelist_gold_new <- gold_hba1c

# Save updated code lists

write.table(hba1c_codelist_aurum_new,
            file = paste0(wd, path_output, "Aurum_HbA1c_codelist_20260127.txt"),
            sep = "\t", row.names = FALSE)

write.table(hba1c_codelist_gold_new,
            file = paste0(wd, path_output, "Gold_HbA1c_codelist_20260127.txt"),
            sep = "\t", row.names = FALSE)



# Combine Aurum and GOLD updated code lists
temp_aurum <- hba1c_codelist_aurum_new %>%
  rename(medcode = medcodeid, readcode = CleansedReadCode) %>%
  select(medcode, term)
temp_gold <- hba1c_codelist_gold_new %>%
  select(medcode, term)
temp_both <- rbind(temp_aurum, temp_gold)
aurum_gold_hba1c_new <- temp_both %>% distinct()

# Save lists of new combined codelist into one .xlsx file

library(writexl)
write_xlsx(aurum_gold_hba1c_new,
           file = paste0(wd, path_output, "Aurum_Gold_HbA1c_codelist_20260127.xlsx"),
           overwrite = TRUE)


# # Combine Aurum and GOLD into one file with a column specifying database
# hba1c_codelist_aurum_new$database <- "Aurum"
# hba1c_codelist_gold_new$database <- "Gold"
# hba1c_codelist_aurum_gold_new <- rbind(
#   hba1c_codelist_aurum_new %>% 
#     rename(medcode = medcodeid) %>%
#     select(medcode, term, database), 
#   hba1c_codelist_gold_new %>%
#     select(medcode, term, database))
# 
# # Save combined code list
# write.table(hba1c_codelist_aurum_gold_new,
#             file = paste0(wd, path_output, "Aurum_Gold_HbA1c_codelist_20251209.txt"),
#             sep = "\t", row.names = FALSE)


hba1c_codelist_aurum_new$database <- "Aurum"
hba1c_codelist_gold_new$database <- "Gold"
hba1c_codelist_aurum_gold_new <- rbind(
  hba1c_codelist_aurum_new %>% 
    rename(medcode = medcodeid) %>%
    select(medcode, term, database), 
  hba1c_codelist_gold_new %>%
    select(medcode, term, database))

write.table(hba1c_codelist_aurum_gold_new,
            file = paste0(wd, path_output, "Aurum_Gold_HbA1c_codelist_20260127.txt"),
            sep = "\t", row.names = FALSE)

