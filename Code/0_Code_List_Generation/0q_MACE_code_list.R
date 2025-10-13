# ==============================================================================
# Generate code lists for MACE using ICD codes
# Author: SM Wu
# Date Created: 2025/09/29
# Date Updated: 2025/09/29
# 
# Details:
# 1) Set up and load data
# 2) Subset to relevant codes and create code list
#
# Inputs:
# 1) Code_Lists/MASTER_Lists/icd102019syst_codes.txt: ICD-10 codes from WHO
# 
# Final Outputs:
# 1) Code_Lists/MACE/MACE_codelist_20250929.txt: Updated MACE ICD code list

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
path_output <- "Code_Lists/MACE/"

## Load data

# Read in ICD-10 dictionary, obtained from https://icdcdn.who.int/icd10/index.html 
icd10_raw <- read_delim(
  paste0(wd, path_input, "MASTER_Lists/icd102019syst_codes.txt"),
  delim = ";", escape_double = FALSE, col_names = FALSE,
  trim_ws = TRUE)
colnames(icd10_raw) <- c("hier_level", "tree_node", "terminal_type", 
                         "chapter", "3chars", "no_dagger", "no_aster", "no_dot",
                         "title")
# Subset to relevant columns
icd10 <- icd10_raw %>% 
  select(chapter, no_dagger, title) %>%
  rename(code = no_dagger)



# ================= 2) Subset to relevant codes and create code list ===========

# Subset to relevant codes
mace_icd <- icd10 %>%
  filter(grepl(paste0("(?i)I21|", # acute myocardial infarction
                      "I60|I61|I62|I63|I64"), # stroke
               code))

# Add in whether or not the code must be under primary diagnosis
mace_icd$primary_only <- TRUE


# # Save updated code lists
# write.table(mace_icd,
#             file = paste0(wd, path_output, "MACE_codelist_20250929.txt"),
#             sep = "\t", row.names = FALSE)



