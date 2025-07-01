# ==============================================================================
# Generate code lists for Sulfonylureas anti-diabetic medication
# Author: SM Wu
# Date Created: 2025/06/20
# Date Updated: 2025/06/20
# 
# Details:
# 1) Set up and load data
# 2) Find all medcodes that match relevant conditions
# 3) Create and save code lists
#
# Inputs:
# 1) Code_Lists/MASTER_Lists/CPRD_Aurum_Product_10Feb2025.txt: Aurum product master code list
# 2) Code_Lists/MASTER_Lists/CPRD_GOLD_Product_23Feb2025.txt: GOLD product master code list
# 3) Code_Lists/medication_reference.xlsx: List of relevant medications
# 4) Code/0_Code_List_Generation/helper_fns_code_lists.R: Helper functions
# 
# Final Outputs:
# 1) Code_Lists/Sulfonylureas/Aurum_Sulfonylureas_codelist_20250614.txt: Updated Aurum Sulfonylureas code list
# 2) Code_Lists/Sulfonylureas/Gold_Sulfonylureas_codelist_20250614.txt: Updated GOLD Sulfonylureas code list
# ==============================================================================


# ================= 1) Set up and load data ====================================

# Clear memory
rm(list = ls())

# Packages
library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

# Set working directory
wd <- "/Volumes/ritd-ag-project-rd00qv-jfhay18/Stephanie/SMI_GLP/" # VPN connection
# wd <- "//live.rd.ucl.ac.uk/ritd-ag-project-rd00qv-jfhay18/Stephanie/SMI_GLP/" #Desktop@UCL
setwd(wd)

# Set input and output paths
path_input <- "Code_Lists/"
path_output <- "Code_Lists/Sulfonylureas/"

# Load in helper functions
source(paste0(wd, "Code/0_Code_List_Generation/helper_fns_code_lists.R"))

## Load data

# Read in Aurum product dictionary
cprd_aurum_product <- 
  read_delim(
    paste0(wd, path_input, "MASTER_Lists/CPRD_Aurum_Product_10Feb2025.txt"), 
    delim = "\t", escape_double = FALSE, 
    col_types = cols(ProdCodeId = col_character(), DrugIssues = col_character(), 
                     BNFChapter = col_character()), 
    trim_ws = TRUE)
# Rename and select columns
cprd_aurum_product <- cprd_aurum_product %>%
  rename(prodcodeid = ProdCodeId, term = `Term from EMIS`, 
         formulation = Formulation, route = RouteOfAdministration, 
         productname = ProductName, ingredient = DrugSubstanceName, 
         strength = SubstanceStrength) %>%
  select(prodcodeid, term, productname, formulation, route, ingredient, 
         strength, BNFChapter)

# Read in Gold product dictionary
cprd_gold_product <- 
  read_delim(
    paste0(wd, path_input, "MASTER_Lists/CPRD_GOLD_Product_23Feb2025.txt"), 
    delim = "\t", escape_double = FALSE, 
    col_types = cols(prodcode = col_character()), trim_ws = TRUE)
# Rename and select columns
cprd_gold_product <- cprd_gold_product %>%
  select(prodcode, therapyevents, productname, ingredient, strength, formulation, 
         routeofadministration, bnftext) %>%
  mutate(term = NA) %>%
  rename(route = routeofadministration)

# Read in medication names
medication_reference <- 
  read_excel(paste0(wd, path_input, "medication_reference.xlsx"), 
             sheet = "sulfonylureas", skip = 1)
# Rename and select columns
medication_reference <- medication_reference %>%
  rename(keyword = Clean, 
         brandnames = `Brand names`) %>%
  mutate(keyword = str_to_lower(keyword)) %>%
  filter(!is.na(keyword) & is.na(Exclude))  %>%
  select(keyword, brandnames)

# Handle brand names and spelling variants and split into separate rows
meds <- medication_reference %>% 
  select(keyword, brandnames) %>%
  separate_rows(brandnames, sep = ",\\s*") %>%
  # filter(!is.na(brandnames)) %>%
  mutate(brandnames = str_to_lower(brandnames))


# ================= 2) Find all medcodes that match relevant conditions ========

## Aurum

# Find codes that match the drugs and brand names of interest
aurum_matches_df <- match_meds(df_codes = cprd_aurum_product, df_drugs = meds)


## GOLD 

# Find codes that match the drugs and brand names of interest
gold_matches_df <- match_meds(df_codes = cprd_gold_product, df_drugs = meds)


# ================= 3) Create and save code lists ==============================

## Aurum

# Add medication name column
aurum_codelist <- match_meds_2(df = aurum_matches_df, 
                               medication_field = "Sulfonylurea", 
                               medication_keyword = medication_reference$keyword)
# Filter to more precise matches
aurum_codelist_excluded <- aurum_codelist %>%
  filter(match == 0)
aurum_codelist <- aurum_codelist %>%
  filter(match == 1) %>%
  select(-match, -concat, -term)

# # Save as text file
# write.table(aurum_codelist, 
#             file = paste0(wd, path_output, "Aurum_Sulfonylureas_codelist_20250614.txt"),
#             sep = "\t", row.names = FALSE)

## Gold

# Add medication name column
gold_codelist <- match_meds_2(df = gold_matches_df, 
                              medication_field = "Sulfonylurea", 
                              medication_keyword = medication_reference$keyword)
# Filter to more precise matches
gold_codelist_excluded <- gold_codelist %>%
  filter(match == 0)
gold_codelist <- gold_codelist %>%
  filter(match == 1) %>%
  select(-match, -concat, - term)

# # Save as text file
# write.table(gold_codelist, 
#             file = paste0(wd, path_output, "Gold_Sulfonylureas_codelist_20250614.txt"),
#             sep = "\t", row.names = FALSE)


