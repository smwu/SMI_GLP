# ==============================================================================
# Generate code lists for other antidiabetic medications
# Author: SM Wu
# Date Created: 2025/08/01
# Date Updated: 2025/08/01
# 
# Details:
# 1) Set up and load data
# 2) Find all medcodes that match relevant conditions
# 4) Create and save code lists for "other antidiabetics" beyond GLP-1RAs, 
# Sulfonylureas, DPP4-is, SGLT2-is, Insulin, and Metformin
#
# Inputs:
# 1) Code_Lists/MASTER_Lists/CPRD_Aurum_Product_10Feb2025.txt: Aurum product master code list
# 2) Code_Lists/MASTER_Lists/CPRD_GOLD_Product_23Feb2025.txt: GOLD product master code list
# 3) Code_Lists/medication_reference.xlsx: List of relevant medications
# 4) Code/0_Code_List_Generation/helper_fns_code_lists.R: Helper functions
# 
# Final Outputs:
# 1) Code_Lists/Other_Antidiabetics/Aurum_Other_Antidiabetics_codelist_20250801.txt: Updated Aurum Other Antidiabetic code list
# 2) Code_Lists/Other_Antidiabetics/Gold_Other_Antidiabetics_codelist_20250801.txt: Updated GOLD Other Antidiabetic code list
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
path_output <- "Code_Lists/Other_Antidiabetics/"

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
             sheet = "antidiabetics", skip = 1)
# Subset to 'other antidiabetics'
medication_reference <- medication_reference %>%
  filter(Group == "Other")
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
  mutate(brandnames = str_to_lower(brandnames))

# ================= 2) Find all medcodes that match relevant conditions ========

## Aurum

# Find codes that match the drugs and brand names of interest
# Preserving antidiabetic class grouping information
aurum_matches_df <- match_meds(df_codes = cprd_aurum_product, df_drugs = meds)

## GOLD 

# Find codes that match the drugs and brand names of interest
# Preserving antidiabetic class grouping information
gold_matches_df <- match_meds(df_codes = cprd_gold_product, df_drugs = meds)


# ================= 3) Create and save code lists ==============================

## Aurum

# Add medication name column
aurum_codelist <- match_meds_2(df = aurum_matches_df, 
                               medication_field = "Other Antidiabetic", 
                               medication_keyword = medication_reference$keyword)
# Filter to more precise matches
aurum_codelist_excluded <- aurum_codelist %>%
  filter(match == 0)
aurum_codelist <- aurum_codelist %>%
  filter(match == 1) %>%
  select(-match, -concat, -term) %>%
  select(prodcodeid, productname, formulation, route, ingredient, strength, 
         BNFChapter, `Other Antidiabetic`)

# # Save as text file
# write.table(aurum_codelist,
#             file = paste0(wd, path_output, "Aurum_Other_Antidiabetics_codelist_20250801.txt"),
#             sep = "\t", row.names = FALSE)

## Gold

# Add medication name column
gold_codelist <- match_meds_2(df = gold_matches_df, 
                              medication_field = "Other Antidiabetic", 
                              medication_keyword = medication_reference$keyword)
# Filter to more precise matches
gold_codelist_excluded <- gold_codelist %>%
  filter(match == 0)
gold_codelist <- gold_codelist %>%
  filter(match == 1) %>%
  select(-match, -concat, - term) %>%
  select(prodcode, productname, formulation, route, ingredient, strength, 
         bnftext, `Other Antidiabetic`)

# # Save as text file
# write.table(gold_codelist,
#             file = paste0(wd, path_output, "Gold_Other_Antidiabetics_codelist_20250801.txt"),
#             sep = "\t", row.names = FALSE)


