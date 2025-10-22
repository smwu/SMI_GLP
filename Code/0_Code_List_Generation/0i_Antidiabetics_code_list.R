# ==============================================================================
# Generate code lists for all antidiabetic medications
# Author: SM Wu
# Date Created: 2025/07/26
# Date Updated: 2025/08/19
# 
# Details:
# 1) Set up and load data
# 2) Find all prodcodes that match relevant conditions
# 3) Create and save code lists
# 4) Adjust formatting for extraction
#
# Inputs:
# 1) Code_Lists/MASTER_Lists/CPRD_Aurum_Product_10Feb2025.txt: Aurum product master code list
# 2) Code_Lists/MASTER_Lists/CPRD_GOLD_Product_23Feb2025.txt: GOLD product master code list
# 3) Code_Lists/medication_reference.xlsx: List of relevant medications
# 4) Code/0_Code_List_Generation/helper_fns_code_lists.R: Helper functions
# 
# Final Outputs:
# 1) Code_Lists/Antidiabetics/Aurum_Antidiabetics_codelist_20250726.txt: Updated Aurum Antidiabetic code list
# 2) Code_Lists/Antidiabetics/Gold_Antidiabetics_codelist_20250726.txt: Updated GOLD Antidiabetic code list
# 3) Code_Lists/Antidiabetics/Aurum_Gold_Antidiabetics_codelist_20250726.txt: Updated Aurum and GOLD Antidiabetic code list
# 4) Code_Lists/Antidiabetics/Aurum_Antidiabetics_codelist_prodcode_20250726.txt: Aurum T2DM comma-separated prodcodes only
# 5) Code_Lists/Antidiabetics/Aurum_Antidiabetics_codelist_processed_20250726.txt: Aurum T2DM reformatted for CPRD extraction
# 6) Code_Lists/Antidiabetics/Gold_Antidiabetics_codelist_prodcode_20250726.txt: GOLD T2DM comma-separated prodcodes only
# 7) Code_Lists/Antidiabetics/Gold_Antidiabetics_codelist_processed_20250726.txt: GOLD T2DM reformatted for CPRD extraction

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
path_output <- "Code_Lists/Antidiabetics/"

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
# Rename and select columns
medication_reference <- medication_reference %>%
  rename(keyword = Clean, 
         brandnames = `Brand names`,
         group = Group) %>%
  mutate(keyword = str_to_lower(keyword)) %>%
  filter(!is.na(keyword) & is.na(Exclude))  %>%
  select(keyword, brandnames, group)

# Handle brand names and spelling variants and split into separate rows
meds <- medication_reference %>% 
  select(keyword, brandnames, group) %>%
  separate_rows(brandnames, sep = ",\\s*") %>%
  mutate(brandnames = str_to_lower(brandnames))

# ================= 2) Find all prodcodes that match relevant conditions ========

## Aurum

# Find codes that match the drugs and brand names of interest
# Preserving antidiabetic class grouping information
aurum_matches_df <- match_meds(df_codes = cprd_aurum_product, df_drugs = meds,
                               group_info = TRUE)

## GOLD 

# Find codes that match the drugs and brand names of interest
# Preserving antidiabetic class grouping information
gold_matches_df <- match_meds(df_codes = cprd_gold_product, df_drugs = meds, 
                              group_info = TRUE)


# ================= 3) Create and save code lists ==============================

## Aurum

# Add medication name column
aurum_codelist <- match_meds_2(df = aurum_matches_df, 
                               medication_field = "Antidiabetic", 
                               medication_keyword = medication_reference$keyword)
# Filter to more precise matches
aurum_codelist_excluded <- aurum_codelist %>%
  filter(match == 0)
aurum_codelist <- aurum_codelist %>%
  filter(match == 1) %>%
  select(-match, -concat, -term) %>%
  select(prodcodeid, productname, formulation, route, ingredient, strength, 
         BNFChapter, Antidiabetic, group)

# # Save as text file
# write.table(aurum_codelist,
#             file = paste0(wd, path_output, "Aurum_Antidiabetics_codelist_20250726.txt"),
#             sep = "\t", row.names = FALSE)

## Gold

# Add medication name column
gold_codelist <- match_meds_2(df = gold_matches_df, 
                              medication_field = "Antidiabetic", 
                              medication_keyword = medication_reference$keyword)
# Filter to more precise matches
gold_codelist_excluded <- gold_codelist %>%
  filter(match == 0)
gold_codelist <- gold_codelist %>%
  filter(match == 1) %>%
  select(-match, -concat, - term) %>%
  select(prodcode, productname, formulation, route, ingredient, strength, 
         bnftext, Antidiabetic, group)

# # Save as text file
# write.table(gold_codelist,
#             file = paste0(wd, path_output, "Gold_Antidiabetics_codelist_20250726.txt"),
#             sep = "\t", row.names = FALSE)


# Combine Aurum and GOLD into one file with a column specifying database
aurum_codelist$database <- "Aurum"
gold_codelist$database <- "Gold"
aurum_gold_codelist <- rbind(
  aurum_codelist %>% 
    rename(prodcode = prodcodeid) %>%
    select(prodcode, productname, formulation, route, ingredient, strength, 
           Antidiabetic, group, database), 
  gold_codelist %>%
    select(prodcode, productname, formulation, route, ingredient, strength, 
           Antidiabetic, group, database))
# # Save combined code list
# write.table(aurum_gold_codelist,
#             file = paste0(wd, path_output, "Aurum_Gold_Antidiabetics_codelist_20250726.txt"),
#             sep = "\t", row.names = FALSE)


# ================= 4) Adjust formatting for extraction ========================

## Adjust format of code lists to assist in CPRD data extraction
# The 'prodcode' will be a simple file with the prodcode ids separated by commas.
# The 'processed' file will contain two columns: one for the prodcode ID and 
# one for the term definition

# Aurum
aurum_codelist_prodcode <- aurum_codelist$prodcodeid |>
  as.character() |>
  trimws() |>
  unique() |>
  na.omit()

# # Write to .txt file with prodcodes separated by commas
# write.table(t(aurum_codelist_prodcode),
#             paste0(wd, path_output,
#                    "Aurum_Antidiabetics_codelist_prodcode_20250726.txt"),
#             sep=", ", row.names=FALSE, col.names=FALSE, quote=FALSE)

aurum_codelist_processed <- aurum_codelist %>%
  select(prodcodeid, productname)

# # Write to .txt file with columns separated by tab
# write.table(aurum_codelist_processed,
#             paste0(wd, path_output,
#                    "Aurum_Antidiabetics_codelist_processed_20250726.txt"),
#             sep = "\t", row.names = FALSE,
#             quote=FALSE)


# GOLD
gold_codelist_prodcode <- gold_codelist$prodcode |>
  as.character() |>
  trimws() |>
  unique() |>
  na.omit()

# # Write to .txt file with prodcodes separated by commas
# write.table(t(gold_codelist_prodcode), 
#             paste0(wd, path_output, 
#                    "Gold_Antidiabetics_codelist_prodcode_20250726.txt"), 
#             sep=", ", row.names=FALSE, col.names=FALSE, quote=FALSE)

gold_codelist_processed <- gold_codelist %>%
  select(prodcode, productname)

# # Write to .txt file with columns separated by tab
# write.table(gold_codelist_processed,
#             paste0(wd, path_output,
#                    "Gold_Antidiabetics_codelist_processed_20250726.txt"),
#             sep = "\t", row.names = FALSE,
#             quote=FALSE)

