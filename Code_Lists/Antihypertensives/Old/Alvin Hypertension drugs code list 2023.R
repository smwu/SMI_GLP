# ----------------
# Generate code lists for:
# Hypertension medications
# ----------------
# Last run: 21/07/23

# Clear memory
rm(list = ls())

# Packages
library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(tidylog)

# Set correct file path
path <- "//live.rd.ucl.ac.uk" #Desktop@UCL
path <- "/Volumes/" # VPN connection

# Set working directory
setwd(paste0(path, "/ritd-ag-project-rd00qv-jfhay18/Alvin"))

# Functions

#Filter CPRD product dictionaries only to columns matching the medication list in either term/productname/ingredient
# To be called as: match_medications_1(CPRDAurumProduct, medication_reference)
match_medications_1 <- function(df, medication_reference) {
  matches <- apply(df, 1, function(row) {
    any(sapply(row[c("term", "productname", "ingredient")], function(x) any(grepl(paste(medication_reference, collapse = "|"), x, ignore.case = TRUE))))
  })
  return(matches)}

# Function to match medication names, and add missing tablet formulations and routes
# To be called as: aurum_matches_df <- match_medications_2(aurum_matches_df, "lipidregulator", medication_reference$antihypertensives)
match_medications_2 <- function(df, medication_field, medication_reference) {
  # Create a concatenated column of productname, term, and ingredient
  df$concat <- with(df, paste(productname, term, ingredient, sep = " "))
  # Convert to lowercase
  df$concat <- str_to_lower(df$concat)
  # Remove all non-alphanumeric characters
  df$concat <- gsub("[^[:alnum:]]", " ", df$concat)
  # If formulation is NA but concat contains e.g. "tablet", update formulation to "Tablet"
  df$formulation[is.na(df$formulation) & grepl("(?i)tablet|pill|tab|starter pack", df$concat, ignore.case = TRUE)] <- "Tablet"
  df$formulation[is.na(df$formulation) & grepl("(?i)granule|sachet", df$concat, ignore.case = TRUE)] <- "Granules"
  df$formulation[is.na(df$formulation) & grepl("(?i)powder", df$concat, ignore.case = TRUE)] <- "Powder"
  df$formulation[is.na(df$formulation) & grepl("(?i)Suppositories|sup", df$concat, ignore.case = TRUE)] <- "Suppository"
  df$formulation[is.na(df$formulation) & grepl("(?i)capsule", df$concat, ignore.case = TRUE)] <- "Capsule"
  df$formulation[is.na(df$formulation) & grepl("(?i)Spansules", df$concat, ignore.case = TRUE)] <- "Modified-release capsule"
  df$formulation[is.na(df$formulation) & grepl("(?i)syrup|Oral Solution|Oral liquid", df$concat, ignore.case = TRUE)] <- "Oral solution"
  df$formulation[is.na(df$formulation) & grepl("(?i)injection|inj|vial|amp|syringe|syr|Concentrate", df$concat, ignore.case = TRUE)] <- "Solution for injection"
  df$formulation[is.na(df$formulation) & grepl("(?i)Oral suspension", df$concat, ignore.case = TRUE)] <- "Oral suspension"
  # If route is NA but formulation contains e.g. "tablet", update formulation to "Oral"
  df$route[is.na(df$route) & grepl("(?i)tablet|oral solution|granules|capsule|oral|powder", df$formulation, ignore.case = TRUE)] <- "Oral"
  df$route[is.na(df$route) & grepl("(?i)injection", df$formulation, ignore.case = TRUE)] <- "Intramuscular"
  df$route[is.na(df$route) & grepl("(?i)Suppository", df$formulation, ignore.case = TRUE)] <- "Rectal"
  #If productname is NA, use term
  df <- df %>%
    mutate(productname = coalesce(productname, term))
  
  # Initialize a column called "match" with 0 values
  df$match <- 0
  
  # Loop through each row of `df$concat`
  for (i in seq_len(nrow(df))) {
    # Initialize a vector to store the matched medication names
    meds_matched <- c()
    
    # Loop through each medication name in `medication_reference`
    for (med in medication_reference) {
      # Use grepl to check if the medication name is found in the concat field of `df`
      if (grepl(paste0("\\b", med, "\\b"), df$concat[i], ignore.case = TRUE)) {
        # If the medication name is found, add it to the `meds_matched` vector and set match = 1
        meds_matched <- c(meds_matched, med)
        df$match[i] <- 1
      }
    }
    
    # Capitalize the first letter of each word in the matched medication names
    meds_capitalized <- sapply(meds_matched, function(med) {
      paste(toupper(substring(med, 1, 1)), substring(med, 2), sep = "")
    })
    
    # Recreate the column with only the unique, filtered medication names with capitalized first letter
    df[[medication_field]][i] <- paste(unique(meds_capitalized), collapse = "/")  }
  
  df$ingredient <- coalesce(df[[medication_field]], df$ingredient) #  coalesce the ingredient column with ingredient and the newly created medication_field
  
  return(df)}

# Load data

# Medication names
medication_reference <- read_excel(paste0(path, "/ritd-ag-project-rd00qv-jfhay18/Alvin/Data files/Misc/medication_reference.xlsx"), 
                                   sheet = "antihypertensives", skip = 1) %>%
  rename(antihypertensives = Clean) %>%
  mutate(antihypertensives = str_to_lower(antihypertensives)) %>%
  filter(!is.na(antihypertensives) & is.na(Exclude)) %>%
  select(antihypertensives, 'Brand names')

# Aurum product dictionary
CPRDAurumProduct <- read_delim("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/PhD project/Data sources/CPRD/Documentation/CPRD-2023/CPRDAurumProduct.txt", 
                               delim = "\t", escape_double = FALSE, 
                               col_types = cols(ProdCodeId = col_character(), DrugIssues = col_character(), BNFChapter = col_character()), 
                               trim_ws = TRUE) %>%
  rename(prodcodeid = ProdCodeId, term = `Term from EMIS`, formulation = Formulation, route = RouteOfAdministration, 
         productname = ProductName, ingredient = DrugSubstanceName, strength = SubstanceStrength) %>%
  select(-dmdid, -DrugIssues)

# Gold product dictionary
CPRDGoldProduct <- read_delim("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/PhD project/Data sources/CPRD/Documentation/CPRD-2023/CPRDGoldProduct.txt", 
                              delim = "\t", escape_double = FALSE, 
                              col_types = cols(prodcode = col_character()), 
                              trim_ws = TRUE) %>%
  select(-gemscriptcode, -dmdcode, -bnfchapter) %>%
  mutate(term = NA) %>%
  rename(ingredient = drugsubstance)

# Handle brand names and spelling variants ####

# Split brandnames into separate rows
brands <- medication_reference %>% 
  rename(brandnames = `Brand names`) %>%
  select(antihypertensives, brandnames) %>%
  separate_rows(brandnames, sep = ",\\s*") %>%
  filter(!is.na(brandnames)) %>%
  mutate(brandnames = str_to_lower(brandnames))

# AURUM

# Create an empty column required for loop in CPRDAurumProduct
CPRDAurumProduct$antihypertensives <- NA_character_
CPRDAurumProduct$brandnames <- NA_character_

# Loop over each brand name in "brands" and search for matches in the "productname" and "term" columns of CPRDAurumProduct
for (brandnames in brands$brandnames) {
  matches <- grepl(paste0("\\b", brandnames, "\\b"), CPRDAurumProduct$productname, ignore.case = TRUE) |
    grepl(paste0("\\b", brandnames, "\\b"), CPRDAurumProduct$term, ignore.case = TRUE)
  
  # Assign the matching brand name and antipsychotic to the "brandnames" and "antihypertensives" columns in CPRDAurumProduct
  CPRDAurumProduct$brandnames[matches] <- brandnames
  CPRDAurumProduct$antihypertensives[matches] <- brands$antihypertensives[brands$brandnames == brandnames]}

CPRDAurumProduct <- CPRDAurumProduct %>%
  mutate(ingredient = coalesce(ingredient, antihypertensives)) %>%
  mutate(ingredient = str_to_title(ingredient)) %>%
  select(-antihypertensives, -brandnames)

# GOLD

# Create an empty column required for loop in CPRDGoldProduct
CPRDGoldProduct$antihypertensives <- NA_character_
CPRDGoldProduct$brandnames <- NA_character_

# Loop over each brand name in "brands" and search for matches in the "productname" and "term" columns of CPRDGoldProduct
for (brandnames in brands$brandnames) {
  matches <- grepl(paste0("\\b", brandnames, "\\b"), CPRDGoldProduct$productname, ignore.case = TRUE) |
    grepl(paste0("\\b", brandnames, "\\b"), CPRDGoldProduct$term, ignore.case = TRUE)
  
  # Assign the matching brand name and antipsychotic to the "brandnames" and "antihypertensives" columns in CPRDGoldProduct
  CPRDGoldProduct$brandnames[matches] <- brandnames
  CPRDGoldProduct$antihypertensives[matches] <- brands$antihypertensives[brands$brandnames == brandnames]}

CPRDGoldProduct <- CPRDGoldProduct %>%
  mutate(ingredient = coalesce(ingredient, antihypertensives)) %>%
  mutate(ingredient = str_to_title(ingredient)) %>%
  select(-antihypertensives, -brandnames)

# CODE LISTS
# Aurum

#Search for matches
aurum_matches <- match_medications_1(CPRDAurumProduct, medication_reference$antihypertensives)
#Assign matches to df
aurum_matches_df <- CPRDAurumProduct[aurum_matches, ]
#Add medication name column
aurum_matches_df <- match_medications_2(aurum_matches_df, "antihypertensive", medication_reference$antihypertensives)
#Filter to more precise matches
aurum_matches_df_excluded <- aurum_matches_df %>%
  filter(match == 0)
aurum_matches_df <- aurum_matches_df %>%
  filter(match == 1) %>%
  filter(!grepl("(?i)eye drops", concat)) %>%
  select(-match, -concat, -term)

# save as text file
write.table(aurum_matches_df, file = "Code lists/Hypertension drugs/CPRD-2023/antihypertensives_AURUM_210723.txt",
            sep = "\t", row.names = FALSE)

remove(aurum_matches_df_excluded)

# Gold

#Search for matches
gold_matches <- match_medications_1(CPRDGoldProduct, medication_reference$antihypertensives)
#Assign matches to df
gold_matches_df <- CPRDGoldProduct[gold_matches, ]
#Add medication name column
gold_matches_df <- match_medications_2(gold_matches_df, "antihypertensive", medication_reference$antihypertensives)
#Filter to more precise matches
gold_matches_df_excluded <- gold_matches_df %>%
  filter(match == 0)
gold_matches_df <- gold_matches_df %>%
  filter(match == 1) %>%
  filter(!grepl("(?i)eye drops", concat)) %>%
  select(-match, -concat, - term)

# save as text file
write.table(gold_matches_df, file = "Code lists/Hypertension drugs/CPRD-2023/antihypertensives_GOLD_210723.txt",
            sep = "\t", row.names = FALSE)

remove(gold_matches_df_excluded)
