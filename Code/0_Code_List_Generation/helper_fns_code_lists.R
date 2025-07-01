# ========================================================
# Code list generation helper functions
# Created by: SM Wu
# Date Created: 2025/06/09
# Date Updated: 2025/06/16
# 
# Details:
# Helper functions used to generate med code lists
#
# ========================================================

library(tidyverse)

# Find codes that match the drugs and brand names of interest
# Inputs:
#   df_codes: Dataframe of code dictionary for GOLD or Aurum
#   df_drugs: Dataframe of unique drug and brand name combinations
# Outputs:
#   df_matches: Dataframe of subset of code dictionary whose term, product name, 
#   or ingredient matches a drug or brand name of interest
match_meds <- function(df_codes, df_drugs) {
  
  # Create an empty column required for loop in df_codes
  df_codes$keyword <- NA_character_
  df_codes$brandnames <- NA_character_
  
  # Loop over all the unique drugs and brandnames
  for (i in 1:nrow(df_drugs)) {
    # For each drug and brandname
    drug <- df_drugs$keyword[i]
    brandname <- df_drugs$brandnames[i]
    if (is.na(brandname)) {
      brandname <- "no brand name"
    } 
    
    # Search for brandname matches in the "productname", "term", and "ingredient" 
    # columns of df_codes
    match_brand <- 
      grepl(paste0("\\b", brandname, "\\b"), df_codes$productname, 
            ignore.case = TRUE) |
      grepl(paste0("\\b", brandname, "\\b"), df_codes$term, 
            ignore.case = TRUE) |
      grepl(paste0("\\b", brandname, "\\b"), df_codes$ingredient, 
            ignore.case = TRUE)
    
    # Search for drug matches in the "productname", "term", and "ingredient
    # columns of df_codes
    match_drug <- 
      grepl(paste0("\\b", drug, "\\b"), df_codes$productname, 
            ignore.case = TRUE) | 
      grepl(paste0("\\b", drug, "\\b"), df_codes$ingredient,
            ignore.case = TRUE) |
      grepl(paste0("\\b", drug, "\\b"), df_codes$term, 
            ignore.case = TRUE)
    
    # Assign matching brand name and drug to the "brandnames" and "keyword" 
    # columns in df_codes
    df_codes$brandnames[match_brand] <- brandname
    df_codes$keyword[match_brand] <- drug
    df_codes$keyword[match_drug] <- drug
  }
  
  # Fill in missing ingredients with the keyword and capitalize the first letter 
  df_codes <- df_codes %>%
    mutate(ingredient = coalesce(ingredient, keyword)) %>%
    mutate(ingredient = str_to_title(ingredient))
  
  # Select subset of med codes that match one of the drugs or brandnames,
  # then drop the keyword and brandnames columns
  df_matches <- df_codes %>%
    filter(!(is.na(keyword) & is.na(brandnames)))  %>%
    select(-keyword, -brandnames)
  
  # Return dataframe of matching med codes for the drugs of interest
  return(df_matches)
}


# Match medication names, and add missing tablet formulations and routes
# Inputs:
#   df: Dataframe of matches for GOLD or Aurum
#   medication_field: Broader name for type of medication
#   medication_keyword: Specific medication keyword term
# Outputs:
#   df_matches: Dataframe of subset of code dictionary whose term, product name, 
#   or ingredient matches a drug or brand name of interest
# To be called as: aurum_matches_df <- match_meds_2(aurum_matches_df, 
# "GLP-1RA", medication_keyword$keyword)
match_meds_2 <- function(df, medication_field, medication_keyword) {
  
  # Create a concatenated column of productname, term, and ingredient
  df$concat <- with(df, paste(productname, term, ingredient, sep = " "))
  # Convert to lowercase
  df$concat <- str_to_lower(df$concat)
  # Remove all non-alphanumeric characters
  df$concat <- gsub("[^[:alnum:]]", " ", df$concat)
  
  # If formulation is NA but concat contains information, update formulation
  # E.g., If concatenated contains "tablet", update formulation to "Tablet"
  df <- df %>%
    mutate(formulation = case_when(
      is.na(formulation) & grepl("(?i)tablet|pill|tab|starter pack", df$concat, 
                                 ignore.case = TRUE) ~ "Tablet",
      is.na(formulation) & grepl("(?i)granule|sachet", df$concat, 
                                 ignore.case = TRUE) ~ "Granules",
      is.na(formulation) & grepl("(?i)powder", df$concat, 
                                 ignore.case = TRUE) ~ "Powder",
      is.na(formulation) & grepl("(?i)Suppositories|sup", df$concat, 
                                 ignore.case = TRUE) ~ "Suppository",
      is.na(formulation) & grepl("(?i)capsule", df$concat, 
                                 ignore.case = TRUE) ~ "Capsule",
      is.na(formulation) & grepl("(?i)Spansules", df$concat, 
                                 ignore.case = TRUE) ~ "Modified-release capsule",
      is.na(formulation) & grepl("(?i)syrup|Oral Solution|Oral liquid", 
                                 df$concat, ignore.case = TRUE) ~ "Oral solution",
      is.na(formulation) & grepl("(?i)injection|inj|vial|amp|syringe|syr|Concentrate", 
                                 df$concat, ignore.case = TRUE) ~ "Solution for injection",
      is.na(formulation) & grepl("(?i)Oral suspension", df$concat, 
                                 ignore.case = TRUE) ~ "Oral suspension",
      .default = formulation))
  
  # If route is NA but formulation contains information, update route
  # E.g., If formulation is "tablet", update route to "Oral"
  df <- df %>%
    mutate(route = case_when(
      is.na(route) & grepl("(?i)tablet|oral solution|granules|capsule|oral|powder", 
                           df$formulation, ignore.case = TRUE) ~ "Oral",
      is.na(route) & grepl("(?i)injection", df$formulation, 
                           ignore.case = TRUE) ~ "Intramuscular",
      is.na(route) & grepl("(?i)Suppository", df$formulation, 
                           ignore.case = TRUE) ~ "Rectal",
      .default = route
    ))

  # If productname is NA, use term
  df <- df %>%
    mutate(productname = coalesce(productname, term))
  
  # Initialize a column called "match" with 0 values
  df$match <- 0
  
  # Loop through each row of `df$concat` containing productname, term, ingredient
  for (i in seq_len(nrow(df))) {
    # Initialize a vector to store the matched medication names
    meds_matched <- c()
    
    # Loop through each medication name in `medication_keyword`
    for (med in medication_keyword) {
      # Use grepl to check if the medication name is found in the `df$concat`
      if (grepl(paste0("\\b", med, "\\b"), df$concat[i], ignore.case = TRUE)) {
        # If the medication name is found, add it to the `meds_matched` vector 
        # and set match = 1
        meds_matched <- c(meds_matched, med)
        df$match[i] <- 1
      }
    }
    
    # Capitalize the first letter of each word in the matched medication names
    meds_capitalized <- sapply(meds_matched, function(med) {
      paste(toupper(substring(med, 1, 1)), substring(med, 2), sep = "")
    })
    
    # Recreate the column with only the unique, filtered medication names with 
    # capitalized first letter
    df[[medication_field]][i] <- paste(unique(meds_capitalized), collapse = "/") 
  }
  
  # Coalesce ingredient column w/ ingredient and newly created medication_field
  df$ingredient <- coalesce(df[[medication_field]], df$ingredient) 
  
  return(df)
}



