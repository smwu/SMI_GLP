# ========================================================
# Data extraction helper functions
# Created by: SM Wu
# Date Created: 2025/06/16
# Date Updated: 2025/06/16
# 
# Details:
# Helper functions used to extract patient data
#
# ========================================================

library(data.table)
library(dplyr)


# Function to read in CPRD txt files for GOLD or Aurum
# Inputs:
#   file: String specifying full path and file name to be read in
#   database: String specifying CPRD database type. Must be 'gold' or 'aurum'
# Outputs:
#   pat_obs: Dataframe containing patient observations
#   medcode: Boolean specifying if a medcode (`TRUE`; for diagnoses) or 
#     prodcode (`FALSE` for medications).
# Example usage:
#   read_pat_obs(file = paste0(file_path, file_names_list[i]), 
#                database = database,
#                medcode = TRUE)
#   read_pat_obs(file = paste0(wd, "2023 CPRD/Gold/Clinical/", 
#                              "SMI_GOLD_Extract_Clinical_001.txt"),
#                database = "gold",
#                medcode = TRUE)
#
read_pat_obs <- function(file, database, medcode) {

  if (database == "gold") {
    # For GOLD, code column is "medcode" or "prodcode" and fill = TRUE 
    # (fill in columns in case rows have unequal length)
    
    # Specify medcode (for diagnoses) or prodcode (for medications)
    if (medcode) {
      # Read in patient observation files
      pat_obs <- fread(file, header = TRUE, sep = "\t", fill = TRUE, 
                       dec = ".", quote = "", 
                       colClasses = c(medcode = "character", patid = "character"))
    } else {  # prodcode
      # Read in patient observation files
      pat_obs <- fread(file, header = TRUE, sep = "\t", fill = TRUE, 
                       dec = ".", quote = "", 
                       colClasses = c(prodcode = "character", patid = "character"))
    }
    
  } else if (database == "aurum") {
    # For Aurum, code column is "medcodeid" or "prodcodeid" and fill = FALSE 
    
    # Specify medcode (for diagnoses) or prodcode (for medications)
    if (medcode) {
      # Read in patient observation files
      pat_obs <- fread(file, header = TRUE, sep = "\t", fill = FALSE, 
                       dec = ".", quote = "", 
                       colClasses = c(medcodeid = "character", patid = "character"))
    } else {  # prodcode
      # Read in patient observation files
      pat_obs <- fread(file, header = TRUE, sep = "\t", fill = FALSE, 
                       dec = ".", quote = "", 
                       colClasses = c(prodcodeid = "character", patid = "character"))
    }
    
  } else {
    # Throw stop condition if database isn't gold or aurum
    stop("Input argument 'database' must be either 'gold' or 'aurum'.")
  }
  
  # Return patient observation files
  return(pat_obs)
}


# Extract patient files matching conditions from code list
# Inputs:
#   file_path: Path to directory where patient files are contained
#   file_names: String vector of the file names containing the datasets to read 
#     in. These files should be located in the `file_path` folder
#   code_list: Dataframe of code list. Must contain "medcode" column to match on
#   database: String specifying CPRD database type. Must be 'gold' or 'aurum'
#   medcode: Boolean specifying if a medcode (`TRUE`; for diagnoses) or 
#     prodcode (`FALSE` for medications).
# Outputs: 
#   pat_obs_extracted: Dataframe  containing extracted patient files matching 
#     conditions from code list
# Example usage: 
#   read_obs_condition(file_path = paste0(wd, "2023 CPRD/Gold/Clinical/"),
#                      file_names = gold_clin_files, 
#                      code_list = smi_gold,
#                      database = "gold",
#                      medcode = TRUE)
read_obs_condition <- function(file_path, file_names, code_list, database,
                               medcode) {
  
  # Specify medcode (for diagnoses) or prodcode (for medications)
  if (medcode) {
    code_type <- "medcode"
  } else {
    code_type <- "prodcode"
  }
  
  # Specify medcode/prodcode column name depending on database type
  if (database == "aurum") {
    # Append 'id' to the end for aurum
    code_type <- paste0(code_type, "id")
  } else if (database != "gold") {
    # Throw error if database isn't 'gold' or 'aurum'
    stop("Input argument 'database' must be either 'gold' or 'aurum'.")
  }
  
  # Initialise data frame for extracted results to be stored in
  pat_obs_extracted <- NULL 
  
  # For each file subset, extract patient files with codes matching condition
  for (i in 1:length(file_names)) {
    
    # load data file
    pat_obs_all <- read_pat_obs(
      file = paste0(file_path, file_names[i]), database = database, 
      medcode = medcode)
    
    # Filter to patients with matching codes for the condition of interest,
    # allowing for multiple matches per patient and multiple patients per code
    pat_obs_condition <- code_list %>%
      inner_join(pat_obs_all, by = code_type, multiple = "all")
    
    # Append extracted patients to the list of patients from all file subsets
    pat_obs_extracted <- rbindlist(list(pat_obs_extracted, pat_obs_condition))
    
    # Paste progress
    print(paste0("Progress: ", i, "/", length(file_names), " completed"))
  }
  
  # Return extracted files
  return(pat_obs_extracted)
}


# Transform and clean up patient dates for diagnoses
# Inputs:
#   patient_data: Dataframe of patient files
#   earliest_date: String specifying earliest acceptable date
#   latest_date: String specifying latest acceptable date
# Outputs:
#   `patient_data` dataframe updated with transformed dates and restricted to
#     those within the acceptable date range
# Example usage:
#   transform_dates(patient_data = pat_smi_comb,
#                   earliest_date = '1900-01-01',
#                   latest_date = '2023-06-01')
transform_dates <- function(patient_data, earliest_date, latest_date) {
  earliest <- as.Date(earliest_date)
  latest <- as.Date(latest_date)

  patient_data %>%
    mutate(
      # Convert to date
      eventdate = as.Date(as.character(eventdate), "%d/%m/%Y"),
      sysdate = as.Date(as.character(sysdate), "%d/%m/%Y"),
      # Set erroneous dates to NA
      eventdate = if_else(eventdate < earliest, as.Date(NA_real_), eventdate),
      sysdate = if_else(sysdate < earliest, as.Date(NA_real_), sysdate),
      eventdate = if_else(eventdate > latest, as.Date(NA_real_), eventdate),
      # If eventdate is likely erroneous but sysdate is more recent, use sysdate
      eventdate = if_else(eventdate < as.Date('1910-01-01') &
                            sysdate > as.Date('1990-01-01'), sysdate, eventdate),
      # Fill in missing using sysdate where possible
      eventdate = coalesce(eventdate, sysdate)) %>%
    # Drop those with missing eventdate or eventdate after latest_date
    filter(!is.na(eventdate), eventdate <= latest)

}


# Transform and clean up patient dates for medications
# Inputs:
#   patient_data: Dataframe of patient files
#   earliest_date: String specifying earliest acceptable date
#   latest_date: String specifying latest acceptable date
# Outputs: 
#   `patient_data` dataframe updated with transformed dates and restricted to 
#     those within the acceptable date range
# Example usage: 
#   transform_dates(patient_data = pat_smi_comb,
#                   earliest_date = '1900-01-01', 
#                   latest_date = '2023-06-01')
transform_dates_meds <- function(patient_data, earliest_date, latest_date) {
  earliest <- as.Date(earliest_date)
  latest <- as.Date(latest_date)
  
  patient_data %>%
    mutate(
      # Convert to date
      issuedate = as.Date(as.character(issuedate), "%d/%m/%Y"), 
      enterdate = as.Date(as.character(enterdate), "%d/%m/%Y"),
      # Set erroneous dates to NA
      issuedate = if_else(issuedate < earliest, as.Date(NA_real_), issuedate), 
      enterdate = if_else(enterdate < earliest, as.Date(NA_real_), enterdate),
      issuedate = if_else(issuedate > latest, as.Date(NA_real_), issuedate),
      # If issuedate is likely erroneous but enterdate is more recent, use enterdate
      issuedate = if_else(issuedate < as.Date('1910-01-01') & 
                            enterdate > as.Date('1990-01-01'), enterdate, issuedate), 
      # Fill in missing using enterdate where possible
      issuedate = coalesce(issuedate, enterdate)) %>%
    # Drop those with missing issuedate or issuedate after latest_date
    filter(!is.na(issuedate), issuedate <= latest)
  
}
