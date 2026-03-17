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





#========================================================================================================
### SQL extraction functions

## Works with medcodes for diagnoses and prodcodes for medications

# Create SQL extraction function for GOLD codes
# This block:
#   1) Reads all tab-delimited files in a folder with read_csv()
#   2) INNER JOINs to the GOLD code list to find code matches
gold_extract_sql <- function(connection, out_table, files_glob, gold_codes_table, 
                             source_label, code_kind = c("medcode", "prodcode")){
  
  # Medcode for diagnoses, prodcode for medications
  code_kind <- match.arg(code_kind)
  
  # Make sure correct output and codelist table names
  out_tbl <- as.character(DBI::dbQuoteIdentifier(connection, out_table))
  code_tbl <- as.character(DBI::dbQuoteIdentifier(connection, gold_codes_table))
  
  
  sql <- sprintf("
    -- Read GOLD raw files
    
    CREATE OR REPLACE TEMP VIEW gold_rawfile AS
    SELECT * FROM read_csv('%1$s',
      delim='\\t', 
      header=true, 
      all_varchar=true,   -- all columns as characters
      null_padding=true,  -- pad shorter rows
      strict_mode=false,  -- tolerate irregular rows
      quote='', escape=''
    );
    
    
    -- Create out_tbl, keeping only rows matching code list medcode (from code_tbl)
    -- Attach term from code list
    -- Also add in database identifier and source type
    
    CREATE OR REPLACE TEMP TABLE %2$s AS
    SELECT
      c.*,  -- code list columns (e.g., medcode, term)
      g.*,  -- all columns from records
      'Gold'          AS database,
      '%3$s'          AS source
    FROM gold_rawfile g
    INNER JOIN %4$s c
      ON trim(g.%5$s) = trim(c.%5$s);
  ", 
                 files_glob, 
                 out_tbl, 
                 source_label, 
                 code_tbl,
                 code_kind
  )
  
  # Execute SQL commands
  DBI::dbExecute(connection, sql)
  # Don't print out 'out_table' by default when returned, but can still call 
  # on it if necessary 
  invisible(out_table)
}


# Create SQL extraction function for Aurum codes
# This block:
#   1) Reads all tab-delimited files in a folder with read_csv()
#   2) INNER JOINs to the GOLD code list to find code matches
aurum_extract_sql <- function(connection, out_table, files_glob, aurum_codes_table, 
                              source_label, code_kind = c("medcodeid", "prodcodeid")){
  
  # Medcodeid for diagnoses, prodcodeid for medications
  code_kind <- match.arg(code_kind)
  
  # Make sure correct output and codelist table names
  out_tbl <- as.character(DBI::dbQuoteIdentifier(connection, out_table))
  code_tbl <- as.character(DBI::dbQuoteIdentifier(connection, aurum_codes_table))
  
  
  sql <- sprintf("
    -- Read Aurum raw files
    
    CREATE OR REPLACE TEMP VIEW aurum_rawfile AS
    SELECT * FROM read_csv('%1$s',
      delim='\\t', 
      header=true, 
      all_varchar=true,   -- all columns as characters
      null_padding=true,  -- pad shorter rows
      strict_mode=false,  -- tolerate irregular rows
      quote='', escape=''
    );
    
    
    -- Create out_tbl, keeping only rows matching code list medcode (from code_tbl)
    -- Attach term from code list
    -- Also add in database identifier and source type
    
    CREATE OR REPLACE TABLE %2$s AS
    SELECT
      c.*,  -- code list columns (e.g., medcodeid, term)
      a.*,  -- all columns from records
      'Aurum'         AS database,
      '%3$s'          AS source
    FROM aurum_rawfile a
    INNER JOIN %4$s c
      ON trim(a.%5$s) = trim(c.%5$s);
  ", 
                 files_glob, 
                 out_tbl, 
                 source_label, 
                 code_tbl,
                 code_kind
  )
  
  # Execute SQL commands
  DBI::dbExecute(connection, sql)
  # Don't print out 'out_table' by default when returned, but can still call 
  # on it if necessary 
  invisible(out_table)
}


# SQL function to transform dates into date format and filter invalid dates
transform_dates_sql <- function(connection, 
                                in_table, 
                                out_table, 
                                earliest_date = "1900-01-01",
                                latest_date   = "2025-06-01",
                                code_kind = c("medcode", "prodcode")) {
  
  # Medcode for diagnoses, prodcode for medications
  code_kind <- match.arg(code_kind)
  # Pick column names based on code kind
  if (code_kind == "medcode") { # diagnosis
    eventdate <- "eventdate"
    sysdate <- "sysdate"
  } else { # medication
    eventdate <- "issuedate"
    sysdate <- "enterdate"
  }
  
  # Ensure quote identifiers are safe for SQL table/view names
  in_tbl <- as.character(DBI::dbQuoteIdentifier(connection, in_table))
  out_tbl <- as.character(DBI::dbQuoteIdentifier(connection, out_table))
  
  # Input:  eventdate/sysdate are TEXT in dd/mm/YYYY
  # Output: eventdate/sysdate are DATE, and rows are filtered
  sql <- sprintf("
    CREATE OR REPLACE TABLE %1$s AS
    WITH parsed AS (
      SELECT
        t.*,
        -- Convert to dates (invalid becomes NULL)
        CAST(try_strptime(%2$s, '%%d/%%m/%%Y') AS DATE) AS event_dt,
        CAST(try_strptime(%3$s,   '%%d/%%m/%%Y') AS DATE) AS sys_dt
      FROM %4$s
    ),
    step1 AS (
      SELECT
        * EXCLUDE (event_dt, sys_dt)

        -- Apply earliest/latest filters to event_dt1: NULL if < earliest OR > latest
        CASE
          WHEN event_dt < DATE '%5$s' THEN NULL
          WHEN event_dt > DATE '%6$s' THEN NULL
          ELSE event_dt
        END AS event_dt1,
        -- Apply earliest filter to sys_dt1: NULL if < earliest 
        CASE
          WHEN sys_dt < DATE '%5$s' THEN NULL
          ELSE sys_dt
        END AS sys_dt1
      FROM parsed
    ),
    step2 AS (
      SELECT
        * EXCLUDE (event_dt1, sys_dt1),
        
        -- event_dt2: replace suspicious early eventdate if sysdate looks plausible
        CASE
          WHEN event_dt1 < DATE '1910-01-01' AND sys_dt1 > DATE '1990-01-01'
            THEN sys_dt1
          ELSE event_dt1
        END AS event_dt2
      FROM step1
    )
    SELECT
      -- Keep columns: medcode/prodcode, term, patid, constype, consid, database, source
      * EXCLUDE (event_dt2, sys_dt1)
      
      -- Fill in missing eventdate using sysdate where possible 
      -- Set final names: eventdate/issuedate or sysdate/enterdate
      COALESCE(event_dt2, sys_dt1) AS %2$s,
      sys_dt1 AS %3$s,
      
    FROM step2
    WHERE
      -- Filter out those with NULL for both eventdate and sysdate
      COALESCE(event_dt2, sys_dt1) IS NOT NULL
      -- Filter out those with final eventdate after latest date
      AND COALESCE(event_dt2, sys_dt1) <= DATE '%6$s';
    ", out_tbl, eventdate, sysdate, in_tbl, earliest_date, latest_date)
  
  DBI::dbExecute(connection, sql)
  invisible(out_table)
}


# Extract with set columns
gold_extract_sql_medcode <- function(connection, out_table, files_glob, gold_codes_table, 
                                      source_label){
  
  # Make sure correct output and codelist table names
  out_tbl <- as.character(DBI::dbQuoteIdentifier(connection, out_table))
  code_tbl <- as.character(DBI::dbQuoteIdentifier(connection, gold_codes_table))
  
  
  sql <- sprintf("
    -- Read GOLD raw files
    
    CREATE OR REPLACE TEMP VIEW gold_rawfile AS
    SELECT * FROM read_csv('%1$s',
      delim='\\t', 
      header=true, 
      all_varchar=true,   -- all columns as characters
      null_padding=true,  -- pad shorter rows
      strict_mode=false,  -- tolerate irregular rows
      quote='', escape=''
    );
    
    
    -- Create out_tbl, keeping only rows matching code list medcode (from code_tbl)
    -- Attach term from code list
    -- Also add in database identifier and source type
    
    CREATE OR REPLACE TABLE %2$s AS
    SELECT
      trim(g.medcode) AS medcode,
      c.term          AS term,
      g.patid         AS patid,
      g.eventdate     AS eventdate,   -- keep as TEXT for now
      g.sysdate       AS sysdate,     -- keep as TEXT for now
      g.constype      AS constype,
      g.consid        AS consid,
      'Gold'          AS database,
      '%3$s'          AS source
    FROM gold_rawfile g
    INNER JOIN %4$s c
      ON trim(g.medcode) = trim(c.medcode);
  ", 
                 files_glob, 
                 out_tbl, 
                 source_label, 
                 code_tbl
  )
  
  # Execute SQL commands
  DBI::dbExecute(connection, sql)
  # Don't print out 'out_table' by default when returned, but can still call 
  # on it if necessary 
  invisible(out_table)
}

# Fixed columns
aurum_extract_sql_medcode <- function(connection, out_table, files_glob, aurum_codes_table, 
                                      source_label){
  
  # Make sure correct output and codelist table names
  out_tbl <- as.character(DBI::dbQuoteIdentifier(connection, out_table))
  code_tbl <- as.character(DBI::dbQuoteIdentifier(connection, aurum_codes_table))
  
  
  sql <- sprintf("
    -- Read Aurum raw files
    
    CREATE OR REPLACE TEMP VIEW aurum_rawfile AS
    SELECT * FROM read_csv('%1$s',
      delim='\\t', 
      header=true, 
      all_varchar=true,   -- all columns as characters
      null_padding=true,  -- pad shorter rows
      strict_mode=false,  -- tolerate irregular rows
      quote='', escape=''
    );
    
    
    -- Create out_tbl, keeping only rows matching code list medcode (from code_tbl)
    -- Attach term from code list
    -- Also add in database identifier and source type
    
    CREATE OR REPLACE TABLE %2$s AS
    SELECT
      trim(a.medcodeid) AS medcode,
      c.term          AS term,
      a.patid         AS patid,
      a.obsdate       AS eventdate,   -- keep as TEXT for now
      a.enterdate     AS sysdate,     -- keep as TEXT for now
      a.obstypeid     AS constype,
      a.consid        AS consid,
      'Aurum'         AS database,
      '%3$s'          AS source
    FROM aurum_rawfile a
    INNER JOIN %4$s c
      ON trim(a.medcodeid) = trim(c.medcodeid);
  ", 
                 files_glob, 
                 out_tbl, 
                 source_label, 
                 code_tbl
  )
  
  # Execute SQL commands
  DBI::dbExecute(connection, sql)
  # Don't print out 'out_table' by default when returned, but can still call 
  # on it if necessary 
  invisible(out_table)
}


# Function to transform dates into date format and filter invalid dates
transform_dates_sql_medcode <- function(connection, 
                                in_table, 
                                out_table, 
                                earliest_date = "1900-01-01",
                                latest_date   = "2025-06-01") {
  # Ensure quote identifiers are safe for SQL table/view names
  in_tbl <- as.character(DBI::dbQuoteIdentifier(connection, in_table))
  out_tbl <- as.character(DBI::dbQuoteIdentifier(connection, out_table))
  
  # Input:  eventdate/sysdate are TEXT in dd/mm/YYYY
  # Output: eventdate/sysdate are DATE, and rows are filtered
  sql <- sprintf("
    CREATE OR REPLACE TABLE %1$s AS
    WITH parsed AS (
      SELECT
        medcode, term, patid, constype, consid, database, source,
        -- Convert to dates
        CAST(try_strptime(eventdate, '%%d/%%m/%%Y') AS DATE) AS event_dt,
        CAST(try_strptime(sysdate,   '%%d/%%m/%%Y') AS DATE) AS sys_dt
      FROM %2$s
    ),
    step1 AS (
      SELECT
        medcode, term, patid, constype, consid, database, source,
        -- event_dt1: NULL if < earliest OR > latest
        CASE
          WHEN event_dt < DATE '%3$s' THEN NULL
          WHEN event_dt > DATE '%4$s' THEN NULL
          ELSE event_dt
        END AS event_dt1,
        -- sys_dt1: NULL if < earliest 
        CASE
          WHEN sys_dt < DATE '%3$s' THEN NULL
          ELSE sys_dt
        END AS sys_dt1
      FROM parsed
    ),
    step2 AS (
      SELECT
        medcode, term, patid, constype, consid, database, source, sys_dt1,
        -- event_dt2: replace suspicious early eventdate if sysdate looks plausible
        CASE
          WHEN event_dt1 < DATE '1910-01-01' AND sys_dt1 > DATE '1990-01-01'
            THEN sys_dt1
          ELSE event_dt1
        END AS event_dt2
      FROM step1
    )
    SELECT
      medcode, term, patid,
      -- Fill in missing eventdate using sysdate where possible
      COALESCE(event_dt2, sys_dt1) AS eventdate,
      sys_dt1 AS sysdate,
      constype, consid, database, source
    FROM step2
    WHERE
      -- Filter out those with NULL for both eventdate and sysdate
      COALESCE(event_dt2, sys_dt1) IS NOT NULL
      -- Filter out those with final eventdate after latest date
      AND COALESCE(event_dt2, sys_dt1) <= DATE '%4$s';
    ",
                 out_tbl, in_tbl, earliest_date, latest_date)
  
  DBI::dbExecute(connection, sql)
  invisible(out_table)
}


# Function to transform dates into date format and filter invalid dates
transform_dates_sql_medcode2 <- function(connection, 
                                        in_table, 
                                        out_table, 
                                        earliest_date = "1900-01-01",
                                        latest_date   = "2025-06-01") {
  # Ensure quote identifiers are safe for SQL table/view names
  in_tbl <- as.character(DBI::dbQuoteIdentifier(connection, in_table))
  out_tbl <- as.character(DBI::dbQuoteIdentifier(connection, out_table))
  
  # Input:  eventdate/sysdate are TEXT in dd/mm/YYYY
  # Output: eventdate/sysdate are DATE, and rows are filtered
  sql <- sprintf("
  CREATE OR REPLACE TABLE %1$s AS
  WITH parsed AS (
    SELECT 
      t.* EXCLUDE (eventdate, sysdate),
      -- Convert to dates
      CAST(try_strptime(eventdate, '%%d/%%m/%%Y') AS DATE) AS event_dt,
      CAST(try_strptime(sysdate,   '%%d/%%m/%%Y') AS DATE) AS sys_dt
    FROM %2$s t
  ),
  step1 AS (
    SELECT 
      *,
      -- event_dt1: NULL if < earliest OR > latest
      CASE
        WHEN event_dt < DATE '%3$s' THEN NULL
        WHEN event_dt > DATE '%4$s' THEN NULL
        ELSE event_dt
      END AS event_dt1,
      -- sys_dt1: NULL if < earliest 
      CASE
        WHEN sys_dt < DATE '%3$s' THEN NULL
        ELSE sys_dt
      END AS sys_dt1
    FROM parsed
  ),
  step2 AS (
    SELECT 
      *,
      -- event_dt2: replace suspicious early eventdate if sysdate looks plausible
      CASE
        WHEN event_dt1 < DATE '1910-01-01' AND sys_dt1 > DATE '1990-01-01'
          THEN sys_dt1
        ELSE event_dt1
      END AS event_dt2
    FROM step1
  )
  SELECT 
    step2.* EXCLUDE (event_dt, sys_dt, event_dt1, sys_dt1, event_dt2),
    -- Fill in missing eventdate using sysdate where possible
    COALESCE(event_dt2, sys_dt1) AS eventdate,
    sys_dt1 AS sysdate,
  FROM step2
  WHERE
    -- Filter out those with NULL for both eventdate and sysdate
    COALESCE(event_dt2, sys_dt1) IS NOT NULL
    -- Filter out those with final eventdate after latest date
    AND COALESCE(event_dt2, sys_dt1) <= DATE '%4$s';
  ",
                                   out_tbl, in_tbl, earliest_date, latest_date)
  
  DBI::dbExecute(connection, sql)
  invisible(out_table)
}


