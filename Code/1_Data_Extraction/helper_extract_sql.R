# =====================================================
# Helper functions that use SQL for fast extraction of 
# medical or product files based on a given codelist
# =====================================================


extract_patients_medcode <- function(wd, path_input, path_gold, path_aurum, code_name, 
                                     path_extract_gold_clin, path_extract_gold_test, 
                                     path_extract_gold_ref, path_extract_aurum_obs, 
                                     save_rdata = TRUE) {
  
  # Globs for DuckDB
  gold_clin_glob <- file.path(path_extract_gold_clin, "*.txt")
  gold_test_glob <- file.path(path_extract_gold_test, "*.txt")
  gold_ref_glob  <- file.path(path_extract_gold_ref,  "*.txt")
  aurum_obs_glob <- file.path(path_extract_aurum_obs, "*.txt")
  
  ## Read in final code lists used to define the CPRD data extraction
  
  # GOLD code list
  gold_file_name <- list.files(path = paste0(wd, path_input),
                               pattern = paste0("^Gold_", code_name, "_codelist"))
  # Check date
  gold_file_name
  codelist_gold <- read_delim(
    file = paste0(wd, path_input, gold_file_name), 
    delim = "\t", escape_double = FALSE, 
    col_types = cols(medcode = col_character()),  trim_ws = TRUE)
  
  
  # AURUM code list
  aurum_file_name <- list.files(path = paste0(wd, path_input),
                                pattern = paste0("^Aurum_", code_name, "_codelist"))
  # Check date
  aurum_file_name
  codelist_aurum <- read_delim(
    file = paste0(wd, path_input, aurum_file_name), 
    delim = "\t", escape_double = FALSE, 
    col_types = cols(medcodeid = col_character()), trim_ws = TRUE)
  
  
  # ================= Use SQL for extraction ==================================
  # DuckDB in-memory connection
  connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  
  # Set up progress bar
  DBI::dbExecute(connection, "SET enable_progress_bar = true;")
  DBI::dbExecute(connection, "SET enable_progress_bar_print = true;")
  # Start timer
  start_time <- Sys.time()
  
  # Write code lists
  gold_codes_table  <- paste0(code_name, "_gold_codes")
  aurum_codes_table <- paste0(code_name, "_aurum_codes")
  dbWriteTable(connection, gold_codes_table,  codelist_gold,  overwrite = TRUE)
  dbWriteTable(connection, aurum_codes_table, codelist_aurum, overwrite = TRUE)
  
  # GOLD CLINICAL
  
  gold_extract_sql(connection = connection, 
                   out_table = "gold_clin_raw", 
                   files_glob = gold_clin_glob,
                   gold_codes_table = gold_codes_table, 
                   source_label = "Clinical",
                   code_kind = "medcode")
  print("Completed GOLD Clinical extract.")
  # Runtime check
  Sys.time() - start_time
  
  # GOLD TEST
  gold_extract_sql(connection = connection, 
                   out_table = "gold_test_raw", 
                   files_glob = gold_test_glob,
                   gold_codes_table = gold_codes_table, 
                   source_label = "Test",
                   code_kind = "medcode")
  print("Completed GOLD Test extract.")
  # Runtime check
  Sys.time() - start_time
  
  # GOLD REFERRAL
  gold_extract_sql(connection = connection, 
                   out_table = "gold_ref_raw", 
                   files_glob = gold_ref_glob,
                   gold_codes_table = gold_codes_table, 
                   source_label = "Referral",
                   code_kind = "medcode")
  print("Completed GOLD Referral extract.")
  # Runtime check
  Sys.time() - start_time
  
  # MERGE ALL GOLD FILES TOGETHER
  
  DBI::dbExecute(connection, "
    CREATE OR REPLACE TABLE pat_gold_raw AS
    SELECT 
      patid, medcode, term, eventdate, sysdate, constype, consid, database, source
    FROM gold_clin_raw
    UNION ALL SELECT 
      patid, medcode, term, eventdate, sysdate, constype, consid, database, source
    FROM gold_test_raw
    UNION ALL SELECT 
      patid, medcode, term, eventdate, sysdate, constype, consid, database, 
      source_1 AS source
    FROM gold_ref_raw;
  ")
  
  # Save raw extracted patient files matching code list conditions as parquet
  out_gold_raw_parquet <- paste0(path_output, "pat_gold_raw.parquet")
  dbExecute(connection, 
            sprintf("COPY pat_gold_raw TO '%s' (FORMAT parquet);", 
                    out_gold_raw_parquet))
  
  # Count number of patients in Gold before transforming dates
  gold_raw_by_source <- DBI::dbGetQuery(connection, "
  SELECT source,
         COUNT(*) AS n_rows,
         COUNT(DISTINCT patid) AS n_patids
  FROM pat_gold_raw
  GROUP BY source
  ORDER BY source;
  ")
  print("Gold raw by source, before transforming dates:")
  print(gold_raw_by_source)
  Sys.time() - start_time
  
  # ================= Read in CPRD Aurum data ==================================
  
  # AURUM OBSERVATION
  
  # Number of Aurum folders
  num_folders <- length(aurum_obs_glob)
  # Initialize output table names and destinations for multiple folders
  aurum_out_table_names <- character(length(aurum_obs_glob))
  aurum_out_save_names <- character(length(aurum_obs_glob))
  
  for (i in 1:num_folders) {
    
    out_tbl <- paste0("aurum_obs_raw_", i)
    
    aurum_extract_sql(connection = connection, 
                      out_table = out_tbl, 
                      files_glob = aurum_obs_glob[i],
                      aurum_codes_table = aurum_codes_table, 
                      source_label = "Observation",
                      code_kind = "medcodeid")
    
    
    # Save partial extracted patient files parquet 
    out_aurum_raw_parquet_i <- paste0(path_output, "pat_aurum_raw_", i, ".parquet")
    DBI::dbExecute(connection, sprintf(
      "COPY %s TO '%s' (FORMAT parquet);", 
      as.character(DBI::dbQuoteIdentifier(connection, out_tbl)),
      out_aurum_raw_parquet_i
    ))
    
    aurum_out_table_names[i] <- out_tbl
    aurum_out_save_names[i] <- out_aurum_raw_parquet_i
    
    print(paste0("Completed Aurum Observation extract part ", i))
    # Runtime check
    Sys.time() - start_time
    
    # Drop the per-folder table from duckdb to free memory
    DBI::dbExecute(connection, sprintf(
      "DROP TABLE IF EXISTS %s;",
      as.character(DBI::dbQuoteIdentifier(connection, out_tbl))
    ))
  }
  
  # Read all exported Parquet files back as one view/table inside duckDB (does not load all into R)
  # Merge all Aurum files from separate folders into one table
  parquet_glob <- paste0(path_output, "pat_aurum_raw_*.parquet")
  DBI::dbExecute(connection, sprintf("
  CREATE OR REPLACE TABLE pat_aurum_raw AS
  SELECT 
    patid, 
    medcodeid     AS medcode,
    term,
    obsdate       AS eventdate,   
    enterdate     AS sysdate,     
    obstypeid     AS constype,
    consid, database, source
  FROM read_parquet('%s');
  ", parquet_glob))
  
  # Save raw extracted patient files matching code list conditions as parquet
  out_aurum_raw_parquet <- paste0(path_output, "pat_aurum_raw.parquet")
  dbExecute(connection, 
            sprintf("COPY pat_aurum_raw TO '%s' (FORMAT parquet);", 
                    out_aurum_raw_parquet))
  
  # Count number of patients in Aurum before transforming dates
  aurum_raw_by_source <- DBI::dbGetQuery(connection, "
  SELECT source,
         COUNT(*) AS n_rows,
         COUNT(DISTINCT patid) AS n_patids
  FROM pat_aurum_raw
  GROUP BY source
  ORDER BY source;
  ")
  print("Aurum raw by source, before transforming dates")
  print(aurum_raw_by_source)
  
  # Runtime check
  Sys.time() - start_time
  
  # ================= Combine GOLD and Aurum and create data files ============
  
  ## Clean dates and filter out those with invalid dates
  
  # Gold
  transform_dates_sql_medcode2(connection = connection, 
                               in_table = "pat_gold_raw", 
                               out_table = "pat_gold_clean",
                               earliest_date = earliest_date,
                               latest_date = latest_date)
  print("Cleaned GOLD")
  # Runtime check
  Sys.time() - start_time
  
  # Aurum
  transform_dates_sql_medcode2(connection = connection, 
                               in_table = "pat_aurum_raw", 
                               out_table = "pat_aurum_clean",
                               earliest_date = earliest_date,
                               latest_date = latest_date)
  print("Cleaned Aurum")
  # Runtime check
  Sys.time() - start_time
  
  # Rearrange columns, add Gold and Aurum identifiers to patid, and drop duplicates
  # Note: output is number of rows in newly created table
  DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE pat_gold_final AS
  SELECT DISTINCT
    patid || '-G' AS patid,
    medcode, term, eventdate, sysdate, constype, consid, database, source
  FROM pat_gold_clean;
  ")
  
  DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE pat_aurum_final AS
  SELECT DISTINCT
    patid || '-A' AS patid,
    medcode, term, eventdate, sysdate, constype, consid, database, source
  FROM pat_aurum_clean;
  ")
  
  # Combine GOLD and Aurum extracted patient files
  DBI::dbExecute(connection, "
  CREATE OR REPLACE TABLE pat_comb_final AS
  SELECT DISTINCT * FROM (
    SELECT 
      medcode, term, patid, eventdate, sysdate, constype, consid, database, source
    FROM pat_gold_final
    UNION ALL
    SELECT 
      medcode, term, patid, eventdate, sysdate, constype, consid, database, source
    FROM pat_aurum_final
  ) x;
  ")
  
  ## Number of unique patients with condition
  
  # Gold
  cat("\nGold rows / patients:\n")
  print(dbGetQuery(
    connection, 
    "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_gold_final;"))
  
  # Aurum
  cat("\nAURUM rows / patients:\n")
  print(dbGetQuery(
    connection, 
    "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_aurum_final;"))
  
  # Total
  cat("\nTotal rows / patients:\n")
  print(dbGetQuery(
    connection, 
    "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_comb_final;"))
  
  
  ## Save patient data for GOLD and Aurum as parquet file
  out_comb_final_parquet <- paste0(path_output, "pat_comb_final.parquet")
  dbExecute(connection, 
            sprintf("COPY pat_comb_final TO '%s' (FORMAT parquet);", 
                    out_comb_final_parquet))
  print("Combined Cleaned GOLD and Aurum")
  
  # Save as .RData file if desired. Note: this can be a huge file
  if (save_rdata) {
    pat_comb_final <- DBI::dbGetQuery(connection, sprintf(
      "SELECT * FROM read_parquet('%s');", 
      out_comb_final_parquet))
    save(pat_comb_final,
         file = paste0(path_output, "pat_comb_final.RData"))
  }
  
  # Runtime check
  Sys.time() - start_time
  
  # Check how much memory was used
  gc()
  # Check tables
  dbListTables(connection)
  
  ### Remove data and shut down connection
  dbDisconnect(connection, shutdown = TRUE)
  cat("\nDone.\n")
  
}


extract_patients_prodcode <- function(wd, path_input, path_gold, path_aurum, code_name,
                                      path_extract_gold_ther, path_extract_aurum_drug,
                                      path_lookups_gold, path_lookups_aurum, save_rdata = TRUE) {
  
  # Globs for DuckDB
  gold_ther_glob <- file.path(path_extract_gold_ther, "*.txt")
  aurum_drug_glob <- file.path(path_extract_aurum_drug, "*.txt")
  
  
  ## Read in final code lists used to define the CPRD data extraction
  
  # GOLD code list
  gold_file_name <- list.files(path = paste0(wd, path_input),
                               pattern = paste0("^Gold_", code_name, "_codelist"))
  # Check date
  gold_file_name
  codelist_gold <- read_delim(
    file = paste0(wd, path_input, gold_file_name), 
    delim = "\t", escape_double = FALSE, 
    col_types = cols(prodcode = col_character()),  trim_ws = TRUE)
  
  
  # AURUM code list
  aurum_file_name <- list.files(path = paste0(wd, path_input),
                                pattern = paste0("^Aurum_", code_name, "_codelist"))
  # Check date
  aurum_file_name
  codelist_aurum <- read_delim(
    file = paste0(wd, path_input, aurum_file_name), 
    delim = "\t", escape_double = FALSE, 
    col_types = cols(prodcodeid = col_character(),
                     BNFChapter = col_character()), trim_ws = TRUE)
  
  # Start timer
  start_time <- Sys.time()
  
  # ================= PROCESS GOLD DATASET ============================================
  
  # Repeat for each folder with data
  for (i in 1:length(path_gold)) {
    path_gold_i <- path_gold[i]
    gold_ther_glob_i <- gold_ther_glob[i]
    
    # ================= Use SQL for extraction ==================================
    
    # DuckDB in-memory connection
    connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    
    # Allow spilling to local disk after hitting memory limit
    DBI::dbExecute(connection, "PRAGMA memory_limit = '40GB';")
    spill_dir <- "N:/Temp/duckdb_spill"
    dir.create(spill_dir, showWarnings = FALSE, recursive = TRUE)
    DBI::dbExecute(connection, sprintf("PRAGMA temp_directory='%s';", spill_dir))
    
    # Set up progress bar
    DBI::dbExecute(connection, "SET enable_progress_bar = true;")
    DBI::dbExecute(connection, "SET enable_progress_bar_print = true;")
    
    # Write code lists
    gold_codes_table  <- paste0(code_name, "_gold_codes")
    aurum_codes_table <- paste0(code_name, "_aurum_codes")
    dbWriteTable(connection, gold_codes_table,  codelist_gold,  overwrite = TRUE)
    dbWriteTable(connection, aurum_codes_table, codelist_aurum, overwrite = TRUE)
    
    # ===== Read in CPRD GOLD data 
    
    # GOLD THERAPY
    
    gold_extract_sql(connection = connection, 
                     out_table = "gold_ther_raw", 
                     files_glob = gold_ther_glob_i,
                     gold_codes_table = gold_codes_table, 
                     source_label = "Therapy",
                     code_kind = "prodcode")
    print("Completed GOLD Therapy extract.")
    # Runtime check
    Sys.time() - start_time
    
    # MERGE ALL GOLD FILES TOGETHER
    
    DBI::dbExecute(connection, "
    CREATE OR REPLACE TABLE pat_gold_raw AS
    SELECT 
      prodcode, productname, 
      patid, eventdate, sysdate, database, source, 
      formulation, route, ingredient, strength, 
      bnfcode, dosageid, qty, numdays, numpacks, packtype
    FROM gold_ther_raw;
    ")
    
    # Count number of patients in Gold before transforming dates
    gold_raw_by_source <- DBI::dbGetQuery(connection, "
    SELECT source,
           COUNT(*) AS n_rows,
           COUNT(DISTINCT patid) AS n_patids
    FROM pat_gold_raw
    GROUP BY source
    ORDER BY source;
    ")
    print(gold_raw_by_source)
    
    # Drop tables to free up memory
    DBI::dbExecute(connection, "
    DROP TABLE IF EXISTS gold_ther_raw;
    ")
    gc()
    
    print("Combined GOLD Raw")
    # Runtime check
    Sys.time() - start_time
    
    
    # ========= Tranform Gold dates 
    
    ## Clean dates and filter out those with invalid dates
    
    # Gold
    transform_dates_sql_medcode2(connection = connection, 
                                 in_table = "pat_gold_raw", 
                                 out_table = "pat_gold_clean",
                                 earliest_date = earliest_date,
                                 latest_date = latest_date)
    
    # Drop tables to free up memory
    DBI::dbExecute(connection, "
    DROP TABLE IF EXISTS pat_gold_raw;
    ")
    
    print("Cleaned GOLD")
    # Runtime check
    Sys.time() - start_time
    
    # Rearrange columns, add Gold identifiers to patid, and drop duplicates
    # Note: output is number of rows in newly created table
    # Slow runtime
    # NOTE: Had to remove the following columns for memory space:
    #   source, formulation, route, ingredient, strength,  
    DBI::dbExecute(connection, "
    CREATE OR REPLACE TABLE pat_gold_transform AS
    SELECT DISTINCT
      patid || '-G' AS patid,
      prodcode, productname, eventdate, sysdate, database, 
      bnfcode, dosageid, qty, numdays, numpacks, packtype
    FROM pat_gold_clean;
    ")
    
    cat("\nGold rows / patients:\n")
    print(dbGetQuery(
      connection, 
      "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_gold_transform;"))
    
    # Drop tables to free up memory
    DBI::dbExecute(connection, "
    DROP TABLE IF EXISTS pat_gold_clean;
    ")
    gc()  
    
    # ======== Add in Gold look up information 
    
    # Lookup file paths
    f_common_dosages_g <- paste0(path_lookups_gold, "common_dosages.txt")
    f_bnfcodes <- paste0(path_lookups_gold, "bnfcodes.txt") # medical entry
    f_packtype <- paste0(path_lookups_gold, "packtype.txt")  # test qualifier
    
    ## GOLD
    
    sql_lookup_g <- sprintf("
    -- Read in look up files
    -- common dosages
    CREATE OR REPLACE TEMP VIEW common_dosages_g AS
    SELECT *
    FROM read_csv('%1$s', 
      delim='\\t', 
      header=true, 
      all_varchar=true,   -- all columns as characters
      null_padding=true,  -- pad shorter rows
      strict_mode=false,  -- tolerate irregular rows
      quote='', escape=''
    );
    
    -- bnfcodes
    CREATE OR REPLACE TEMP VIEW bnfcodes AS
    SELECT *
    FROM read_csv('%2$s', 
      delim='\\t', 
      header=true, 
      all_varchar=true,   -- all columns as characters
      null_padding=true,  -- pad shorter rows
      strict_mode=false,  -- tolerate irregular rows
      quote='', escape=''
    );
    
    -- packtype
    CREATE OR REPLACE TEMP VIEW packtype AS
    SELECT *
    FROM read_csv('%3$s', 
      delim='\\t', 
      header=true, 
      all_varchar=true,   -- all columns as characters
      null_padding=true,  -- pad shorter rows
      strict_mode=false,  -- tolerate irregular rows
      quote='', escape=''
    );
    
    -- Standardise field names  and add in look up information
    CREATE OR REPLACE TABLE pat_gold_lookup AS
    SELECT
      -- choose final columns to exclude/rename from lookup files
      -- cdg columns: dosage_text, daily_dose, dose_number, dose_unit, dose_frequency, 
      --    dose_interval, choice_of_dose, dose_max_average, change_dose, dose_duration 
      -- b columns: bnf
      -- pt columns: packtype descriptions (renamed to packtype)
      g.*,
      
      cdg.* EXCLUDE (dosageid),
      b.* EXCLUDE (bnfcode),
      CAST(b.bnf AS VARCHAR) AS bnf,
      pt.* EXCLUDE (packtype, packtype_desc),
      pt.packtype_desc AS packtype
      
    -- Merge in lookup information (note that all columns are available for merging)
    FROM pat_gold_transform g
    LEFT JOIN common_dosages_g cdg ON g.dosageid = cdg.dosageid
    LEFT JOIN bnfcodes b ON g.bnfcode = b.bnfcode
    LEFT JOIN packtype pt ON g.packtype = pt.packtype;
    ", f_common_dosages_g, f_bnfcodes, f_packtype)
    
    # Execute SQL code for lookups
    DBI::dbExecute(connection, sql_lookup_g)
    
    print("Added Gold Lookups")
    
    # Drop tables to free up memory
    DBI::dbExecute(connection, "
    DROP TABLE IF EXISTS pat_gold_transform;
    ")
    
    # Finalize GOLD extracted patient files
    DBI::dbExecute(connection, "
      CREATE OR REPLACE TABLE pat_gold_final AS
      SELECT 
        prodcode, productname, patid, eventdate, sysdate, database, 
        qty, numdays, numpacks, packtype, bnf,
        dosage_text, daily_dose, dose_number, dose_unit, dose_frequency, 
        dose_interval, choice_of_dose, dose_max_average, change_dose, dose_duration 
      FROM pat_gold_lookup
    ")
    
    ## Number of unique patients with condition
    
    # Gold
    cat("\nGold rows / patients:\n")
    print(dbGetQuery(
      connection, 
      "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_gold_final;"))
    
    ## Save patient data for Gold as parquet file
    out_gold_final_parquet_i <- paste0(path_output, "pat_gold_final_", i, ".parquet")
    dbExecute(connection, 
              sprintf("COPY pat_gold_final TO '%s' (FORMAT parquet);", 
                      out_gold_final_parquet_i))
    print("Combined Cleaned Gold")
    
    # Disconnect and wipe the spillover folder
    dbDisconnect(connection, shutdown = TRUE)
    unlink(spill_dir, recursive = TRUE, force = TRUE)
    # Check how much memory was used
    gc()
    
    print(paste0("Gold ", i, " completed!"))
    # Runtime check
    Sys.time() - start_time
    
  }
  
  
  # ================= PROCESS AURUM DATASET ========================================
  
  # Repeat for each folder with data
  for (i in 1:length(path_aurum)) {
    path_aurum_i <- path_aurum[i]
    aurum_drug_glob_i <- aurum_drug_glob[i]
    
    # ===== Restart duckdb connection 
    connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    # Allow spilling to local disk after hitting memory limit
    DBI::dbExecute(connection, "PRAGMA memory_limit = '40GB';")
    spill_dir <- "N:/Temp/duckdb_spill"
    dir.create(spill_dir, showWarnings = FALSE, recursive = TRUE)
    DBI::dbExecute(connection, sprintf("PRAGMA temp_directory='%s';", spill_dir))
    # Set up progress bar
    DBI::dbExecute(connection, "SET enable_progress_bar = true;")
    DBI::dbExecute(connection, "SET enable_progress_bar_print = true;")
    
    # Write code lists
    aurum_codes_table <- paste0(code_name, "_aurum_codes")
    dbWriteTable(connection, aurum_codes_table, codelist_aurum, overwrite = TRUE)
    
    # ====== Read in CPRD Aurum data 
    
    # AURUM DRUGISSUE
    
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
      
      
      -- Create out_tbl aurum_drug_raw, keeping only rows matching code list medcode (from code_tbl)
      -- Attach term from code list
      -- Also add in database identifier and source type
      
      CREATE OR REPLACE TABLE %2$s AS
      SELECT
        trim(a.prodcodeid) AS prodcode,
        c.productname          AS productname,
        a.patid         AS patid,
        a.pracid        AS pracid,
        a.issuedate     AS eventdate,   -- keep as TEXT for now
        a.enterdate     AS sysdate,     -- keep as TEXT for now
        'Aurum'         AS database,
        '%3$s'          AS source,
        c.formulation   AS formulation,
        c.route         AS route,
        c.ingredient    AS ingredient,
        c.strength      AS strength,
        c.BNFChapter    AS BNFChapter,
        a.dosageid      AS dosageid,
        a.quantunitid   AS quantunitid,
        a.quantity      AS qty,
        a.duration      AS numdays
      FROM aurum_rawfile a
      INNER JOIN %4$s c
        ON trim(a.prodcodeid) = trim(c.prodcodeid);
    ", 
                   aurum_drug_glob_i, 
                   "aurum_drug_raw", 
                   "DrugIssue", 
                   aurum_codes_table
    )
    DBI::dbExecute(connection, sql)
    
    print(paste0("Completed Aurum DrugIssue extract part ", i))
    
    # Runtime check
    Sys.time() - start_time
    
    # MERGE ALL Aurum FILES TOGETHER
    
    DBI::dbExecute(connection, "
    CREATE OR REPLACE TABLE pat_aurum_raw AS
    SELECT 
      prodcode, productname, patid, eventdate, sysdate, database, 
      BNFChapter, dosageid, quantunitid, qty, numdays,
      pracid, source, formulation, route, ingredient, strength
    FROM aurum_drug_raw;
    ")
    
    # Count number of patients in Aurum before transforming dates
    aurum_raw_by_source <- DBI::dbGetQuery(connection, "
      SELECT database,
             COUNT(*) AS n_rows,
             COUNT(DISTINCT patid) AS n_patids
      FROM pat_aurum_raw
      GROUP BY database
      ORDER BY database;
    ")
    print(aurum_raw_by_source)
    
    # Drop tables to free up memory
    DBI::dbExecute(connection, "
      DROP TABLE IF EXISTS aurum_drug_raw;
    ")
    gc()
    
    print("Combined Aurum Raw")
    # Runtime check
    Sys.time() - start_time
    
    # ========= Tranform Aurum dates 
    
    # Aurum
    transform_dates_sql_medcode2(connection = connection, 
                                 in_table = "pat_aurum_raw", 
                                 out_table = "pat_aurum_clean",
                                 earliest_date = earliest_date,
                                 latest_date = latest_date)
    print("Cleaned Aurum")
    # Runtime check
    Sys.time() - start_time
    
    # Drop tables to free up memory
    DBI::dbExecute(connection, "
      DROP TABLE IF EXISTS pat_aurum_raw;
    ")
    
    # Rearrange columns, add Aurum identifiers to patid, and drop duplicates
    # Note: output is number of rows in newly created table
    # NOTE: Had to remove the following columns for memory space:
    #   source, formulation, route, ingredient, strength 
    DBI::dbExecute(connection, "
      CREATE OR REPLACE TABLE pat_aurum_transform AS
      SELECT DISTINCT
        patid || '-A' AS patid,
        prodcode, productname, pracid, eventdate, sysdate, database, 
        BNFChapter, dosageid, quantunitid, qty, numdays
      FROM pat_aurum_clean;
    ")
    
    cat("\nAurum rows / patients:\n")
    print(dbGetQuery(
      connection, 
      "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_aurum_transform;"))
    
    # Drop tables to free up memory
    DBI::dbExecute(connection, "
      DROP TABLE IF EXISTS pat_aurum_clean;
    ")
    gc()
    
    # ======== Add in Aurum look up information 
    
    f_common_dosages_a <- paste0(path_lookups_aurum, "common_dosages.txt") # observation type
    f_quantunit <- paste0(path_lookups_aurum, "QuantUnit.txt") # unit of measurement
    
    ## AURUM
    
    sql_lookup_a <- sprintf("
      -- Read in look up files
      -- Common dosages
      CREATE OR REPLACE TEMP VIEW common_dosages_a AS
      SELECT *
      FROM read_csv('%1$s', 
        delim='\\t', 
        header=true, 
        all_varchar=true,   -- all columns as characters
        null_padding=true,  -- pad shorter rows
        strict_mode=false,  -- tolerate irregular rows
        quote='', escape=''
      );
      
      -- Quantity unit
      CREATE OR REPLACE TEMP VIEW quantunit AS
      SELECT *
      FROM read_csv('%2$s', 
        delim='\\t', 
        header=true, 
        all_varchar=true,   -- all columns as characters
        null_padding=true,  -- pad shorter rows
        strict_mode=false,  -- tolerate irregular rows
        quote='', escape=''
      );
      
      -- Add in look up information
      CREATE OR REPLACE TABLE pat_aurum_lookup AS
      SELECT
        -- select all pat_aurum_transform columns
        a.*,
        a.BNFChapter AS bnf,
        
        -- choose final columns to exclude/rename from lookup files
        -- cda columns: dosage_text, daily_dose, dose_number, dose_unit, dose_frequency, 
        --    dose_interval, choice_of_dose, dose_max_average, change_dose, dose_duration 
        -- qu columns: packtype descriptions (renamed to packtype)
        cda.* EXCLUDE (dosageid),
        qu.* EXCLUDE (quantunitid),
        qu.Description AS packtype
        
      -- Merge in lookup information (note that all columns are available for merging)
      FROM pat_aurum_transform a
      LEFT JOIN common_dosages_a cda ON a.dosageid = cda.dosageid
      LEFT JOIN quantunit qu ON a.quantunitid = qu.quantunitid;
    ", f_common_dosages_a, f_quantunit)
    
    # Execute SQL code for lookups
    DBI::dbExecute(connection, sql_lookup_a)
    
    print("Added Aurum Lookups")
    
    # Drop tables to free up memory
    DBI::dbExecute(connection, "
      DROP TABLE IF EXISTS pat_aurum_transform;
    ")
    
    # Finalize Aurum extracted patient files
    DBI::dbExecute(connection, "
      CREATE OR REPLACE TABLE pat_aurum_final AS
      SELECT 
        prodcode, productname, patid, eventdate, sysdate, database, 
        qty, numdays, pracid, packtype, bnf,
        dosage_text, daily_dose, dose_number, dose_unit, dose_frequency, 
        dose_interval, choice_of_dose, dose_max_average, change_dose, dose_duration 
      FROM pat_aurum_lookup
    ")
    
    # Drop tables to free up memory
    DBI::dbExecute(connection, "
      DROP TABLE IF EXISTS pat_aurum_lookup;
    ")
    
    ## Number of unique patients with condition
    
    # Aurum
    cat("\nAURUM rows / patients:\n")
    print(dbGetQuery(
      connection, 
      "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_aurum_final;"))
    
    ## Save patient data for Aurum as parquet file
    out_aurum_final_parquet_i <- paste0(path_output, "pat_aurum_final_", i, ".parquet")
    dbExecute(connection, 
              sprintf("COPY pat_aurum_final TO '%s' (FORMAT parquet);", 
                      out_aurum_final_parquet_i))
    print("Combined Cleaned Aurum")
    
    gc()
    
    # Check tables
    dbListTables(connection)
    
    # Disconnect and wipe the spillover folder
    dbDisconnect(connection, shutdown = TRUE)
    closeAllConnections()
    unlink(spill_dir, recursive = TRUE, force = TRUE)
    
    print(paste0("Aurum ", i, " completed!"))
    # Runtime check
    Sys.time() - start_time
    
  }
  
  # ================= Combine GOLD and Aurum =====================
  
  # Restart duckdb connection
  
  connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  # Allow spilling to local disk after hitting memory limit
  DBI::dbExecute(connection, "PRAGMA memory_limit = '40GB';")
  spill_dir <- "N:/Temp/duckdb_spill"
  dir.create(spill_dir, showWarnings = FALSE, recursive = TRUE)
  DBI::dbExecute(connection, sprintf("PRAGMA temp_directory='%s';", spill_dir))
  
  
  ### Read in gold and aurum records
  
  out_file_names <- c(paste0("pat_gold_final_", 1:length(path_gold)),
                      paste0("pat_aurum_final_", 1:length(path_aurum)))
  extraction_files <- paste0(path_output, out_file_names, ".parquet")
  
  # Stack together all of the Gold and Aurum files
  merge_sql <- paste(sprintf(
    "SELECT
        prodcode, productname, patid, eventdate, sysdate, database, 
        qty, numdays, packtype, bnf,
        dosage_text, daily_dose, dose_number, dose_unit, dose_frequency,
        dose_interval, choice_of_dose, dose_max_average, change_dose, dose_duration
    FROM read_parquet('%s')",
    extraction_files), 
    collapse = "\n UNION ALL\n"
  )
  
  dbExecute(connection, sprintf(
    "CREATE OR REPLACE TABLE pat_comb_final AS
    SELECT DISTINCT 
        prodcode, productname, patid, eventdate, sysdate, database, 
        qty, numdays, packtype, bnf,
        dosage_text, daily_dose, dose_number, dose_unit, dose_frequency,
        dose_interval, choice_of_dose, dose_max_average, change_dose, dose_duration
    FROM (
      %s
    ) x", merge_sql
  ))
  
  
  ## Number of unique patients with condition
  
  # Total
  cat("\nTotal rows / patients:\n")
  print(dbGetQuery(
    connection, 
    "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT patid) AS n_pats FROM pat_comb_final;"))
  
  
  ## Save patient data for GOLD and Aurum as parquet file
  out_comb_final_parquet <- paste0(path_output, "pat_comb_final.parquet")
  dbExecute(connection, 
            sprintf("COPY pat_comb_final TO '%s' (FORMAT parquet);", 
                    out_comb_final_parquet))
  print("Combined Cleaned GOLD and Aurum")
  # Runtime check
  Sys.time() - start_time
  
  # Save as .RData file if desired
  if (save_rdata) {
    pat_comb_final <- DBI::dbGetQuery(connection, sprintf(
      "SELECT * FROM read_parquet('%s');", 
      out_comb_final_parquet))
    save(pat_comb_final,
         file = paste0(path_output, "pat_comb_final.RData"))
  }
  
  # Check how much memory was used
  gc()
  # Check tables
  dbListTables(connection)
  
  ### Remove data and shut down connection
  dbDisconnect(connection, shutdown = TRUE)
  cat("\nDone.\n")
  
}
