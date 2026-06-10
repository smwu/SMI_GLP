# ====================================================================
# Create table of sociodemographic and clinical characteristics of patients 
# prescribed GLP-1RA medications for T2DM among those with and without SMI
# Author: SM Wu
# Date Created: 2026/04/24
# Date Updated: 2026/04/24
# 
# Details:
# This script creates table of sociodemographic and clinical 
# characteristics of patients prescribed GLP-1RA medications 
# for T2DM among those with and without SMI.
# 
# Inputs:
# A patient-level dataset called `analysis_df`, with one row per patient.
# 
# Outputs:
#   1. Table1_GLP1RA_SMI.docx  -> formatted Word table for manuscript use
#   2. Table1_GLP1RA_SMI.csv   -> raw table for checking / reuse
# 
# =========================================================
# REQUIRED VARIABLE NAMES AND EXPECTED VALUES IN `analysis_df`
# 
# Identifier:
#   patid                Unique patient identifier
#
# Grouping variables:
#   smi                  Severe mental illness indicator
#                        Expected values: 1/0, TRUE/FALSE
#                        1 = SMI diagnosis, 0 = no SMI diagnosis
#
#   any_glp1ra           Any GLP-1RA prescribed during the study period
#                        Expected values: 1/0, TRUE/FALSE
#                        1 = prescribed GLP-1RA, 0 = not prescribed GLP-1RA
#
# Sociodemographic variables:
#   female               Sex indicator for female
#                        Expected values: 1/0, TRUE/FALSE
#                        1 = female, 0 = not female
#
#   age_study            Age during the study period
#                        Expected values: continuous numeric
#                        Summary displayed as median (IQR)
#
#   ethnicity_cat        Ethnicity category
#                        Expected values exactly:
#                        "White", "Black", "Asian", "Mixed/Other", "Unknown"
#
#   region               Geographic region
#                        Expected values exactly:
#                        "East Midlands"
#                        "East of England"
#                        "London"
#                        "North East"
#                        "North West"
#                        "Northern Ireland"
#                        "Scotland"
#                        "South East"
#                        "South West"
#                        "Wales"
#                        "West Midlands"
#                        "Yorkshire & The Humber"
#
#   imd_quintile         English 2019 IMD quintile
#                        Expected values exactly:
#                        "1 (Least deprived)", "2", "3", "4",
#                        "5 (Most deprived)", "Unknown"
#
# Health behaviour variables:
#   smoking_3cat         Smoking status
#                        Expected values exactly:
#                        "Prior or current", "Never", "Unknown"
#
#   alcohol_3cat         Alcohol misuse status
#                        Expected values exactly:
#                        "Prior or current", "Never", "Unknown"
#
#   substance_3cat       Substance misuse status
#                        Expected values exactly:
#                        "Prior or current", "Never", "Unknown"
#
# Continuous study variables:
#   time_registered_yrs  Time actively registered during the study period
#                        Expected values: continuous numeric
#                        Example unit: years
#
#   gp_attendances       GP attendances during the study period
#                        Expected values: continuous numeric
#                        Summary displayed as median (IQR)
#
# Comorbidity indicators (all 1/0 or TRUE/FALSE):
#   comorb_t2d_only
#   comorb_obesity_only
#   comorb_cvd_only
#   comorb_t2d_obesity
#   hypertension
#   dyslipidemia
#   cardiovascular_disease
#   stroke
#   liver_disease
#   chronic_kidney_disease
#
# Other medication indicators during study period (all 1/0 or TRUE/FALSE):
#   metformin
#   insulin
#   orlistat
#   anticoagulants
#   antiplatelets
#   antihypertensives
#   lipid_regulating
#
# =====================================================================


# Clear memory
rm(list = ls())
gc()

# Packages
library(dplyr)
library(tidyr)
library(tidylog)
library(stringr)
library(lubridate)
library(purrr)
library(flextable)
library(officer)
library(readr)
library(data.table)
library(DBI)     # database interface
library(duckdb)  # connect to SQL



# ============= 2) Prepare variables ===============================

# Upload cohort demographics to duckdb as a table
DBI::dbWriteTable(connection, "cohort_patids", cohort_patids,
                  overwrite = TRUE)
# Select relevant variables from cohort table
cohort_table <- cohort_demog_dx_date_smi %>%
  select(patid, pracid, gender, yob, regstartdate, regenddate, deathdate, database, 
         startfollow, endfollow, index_date, smi_group, smi_dx_date, 
         date_schiz, date_bpd, date_psych)


# Create SMI status at start of follow-up
smi_at_startfollow <- apply_smi_hierarchy(cohort_data = cohort_table, 
                                          cutoff_date_var = "startfollow")
cohort_table$smi_at_fu <- smi_at_startfollow$latest_smi_group
cohort_table$smi_dx_date_at_fu <- smi_at_startfollow$latest_smi_dx_date
### NEED TO CREATE!!!
# SMI at first_glp




## Smoking
var <- "Smoking"
var_path <- paste0(path_input_extract, var, "/pat_comb_final.parquet")

# Read extracted variable data from parquet, restricting to records from 
# patients in the cohort
DBI::dbExecute(connection, sprintf("
  CREATE OR REPLACE TABLE var_cohort AS
  SELECT 
    a.patid, a.medcode, a.term, a.eventdate, a.database
  FROM read_parquet('%s') a
  WHERE EXISTS (
    SELECT 1
    FROM cohort_patids p
    WHERE p.patid = a.patid
  );", var_path))

# Pull data into R. Note: may be quite large
var_data <- dbGetQuery(connection, "SELECT * FROM var_cohort")

var_data[, var] <- "Yes"

# Convert to DT
setDT(var_data)
setDT(cohort_table)

# Restrict to records before start of follow-up
var_data_prior <- cohort_table[, .(patid, startfollow, endfollow)
][                              
  var_data, 
  on = "patid", 
  nomatch = 0
][
  eventdate < startfollow
]
nrow(var_data_prior) # 11,073,313
setorder(var_data_prior, patid, -eventdate)
var_data_prior_unique <- var_data_prior[,
  .SD[1],
  by = patid
]
nrow(var_data_prior_unique) # 1,546,691
uniqueN(var_data_prior_unique$patid)




# a.patid, a.prodcode, a.productname, a.antidiabetic, a.eventdate, a.sysdate, a.database


# ============= 1) Prepare analysis dataset ===============================

load(paste0(wd, path_input, "cohort_demog_cleaned_with_smi.RData"))

analysis_df

required_vars <- c(
  "patid", "smi", "any_glp1ra", "female", "age_study", "ethnicity_cat",
  "region", "imd_quintile", "smoking_3cat", "alcohol_3cat", "substance_3cat",
  "time_registered_yrs", "gp_attendances", "comorb_t2d_only",
  "comorb_obesity_only", "comorb_cvd_only", "comorb_t2d_obesity",
  "hypertension", "dyslipidemia", "cardiovascular_disease", "stroke",
  "liver_disease", "chronic_kidney_disease", "metformin", "insulin",
  "orlistat", "anticoagulants", "antiplatelets", "antihypertensives",
  "lipid_regulating"
)

missing_vars <- setdiff(required_vars, names(analysis_df))
if (length(missing_vars) > 0) {
  stop(
    "The following required variables are missing from analysis_df: ",
    paste(missing_vars, collapse = ", ")
  )
}

df <- analysis_df %>%
  transmute(
    patid,
    smi,
    glp = any_glp1ra,
    female,
    age = age_study,
    ethnicity = ethnicity_cat,
    region,
    imd = imd_quintile,
    smoking = smoking_3cat,
    alcohol = alcohol_3cat,
    substance = substance_3cat,
    time_registered = time_registered_yrs,
    gp_attendances,
    comorb_t2d_only,
    comorb_obesity_only,
    comorb_cvd_only,
    comorb_t2d_obesity,
    hypertension,
    dyslipidemia,
    cardiovascular = cardiovascular_disease,
    stroke,
    liver_disease,
    ckd = chronic_kidney_disease,
    metformin,
    insulin,
    orlistat,
    anticoagulants,
    antiplatelets,
    antihypertensives,
    lipid_regulating
  ) %>%
  mutate(
    smi = smi %in% c(1, TRUE),
    glp = glp %in% c(1, TRUE),
    grp = case_when(
      smi  & glp  ~ "SMI diagnosis\nGLP",
      smi  & !glp ~ "SMI diagnosis\nNo GLP",
      !smi & glp  ~ "No SMI diagnosis\nGLP",
      !smi & !glp ~ "No SMI diagnosis\nNo GLP"
    ),
    ethnicity = factor(
      ethnicity,
      levels = c("White", "Black", "Asian", "Mixed/Other", "Unknown")
    ),
    imd = factor(
      imd,
      levels = c("1 (Least deprived)", "2", "3", "4", "5 (Most deprived)", "Unknown")
    ),
    smoking = factor(
      smoking,
      levels = c("Prior or current", "Never", "Unknown")
    ),
    alcohol = factor(
      alcohol,
      levels = c("Prior or current", "Never", "Unknown")
    ),
    substance = factor(
      substance,
      levels = c("Prior or current", "Never", "Unknown")
    )
  )

# Split once for repeated summaries
# Column percentages are calculated within each of the four table columns.
group_list <- split(df, df$grp)
group_order <- c(
  "SMI diagnosis\nGLP",
  "SMI diagnosis\nNo GLP",
  "No SMI diagnosis\nGLP",
  "No SMI diagnosis\nNo GLP"
)
group_list <- group_list[group_order]
group_n <- map_int(group_list, nrow)


# ============= 3) Build sociodemographic and clinical table ===============================

tab1 <- bind_rows(
  add_row("Sex, female, n (%)", map2_chr(group_list, group_n, ~ fmt_n_pct(.x$female, .y))),
  add_row("Age+, median (IQR)", map_chr(group_list, ~ fmt_median_iqr(.x$age))),
  
  add_row("Ethnicity, n (%)", rep("", 4)),
  add_row("   White",       map2_chr(group_list, group_n, ~ fmt_cat(.x$ethnicity, "White", .y))),
  add_row("   Black",       map2_chr(group_list, group_n, ~ fmt_cat(.x$ethnicity, "Black", .y))),
  add_row("   Asian",       map2_chr(group_list, group_n, ~ fmt_cat(.x$ethnicity, "Asian", .y))),
  add_row("   Mixed/Other", map2_chr(group_list, group_n, ~ fmt_cat(.x$ethnicity, "Mixed/Other", .y))),
  add_row("   Unknown",     map2_chr(group_list, group_n, ~ fmt_cat(.x$ethnicity, "Unknown", .y))),
  
  add_row("Geographic region, n (%)", rep("", 4)),
  add_row("   East Midlands",          map2_chr(group_list, group_n, ~ fmt_cat(.x$region, "East Midlands", .y))),
  add_row("   East of England",        map2_chr(group_list, group_n, ~ fmt_cat(.x$region, "East of England", .y))),
  add_row("   London",                 map2_chr(group_list, group_n, ~ fmt_cat(.x$region, "London", .y))),
  add_row("   North East",             map2_chr(group_list, group_n, ~ fmt_cat(.x$region, "North East", .y))),
  add_row("   North West",             map2_chr(group_list, group_n, ~ fmt_cat(.x$region, "North West", .y))),
  add_row("   Northern Ireland",       map2_chr(group_list, group_n, ~ fmt_cat(.x$region, "Northern Ireland", .y))),
  add_row("   Scotland",               map2_chr(group_list, group_n, ~ fmt_cat(.x$region, "Scotland", .y))),
  add_row("   South East",             map2_chr(group_list, group_n, ~ fmt_cat(.x$region, "South East", .y))),
  add_row("   South West",             map2_chr(group_list, group_n, ~ fmt_cat(.x$region, "South West", .y))),
  add_row("   Wales",                  map2_chr(group_list, group_n, ~ fmt_cat(.x$region, "Wales", .y))),
  add_row("   West Midlands",          map2_chr(group_list, group_n, ~ fmt_cat(.x$region, "West Midlands", .y))),
  add_row("   Yorkshire & The Humber", map2_chr(group_list, group_n, ~ fmt_cat(.x$region, "Yorkshire & The Humber", .y))),
  
  add_row("English 2019 IMD quintile, n (%)", rep("", 4)),
  add_row("   1 (Least deprived)", map2_chr(group_list, group_n, ~ fmt_cat(.x$imd, "1 (Least deprived)", .y))),
  add_row("   2",                  map2_chr(group_list, group_n, ~ fmt_cat(.x$imd, "2", .y))),
  add_row("   3",                  map2_chr(group_list, group_n, ~ fmt_cat(.x$imd, "3", .y))),
  add_row("   4",                  map2_chr(group_list, group_n, ~ fmt_cat(.x$imd, "4", .y))),
  add_row("   5 (Most deprived)",  map2_chr(group_list, group_n, ~ fmt_cat(.x$imd, "5 (Most deprived)", .y))),
  add_row("   Unknown",            map2_chr(group_list, group_n, ~ fmt_cat(.x$imd, "Unknown", .y))),
  
  add_row("Smoker, n (%)", rep("", 4)),
  add_row("   Prior or current", map2_chr(group_list, group_n, ~ fmt_cat(.x$smoking, "Prior or current", .y))),
  add_row("   Never",            map2_chr(group_list, group_n, ~ fmt_cat(.x$smoking, "Never", .y))),
  add_row("   Unknown",          map2_chr(group_list, group_n, ~ fmt_cat(.x$smoking, "Unknown", .y))),
  
  add_row("Alcohol misuse, n (%)", rep("", 4)),
  add_row("   Prior or current", map2_chr(group_list, group_n, ~ fmt_cat(.x$alcohol, "Prior or current", .y))),
  add_row("   Never",            map2_chr(group_list, group_n, ~ fmt_cat(.x$alcohol, "Never", .y))),
  add_row("   Unknown",          map2_chr(group_list, group_n, ~ fmt_cat(.x$alcohol, "Unknown", .y))),
  
  add_row("Substance misuse, n (%)", rep("", 4)),
  add_row("   Prior or current", map2_chr(group_list, group_n, ~ fmt_cat(.x$substance, "Prior or current", .y))),
  add_row("   Never",            map2_chr(group_list, group_n, ~ fmt_cat(.x$substance, "Never", .y))),
  add_row("   Unknown",          map2_chr(group_list, group_n, ~ fmt_cat(.x$substance, "Unknown", .y))),
  
  add_row("Time actively registered+, median (IQR)",
          map_chr(group_list, ~ fmt_median_iqr(.x$time_registered))),
  add_row("GP attendances+, median (IQR)",
          map_chr(group_list, ~ fmt_median_iqr(.x$gp_attendances))),
  
  add_row("Comorbidities +, n (%)", rep("", 4)),
  add_row("   Type 2 diabetes (T2D) only", map2_chr(group_list, group_n, ~ fmt_n_pct(.x$comorb_t2d_only, .y))),
  add_row("   Obesity only",               map2_chr(group_list, group_n, ~ fmt_n_pct(.x$comorb_obesity_only, .y))),
  add_row("   CVD only",                   map2_chr(group_list, group_n, ~ fmt_n_pct(.x$comorb_cvd_only, .y))),
  add_row("   T2D and obesity",            map2_chr(group_list, group_n, ~ fmt_n_pct(.x$comorb_t2d_obesity, .y))),
  add_row("   Hypertension",               map2_chr(group_list, group_n, ~ fmt_n_pct(.x$hypertension, .y))),
  add_row("   Dyslipidemia",               map2_chr(group_list, group_n, ~ fmt_n_pct(.x$dyslipidemia, .y))),
  add_row("   Cardiovascular disease*",    map2_chr(group_list, group_n, ~ fmt_n_pct(.x$cardiovascular, .y))),
  add_row("   Stroke",                     map2_chr(group_list, group_n, ~ fmt_n_pct(.x$stroke, .y))),
  add_row("   Liver disease",              map2_chr(group_list, group_n, ~ fmt_n_pct(.x$liver_disease, .y))),
  add_row("   Chronic kidney disease",     map2_chr(group_list, group_n, ~ fmt_n_pct(.x$ckd, .y))),
  
  add_row("Other medications prescribed+, n (%)", rep("", 4)),
  add_row("   Metformin",                  map2_chr(group_list, group_n, ~ fmt_n_pct(.x$metformin, .y))),
  add_row("   Insulin",                    map2_chr(group_list, group_n, ~ fmt_n_pct(.x$insulin, .y))),
  add_row("   Orlistat",                   map2_chr(group_list, group_n, ~ fmt_n_pct(.x$orlistat, .y))),
  add_row("   Anticoagulants",             map2_chr(group_list, group_n, ~ fmt_n_pct(.x$anticoagulants, .y))),
  add_row("   Antiplatelets",              map2_chr(group_list, group_n, ~ fmt_n_pct(.x$antiplatelets, .y))),
  add_row("   Antihypertensives",          map2_chr(group_list, group_n, ~ fmt_n_pct(.x$antihypertensives, .y))),
  add_row("   Lipid-regulating medications", map2_chr(group_list, group_n, ~ fmt_n_pct(.x$lipid_regulating, .y)))
)

# Add group sizes to the column headers
names(tab1) <- c(
  "Characteristic",
  paste0("SMI diagnosis\nGLP\nN=", format(group_n[1], big.mark = ",")),
  paste0("SMI diagnosis\nNo GLP\nN=", format(group_n[2], big.mark = ",")),
  paste0("No SMI diagnosis\nGLP\nN=", format(group_n[3], big.mark = ",")),
  paste0("No SMI diagnosis\nNo GLP\nN=", format(group_n[4], big.mark = ","))
)

# # Save raw table for checking / reproducibility
# write_csv(tab1, out_csv, na = "")


### Create formatted Word table

ft <- flextable(tab1) %>%
  autofit() %>%
  width(j = 1, width = 2.9) %>%
  fontsize(size = 9, part = "all") %>%
  bold(i = ~ !str_detect(Characteristic, "^\\s"), j = 1, bold = TRUE) %>%
  align(j = 1, align = "left", part = "all") %>%
  align(j = 2:5, align = "center", part = "all") %>%
  valign(valign = "top", part = "all") %>%
  theme_booktabs()

doc <- read_docx() %>%
  body_add_par(
    "Table 1. Characteristics of individuals prescribed and not prescribed GLP-1RA medications in primary care, among individuals with and without an SMI diagnosis, between 2000 and 2025.",
    style = "heading 1"
  ) %>%
  body_add_flextable(ft) %>%
  body_add_par("+During the study period", style = "Normal") %>%
  body_add_par(
    "*Cardiovascular disease consists of coronary artery disease, angina, and heart attack",
    style = "Normal"
  )

# # Save table in word document
# print(doc, target = out_docx)

message("Saved: ", out_docx)
message("Saved: ", out_csv)
