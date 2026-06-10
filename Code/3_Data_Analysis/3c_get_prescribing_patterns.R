# ====================================================================
# Calculate prevalence of GLP-1RA prescriptions over time
# Author: SM Wu
# Date Created: 2026/02/27
# Date Updated: 2026/04/13
# 
# Details:
# 
# Inputs:
# 
# Outputs:
# 
# =====================================================================

# Clear memory
rm(list = ls())
gc()

# Packages
library(dplyr)
library(tidyr)
library(gtsummary)
library(lubridate)
library(readr)
library(forcats)
library(data.table)
library(tidylog)
library(DBI)     # database interface
library(duckdb)  # connect to SQL
library(dbplyr)  # dplyr w/ SQL
library(stringr)
library(purrr)
library(scales)
library(ggpubr)  # multiple plots
library(data.table)
library(gt)      # format tables


# ============= 1) Set up and load data ===============================

# ### For running in Data Safe Haven
# Set working directory
wd <- "S:/CDSTP_CPRD_25_005368/"
setwd(wd)

# Set input and output paths
path_input <- paste0("SMI_GLP/Data/Cleaned_Data/")
path_input_extract <- paste0("SMI_GLP/Data/Extraction_Files/")
path_output <- paste0("SMI_GLP/Outputs/")

# Load in helper functions
source(paste0(wd, "SMI_GLP/Code/4_Data_Analysis/",
              "4_helper_functions.R"))


# Read in prescription data
# Read in GLP-1RA prescriptions:  242,327 patients w/ GLP prescriptions (335 removed)
load(paste0(wd, path_input, "t2dm_cohort_glp1ras.RData"))
# Remove duplicates
glp1ras <- glp1ras %>% distinct()
length(unique(glp1ras$patid))

# Read in Metformin prescriptions: 1,957,527 patients
load(paste0(wd, path_input, "t2dm_cohort_metformin.RData"))
metformin <- metformin %>% distinct()
length(unique(metformin$patid))

# Read in DPP4-i prescriptions: 587,785 patients
load(paste0(wd, path_input, "t2dm_cohort_dpp4is.RData"))
dpp4is <- dpp4is %>% distinct()
length(unique(dpp4is$patid))

# Read in SGLT2-i prescriptions: 531,866 patients
load(paste0(wd, path_input, "t2dm_cohort_sglt2is.RData"))
sglt2is <- sglt2is %>% distinct()
length(unique(sglt2is$patid))

# Read in Sulfonylureas prescriptions: 968,236 patients
load(paste0(wd, path_input, "t2dm_cohort_sulfonylureas.RData"))
sulfonylureas <- sulfonylureas %>% distinct()
length(unique(sulfonylureas$patid))

# Read in Insulin prescriptions: 552,968 patients
load(paste0(wd, path_input, "t2dm_cohort_insulin.RData"))
insulin <- insulin %>% distinct()
length(unique(insulin$patid))

# Read in other antidiabetic prescriptions: 299,283 patients
load(paste0(wd, path_input, "t2dm_cohort_other.RData"))
other <- other %>% distinct()
length(unique(other$patid))

gc()
# Combine all prescriptions and remove duplications
all_antidiab_prescriptions <- bind_rows(glp1ras, metformin, dpp4is,
                                        sglt2is, sulfonylureas, insulin, other)
# Save memory
rm(glp1ras, metformin, dpp4is, sglt2is, sulfonylureas, insulin, other)
gc()
# # Save
# save(all_antidiab_prescriptions, file = paste0(wd, path_input, "all_antidiab_prescriptions.RData"))
# # Read in all prescriptions for T2DM cohort. Takes a while. 23 GB
# load(paste0(wd, path_input, "all_antidiab_prescriptions.RData"))

# Total number of patients with antidiabetic prescriptions: 2,067,489
length(unique(all_antidiab_prescriptions$patid))


# Read in T2DM cohort. Loads as 'cohort_demog_dx_date_smi'. 7 GB
load(paste0(path_input, "cohort_demog_cleaned_with_smi.RData"))

# Those with incident T2DM during follow-up
cohort_inc_t2dm <- cohort_demog_dx_date_smi %>%
  filter(incident_t2dm == 1)

# Filter to those with incident T2DM: 1,254,862 patients
all_prescriptions_inc_t2dm <- all_antidiab_prescriptions %>%
  filter(patid %in% cohort_inc_t2dm$patid)
length(unique(all_prescriptions_inc_t2dm$patid))
# # Save
# save(all_prescriptions_inc_t2dm,
#      file = paste0(wd, path_input, "all_inc_t2dm_antidiab_prescriptions.RData"))
# # Read in incident T2DM antidiabetic prescriptions. 9 GB
# load(paste0(wd, path_input, "all_inc_t2dm_antidiab_prescriptions.RData"))

# Save memory
rm(all_antidiab_prescriptions, cohort_demog_dx_date_smi)
gc()


# Get number of prescriptions in incident T2DM cohort per group
inc_t2dm_summary <- all_prescriptions_inc_t2dm %>%
  group_by(antidiab_group) %>%
  summarise(n_patid = n_distinct(patid))
# Incident T2DM patients on metformin: 918,458
length(unique(all_prescriptions_inc_t2dm$patid[
  grepl("Metformin", all_prescriptions_inc_t2dm$antidiab_group)]))
# Incident T2DM patients on dpp4is: 238,760
length(unique(all_prescriptions_inc_t2dm$patid[
  grepl("DPP-4is", all_prescriptions_inc_t2dm$antidiab_group)]))
# Incident T2DM patients on sglt2is: 279,675
length(unique(all_prescriptions_inc_t2dm$patid[
  grepl("SGLT-2is", all_prescriptions_inc_t2dm$antidiab_group)]))
# Incident T2DM patients on sulfonylureas: 306,326
length(unique(all_prescriptions_inc_t2dm$patid[
  grepl("Sulfonylureas", all_prescriptions_inc_t2dm$antidiab_group)]))
# Incident T2DM patients on other (TZDs and AGIs): 46,151
length(unique(all_prescriptions_inc_t2dm$patid[
  grepl("Other", all_prescriptions_inc_t2dm$antidiab_group)]))
# Incident T2DM patients on glp1ras: 105,714
length(unique(all_prescriptions_inc_t2dm$patid[
  grepl("GLP-1RAs", all_prescriptions_inc_t2dm$antidiab_group)]))
# Incident T2DM patients on insulin: 144,637
length(unique(all_prescriptions_inc_t2dm$patid[
  grepl("Insulin", all_prescriptions_inc_t2dm$antidiab_group)]))


# ============= 3) Compute treatment pathways ===============================

# Convert to DT
setDT(all_prescriptions_inc_t2dm)

# Create DT of prescriptions per patient
# Collapse same-patient, same-day records into one regimen
# Runtime: around 1 hour
day_rx <- all_prescriptions_inc_t2dm[
  ,.(antidiab_group = paste(sort(unique(antidiab_group)), collapse = ", "),
     startfollow = min(startfollow, na.rm = TRUE),
     endfollow = max(endfollow, na.rm = TRUE)), 
  by = .(patid, eventdate)]
setorder(day_rx, patid, eventdate)

head(day_rx)
# Most common antidiabetic group same-day prescription combinations
sort(table(day_rx$antidiab_group), decreasing = TRUE)

# Parse combined classes into their component parts
regimen_lookup <- unique(day_rx[, .(antidiab_group)]) # 342 unique combos
regimen_lookup[, antidiab_group_parsed := lapply(antidiab_group, parse_classes)] # parse combos
head(regimen_lookup$antidiab_group_parsed)
# Join parsed classes back to day_rx. Column containing lists
day_rx[regimen_lookup, 
       on = "antidiab_group",
       antidiab_group_parsed := i.antidiab_group_parsed]
# # Save all antidiabetic groups per patient
# save(day_rx, file = paste0(wd, path_input, "day_rx.RData"))

# # Load in per-patient antidiabetic groups. 1 GB
# load(paste0(wd, path_input, "day_rx.RData"))



# Remove exact consecutive duplicate regimens
# Repeat refill of the same regimen is not a new line of therapy
day_rx_copy <- copy(day_rx)
day_rx_copy[
  ,
  prev_group := shift(antidiab_group), 
  by = patid
]
day_rx_unique_reg <- day_rx_copy[
  is.na(prev_group) | antidiab_group != prev_group
]
day_rx_unique_reg[
  ,
  line_no := seq_len(.N),
  by = patid
]


# For each patient and each set of prescription classes in a day, 
# get line number and newly introduced classes
# Note: takes 15 mins to run
lines <- day_rx_unique_reg[,
       c("line_no", 
         "is_new_line", 
         "newly_introduced", 
         "cumulative_classes") := {
           out <- build_lines_cumulative(classes_list = antidiab_group_parsed) 
           .(
             out$line_no,
             out$is_new_line,
             out$newly_introduced,
             out$cumulative_classes
           )
         },
       by = patid]
# Maximum line number is 7
summary(lines$line_no)

# # Save
# save(lines, file = paste0(wd, path_input, "lines.RData"))
# # Load in lines
# load(paste0(wd, path_input, "lines.RData"))

# Filter to new lines
new_lines <- lines[is_new_line == TRUE]

# Set one row per patient and set each new line as a column
# 971,806 patients
new_lines_wide <- dcast(new_lines,
                        patid + startfollow + endfollow ~ line_no,
                        value.var = c("eventdate", "newly_introduced", "cumulative_classes"),
                        sep = "_line")

# Merge in T2DM diagnosis date and SMI information
new_lines_wide <- new_lines_wide[, -c("startfollow", "endfollow")][
  cohort_inc_t2dm, on = 'patid']

# Count patients reaching each therapy line
count_lines <- data.table(
  line_no = c(NA, 1:7),
  line = c("Total", paste0(c("First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh"), "-line")),
  n_patients = c(
    new_lines_wide[, uniqueN(patid)],
    new_lines_wide[!is.na(eventdate_line1), uniqueN(patid)],
    new_lines_wide[!is.na(eventdate_line2), uniqueN(patid)],
    new_lines_wide[!is.na(eventdate_line3), uniqueN(patid)],
    new_lines_wide[!is.na(eventdate_line4), uniqueN(patid)],
    new_lines_wide[!is.na(eventdate_line5), uniqueN(patid)],
    new_lines_wide[!is.na(eventdate_line6), uniqueN(patid)],
    new_lines_wide[!is.na(eventdate_line7), uniqueN(patid)]
  )
)
count_lines

# Get days to each treatment line for each patient. Set to NA if a treatment line is not available
new_lines_wide[, `:=`(
  days_to_line1 = as.integer(eventdate_line1 - index_date),
  days_line1_to_line2 = as.integer(eventdate_line2 - eventdate_line1),
  days_line2_to_line3 = as.integer(eventdate_line3 - eventdate_line2),
  days_line3_to_line4 = as.integer(eventdate_line4 - eventdate_line3),
  days_line4_to_line5 = as.integer(eventdate_line5 - eventdate_line4),
  days_line5_to_line6 = as.integer(eventdate_line6 - eventdate_line5),
  days_line6_to_line7 = as.integer(eventdate_line7 - eventdate_line6)
)]

summary(new_lines_wide[, `days_to_line1`:`days_line6_to_line7`])


# ================ 4) Create tables ==================================================

### Treatment line progression table

antidiab_lines_tb_list <- 
  create_antidiab_lines_gt(cohort = cohort_inc_t2dm, 
                           new_lines_wide_dt = new_lines_wide)
antidiab_lines_tb_gt <- antidiab_lines_tb_list$antidiab_lines_tb_gt
antidiab_lines_tb_gt

# # Save table
# gtsave(antidiab_lines_tb_gt, 
#        filename = paste0(wd, path_output, "Tables/antidiab_trt_lines_", 
#                          today(), ".docx"))


### Create line class table

classes <- c("GLP-1RAs", "Metformin", "SGLT-2is", "DPP-4is", 
             "Sulfonylureas", "Other", "Insulin")

line_class_tb_list <- 
  create_line_class_gt(new_lines_wide_dt = new_lines_wide, 
                       classes = classes)
line_class_tb_gt <- line_class_tb_list$line_class_tb_gt
line_class_tb_gt
# # Save table
# gtsave(line_class_tb_gt, 
#        filename = paste0(wd, path_output, "Tables/new_line_classes_", 
#                          today(), ".docx"))


### Table among those w/ GLP-1RA prescriptions, examining past exposure 
### to other antidiabetic classes

glp1ra_line_tb_list <- 
  create_glp1ra_line_gt(new_lines_wide_dt = new_lines_wide, 
                       classes_no_glp = classes[-1])
glp1ra_line_tb_gt <- glp1ra_line_tb_list$glp1ra_line_tb_gt
glp1ra_line_tb_gt

# # Save table
# gtsave(glp1ra_line_tb_gt, 
#        filename = paste0(wd, path_output, "Tables/glp1ra_prior_classes_", 
#                          today(), ".docx"))


# ============= 5) Create tables for SMI group ====================================

# Get SMI status at time of T2DM diagnosis
smi_at_t2dm_dx <- apply_smi_hierarchy(cohort_data = cohort_inc_t2dm, 
                                      cutoff_date_var = "index_date")
# Add in SMI status and date of latest SMI diagnosis prior to T2DM diagnosis to
# incident T2DM cohort
cohort_inc_t2dm$smi_at_t2dm_dx <- smi_at_t2dm_dx$latest_smi_group
cohort_inc_t2dm$smi_at_t2dm_dx_date <- smi_at_t2dm_dx$latest_smi_dx_date

# Restrict to those w/ SMI at T2DM dx
# Removed 905,601 (72%) patients. 349,261 patients remaining
cohort_inc_t2dm_smi <- cohort_inc_t2dm %>%
  filter(!is.na(smi_at_t2dm_dx))

# Restrict prescriptions to these patients
new_lines_wide_smi <- new_lines_wide %>%
  filter(patid %in% cohort_inc_t2dm_smi$patid)

### Treatment line progression

antidiab_lines_smi_tb_list <- 
  create_antidiab_lines_gt(cohort = cohort_inc_t2dm_smi, 
                           new_lines_wide_dt = new_lines_wide_smi)
antidiab_lines_smi_tb_gt <- antidiab_lines_smi_tb_list$antidiab_lines_tb_gt
antidiab_lines_smi_tb_gt %>% 
  tab_header(title = md("Antidiabetic Treatment Line Progression for Patients with SMI at T2DM Diagnosis"),
             subtitle = "Treatment initiation among patients with incident T2DM")

# # Save table
# gtsave(antidiab_lines_smi_tb_gt,
#        filename = paste0(wd, path_output, "Tables/antidiab_trt_lines_smi_",
#                          today(), ".docx"))


### Create line class table

classes <- c("GLP-1RAs", "Metformin", "SGLT-2is", "DPP-4is", 
             "Sulfonylureas", "Other", "Insulin")

line_class_smi_tb_list <- 
  create_line_class_gt(new_lines_wide_dt = new_lines_wide_smi, 
                           classes = classes)
line_class_smi_tb_gt <- line_class_smi_tb_list$line_class_tb_gt
line_class_smi_tb_gt %>% 
  tab_header(title = md("Newly Introduced Antidiabetic Classes by Treatment Line for Patients with SMI at T2DM Diagnosis"),
             subtitle = "Counts and percentages among patients with incident T2DM starting each treatment line")

# # Save table
# gtsave(line_class_smi_tb_gt, 
#        filename = paste0(wd, path_output, "Tables/new_line_classes_smi_", 
#                          today(), ".docx"))


### Table among those w/ GLP-1RA prescriptions, examining past exposure 
### to other antidiabetic classes

glp1ra_line_smi_tb_list <- 
  create_glp1ra_line_gt(new_lines_wide_dt = new_lines_wide_smi, 
                        classes_no_glp = classes[-1])
glp1ra_line_smi_tb_gt <- glp1ra_line_smi_tb_list$glp1ra_line_tb_gt
glp1ra_line_smi_tb_gt %>% 
  tab_header(title = md("Prior Antidiabetic Prescribing Among Patients with SMI and Incident T2DM Starting GLP-1RAs"),
             subtitle = "Counts and percentages among patients with incident T2DM starting each treatment line")

# # Save table
# gtsave(glp1ra_line_smi_tb_gt, 
#        filename = paste0(wd, path_output, "Tables/glp1ra_prior_classes_smi_", 
#                          today(), ".docx"))


# ============= 2) Prescribing pathway descriptives ===============================

n_cohort <- length(unique(cohort_demog_dx_date_smi$patid))

# How many have first T2DM diagnosis during follow-up: 1,254,862 (948,689 Aurum and 306,173 Gold)
# 1,163,463 (48%) removed
cohort_inc_t2dm <- cohort_demog_dx_date_smi %>%
  filter(incident_t2dm == 1)
n_cohort_inc_t2dm <- length(unique(cohort_inc_t2dm$patid))

# Proportion of those in the cohort with GLP-1RA prescription
n_cohort_glp <- length(unique(glp1ras$patid))
n_cohort_glp/n_cohort

# Remove prescriptions occurring before start to follow-up.
# Note: all individuals have at least 1 year registration before follow-up start.
# Removed: 665,423 prescriptions (9%)
# Total: 232,246 patients (10,081 removed)
glp1ras_fu <- glp1ras %>%
  filter(!is.na(startfollow)) %>%
  filter(eventdate >= startfollow)
n_cohort_glp_fu <- length(unique(glp1ras_fu$patid))
n_cohort_glp_fu/n_cohort

## Calculate how many with incident T2DM had GLP-1RA prescription: 105,714
cohort_inc_t2dm_glp <- cohort_inc_t2dm %>%
  inner_join(glp1ras %>% select(patid, productname, antidiabetic, antidiab_group, eventdate), 
             by = "patid")
n_cohort_inc_t2dm_glp <- length(unique(cohort_inc_t2dm_glp$patid))
n_cohort_inc_t2dm_glp
n_cohort_inc_t2dm_glp/n_cohort_inc_t2dm

# How many after removing prescriptions occurring before start to follow-up? Same
cohort_inc_t2dm_glp <- cohort_inc_t2dm_glp %>%
  filter(eventdate >= startfollow)
n_cohort_inc_t2dm_glp_fu <- length(unique(cohort_inc_t2dm_glp$patid))
n_cohort_inc_t2dm_glp_fu/n_cohort_inc_t2dm


# ============= 3) Restrict to those with incident T2DM ===============




# ============= 1) Set up and load data ===============================

# DuckDB in-memory connection
connection <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
# Allow spilling to local disk after hitting memory limit
DBI::dbExecute(connection, "PRAGMA memory_limit = '35GB';")
spill_dir <- "N:/Temp/duckdb_spill"
dir.create(spill_dir, showWarnings = FALSE, recursive = TRUE)
DBI::dbExecute(connection, sprintf("PRAGMA temp_directory='%s';", spill_dir))
# Set up progress bar
DBI::dbExecute(connection, "SET enable_progress_bar = true;")
DBI::dbExecute(connection, "SET enable_progress_bar_print = true;")
# Set to 1 thread and set seed for reproducible RNG
dbExecute(connection, "SET threads=1;")
dbExecute(connection, "SELECT setseed(0.42);")


# Read in GLP-1RA prescriptions
glp1ras <- dbGetQuery(connection, sprintf("
  SELECT * FROM read_parquet('%s');
;", paste0(path_input, "glp1ras.parquet")))
table(glp1ras$antidiab_group)
table(glp1ras$antidiabetic)

# Set insulin combinations as the GLP-1RA drug
glp1ras <- glp1ras %>%
  mutate(antidiabetic = case_when(
    antidiabetic %in% c("Liraglutide/Insulin") ~ "Liraglutide",
    antidiabetic %in% c("Lixisenatide/Insulin") ~ "Lixisenatide",
    .default = antidiabetic
  ))
table(glp1ras$antidiabetic)

# Remove invalid prescriptions occurring after follow-up ends
# Removed: 12 prescriptions not in T2DM cohort
# Removed: 14,907 prescriptions occurring after follow-up
# Total: 242,327 patients w/ GLP prescriptions (335 removed)
glp1ras <- glp1ras %>%
  inner_join(
    cohort_demog_dx_date_smi %>% select(patid, startfollow, endfollow),
    by = "patid") %>%
  filter(!is.na(endfollow), eventdate <= endfollow)
length(unique(glp1ras$patid))


# Years of follow-up 
years_fu <- 2005:2025
# Formulations
all_formulations <- c("Albiglutide", "Dulaglutide", "Exenatide", "Liraglutide", 
                      "Lixisenatide", "Semaglutide", "Tirzepatide", "Any")

# ============= Get those with incident T2DM ==========================================

# Get patients w/ prescriptions occurring before start of follow-up
# 35,153 patients had prescriptions before start to follow-up, with 
# a total of 665,423 prior prescriptions
prior_glp <- glp1ras %>%
  filter(!is.na(startfollow), eventdate < startfollow)
length(unique(prior_glp$patid))

# Remove individuals with prescriptions before start to follow-up, restricting to incident GLP-1RA
# Note: all individuals have at least 1 year registration before follow-up start.
# Removed: 1,332,741 prescriptions (18%) from patients w/ prevalent GLP-1RA
# Total: 207,174 patients (35,153 removed)
glp1ras_fu <- glp1ras %>%
  filter(!(patid %in% prior_glp$patid)) %>%
  mutate(year = year(eventdate)) %>%
  rename(type = antidiabetic)
length(unique(glp1ras_fu$patid))

### Calculating first-ever prescription within follow-up

# For each patient and formulation, get first-ever prescription
glp1ras_incident <- glp1ras_fu %>%
  group_by(patid, type) %>%
  summarise(first_date = min(eventdate), .groups = "drop")
table(glp1ras_incident$type)

# Pivot wider so each formulation has its own column, then get 
# first-ever GLP-1RA prescription of any type (Any column)
# Each row is a patient
glp1ras_incident_wide <- glp1ras_incident %>%
  pivot_wider(names_from = type, values_from = first_date)
glp1ras_incident_wide <- glp1ras_incident_wide %>%
  mutate(`Any` = pmin(Albiglutide, Dulaglutide, Exenatide, Liraglutide, 
                      Lixisenatide, Semaglutide, Tirzepatide, na.rm = TRUE))
# Convert back into long format, with Any
glp1ras_incident_long <- glp1ras_incident_wide %>%
  pivot_longer(-patid, names_to = "type", values_to = "first_date")

### Calculating at-risk time

# Check no first-dates occuring prior to start of follow-up or after end of follow-up
temp <- glp1ras_incident_long %>% 
  inner_join(cohort_demog_dx_date_smi %>% select(patid, startfollow, endfollow), 
             by = "patid") %>% 
  filter(first_date < startfollow | first_date > endfollow)

# Define end of at-risk follow-up for individuals with GLP-1RA
risk_df <- glp1ras_incident_long %>% 
  inner_join(cohort_demog_dx_date_smi %>% select(patid, startfollow, endfollow), 
             by = "patid") %>%
  mutate(
    # End of follow-up occurs at first_date if valid, else at endfollow date
    risk_end = if_else(!is.na(first_date) & first_date >= startfollow & first_date <= endfollow,
                       first_date,
                       endfollow),
    had_event = !is.na(first_date) & first_date >= startfollow & first_date <= endfollow,
    event_year = if_else(!is.na(first_date), year(first_date), NA)
  )


# Define end of at-risk follow-up for individuals who never have any GLP-1RA prescription
# 2,175,998 patients
risk_df_no_glp <- cohort_demog_dx_date_smi %>%
  filter(!(patid %in% glp1ras$patid)) %>%
  select(patid, startfollow, endfollow) %>%
  mutate(risk_end = endfollow, 
         had_event = FALSE)


### Calculate incidence for all years and formulations

# Turn off tidylog
old <- getOption("tidylog.display")
options(tidylog.display = list())

# Calculate incidence for all years
inc_df <- calc_inc_prev_all_years(years_fu = years_fu, 
                                  all_formulations = all_formulations, 
                                  risk_df_no_glp = risk_df_no_glp, 
                                  incidence = TRUE, risk_df = risk_df,
                                  period_prevalence = FALSE, point_prevalence = FALSE)

# Sanity check
hist(inc_df$inc_df$incidence_rate, breaks = 30)


### Plot incidence

plot_inc <- create_lineplot(data = inc_df$inc_df, all_formulations = all_formulations,
                            y_var = "incidence_rate")

plot_inc

# Save incidence plot
ggsave(filename = paste0(wd, path_output, "Figures/glp1ra_lineplot_inc_", today(), ".png"),
       width = 9, height = 6, units = "in", dpi = 600)

# Save incidence data
glp1ra_inc_df <- inc_df$inc_df
save(glp1ra_inc_df, file = paste0(wd, path_output, "Analyses/glp1ra_inc_df.RData"))

# ============= Incidence by SMI ===============================================================

# smi_patids <- cohort_demog_dx_date_smi %>%
#   filter(!is.na(smi_group))

# Add in SMI subtype information to risk_df
risk_df_smi <- risk_df %>%
  left_join(cohort_demog_dx_date_smi %>% 
              select(patid, smi_group, smi_dx_date, date_schiz, date_bpd, date_psych),
            by = "patid")
risk_df_no_glp_smi <- risk_df_no_glp %>%
  left_join(cohort_demog_dx_date_smi %>% 
              select(patid, smi_group, smi_dx_date,date_schiz, date_bpd, date_psych),
            by = "patid")




# Disconnect and wipe the spillover folder
dbDisconnect(connection, shutdown = TRUE)
closeAllConnections()
unlink(spill_dir, recursive = TRUE, force = TRUE)
gc()
