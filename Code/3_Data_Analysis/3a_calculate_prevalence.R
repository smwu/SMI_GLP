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

# ============= 1) Set up and load data ===============================

# ### For running in Data Safe Haven
# Set working directory
wd <- "S:/CDSTP_CPRD_25_005368/"
setwd(wd)

# Set input and output paths
path_input <- paste0("SMI_GLP/Data/Cleaned_Data/")
path_output <- paste0("SMI_GLP/Outputs/")
path_lookups_gold <- "Lookups/202506_Lookups_GOLD2025_09/"
path_lookups_aurum <- "Lookups/202506_Lookups_CPRDAurum/"

# Load in helper functions
source(paste0(wd, "SMI_GLP/Code/4_Data_Analysis/",
              "4_helper_functions.R"))


# DuckDB in-memory connection
connection <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# Allow spilling to local disk after hitting memory limit
DBI::dbExecute(connection, "PRAGMA memory_limit = '35GB';")
spill_dir <- "N:/Temp/glp_spill"
dir.create(spill_dir, showWarnings = FALSE, recursive = TRUE)
DBI::dbExecute(connection, sprintf("PRAGMA temp_directory='%s';", spill_dir))

# Set up progress bar
DBI::dbExecute(connection, "SET enable_progress_bar = true;")
DBI::dbExecute(connection, "SET enable_progress_bar_print = true;")


# Read in Final T2DM cohort with SMI information 
# Loads as 'cohort_demog_dx_date_smi'
load(paste0(wd, path_input, "cohort_demog_cleaned_with_smi.RData"))

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

# ============= Calculate denominator ===================================

# Period prevalence denominator: patients under follow-up in each year 
denom_period_year <- tibble(year = years_fu) %>%
  rowwise() %>%
  mutate(
    denominator = sum(
      year(cohort_demog_dx_date_smi$startfollow) <= year &
        (is.na(cohort_demog_dx_date_smi$endfollow) | 
           year(cohort_demog_dx_date_smi$endfollow) >= year))
  ) %>%
  ungroup() %>%
  select(year, denominator)

# Point prevalence denominator: patients under follow-up at a specific date: Feb 1
index_dates <- as.Date(paste0(years_fu, "-02-01"))
denom_point_year <- data.frame(
  year = years_fu, 
  denominator = sapply(index_dates, function(year) {
    cohort_demog_dx_date_smi %>%
      filter(startfollow <= year, endfollow >= year) %>% 
      summarise(n = n_distinct(patid)) %>% 
      pull(n)
  }))

# Compare denominators
denom_period_year$denominator
denom_point_year$denominator
denom_period_year$denominator - denom_point_year$denominator

# Plot denominators for period and point prevalences
plot_denom <- denom_period_year %>% 
  left_join(denom_point_year, by = "year", suffix = c("_period", "_point")) %>%
  pivot_longer(-year, names_to = "prev_type", values_to = "denominator") %>%
  mutate(prev_type = factor(prev_type, levels = c("denominator_period", "denominator_point"),
                            labels = c("Period", "Point")))

plot_denom %>% 
  ggplot(aes(x = year, y = denominator, color = prev_type)) + 
  geom_point() + 
  theme_minimal() + 
  labs(color = "Prevalence Type", y = "Denominator (Number of Patients Under Follow-Up)", x = "Year")

# # Save plot of denominator comparisons
# ggsave(filename = paste0(wd, path_output, "Figures/prev_denom_compare_", today(), ".png"),
#        width = 9, height = 6, units = "in", dpi = 600)





# ============= Calculate incidence ==========================================================

### Removing prevalent prescriptions

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

# Calculate incidence for all years and formulations for those with SMI
inc_df_smi <- calc_inc_prev_all_years(years_fu = years_fu, 
                                 all_formulations = all_formulations, 
                                 risk_df_no_glp = risk_df_no_glp_smi,
                                 incidence = TRUE,
                                 risk_df = risk_df_smi, 
                                 period_prevalence = FALSE, point_prevalence = FALSE,
                                 smid_type = "all")
# Sanity check
hist(inc_df_smi$inc_df$incidence_rate, breaks = 30)


# Calculate incidence for all years and formulations for those without SMI
inc_df_no_smi <- calc_inc_prev_all_years(years_fu = years_fu, 
                                    all_formulations = all_formulations, 
                                    risk_df_no_glp = risk_df_no_glp_smi,
                                    incidence = TRUE,
                                    risk_df = risk_df_smi, 
                                    period_prevalence = FALSE, point_prevalence = FALSE,
                                    smid_type = "none")
# Sanity check
hist(inc_df_no_smi$inc_df$incidence_rate, breaks = 30)


### Plot incidence

plot_inc_smi <- create_lineplot(data = inc_df_smi$inc_df, all_formulations = all_formulations,
                                y_var = "incidence_rate")
plot_inc_no_smi <- create_lineplot(data = inc_df_no_smi$inc_df, all_formulations = all_formulations,
                                   y_var = "incidence_rate")


ggarrange(plot_inc_smi + ggtitle("Patients Diagnosed with Schizophrenia, Bipolar Disorder, Psychosis, or Depression") + ylim(0, 100), 
          plot_inc_no_smi + ggtitle("Patients Without Schizophrenia, Bipolar Disorder, Psychosis, or Depression") + ylim(0, 100), 
          common.legend = TRUE, nrow = 2, legend = "right")

# # Save incidence plot
ggsave(filename = paste0(wd, path_output, "Figures/glp1ra_lineplot_inc_smi_", today(), ".png"),
       width = 10, height = 9, units = "in", dpi = 600)

# Save incidence data
glp1ra_inc_df_smi <- inc_df_smi$inc_df
glp1ra_inc_df_no_smi <- inc_df_no_smi$inc_df
save(glp1ra_inc_df_smi, file = paste0(wd, path_output, "Analyses/glp1ra_inc_df_smi.RData"))
save(glp1ra_inc_df_no_smi, file = paste0(wd, path_output, "Analyses/glp1ra_inc_df_no_smi.RData"))


# ============= Incidence by SMI Subtype ==================================================


# Calculate incidence for all years and formulations for those with each SMI subtype
inc_df_smi_schiz <- calc_inc_prev_all_years(years_fu = years_fu, 
                                       all_formulations = all_formulations, 
                                       risk_df_no_glp = risk_df_no_glp_smi,
                                       incidence = TRUE,
                                       risk_df = risk_df_smi,
                                       period_prevalence = FALSE, point_prevalence = FALSE,
                                       smid_type = "schizophrenia")
inc_df_smi_bpd <- calc_inc_prev_all_years(years_fu = years_fu, 
                                     all_formulations = all_formulations, 
                                     risk_df_no_glp = risk_df_no_glp_smi,
                                     incidence = TRUE,
                                     risk_df = risk_df_smi,
                                     period_prevalence = FALSE, point_prevalence = FALSE,
                                     smid_type = "bipolar")
inc_df_smi_psych <- calc_inc_prev_all_years(years_fu = years_fu, 
                                       all_formulations = all_formulations, 
                                       risk_df_no_glp = risk_df_no_glp_smi,
                                       incidence = TRUE,
                                       risk_df = risk_df_smi,
                                       period_prevalence = FALSE, point_prevalence = FALSE,
                                       smid_type = "other psychosis")
inc_df_smi_dep <- calc_inc_prev_all_years(years_fu = years_fu, 
                                     all_formulations = all_formulations, 
                                     risk_df_no_glp = risk_df_no_glp_smi,
                                     incidence = TRUE,
                                     risk_df = risk_df_smi,
                                     period_prevalence = FALSE, point_prevalence = FALSE,
                                     smid_type = "depression")


### Plot incidence

plot_inc_smi_schiz <- create_lineplot(data = inc_df_smi_schiz$inc_df, all_formulations = all_formulations,
                                      y_var = "incidence_rate")
plot_inc_smi_bpd <- create_lineplot(data = inc_df_smi_bpd$inc_df, all_formulations = all_formulations,
                                    y_var = "incidence_rate")
plot_inc_smi_psych <- create_lineplot(data = inc_df_smi_psych$inc_df, all_formulations = all_formulations,
                                      y_var = "incidence_rate")
plot_inc_smi_dep <- create_lineplot(data = inc_df_smi_dep$inc_df, all_formulations = all_formulations,
                                    y_var = "incidence_rate")

ggarrange(plot_inc_smi_schiz + ggtitle("Patients Diagnosed with Schizophrenia") + ylim(0, 100) + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)), 
          plot_inc_smi_bpd + ggtitle("Patients Diagnosed with Bipolar Disorder") + ylim(0, 100) + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)), 
          plot_inc_smi_psych + ggtitle("Patients Diagnosed with Psychosis") + ylim(0, 100) + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)), 
          plot_inc_smi_dep + ggtitle("Patients Diagnosed with Depression") + ylim(0, 100) + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)), 
          common.legend = TRUE, nrow = 2, ncol = 2, legend = "right")

# # Save incidence plot
ggsave(filename = paste0(wd, path_output, "Figures/glp1ra_lineplot_inc_smisubtype_", today(), ".png"),
       width = 14, height = 12, units = "in", dpi = 600)

# Save incidence data
glp1ra_inc_df_smi_schiz <- inc_df_smi_schiz$inc_df
glp1ra_inc_df_smi_bpd <- inc_df_smi_bpd$inc_df
glp1ra_inc_df_smi_psych <- inc_df_smi_psych$inc_df
glp1ra_inc_df_smi_dep <- inc_df_smi_dep$inc_df
save(glp1ra_inc_df_smi_schiz, file = paste0(wd, path_output, "Analyses/glp1ra_inc_df_smi_schiz.RData"))
save(glp1ra_inc_df_smi_bpd, file = paste0(wd, path_output, "Analyses/glp1ra_inc_df_smi_bpd.RData"))
save(glp1ra_inc_df_smi_psych, file = paste0(wd, path_output, "Analyses/glp1ra_inc_df_smi_psych.RData"))
save(glp1ra_inc_df_smi_dep, file = paste0(wd, path_output, "Analyses/glp1ra_inc_df_smi_dep.RData"))


# # Restore tidylog
# options(tidylog.display = old)



# ============= 2) Prevalence of GLP-1RA prescriptions over time ===============================
# Calculate the prevalence of GLP-1RA prescriptions over time among individuals 
# with T2DM, comparing those with and without SMI


# Add follow-up start and end time for all patients w/ GLP-1RA prescriptions

glp1ras_prev_df <- glp1ras %>% 
  select(patid, antidiabetic, eventdate) %>%
  mutate(type = antidiabetic) 
prev_glp <- glp1ras_prev_df %>%
  inner_join(cohort_demog_dx_date_smi %>% select(patid, startfollow, endfollow), 
             by = "patid")

# Calculate period prevalence for all years
calc_period_prev <- calc_inc_prev_all_years(years_fu = years_fu, 
                                          all_formulations = all_formulations, 
                                          risk_df_no_glp = risk_df_no_glp, 
                                          incidence = FALSE, risk_df = NULL,
                                          period_prevalence = TRUE, prev_glp = prev_glp, 
                                          point_prevalence = FALSE)

# Sanity check
hist(calc_period_prev$period_prev_df$period_prev, breaks = 30)

### Plot period prevalence

plot_period_prev <- create_lineplot(data = calc_period_prev$period_prev_df, 
                                    all_formulations = all_formulations, years_fu = years_fu,
                                    y_var = "period_prev") + ylim(0, 15)

plot_period_prev

# Save period prevalence plot
ggsave(filename = paste0(wd, path_output, "Figures/glp1ra_lineplot_period_prev_", today(), ".png"),
       width = 9, height = 6, units = "in", dpi = 600)

# Save prevalence data
glp1ra_period_prev_df <- calc_period_prev$period_prev_df
save(glp1ra_period_prev_df, file = paste0(wd, path_output, "Analyses/glp1ra_period_prev_df.RData"))


# ============= Period prevalence by SMI ===============================================================


# Add in SMI subtype information to prev_glp
prev_glp_smi <- prev_glp %>%
  left_join(cohort_demog_dx_date_smi %>% 
              select(patid, smi_group, smi_dx_date,date_schiz, date_bpd, date_psych),
            by = "patid")

# Calculate prevalence for all years and formulations for those with SMI
period_prev_df_smi <- calc_inc_prev_all_years(years_fu = years_fu, 
                                      all_formulations = all_formulations, 
                                      risk_df_no_glp = risk_df_no_glp_smi,
                                      incidence = FALSE,
                                      period_prevalence = TRUE, 
                                      prev_glp = prev_glp_smi,
                                      point_prevalence = FALSE,
                                      smid_type = "all")
# Sanity check
hist(period_prev_df_smi$period_prev_df$period_prev, breaks = 30)


# Calculate incidence for all years and formulations for those without SMI
period_prev_no_smi <- calc_inc_prev_all_years(years_fu = years_fu, 
                                         all_formulations = all_formulations, 
                                         risk_df_no_glp = risk_df_no_glp_smi,
                                         incidence = FALSE,
                                         period_prevalence = TRUE, 
                                         prev_glp = prev_glp_smi,
                                         point_prevalence = FALSE,
                                         smid_type = "none")
# Sanity check
hist(period_prev_no_smi$period_prev_df$period_prev, breaks = 30)


### Plot period prevalence

plot_period_prev_smi <- create_lineplot(data = period_prev_df_smi$period_prev_df, 
                                        all_formulations = all_formulations,
                                        y_var = "period_prev")
plot_period_prev_no_smi <- create_lineplot(data = period_prev_no_smi$period_prev_df, 
                                           all_formulations = all_formulations,
                                          y_var = "period_prev")


ggarrange(plot_period_prev_smi + ggtitle("Patients Diagnosed with Schizophrenia, Bipolar Disorder, Psychosis, or Depression") + 
            ylim(0, 15), 
          plot_period_prev_no_smi + ggtitle("Patients Without Schizophrenia, Bipolar Disorder, Psychosis, or Depression") + 
            ylim(0, 15), 
          common.legend = TRUE, nrow = 2, legend = "right")

# # Save period prevalence by SMI plot
ggsave(filename = paste0(wd, path_output, "Figures/glp1ra_lineplot_period_prev_smi_", today(), ".png"),
       width = 10, height = 9, units = "in", dpi = 600)

# Save period prevalence data
glp1ra_period_prev_smi <- period_prev_df_smi$period_prev_df
glp1ra_period_prev_no_smi <- period_prev_no_smi$period_prev_df
save(glp1ra_period_prev_smi, 
     file = paste0(wd, path_output, "Analyses/glp1ra_period_prev_smi.RData"))
save(glp1ra_period_prev_no_smi, 
     file = paste0(wd, path_output, "Analyses/glp1ra_period_prev_no_smi.RData"))


# ============= Period prevalence by SMI subtype ===============================================================


# Calculate period prevalence for all years and formulations for those with each SMI subtype
period_prev_df_smi_schiz <- calc_inc_prev_all_years(years_fu = years_fu, 
                                            all_formulations = all_formulations, 
                                            risk_df_no_glp = risk_df_no_glp_smi,
                                            incidence = FALSE,
                                            period_prevalence = TRUE, 
                                            prev_glp = prev_glp_smi,
                                            point_prevalence = FALSE,
                                            smid_type = "schizophrenia")
period_prev_df_smi_bpd <- calc_inc_prev_all_years(years_fu = years_fu, 
                                          all_formulations = all_formulations, 
                                          risk_df_no_glp = risk_df_no_glp_smi,
                                          incidence = FALSE,
                                          period_prevalence = TRUE, 
                                          prev_glp = prev_glp_smi,
                                          point_prevalence = FALSE,
                                          smid_type = "bipolar")
period_prev_df_smi_psych <- calc_inc_prev_all_years(years_fu = years_fu, 
                                            all_formulations = all_formulations, 
                                            risk_df_no_glp = risk_df_no_glp_smi,
                                            incidence = FALSE,
                                            period_prevalence = TRUE, 
                                            prev_glp = prev_glp_smi,
                                            point_prevalence = FALSE,
                                            smid_type = "other psychosis")
period_prev_df_smi_dep <- calc_inc_prev_all_years(years_fu = years_fu, 
                                          all_formulations = all_formulations, 
                                          risk_df_no_glp = risk_df_no_glp_smi,
                                          incidence = FALSE,
                                          period_prevalence = TRUE, 
                                          prev_glp = prev_glp_smi,
                                          point_prevalence = FALSE,
                                          smid_type = "depression")


### Plot period prevalence

plot_period_prev_smi_schiz <- create_lineplot(data = period_prev_df_smi_schiz$period_prev_df, 
                                              all_formulations = all_formulations,
                                      y_var = "period_prev")
plot_period_prev_smi_bpd <- create_lineplot(data = period_prev_df_smi_bpd$period_prev_df, 
                                            all_formulations = all_formulations,
                                    y_var = "period_prev")
plot_period_prev_smi_psych <- create_lineplot(data = period_prev_df_smi_psych$period_prev_df, 
                                              all_formulations = all_formulations,
                                      y_var = "period_prev")
plot_period_prev_smi_dep <- create_lineplot(data = period_prev_df_smi_dep$period_prev_df, 
                                            all_formulations = all_formulations,
                                    y_var = "period_prev")

ggarrange(plot_period_prev_smi_schiz + 
            ggtitle("Patients Diagnosed with Schizophrenia") + ylim(0, 15) + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)), 
          plot_period_prev_smi_bpd + 
            ggtitle("Patients Diagnosed with Bipolar Disorder") + ylim(0, 15) + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)), 
          plot_period_prev_smi_psych + 
            ggtitle("Patients Diagnosed with Psychosis") + ylim(0, 15) + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)), 
          plot_period_prev_smi_dep + 
            ggtitle("Patients Diagnosed with Depression") + ylim(0, 15) + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)), 
          common.legend = TRUE, nrow = 2, ncol = 2, legend = "right")

# # Save period prevalence plot
ggsave(filename = paste0(wd, path_output, "Figures/glp1ra_lineplot_period_prev_smisubtype_", today(), ".png"),
       width = 14, height = 12, units = "in", dpi = 600)

# Save period prevalence data
glp1ra_period_prev_df_smi_schiz <- period_prev_df_smi_schiz$period_prev_df
glp1ra_period_prev_df_smi_bpd <- period_prev_df_smi_bpd$period_prev_df
glp1ra_period_prev_df_smi_psych <- period_prev_df_smi_psych$period_prev_df
glp1ra_period_prev_df_smi_dep <- period_prev_df_smi_dep$period_prev_df
save(glp1ra_period_prev_df_smi_schiz, 
     file = paste0(wd, path_output, "Analyses/glp1ra_period_prev_df_smi_schiz.RData"))
save(glp1ra_period_prev_df_smi_bpd, 
     file = paste0(wd, path_output, "Analyses/glp1ra_period_prev_df_smi_bpd.RData"))
save(glp1ra_period_prev_df_smi_psych, 
     file = paste0(wd, path_output, "Analyses/glp1ra_period_prev_df_smi_psych.RData"))
save(glp1ra_period_prev_df_smi_dep, 
     file = paste0(wd, path_output, "Analyses/glp1ra_period_prev_df_smi_dep.RData"))



# ============= 4-month Period Prevalence of GLP-1RA prescriptions over time ===============================

# Define end of at-risk follow-up for individuals who never have any GLP-1RA prescription
# 2,175,998 patients
risk_df_no_glp <- cohort_demog_dx_date_smi %>%
  filter(!(patid %in% glp1ras$patid)) %>%
  select(patid, startfollow, endfollow) %>%
  mutate(risk_end = endfollow, 
         had_event = FALSE)

# Calculate period prevalence for all years
calc_period_prev_4mo <- calc_inc_prev_all_years(years_fu = years_fu, 
                                            all_formulations = all_formulations, 
                                            risk_df_no_glp = risk_df_no_glp, 
                                            incidence = FALSE, risk_df = NULL,
                                            period_prevalence = TRUE, end_april = TRUE,
                                            prev_glp = prev_glp, 
                                            point_prevalence = FALSE)

# Sanity check
hist(calc_period_prev_4mo$period_prev_df$period_prev, breaks = 30)

### Plot period prevalence

plot_period_prev <- create_lineplot(data = calc_period_prev_4mo$period_prev_df, 
                                    all_formulations = all_formulations, years_fu = years_fu,
                                    y_var = "period_prev") + ylim(0, 15)

plot_period_prev

# Save period prevalence plot
ggsave(filename = paste0(wd, path_output, "Figures/glp1ra_lineplot_period_prev_4mo_", today(), ".png"),
       width = 9, height = 6, units = "in", dpi = 600)

# Save prevalence data
glp1ra_period_prev_df_4mo <- calc_period_prev_4mo$period_prev_df
save(glp1ra_period_prev_df_4mo, file = paste0(wd, path_output, "Analyses/glp1ra_period_prev_df_4mo.RData"))


#========== 4-month Period prevalence by SMI ===============================================================

# Add in SMI subtype information to risk_df
risk_df_smi <- risk_df %>%
  left_join(cohort_demog_dx_date_smi %>% 
              select(patid, smi_group, smi_dx_date, date_schiz, date_bpd, date_psych),
            by = "patid")
risk_df_no_glp_smi <- risk_df_no_glp %>%
  left_join(cohort_demog_dx_date_smi %>% 
              select(patid, smi_group, smi_dx_date,date_schiz, date_bpd, date_psych),
            by = "patid")

# Add in SMI subtype information to prev_glp
prev_glp_smi <- prev_glp %>%
  left_join(cohort_demog_dx_date_smi %>% 
              select(patid, smi_group, smi_dx_date,date_schiz, date_bpd, date_psych),
            by = "patid")

# Calculate prevalence for all years and formulations for those with SMI
period_prev_df_smi_4mo <- calc_inc_prev_all_years(years_fu = years_fu, 
                                              all_formulations = all_formulations, 
                                              risk_df_no_glp = risk_df_no_glp_smi,
                                              incidence = FALSE,
                                              period_prevalence = TRUE, 
                                              prev_glp = prev_glp_smi, end_april = TRUE,
                                              point_prevalence = FALSE,
                                              smid_type = "all")
# Sanity check
hist(period_prev_df_smi_4mo$period_prev_df$period_prev, breaks = 30)


# Calculate incidence for all years and formulations for those without SMI
period_prev_no_smi_4mo <- calc_inc_prev_all_years(years_fu = years_fu, 
                                              all_formulations = all_formulations, 
                                              risk_df_no_glp = risk_df_no_glp_smi,
                                              incidence = FALSE,
                                              period_prevalence = TRUE, end_april = TRUE,
                                              prev_glp = prev_glp_smi,
                                              point_prevalence = FALSE,
                                              smid_type = "none")
# Sanity check
hist(period_prev_no_smi_4mo$period_prev_df$period_prev, breaks = 30)


### Plot period prevalence

plot_period_prev_smi <- create_lineplot(data = period_prev_df_smi_4mo$period_prev_df, 
                                        all_formulations = all_formulations,
                                        y_var = "period_prev")
plot_period_prev_no_smi <- create_lineplot(data = period_prev_no_smi_4mo$period_prev_df, 
                                           all_formulations = all_formulations,
                                           y_var = "period_prev")


ggarrange(plot_period_prev_smi + ggtitle("Patients Diagnosed with Schizophrenia, Bipolar Disorder, Psychosis, or Depression") + 
            ylim(0, 15), 
          plot_period_prev_no_smi + ggtitle("Patients Without Schizophrenia, Bipolar Disorder, Psychosis, or Depression") + 
            ylim(0, 15), 
          common.legend = TRUE, nrow = 2, legend = "right")

# # Save period prevalence by SMI plot
ggsave(filename = paste0(wd, path_output, "Figures/glp1ra_lineplot_period_prev_smi_4mo_", today(), ".png"),
       width = 10, height = 9, units = "in", dpi = 600)

# Save period prevalence data
glp1ra_period_prev_smi_4mo <- period_prev_df_smi_4mo$period_prev_df
glp1ra_period_prev_no_smi_4mo <- period_prev_no_smi_4mo$period_prev_df
save(glp1ra_period_prev_smi_4mo, 
     file = paste0(wd, path_output, "Analyses/glp1ra_period_prev_smi_4mo.RData"))
save(glp1ra_period_prev_no_smi_4mo, 
     file = paste0(wd, path_output, "Analyses/glp1ra_period_prev_no_smi_4mo.RData"))


# ======= 4-month Period prevalence by SMI subtype ===============================================================


# Calculate period prevalence for all years and formulations for those with each SMI subtype
period_prev_df_smi_schiz_4mo <- calc_inc_prev_all_years(years_fu = years_fu, 
                                                    all_formulations = all_formulations, 
                                                    risk_df_no_glp = risk_df_no_glp_smi,
                                                    incidence = FALSE,
                                                    period_prevalence = TRUE, end_april = TRUE,
                                                    prev_glp = prev_glp_smi,
                                                    point_prevalence = FALSE,
                                                    smid_type = "schizophrenia")
period_prev_df_smi_bpd_4mo <- calc_inc_prev_all_years(years_fu = years_fu, 
                                                  all_formulations = all_formulations, 
                                                  risk_df_no_glp = risk_df_no_glp_smi,
                                                  incidence = FALSE,
                                                  period_prevalence = TRUE, end_april = TRUE,
                                                  prev_glp = prev_glp_smi,
                                                  point_prevalence = FALSE,
                                                  smid_type = "bipolar")
period_prev_df_smi_psych_4mo <- calc_inc_prev_all_years(years_fu = years_fu, 
                                                    all_formulations = all_formulations, 
                                                    risk_df_no_glp = risk_df_no_glp_smi,
                                                    incidence = FALSE,
                                                    period_prevalence = TRUE, end_april = TRUE,
                                                    prev_glp = prev_glp_smi,
                                                    point_prevalence = FALSE,
                                                    smid_type = "other psychosis")
period_prev_df_smi_dep_4mo <- calc_inc_prev_all_years(years_fu = years_fu, 
                                                  all_formulations = all_formulations, 
                                                  risk_df_no_glp = risk_df_no_glp_smi,
                                                  incidence = FALSE,
                                                  period_prevalence = TRUE, end_april = TRUE,
                                                  prev_glp = prev_glp_smi,
                                                  point_prevalence = FALSE,
                                                  smid_type = "depression")


### Plot period prevalence

plot_period_prev_smi_schiz <- create_lineplot(data = period_prev_df_smi_schiz_4mo$period_prev_df, 
                                              all_formulations = all_formulations,
                                              y_var = "period_prev")
plot_period_prev_smi_bpd <- create_lineplot(data = period_prev_df_smi_bpd_4mo$period_prev_df, 
                                            all_formulations = all_formulations,
                                            y_var = "period_prev")
plot_period_prev_smi_psych <- create_lineplot(data = period_prev_df_smi_psych_4mo$period_prev_df, 
                                              all_formulations = all_formulations,
                                              y_var = "period_prev")
plot_period_prev_smi_dep <- create_lineplot(data = period_prev_df_smi_dep_4mo$period_prev_df, 
                                            all_formulations = all_formulations,
                                            y_var = "period_prev")

ggarrange(plot_period_prev_smi_schiz + 
            ggtitle("Patients Diagnosed with Schizophrenia") + ylim(0, 15) + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)), 
          plot_period_prev_smi_bpd + 
            ggtitle("Patients Diagnosed with Bipolar Disorder") + ylim(0, 15) + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)), 
          plot_period_prev_smi_psych + 
            ggtitle("Patients Diagnosed with Psychosis") + ylim(0, 15) + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)), 
          plot_period_prev_smi_dep + 
            ggtitle("Patients Diagnosed with Depression") + ylim(0, 15) + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)), 
          common.legend = TRUE, nrow = 2, ncol = 2, legend = "right")

# # Save period prevalence plot
ggsave(filename = paste0(wd, path_output, "Figures/glp1ra_lineplot_period_prev_smisubtype_4mo_", today(), ".png"),
       width = 14, height = 12, units = "in", dpi = 600)

# Save period prevalence data
glp1ra_period_prev_df_smi_schiz_4mo <- period_prev_df_smi_schiz_4mo$period_prev_df
glp1ra_period_prev_df_smi_bpd_4mo <- period_prev_df_smi_bpd_4mo$period_prev_df
glp1ra_period_prev_df_smi_psych_4mo <- period_prev_df_smi_psych_4mo$period_prev_df
glp1ra_period_prev_df_smi_dep_4mo <- period_prev_df_smi_dep_4mo$period_prev_df
save(glp1ra_period_prev_df_smi_schiz_4mo, 
     file = paste0(wd, path_output, "Analyses/glp1ra_period_prev_df_smi_schiz_4mo.RData"))
save(glp1ra_period_prev_df_smi_bpd_4mo, 
     file = paste0(wd, path_output, "Analyses/glp1ra_period_prev_df_smi_bpd_4mo.RData"))
save(glp1ra_period_prev_df_smi_psych_4mo, 
     file = paste0(wd, path_output, "Analyses/glp1ra_period_prev_df_smi_psych_4mo.RData"))
save(glp1ra_period_prev_df_smi_dep_4mo, 
     file = paste0(wd, path_output, "Analyses/glp1ra_period_prev_df_smi_dep_4mo.RData"))


# ============= Point Prevalence of GLP-1RA prescriptions over time ===============================

# Add follow-up start and end time for all patients w/ GLP-1RA prescriptions

glp1ras_prev_df <- glp1ras %>% 
  select(patid, antidiabetic, eventdate) %>%
  mutate(type = antidiabetic) 
prev_glp <- glp1ras_prev_df %>%
  inner_join(cohort_demog_dx_date_smi %>% select(patid, startfollow, endfollow), 
             by = "patid")

# Define end of at-risk follow-up for individuals who never have any GLP-1RA prescription
risk_df_no_glp <- cohort_demog_dx_date_smi %>%
  filter(!(patid %in% glp1ras$patid)) %>%
  select(patid, startfollow, endfollow) %>%
  mutate(risk_end = endfollow, 
         had_event = FALSE)

# Calculate point prevalence for all years
calc_point_prev <- calc_inc_prev_all_years(years_fu = years_fu, 
                                            all_formulations = all_formulations, 
                                            risk_df_no_glp = risk_df_no_glp, 
                                            incidence = FALSE, risk_df = NULL,
                                            period_prevalence = FALSE, prev_glp = prev_glp, 
                                            point_prevalence = TRUE)

# Sanity check
hist(calc_point_prev$point_prev_df$point_prev, breaks = 30)

### Plot point prevalence

plot_point_prev <- create_lineplot(data = calc_point_prev$point_prev_df, 
                                    all_formulations = all_formulations, years_fu = years_fu,
                                    y_var = "point_prev") + ylim(0, 15)

plot_point_prev

# Save point prevalence plot
ggsave(filename = paste0(wd, path_output, "Figures/glp1ra_lineplot_point_prev_", today(), ".png"),
       width = 9, height = 6, units = "in", dpi = 600)

# Save prevalence data
glp1ra_point_prev_df <- calc_point_prev$point_prev_df
save(glp1ra_point_prev_df, file = paste0(wd, path_output, "Analyses/glp1ra_point_prev_df.RData"))


# ============= Point prevalence by SMI ===============================================================


# Add in SMI subtype information to prev_glp
prev_glp_smi <- prev_glp %>%
  left_join(cohort_demog_dx_date_smi %>% 
              select(patid, smi_group, smi_dx_date,date_schiz, date_bpd, date_psych),
            by = "patid")

# Calculate prevalence for all years and formulations for those with SMI
point_prev_df_smi <- calc_inc_prev_all_years(years_fu = years_fu, 
                                              all_formulations = all_formulations, 
                                              risk_df_no_glp = risk_df_no_glp_smi,
                                              incidence = FALSE,
                                              period_prevalence = FALSE, 
                                              prev_glp = prev_glp_smi,
                                              point_prevalence = TRUE,
                                              smid_type = "all")
# Sanity check
hist(point_prev_df_smi$point_prev_df$point_prev, breaks = 30)


# Calculate incidence for all years and formulations for those without SMI
point_prev_no_smi <- calc_inc_prev_all_years(years_fu = years_fu, 
                                              all_formulations = all_formulations, 
                                              risk_df_no_glp = risk_df_no_glp_smi,
                                              incidence = FALSE,
                                              period_prevalence = FALSE, 
                                              prev_glp = prev_glp_smi,
                                              point_prevalence = TRUE,
                                              smid_type = "none")
# Sanity check
hist(point_prev_no_smi$point_prev_df$point_prev, breaks = 30)


### Plot point prevalence

plot_point_prev_smi <- create_lineplot(data = point_prev_df_smi$point_prev_df, 
                                        all_formulations = all_formulations,
                                        y_var = "point_prev")
plot_point_prev_no_smi <- create_lineplot(data = point_prev_no_smi$point_prev_df, 
                                           all_formulations = all_formulations,
                                           y_var = "point_prev")


ggarrange(plot_point_prev_smi + ggtitle("Patients Diagnosed with Schizophrenia, Bipolar Disorder, Psychosis, or Depression") + 
            ylim(0, 15), 
          plot_point_prev_no_smi + ggtitle("Patients Without Schizophrenia, Bipolar Disorder, Psychosis, or Depression") + 
            ylim(0, 15), 
          common.legend = TRUE, nrow = 2, legend = "right")

# # Save point prevalence by SMI plot
ggsave(filename = paste0(wd, path_output, "Figures/glp1ra_lineplot_point_prev_smi_", today(), ".png"),
       width = 10, height = 9, units = "in", dpi = 600)

# Save point prevalence data
glp1ra_point_prev_smi <- point_prev_df_smi$point_prev_df
glp1ra_point_prev_no_smi <- point_prev_no_smi$point_prev_df
save(glp1ra_point_prev_smi, 
     file = paste0(wd, path_output, "Analyses/glp1ra_point_prev_smi.RData"))
save(glp1ra_point_prev_no_smi, 
     file = paste0(wd, path_output, "Analyses/glp1ra_point_prev_no_smi.RData"))


# ============= Point prevalence by SMI subtype ===============================================================


# Calculate point prevalence for all years and formulations for those with each SMI subtype
point_prev_df_smi_schiz <- calc_inc_prev_all_years(years_fu = years_fu, 
                                                    all_formulations = all_formulations, 
                                                    risk_df_no_glp = risk_df_no_glp_smi,
                                                    incidence = FALSE,
                                                    period_prevalence = FALSE, 
                                                    prev_glp = prev_glp_smi,
                                                    point_prevalence = TRUE,
                                                    smid_type = "schizophrenia")
point_prev_df_smi_bpd <- calc_inc_prev_all_years(years_fu = years_fu, 
                                                  all_formulations = all_formulations, 
                                                  risk_df_no_glp = risk_df_no_glp_smi,
                                                  incidence = FALSE,
                                                  period_prevalence = FALSE, 
                                                  prev_glp = prev_glp_smi,
                                                  point_prevalence = TRUE,
                                                  smid_type = "bipolar")
point_prev_df_smi_psych <- calc_inc_prev_all_years(years_fu = years_fu, 
                                                    all_formulations = all_formulations, 
                                                    risk_df_no_glp = risk_df_no_glp_smi,
                                                    incidence = FALSE,
                                                    period_prevalence = FALSE, 
                                                    prev_glp = prev_glp_smi,
                                                    point_prevalence = TRUE,
                                                    smid_type = "other psychosis")
point_prev_df_smi_dep <- calc_inc_prev_all_years(years_fu = years_fu, 
                                                  all_formulations = all_formulations, 
                                                  risk_df_no_glp = risk_df_no_glp_smi,
                                                  incidence = FALSE,
                                                  period_prevalence = FALSE, 
                                                  prev_glp = prev_glp_smi,
                                                  point_prevalence = TRUE,
                                                  smid_type = "depression")


### Plot point prevalence

plot_point_prev_smi_schiz <- create_lineplot(data = point_prev_df_smi_schiz$point_prev_df, 
                                              all_formulations = all_formulations,
                                              y_var = "point_prev")
plot_point_prev_smi_bpd <- create_lineplot(data = point_prev_df_smi_bpd$point_prev_df, 
                                            all_formulations = all_formulations,
                                            y_var = "point_prev")
plot_point_prev_smi_psych <- create_lineplot(data = point_prev_df_smi_psych$point_prev_df, 
                                              all_formulations = all_formulations,
                                              y_var = "point_prev")
plot_point_prev_smi_dep <- create_lineplot(data = point_prev_df_smi_dep$point_prev_df, 
                                            all_formulations = all_formulations,
                                            y_var = "point_prev")

ggarrange(plot_point_prev_smi_schiz + 
            ggtitle("Patients Diagnosed with Schizophrenia") + ylim(0, 15) + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)), 
          plot_point_prev_smi_bpd + 
            ggtitle("Patients Diagnosed with Bipolar Disorder") + ylim(0, 15) + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)), 
          plot_point_prev_smi_psych + 
            ggtitle("Patients Diagnosed with Psychosis") + ylim(0, 15) + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)), 
          plot_point_prev_smi_dep + 
            ggtitle("Patients Diagnosed with Depression") + ylim(0, 15) + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1)), 
          common.legend = TRUE, nrow = 2, ncol = 2, legend = "right")

# # Save point prevalence plot
ggsave(filename = paste0(wd, path_output, "Figures/glp1ra_lineplot_point_prev_smisubtype_", today(), ".png"),
       width = 14, height = 12, units = "in", dpi = 600)

# Save point prevalence data
glp1ra_point_prev_df_smi_schiz <- point_prev_df_smi_schiz$point_prev_df
glp1ra_point_prev_df_smi_bpd <- point_prev_df_smi_bpd$point_prev_df
glp1ra_point_prev_df_smi_psych <- point_prev_df_smi_psych$point_prev_df
glp1ra_point_prev_df_smi_dep <- point_prev_df_smi_dep$point_prev_df
save(glp1ra_point_prev_df_smi_schiz, 
     file = paste0(wd, path_output, "Analyses/glp1ra_point_prev_df_smi_schiz.RData"))
save(glp1ra_point_prev_df_smi_bpd, 
     file = paste0(wd, path_output, "Analyses/glp1ra_point_prev_df_smi_bpd.RData"))
save(glp1ra_point_prev_df_smi_psych, 
     file = paste0(wd, path_output, "Analyses/glp1ra_point_prev_df_smi_psych.RData"))
save(glp1ra_point_prev_df_smi_dep, 
     file = paste0(wd, path_output, "Analyses/glp1ra_point_prev_df_smi_dep.RData"))




# Disconnect and wipe the spillover folder
dbDisconnect(connection, shutdown = TRUE)
closeAllConnections()
unlink(spill_dir, recursive = TRUE, force = TRUE)
gc()
