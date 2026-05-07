# ======================================
# Helper functions
# ======================================

library(ggplot2)
library(ggrepel)
library(purrr)
library(rlang)
library(dplyr)


# =========================================================
# Incidence and prevalence helper functions
# =========================================================

# `calc_inc_prev` calculates the incidence rate, period prevalence, and point prevalence 
# for a given `year` and `formulation`
# Inputs:
#   year: Number specifying year for incidence rate
#   formulation: String specifying GLP-1RA formulation type
#   risk_df_no_glp: input follow-up dataset for those without GLP-1RAs
#   incidence: Boolean (default = TRUE) specifying if the incidence rate should be calculated
#   risk_df: input follow-up dataset for those with GLP-1RAs, needed for incidence. Default is NULL.
#   period_prevalence: Boolean (default = TRUE) specifying if the period prevalence should be calculated
#   end_april: Boolean specifying if period prevalence should end 30 Apr (TRUE) or 31 Dec (FALSE; default)
#   point_prevalence: Boolean (default = TRUE) specifying if the point prevalence should be calculated
#   prev_glp: dataframe of all valid GLP-1RA prescriptions, needed for prevalence. Default is NULL.
# Output: List containing the incidence rate, numerator, and denominator
# 
calc_inc_prev <- function(year, formulation, risk_df_no_glp, 
                          incidence = TRUE, risk_df = NULL, 
                          period_prevalence = TRUE, end_april = FALSE, 
                          point_prevalence = TRUE, prev_glp = NULL) {
  
  # Initialize results list
  calc_res <- list()
  
  # Set start and end of year
  year_start <- as.Date(paste0(year, "-01-01"), format = "%Y-%m-%d")
  
  # If end_april = TRUE, only calculate prevalence from 1 Jan to 30 Apr of each year
  if (end_april) {
    year_end <- as.Date(paste0(year, "-04-30"), format = "%Y-%m-%d")
  } else {
    year_end <- if_else(year == 2025, 
                        as.Date(paste0(year, "-04-30"), format = "%Y-%m-%d"),
                        as.Date(paste0(year, "-12-31"), format = "%Y-%m-%d"))
  }
  
  ### Calculate incidence rate
  
  if (incidence) {
    
    # For each year, exclude those with first-ever prescription prior to the year start.
    # Year's follow-up starts at the latest of: patient's follow-up start date or 
    # `start_date` (i.e., Jan 1 of that year).
    # Year's follow-up ends at the earliest of: patient's follow-up end date, 
    # `end_date` (i.e., Dec 31 of that year, except is Apr 30 for 2025), or when
    # the patient receives first prescription.
    
    # Filter to those still at-risk during the year and to the specific formulation
    risk_df_filtered <- risk_df %>%
      filter(event_year >= year) %>%
      filter(type == formulation)
    
    # Combine glp-1ra at-risk with non-glp at-risk population
    risk_df_combined <- risk_df_filtered %>%
      bind_rows(risk_df_no_glp)
    
    # Calculate follow-up time for each patient and each formulation during the year
    risk_df_combined <- risk_df_combined %>%
      mutate(
        at_risk_start = pmax(startfollow, year_start),
        at_risk_end = pmin(risk_end, year_end),
        at_risk_days = if_else(at_risk_start <= at_risk_end, 
                               as.numeric(at_risk_end - at_risk_start + 1),
                               0)
      ) %>%
      # Restrict to those with follow-up during the year
      filter(at_risk_days > 0)
    
    # Incidence denominator: total person-time at risk
    inc_denom <- risk_df_combined %>%
      mutate(person_years = at_risk_days / 365.25)
    inc_denom_value <- sum(inc_denom$at_risk_days) / 365.25
    
    # Incidence numerator: Number of patients with first prescription in that year   
    inc_numer <- risk_df_combined %>%
      filter(eventdate >= year_start & eventdate <= year_end)
    inc_numer_value <- length(unique(inc_numer$patid))
    
    # Combine numerator and denominator to get incidence per 1000 person-years follow-up
    incidence_rate = if_else(inc_denom_value == 0, 0,  # Correct for 0 denominator (albiglutide)
                             1000 * inc_numer_value / inc_denom_value)
    
    # Add to results
    calc_res$incidence_rate <- incidence_rate
    calc_res$inc_numer_value <- inc_numer_value
    calc_res$inc_denom_value <- inc_denom_value
  }

  
  ### Calculate period prevalence
  
  if (period_prevalence) {
    # Combine  with those who never had any GLP-1RA prescription
    prev_df <- prev_glp %>%
      bind_rows(risk_df_no_glp %>% select(patid, startfollow, endfollow))
    
    # Period prevalence denominator: total number of patients with follow-up during the year
    period_prev_denom <- prev_df %>%
      filter(startfollow <= year_end & 
               (is.na(endfollow) | endfollow >= year_start))
      # filter(year(startfollow) <= year & 
      #          (is.na(endfollow) | year(endfollow) >= year))
    period_prev_denom_value <- length(unique(period_prev_denom$patid))

    # Period prevalence numerator: Number of patients with relevant prescription in that year 
    period_prev_num <- period_prev_denom %>%
      filter(eventdate >= year_start & eventdate <= year_end)
    if (formulation != "Any") {
      period_prev_num <- period_prev_num %>%
        filter(type == formulation)
    }  
    period_prev_numer_value <- length(unique(period_prev_num$patid))
    
    # Combine numerator and denominator to get period prevalence as a percentage
    period_prev = if_else(period_prev_denom_value == 0, 0,  # Correct for 0 denominator (albiglutide)
                          100 * period_prev_numer_value / period_prev_denom_value)
    
    # Add to results
    calc_res$period_prev <- period_prev
    calc_res$period_prev_numer_value <- period_prev_numer_value
    calc_res$period_prev_denom_value <- period_prev_denom_value
  }
  
  return(calc_res)
}


# Filter df rows to those with SMI matching smid_type at the specified year
# Helper function for calculating yearly incidence and prevalence
filter_smid_type <- function(df, smid_type, year) {
  # Add end of year as a column
  df <- df %>%
    mutate(cutoff_year = as.Date(paste0(year, "-12-31"), "%Y-%m-%d"))
  
  # Get SMI diagnosis during year, using SMI hierarhcy
  smi_at_year <- apply_smi_hierarchy(cohort_data = df, 
                                     cutoff_date_var = "cutoff_year")
  df$smi_at_year <- smi_at_year$latest_smi_group
  
  # Filter dataframe to those matching the desired SMI type using smi_at_year
  if (smid_type == "all") {
    # Filter to those with SMI up to the year in question
    df <- df %>%
      filter(!is.na(smi_at_year))
  } else if (smid_type == "none") {
    # Filter to those without SMI up to the year in question
    df <- df %>%
      filter(is.na(smi_at_year))
  } else if (smid_type == "schizophrenia") {
    df <- df %>%
      filter(smi_at_year == "schizophrenia")
  } else if (smid_type == "bipolar") {
    df <- df %>%
      filter(smi_at_year == "bipolar")
  } else if (smid_type == "other psychosis") {
    df <- df %>%
      filter(smi_at_year == "other psychosis")
  } else if (smid_type == "depression") {
    df <- df %>%
      filter(smi_at_year == "depression")
  } else {
    stop(paste0("Invalid 'smid_type' value"))
  }
  return(df)
}


# `calc_inc_prev_all_years` calculates the incidence rate and prevalence for all years and formulations
# Inputs:
#   risk_df_no_glp: Dataframe of at-risk follow-up for individuals without GLP-1RA prescriptions
#   years_fu: Numeric vector of all years of follow-up in the study. Default is 2005-2025.
#   all_formulations: String vector of all GLP-1RA formulation types
#   incidence: Boolean (default = TRUE) specifying if the incidence rate should be calculated
#   risk_df: Dataframe of at-risk follow-up for individuals with GLP-1RA prescriptions, needed for 
#     incidence calculations. Default is NULL. 
#   period_prevalence: Boolean (default = TRUE) specifying if the period prevalence should be calculated
#   end_april: Boolean specifying if period prevalence should end 30 Apr (TRUE) or 31 Dec (FALSE; default)
#   point_prevalence: Boolean (default = TRUE) specifying if the point prevalence should be calculated
#   prev_glp: dataframe of all valid GLP-1RA prescriptions, needed for prevalence. Default is NULL.
#   smid_type: String indicator if incidence should be subsetted to an SMI subtype. 
#     Default is `NULL` (no subsetting). Must be one of "all", "none", "schizophrenia", "bipolar", 
#     "other psychosis", or "depression".
calc_inc_prev_all_years <- function(years_fu = c(2005:2025), all_formulations, risk_df_no_glp, 
                                    incidence = TRUE, risk_df = NULL, 
                                    period_prevalence = TRUE, end_april = FALSE,
                                    point_prevalence = TRUE, prev_glp = NULL, 
                                    smid_type = NULL) {
  
  # Error check
  if (incidence) {
    if (is.null(risk_df)) {
      stop("If incidence = TRUE, need to specify risk_df input.")
    }
  } else if (period_prevalence | point_prevalence) {
    if (is.null(prev_glp)) {
      stop("If period_prevalence = TRUE or point_prevalence = TRUE, need to specify prev_glp input.")
    }
  }
  
  # Initialize dfs for each year and formulation type
  inc_df <- expand.grid(year = years_fu, type = all_formulations) %>%
    mutate(incidence_rate = NA, 
           events = NA, 
           pyar = NA) # person-years at-risk
  period_prev_df <- expand.grid(year = years_fu, type = all_formulations) %>%
    mutate(period_prev = NA, 
           numerator = NA, 
           denominator = NA) 
  point_prev_df <- expand.grid(year = years_fu, type = all_formulations) %>%
    mutate(point_prev = NA, 
           numerator = NA, 
           denominator = NA) 
  
      # # If subsetting to SMI subtype, for each of computation, replace NAs with some far-off
      # # future date
      # if (!is.null(smid_type)) {
      #   risk_df <- risk_df %>%
      #     mutate_at(c("date_schiz", "date_bpd", "date_psych"), 
      #               ~replace_na(., as.Date("3000-01-01", "%Y-%m-%d")))
      #   risk_df_no_glp <- risk_df_no_glp %>%
      #     mutate_at(c("date_schiz", "date_bpd", "date_psych"), 
      #               ~replace_na(., as.Date("3000-01-01", "%Y-%m-%d")))
      # }
  
  # Calculate incidence for each year and formulation type
  for (i in 1:length(years_fu)) {
    year = years_fu[i]
    
    # Filter to SMI group if desired
    if (!is.null(smid_type)) {
      risk_df_no_glp <- filter_smid_type(df = risk_df_no_glp, 
                                         smid_type = smid_type, year = year)
        
      if (incidence) {
        risk_df <- filter_smid_type(df = risk_df, 
                                    smid_type = smid_type, year = year)
      }
      if (period_prevalence | point_prevalence) {
        prev_glp <- filter_smid_type(df = prev_glp, 
                                     smid_type = smid_type, year = year)
      }
    }
        # # Filter to those with SMID subtype, incorporating SMI hierarchy
        # if (!is.null(smid_type)) {
        #   if (smid_type == "schizophrenia") {
        #     # Filter to those diagnosed with schizophrenia up to the year in question.
        #     # Include those later diagnosed with schizophrenia, up to the year in question
        #     risk_df = risk_df %>%
        #       filter((smi_group == "schizophrenia" & year(smi_dx_date) <= year) | 
        #                (year(date_schiz) <= year))
        #     risk_df_no_glp <- risk_df_no_glp %>%
        #       filter((smi_group == "schizophrenia" & year(smi_dx_date) <= year) | 
        #                (year(date_schiz) <= year))
        #   } else if (smid_type == "bipolar") {
        #     # Filter to those diagnosed with bipolar up to the year in question.
        #     # Include those later diagnosed with bipolar, up to the year in question.
        #     # Exclude those later diagnosed with schizophrenia
        #     risk_df = risk_df %>%
        #       filter((smi_group == "bipolar" & year(smi_dx_date) <= year & year(date_schiz) > year) | 
        #                (year(date_bpd) <= year & year(date_schiz) > year))
        #     risk_df_no_glp <- risk_df_no_glp %>%
        #       filter((smi_group == "bipolar" & year(smi_dx_date) <= year & year(date_schiz) > year) | 
        #                (year(date_bpd) <= year & year(date_schiz) > year))
        #   } else if (smid_type == "other psychosis") {
        #     # Filter to those diagnosed with psychosis up to the year in question.
        #     # Include those later diagnosed with psychosis, up to the year in question.
        #     # Exclude those later diagnosed with schizophrenia or bipolar
        #     risk_df = risk_df %>%
        #       filter((smi_group == "other psychosis" & year(smi_dx_date) <= year & 
        #                 year(date_schiz) > year & year(date_bpd) > year) | 
        #                (year(date_psych) <= year & year(date_schiz) > year & year(date_bpd) > year))
        #     risk_df_no_glp <- risk_df_no_glp %>%
        #       filter((smi_group == "other psychosis" & year(smi_dx_date) <= year & 
        #                 year(date_schiz) > year & year(date_bpd) > year) | 
        #                (year(date_psych) <= year & year(date_schiz) > year & year(date_bpd) > year))
        #   } else if (smid_type == "depression") {
        #     # Filter to those diagnosed with depression up to the year in question.
        #     # Exclude those later diagnosed with schizophrenia or bipolar or psychosis
        #     risk_df = risk_df %>%
        #       filter((smi_group == "depression" & year(smi_dx_date) <= year & 
        #                 year(date_schiz) > year & year(date_bpd) > year & year(date_psych) > year))
        #     risk_df_no_glp <- risk_df_no_glp %>%
        #       filter((smi_group == "depression" & year(smi_dx_date) <= year & 
        #                 year(date_schiz) > year & year(date_bpd) > year & year(date_psych) > year))
        #   } else {
        #     stop("smid_type must be either NULL or one of 'schizophrenia', 'bipolar', 'other psychosis', or 'depression.")
        #   }

    
    for (j in 1:length(all_formulations)) {
      formulation = all_formulations[j]
      # Calculate incidence and/or prevalence
      inc_prev_df_temp <- calc_inc_prev(year = year, formulation = formulation, risk_df_no_glp = risk_df_no_glp, 
                                        incidence = incidence, risk_df = risk_df, 
                                        period_prevalence = period_prevalence, end_april = end_april,
                                        point_prevalence = point_prevalence, prev_glp = prev_glp)
      
      # Update results
      if (incidence) {
        inc_df[inc_df$year == year & inc_df$type == formulation, 
               c("incidence_rate", "events", "pyar")] <- c(inc_prev_df_temp$incidence_rate, 
                                                           inc_prev_df_temp$inc_numer_value,
                                                           inc_prev_df_temp$inc_denom_value)
      }
      if (period_prevalence) {
        period_prev_df[period_prev_df$year == year & period_prev_df$type == formulation, 
               c("period_prev", "numerator", "denominator")] <- c(inc_prev_df_temp$period_prev, 
                                                                  inc_prev_df_temp$period_prev_numer_value,
                                                                  inc_prev_df_temp$period_prev_denom_value)
      }
      if (point_prevalence) {
        point_prev_df[point_prev_df$year == year & point_prev_df$type == formulation, 
                       c("point_prev", "numerator", "denominator")] <- c(inc_prev_df_temp$point_prev, 
                                                                        inc_prev_df_temp$point_prev_numer_value,
                                                                        inc_prev_df_temp$point_prev_denom_value)
      }

    }
    print(paste0("Year ", year, " done!"))
  }
  
  ret_list <- list(inc_df = inc_df, period_prev_df = period_prev_df, point_prev_df = point_prev_df)
  
  return(ret_list)
}




# `create_lineplot` creates a lineplot of annual incidence rates or prevalences 
# Inputs:
#   data: Input dataset `data` with columns 'year', 'incidence_rate/period_prev/point_prev', and 'type'
#   drug_colors: Mapping of formulations to colors
#   drug_linetypes: Mapping of formulations to linetypes
#   all_formulations: String vector of all formulation types
#   years_fu: Numeric vector of all years of follow-up in the study. Default is 2005-2025.
#   y_var: String of either "incidence_rate", "period_prev", or "point_prev"
# Output: 'plot_inc' ggplot2 object
create_lineplot <- function(data, drug_colors = NULL, drug_linetypes = NULL, 
                            all_formulations, years_fu = c(2005:2025),
                            y_var = c("incidence_rate", "period_prev", "point_prev")) {
  y_var <- match.arg(y_var)
  
  # Set default for drug colors and linetypes
  if (is.null(drug_colors)) {
    drug_colors <- c("Albiglutide" = "#E41A1C", 
                     "Dulaglutide" = "#377EB8", 
                     "Exenatide" = "#FFD92F", 
                     "Liraglutide" = "#984EA3", 
                     "Lixisenatide" = "#FF7F00", 
                     "Semaglutide" = "#4DAF4A", 
                     "Tirzepatide" = "#994f00",
                     "Any" = "#999999")
  }
  
  if (is.null(drug_linetypes)) {
    drug_linetypes <- c("Albiglutide" = "solid", 
                        "Dulaglutide" = "dashed", 
                        "Exenatide" = "dotdash", 
                        "Liraglutide" = "longdash", 
                        "Lixisenatide" = "twodash", 
                        "Semaglutide" = "dotdash", 
                        "Tirzepatide" = "dotted",
                        "Any" = "solid")
  }
  
  y_lab <- if (y_var == "incidence_rate") {
    "Incidence Rate per 1000 Person-Years"
  } else if (y_var == "period_prev") {
    "Period Prevalence (%)"
  } else if (y_var == "point_prev") {
    "Point Prevalence (%)"
  }
  
  # Create plot
  plot_inc <- data %>%
    ggplot(aes(x = year, y = .data[[y_var]], color = type, group = type)) + 
    geom_line(aes(linetype = type), linewidth = 1, alpha = 0.7) + 
    geom_point(size = 2) + 
    scale_x_continuous(
      breaks = years_fu,
      limits = c(min(years_fu), max(years_fu)),
      expand = expansion(mult = c(0.01, 0.01)) 
    ) + 
    labs(
      x = "Year", y = y_lab, 
      color = "Formulation", linetype = "Formulation"
    ) + 
    theme_minimal(base_size = 12) + 
    scale_color_manual(values = drug_colors,
                       labels = all_formulations,
                       name = "Formulation") + 
    scale_linetype_manual(values = drug_linetypes,
                          labels = all_formulations,
                          name = "Formulation") + 
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 10),
      plot.margin = margin(10, 12, 10, 10)
    ) 
  
  return(plot_inc)
  
}



# =========================================================
# Sociodemographic table helper functions
# =========================================================

fmt_n_pct <- function(x, denom) {
  n <- sum(x %in% c(1, TRUE), na.rm = TRUE)
  pct <- ifelse(denom > 0, 100 * n / denom, NA_real_)
  sprintf("%s (%.1f%%)", format(n, big.mark = ","), pct)
}

fmt_median_iqr <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("NA")
  med <- median(x)
  q1  <- quantile(x, 0.25)
  q3  <- quantile(x, 0.75)
  sprintf("%.1f (%.1f to %.1f)", med, q1, q3)
}

fmt_cat <- function(x, level, denom) {
  n <- sum(x == level, na.rm = TRUE)
  pct <- ifelse(denom > 0, 100 * n / denom, NA_real_)
  sprintf("%s (%.1f%%)", format(n, big.mark = ","), pct)
}

add_row <- function(label, values) {
  tibble(
    Characteristic = label,
    `SMI diagnosis\nGLP` = values[1],
    `SMI diagnosis\nNo GLP` = values[2],
    `No SMI diagnosis\nGLP` = values[3],
    `No SMI diagnosis\nNo GLP` = values[4]
  )
}



# `apply_smi_hierarchy` obtains the latest SMI diagnosis prior to a date 
# specified by a given column, using the SMI hierarchy of 
# schizophrenia > bipolar > other psychosis > depression
# Details: `cohort_data` must have column specified in `cutoff_date_var`
# Returns: list containing `latest_smi_group` and `latest_smi_dx_date` vectors
# usage example: 
# apply_smi_hierarchy(cohort_data = cohort_demog_dx_date_smi, 
#                     cutoff_date_var = "startfollow")
apply_smi_hierarchy <- function(cohort_data, cutoff_date_var) {
  if (!(cutoff_date_var %in% colnames(cohort_data))) {
    stop("`cutoff_date_var` must be a column in `cohort_data")
  }
  
  # For any diagnoses occuring after the date in cutoff_date_var column, set to NA
  # Note: if cutoff_date_var value is NA, nothing changes
  cohort_data <- cohort_data %>%
    mutate(across(c(smi_dx_date, date_schiz, date_bpd, date_psych), 
                  ~ replace(., . > .data[[cutoff_date_var]], NA)))
  # Find the latest SMI dx date
  cohort_data <- cohort_data %>%
    mutate(latest_smi_dx_date = pmax(smi_dx_date, date_schiz, date_bpd, date_psych,
                                     na.rm = TRUE)) %>%
    mutate(latest_smi_group = case_when(
      is.na(latest_smi_dx_date) ~ NA,
      latest_smi_dx_date == smi_dx_date ~ smi_group,
      latest_smi_dx_date == date_schiz ~ "schizophrenia",
      latest_smi_dx_date == date_bpd ~ "bipolar",
      latest_smi_dx_date == date_psych ~ "other psychosis",
      TRUE ~ NA
    ))
  
  ret_list <- list(latest_smi_group = cohort_data$latest_smi_group,
                   latest_smi_dx_date = cohort_data$latest_smi_dx_date)
  return(ret_list)
}


# # `create_lineplot` creates a lineplot of the % of patients taking each 
# # type of drug in each year
# # Inputs:
# #   final_df: Final dataframe of drug types per patient and year
# #   drug_levels: String vector specifying all possible drug type names
# #   label_map: Named vector mapping drug types to label positions
# #   box_padding: Numeric specifying how far away the labels should be
# #   force_input: Numeric specifying amount of propulsion between labels
# #   y_max: Numeric specifying maximum percentage of y-axis
# #   drug_colors: String vector mapping drug types to colors
# #   drug_labels: String vector specifying drug names
# #   percentage: Boolean specifying if percentage or number should be displayed
# # Output: lineplot with labels
# create_lineplot <- function(final_df, drug_levels, label_map, 
#                             box_padding = 1, force_input = 8, y_max = 50000,
#                             drug_colors, drug_labels, 
#                             percentage = FALSE) {
#   
#   # Number of patients
#   n_patients <- length(unique(final_df$patid))
#   
#   # Summary table
#   drug_summary_updated <- final_df %>%
#     select(starts_with("year_")) %>%
#     tbl_summary(
#       by = NULL,
#       statistic = list(all_categorical() ~ "{n} ({p}%"),
#       missing = "no"
#     )
#   
#   # Convert wide to long
#   long_summary <- final_df %>%
#     pivot_longer(
#       cols = -patid,
#       names_to = "year",
#       values_to = "drug_type"
#     )
#   
#   # Calculate prevalence of each drug type
#   # Denominator is all individuals in cohort (including those with no prescriptions)
#   # contributing non-censored time. Individuals are dropped from the year
#   # after censoring
#   summary_long <- long_summary %>%
#     filter(!is.na(drug_type)) %>%
#     group_by(year, drug_type) %>%
#     summarise(n = n(), .groups = "drop") %>%
#     group_by(year) %>%
#     mutate(percent = n / sum(n) * 100) %>%
#     ungroup()
#   
#   # Add numeric year and relevel drug type
#   summary_long <- summary_long %>%
#     mutate(
#       year_num = as.integer(str_extract(year, "-?\\d+")),
#       year_label = factor(year_num, levels = 2005:2025, 
#                           labels = paste0("Year ", 2005:2025)),
#       drug_type = factor(drug_type, levels = drug_levels)
#     )
#   
#   # Turn the named label_map vector into case_match formulas
#   pairs <- imap(label_map, ~ new_formula(expr(!!.y), expr(!!.x)))
#   
#   # Make label positions staggered by drug combination
#   if (percentage) {
#     summary_labels <- summary_long %>%
#       group_by(drug_type) %>%
#       mutate(
#         label_x = case_match(drug_type, !!!pairs, .default = 2020),
#         label_y = percent[match(label_x, year_num)]
#       ) %>%
#       distinct(drug_type, label_x, label_y, .keep_all = TRUE) %>%
#       ungroup()
#   } else {
#     summary_labels <- summary_long %>%
#       group_by(drug_type) %>%
#       mutate(
#         label_x = case_match(drug_type, !!!pairs, .default = 2020),
#         label_y = n[match(label_x, year_num)]
#       ) %>%
#       distinct(drug_type, label_x, label_y, .keep_all = TRUE) %>%
#       ungroup()
#   }
#   
#   
#   # Plot (no title), colorblind-friendly
#   if (percentage) {
#     lineplot <- summary_long %>%
#       filter()
#       ggplot(aes(x = year_num, y = percent, color = drug_type)) + 
#       geom_line(linewidth = 0.8, alpha = 0.85) + # moderate transparency
#       geom_point(size = 1.3, alpha = 0.85) +  # points
#       # Labels at last observed year for each combination
#       geom_label_repel(
#         data = summary_labels,
#         aes(x = label_x, y = label_y, label = drug_type),
#         nudge_x = 0,
#         nudge_y = 0,
#         size = 3,
#         segment.curvature = 0,
#         segment.ncp = 0,
#         segment.size = 0.2,
#         show.legend = FALSE, 
#         min.segment.length = 0,
#         box.padding = box_padding, # Increase: pushes labels further
#         point.padding = 0.1, # Increase: pushes label further from point
#         force = force_input, # More repulsion force among labels
#         max.time = 8, # More iterations to find better spacing
#         max.overlaps = Inf
#       ) + 
#       scale_x_continuous(
#         breaks = c(2005:2025),
#         labels = c(2005:2025),
#         expand = expansion(add = c(0.2, 0.2)) # small spacing at plot edges
#       ) + 
#       scale_y_continuous(
#         limits = c(0, y_max),
#         breaks = seq(0, y_max, y_max/10), # labels every 10%
#         minor_breaks = seq(0, y_max, y_max/20), # gridlines every 5%
#         expand = expansion(mult = c(0, 0.05))
#       ) + 
#       scale_color_manual(values = drug_colors,
#                          labels = drug_labels,
#                          name = "Formulation") + 
#       labs(
#         x = "Year",
#         y = "Percentage of Patients",
#         color = "Formulation"
#       ) + 
#       theme_minimal() + 
#       theme(
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "right",
#         panel.grid.minor.y = element_line(color = "gray95"), # light minor gridlines
#         panel.grid.major.y = element_line(color = "gray90"), # horziontal grid every 10%
#         panel.grid.minor.x = element_blank(),
#         panel.grid.major.x = element_line(color = "gray90"), # vertical grid every year
#       ) + 
#       annotate( # Add in number of patients
#         "text",
#         x = 2007, y = y_max - 1,
#         label = paste0("n = ", n_patients),
#         hjust = 1, vjust = 1,
#         size = 3.5
#       )
#     
#   } else {
#     
#     lineplot <- summary_long %>%
#       # Remove "None" category
#       filter(drug_type != "None") %>%
#       ggplot(aes(x = year_num, y = n, color = drug_type)) + 
#       geom_line(linewidth = 0.8, alpha = 0.85) + # moderate transparency
#       geom_point(size = 1.3, alpha = 0.85) +  # points
#       # # Labels at last observed year for each combination
#       # geom_label_repel(
#       #   data = summary_labels,
#       #   aes(x = label_x, y = label_y, label = drug_type),
#       #   nudge_x = 0,
#       #   nudge_y = 0,
#       #   size = 3,
#       #   segment.curvature = 0,
#       #   segment.ncp = 0,
#       #   segment.size = 0.2,
#       #   show.legend = FALSE, 
#       #   min.segment.length = 0,
#       #   box.padding = box_padding, # Increase: pushes labels further
#       #   point.padding = 0.1, # Increase: pushes label further from point
#       #   force = force_input, # More repulsion force among labels
#       #   max.time = 8, # More iterations to find better spacing
#       #   max.overlaps = Inf
#       # ) + 
#       scale_x_continuous(
#         limits = c(2005, 2025),
#         breaks = c(2005:2025),
#         labels = c(2005:2025),
#         expand = expansion(add = c(0.2, 0.2)) # small spacing at plot edges
#       ) + 
#       scale_y_continuous(
#         limits = c(0, y_max),
#         breaks = seq(0, y_max, 10000), # labels every 10000
#         minor_breaks = seq(0, y_max, 5000), # gridlines every 5000
#         expand = expansion(mult = c(0, 0.05))
#       ) + 
#       scale_color_manual(values = drug_colors,
#                          labels = drug_labels,
#                          name = "Formulation") + 
#       labs(
#         x = "Year",
#         y = "Number of Patients",
#         color = "Formulation"
#       ) + 
#       theme_minimal() + 
#       theme(
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "right",
#         panel.grid.minor.y = element_line(color = "gray95"), # light minor gridlines
#         panel.grid.major.y = element_line(color = "gray90"), # horziontal grid every 10%
#         panel.grid.minor.x = element_blank(),
#         panel.grid.major.x = element_line(color = "gray90"), # vertical grid every year
#       ) + 
#       annotate( # Add in number of patients
#         "text",
#         x = 2007, y = y_max - 1,
#         label = paste0("n = ", n_patients),
#         hjust = 1, vjust = 1,
#         size = 3.5
#       )
#   }
#   
#   
#   return(lineplot)
# }


