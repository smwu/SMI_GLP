library(ggplot2)
library(dplyr)
library(ggpubr)
library(tidyr)

# --- Setup ---

set.seed(123)
years <- 2000:2025
formulations <- c("exenatide", "liraglutide", "lixisenatide",
                  "albiglutide", "dulaglutide", "semaglutide", "tirzepatide")
smi_status <- c("with", "without")

# UK-based launch years
start_years <- c(
  exenatide = 2006,
  liraglutide = 2009,
  lixisenatide = 2013,
  albiglutide = 2014,
  dulaglutide = 2015,
  semaglutide = 2019,
  tirzepatide = 2023
)

# Color-blind friendly (Okabe-Ito) palette
cb_palette <- c(
  "all" = "#000000",
  "exenatide" = "#E69F00",
  "liraglutide" = "#56B4E9",
  "lixisenatide" = "#009E73",
  "albiglutide" = "#F0E442",
  "dulaglutide" = "#0072B2",
  "semaglutide" = "#D55E00",
  "tirzepatide" = "#CC79A7"
)

# Function to simulate prescribing rates with a sharp increase for semaglutide
simulate_rate <- function(year, start, peak_rate, type = "standard") {
  if (is.na(start) || year < start) return(NA)
  t <- year - start
  
  rate <- switch(type,
                 "standard" = peak_rate / (1 + exp(-0.3 * (t - 5))),  # sigmoid
                 "decay"    = peak_rate * exp(-0.1 * (t - 5)^2 / 25), # bell curve
                 "steep"    = peak_rate / (1 + exp(-0.6 * (t - 3))),  # steeper sigmoid
                 0
  )
  
  rate + rnorm(1, 0, 0.15)
}

# --- Simulate individual formulations ---

formulation_data <- expand.grid(
  year = years,
  formulation = formulations,
  SMI = smi_status
) %>%
  rowwise() %>%
  mutate(
    start = start_years[formulation],
    peak = case_when(
      formulation == "exenatide" ~ 2,
      formulation == "liraglutide" ~ 2.8,
      formulation == "lixisenatide" ~ 1,
      formulation == "albiglutide" ~ 1,
      formulation == "dulaglutide" ~ 3.5,
      formulation == "semaglutide" ~ 9,  # High final uptake
      formulation == "tirzepatide" ~ 3.5
    ),
    type = case_when(
      formulation %in% c("exenatide", "liraglutide", "lixisenatide", "albiglutide") ~ "decay",
      formulation == "semaglutide" ~ "steep",
      TRUE ~ "standard"
    ),
    rate = simulate_rate(year, start, peak, type)
  ) %>%
  ungroup()

# --- Add "all" as the sum of non-NA formulation rates ---

all_data <- formulation_data %>%
  filter(!is.na(rate)) %>%
  group_by(year, SMI) %>%
  summarise(rate = sum(rate), .groups = "drop") %>%
  mutate(formulation = "all")

# --- Combine all data together ---

data <- bind_rows(formulation_data, all_data) %>%
  mutate(formulation = factor(formulation, levels = c("all", formulations)))  # Control legend order

# --- Plot Function ---

plot_glp1ra <- function(df, smi_label) {
  ggplot(df %>% filter(SMI == smi_label),
         aes(x = year, y = rate, group = formulation)) +
    geom_line(aes(
      color = formulation,
      alpha = formulation == "all",
      linewidth = formulation == "all"
    )) +
    scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.7), guide = "none") +
    scale_linewidth_manual(values = c(`TRUE` = 1.4, `FALSE` = 0.8), guide = "none") +
    scale_color_manual(values = cb_palette) +
    scale_x_continuous(breaks = c(2005, 2010, 2015, 2020, 2025)) +
    labs(
      x = "Year",
      y = "Prescribing rate per 1000",
      title = ifelse(smi_label == "with", "People with SMI diagnosis", "People without SMI diagnosis")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      legend.title = element_blank(),
      legend.position = "right"
    )
}

# --- Create and Combine Plots ---

plot1 <- plot_glp1ra(data, "with")
plot2 <- plot_glp1ra(data, "without")

combined_plot <- ggarrange(
  plot1, plot2,
  ncol = 2, common.legend = TRUE, legend = "right"
)

# --- Add Caption ---

final_plot <- annotate_figure(
  combined_plot,
  bottom = text_grob(
    "Annual prevalence rates for the prescribing of GLP-1RA medications among people with and without SMI diagnosis.",
    size = 10
  )
)

# --- Display ---

print(final_plot)




#=========== age line plots



library(ggplot2)
library(dplyr)
library(ggpubr)

# Setup
set.seed(123)
years <- 2000:2025
age_groups <- c("18-44", "45-64", "65-74", "75+")
smi_status <- c("with", "without")

# Simulate prescribing trends by age group and SMI status
data <- expand.grid(year = years, age_group = age_groups, SMI = smi_status) %>%
  rowwise() %>%
  mutate(
    base_rate = case_when(
      age_group == "18-44" ~ 0.5,
      age_group == "45-64" ~ 2,
      age_group == "65-74" ~ 4,
      age_group == "75+" ~ 3
    ),
    growth = case_when(
      age_group == "18-44" ~ 0.1,
      age_group == "45-64" ~ 0.15,
      age_group == "65-74" ~ 0.2,
      age_group == "75+" ~ 0.12
    ),
    smi_adjust = ifelse(SMI == "with", 0.8, 1),  # maybe slightly lower rates for SMI
    rate = (base_rate + growth * (year - 2005)) * smi_adjust + rnorm(1, 0, 0.2)
  ) %>%
  ungroup() %>%
  mutate(rate = pmax(rate, 0))  # ensure no negatives

# Color-blind friendly palette
cb_palette <- c("#E69F00", "#56B4E9", "#009E73", "#D55E00")

# Plot function
plot_by_smi <- function(data, smi_label) {
  ggplot(data %>% filter(SMI == smi_label), aes(x = year, y = rate, color = age_group)) +
    geom_line(size = 1) +
    scale_color_manual(values = cb_palette) +
    scale_x_continuous(breaks = c(2005, 2010, 2015, 2020, 2025)) +
    labs(
      x = "Year",
      y = "Prescribing rate per 1000",
      title = paste("People", ifelse(smi_label == "with", "with", "without"), "SMI diagnosis")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      legend.position = "right"
    )
}

# Create plots
plot_with_smi <- plot_by_smi(data, "with")
plot_without_smi <- plot_by_smi(data, "without")

# Combine using ggpubr
combined_plot <- ggarrange(
  plot_with_smi, plot_without_smi,
  ncol = 2, common.legend = TRUE, legend = "right"
)

# Add caption
final_plot <- annotate_figure(
  combined_plot,
  bottom = text_grob(
    "Annual prevalence rates for the prescribing of GLP-1RA medications by age group and SMI status.",
    size = 10
  )
)

# Display
print(final_plot)





# Load libraries
library(ggplot2)
library(ggalluvial)
library(dplyr)
library(tidyr)

# -------------------------------------------
# Simulated treatment sequences with combined states
# -------------------------------------------

# Define treatment regimens at each step as combinations
treatment_sequence <- data.frame(
  Step1 = c("Metformin",
            "Metformin",
            "Metformin",
            "Metformin",
            "Metformin",
            "Metformin + SGLT2i",
            "Metformin + DPP4i"),
  
  Step2 = c("Metformin + SGLT2i",
            "Metformin + GLP-1RA",
            "SGLT2i",                   # switched from Metformin
            "Metformin + DPP4i",
            "Metformin + Other",
            "Metformin + SGLT2i + GLP-1RA",
            "Metformin + DPP4i + SGLT2i"),
  
  Step3 = c("Metformin + SGLT2i + GLP-1RA",
            "Metformin + GLP-1RA + Insulin",
            "SGLT2i + Insulin",
            "Metformin + DPP4i + Insulin",
            "Metformin + Other + Insulin",
            "Metformin + SGLT2i + GLP-1RA + Insulin",
            "Metformin + DPP4i + SGLT2i + Insulin"),
  
  freq = c(100, 80, 60, 50, 40, 30, 25)
)

# -------------------------------------------
# Alluvial plot of treatment regimens
# -------------------------------------------

library(ggplot2)
library(ggalluvial)
library(dplyr)
library(tidyr)

# Simulate patient treatment trajectories
set.seed(42)
n <- 300
drug_classes <- c(
  "metformin", "sulfonylureas", "SGLT-2 inhibitors", 
  "DPP-4 inhibitors", "GLP-1RAs", "insulin", 
  "other antidiabetics", "censored"
)

# Create baseline and follow-up treatment states
df <- data.frame(
  id = 1:n,
  T0 = sample(drug_classes[1:7], n, replace = TRUE, prob = c(0.4, 0.2, 0.1, 0.1, 0.1, 0.05, 0.05))
)

# Transition logic (random but realistic)
transition_function <- function(prev_state) {
  if (prev_state == "censored") return("censored")
  sample(c(drug_classes[1:7], "censored"), 1, prob = c(0.2, 0.1, 0.15, 0.15, 0.15, 0.1, 0.1, 0.05))
}

df$T1 <- sapply(df$T0, transition_function)
df$T2 <- sapply(df$T1, transition_function)

# Convert to long format
df_long <- df %>%
  pivot_longer(cols = starts_with("T"), names_to = "time", values_to = "state") %>%
  mutate(
    time = factor(recode(time, T0 = "Start", T1 = "6 months", T2 = "12 months"),
                  levels = c("Start", "6 months", "12 months")),
    state = factor(state, levels = drug_classes)
  )

# Plot alluvial diagram
ggplot(df_long,
       aes(x = time, stratum = state, alluvium = id,
           fill = state, label = state)) +
  geom_flow(stat = "alluvium", lode.guidance = "forward", alpha = 0.7) +
  geom_stratum(width = 1/8) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Antidiabetic Treatment Trajectories Over Time",
    x = "Time since first-line therapy",
    y = "Number of patients",
    fill = "Drug class"
  )



# -----------------------------------------------------
# Kaplan-Meier curve of time to treatment modification 
# -----------------------------------------------------

library(survival)
library(survminer)
library(dplyr)

# Simulated example data # REPLACE WITH REAL DATA
set.seed(123)
n <- 500
df_km <- data.frame(
  time_to_mod = rexp(n, rate = 0.05),  # time to event
  event = rbinom(n, 1, 0.7),           # 1 = treatment modification occurred
  SMI = sample(c("with", "without"), n, replace = TRUE)
)

# Fit survival model
surv_obj <- Surv(df_km$time_to_mod, df_km$event)  
fit <- survfit(surv_obj ~ SMI, data = df_km)

# Kaplan-Meier plot
ggsurvplot(
  fit,
  data = df_km,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = FALSE,
  legend.labs = c("With SMI", "Without SMI"),
  legend.title = "SMI status",
  xlab = "Time since GLP-1RA initiation (months)",
  ylab = "Proportion without treatment modification",
  palette = c("#D55E00", "#0072B2"),
  ggtheme = theme_minimal()
)



# -------------------------------------------
# Latent biomarker trajectories over time
# BMI, HbA1c, weight for the joint model
# -------------------------------------------

library(JMbayes2)
library(ggplot2)
library(dplyr)

### Example JMfit model

library(JMbayes2)
library(nlme)
library(survival)
library(dplyr)

# Simulate longitudinal and survival data
set.seed(123)
n <- 300
n_obs <- 5  # repeated measures per person

# Create repeated measures
df_long <- data.frame(
  id = rep(1:n, each = n_obs),
  time = rep(seq(0, 24, length.out = n_obs), n),
  SMI = rep(sample(c("with", "without"), n, replace = TRUE), each = n_obs)
) %>%
  mutate(
    HbA1c = 50 + 0.5 * time + 5 * (SMI == "with") + rnorm(n * n_obs, 0, 4)
  )

# Baseline data for survival
df_surv <- df_long %>%
  group_by(id) %>%
  summarise(
    SMI = first(SMI),
    age = sample(50:80, 1),
    event_time = rexp(1, rate = 0.05),
    event = rbinom(1, 1, 0.7)
  )

# Merge into long format for joint modeling
df_merged <- merge(df_long, df_surv, by = "id")

# Fit longitudinal submodel
lme_fit <- lme(HbA1c ~ time + SMI, random = ~ time | id, data = df_merged)

# Fit survival submodel
cox_fit <- coxph(Surv(event_time, event) ~ SMI + age, data = df_surv, x = TRUE)

# Fit joint model
jm_fit <- jm(cox_fit, lme_fit, time_var = "time")


### Create figure


# Add subject IDs for prediction
newdata$id <- 1:nrow(newdata)

# Predict biomarker values from the longitudinal submodel
pred <- fitted(jm_fit,
               process = "Longitudinal",
               newdata = newdata,
               var.type = "CI", return_newdata = TRUE)

# Plot latent HbA1c trajectories
ggplot(pred, aes(x = time, y = Estimate, color = SMI)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = `Lower`, ymax = `Upper`, fill = SMI), alpha = 0.2, color = NA) +
  scale_color_manual(values = c("with" = "#D55E00", "without" = "#0072B2")) +
  scale_fill_manual(values = c("with" = "#D55E00", "without" = "#0072B2")) +
  labs(
    title = "Latent HbA1c Trajectories by SMI Status",
    x = "Time since triple therapy failure (months)",
    y = "Estimated HbA1c (mmol/mol)",
    color = "SMI Status",
    fill = "SMI Status"
  ) +
  theme_minimal(base_size = 12)



# ------------------------------------------
# Violin plot of treatment interruptions 
# (days until first gap >30 days)
# ------------------------------------------

library(ggplot2)

# Simulated example data  # REPLACE WITH REAL DATA
set.seed(456)
df_violin <- data.frame(
  duration_no_gap = c(rgamma(250, 6, 0.03), rgamma(250, 5, 0.04)),
  SMI = rep(c("SMI", "No SMI"), each = 250)
)

# Violin plot
ggplot(df_violin, aes(x = SMI, y = duration_no_gap, fill = SMI)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5) +
  labs(
    x = "SMI status",
    y = "Days without gap >30 days",
    title = "Duration of continuous GLP-1RA use without 30-day gap"
  ) +
  scale_fill_manual(values = c("SMI" = "#D55E00", "No SMI" = "#0072B2")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")


# --------------------------------------
# Boxplot of proportion of days covered 
# PDC/CMA-4
# --------------------------------------

# Simulated example data
set.seed(789)
df_pdc <- data.frame(
  PDC = c(rbeta(250, 0.8, 0.3), rbeta(250, 0.9, 0.2)),  # more adherence in non-SMI group
  SMI = rep(c("SMI", "No SMI"), each = 250)
)

# Boxplot
ggplot(df_pdc, aes(x = SMI, y = PDC, fill = SMI)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  scale_fill_manual(values = c("SMI" = "#D55E00", "No SMI" = "#0072B2")) +
  labs(
    x = "SMI status",
    y = "Proportion of days covered (PDC)",
    title = "GLP-1RA Medication Adherence (CMA-4)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")


