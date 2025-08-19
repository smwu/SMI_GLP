# ==============================================================================
# Clean dataset of patients with SMI diagnoses using linked mortality data
# Author: SM Wu
# Date Created: 2025/06/17
# Date Updated: 2025/06/17
# 
# Details:
# 1) 
#
# Inputs:
# 1) 
# 
# Intermediate outputs:
# 1) 
# 
# Final Outputs:
# 1) 

# ==============================================================================



# ================= 5) Harmonize with birth and death data =====================

## Read in linked ONS mortality data
# These are restricted to patients from England who consented to data linkage

# GOLD
death_patient_gold <- read_delim(
  file = paste0(wd, "2023 CPRD/Linkages/Results/Final Gold/Deaths/",
                "death_patient_21_000729.txt"), 
  delim = "\t", escape_double = FALSE, 
  col_types = cols(patid = col_character(), dod = col_date(format = "%d/%m/%Y")),
  trim_ws = TRUE) %>%
  select(patid, dod, match_rank) %>%
  mutate(database = "Gold")

# Aurum
death_patient_aurum <- read_delim(
  file = paste0(wd, "2023 CPRD/Linkages/Results/Aurum_linked/Final/Deaths/",
                "death_patient_21_000729.txt"), 
  delim = "\t", escape_double = FALSE, 
  col_types = cols(patid = col_character(), dod = col_date(format = "%d/%m/%Y")), 
  trim_ws = TRUE) %>%
  select(patid, dod, match_rank) %>%
  mutate(database = "Aurum")

# Combined
deaths_ons <- death_patient_aurum %>%
  bind_rows(death_patient_gold) %>%
  mutate(
    patid = case_when(
      database == "Gold" ~ paste0(patid, "-G"),
      database == "Aurum" ~ paste0(patid, "-A"),
      .default = patid)) %>%
  rename(ons_dod = dod, ons_match_rank = match_rank) %>%
  select(-database)

# Remove separate files to save memory
rm(death_patient_gold, death_patient_aurum)

ons_earliest <- "1998-01-02"
ons_latest <- "2021-03-29"

# Merge ONS deaths into CPRD patient data
pat_smi_clean_deaths <- pat_smi_comb %>%
  left_join(deaths_ons, by = "patid") %>%
  # Restrict to acceptable matches, with ons_match_rank equal to either
  #   1: exact NHS #, sex, DOB, postcode
  #   2: exact NHS #, sex, DOB
  mutate(deathdate = case_when(
    # ONS death within linkage period with acceptable match, use ONS
    !is.na(ons_dod) & ons_dod >= ons_earliest & ons_dod <= ons_latest & 
      ons_match_rank <= 2 ~ ons_dod, 
    .default = NA)) 

# mutate(deathdate = case_when(
#   # CPRD death recorded, no ONS death, use CPRD
#   !is.na(cprddeathdate) & is.na(ons_dod) ~ cprddeathdate, 
#   # no CPRD, ONS death within linkage period with acceptable match, use ONS
#   ons_death_e == 1 & !is.na(ons_dod) & is.na(cprddeathdate) & 
#     ons_dod >= ons_earliest & ons_dod <= ons_latest & ons_match_rank <= 2 ~ ons_dod, 
#   # CPRD and ONS present, ONS death within linkage period with acceptable match, use ONS
#   ons_death_e == 1 & !is.na(ons_dod) & !is.na(cprddeathdate) & 
#     ons_dod >= ons_earliest & ons_dod <= ons_latest & ons_match_rank <= 2 ~ ons_dod, 
#   # CPRD and ONS present, but ONS either outside coverage or low rank, use CPRD
#   ons_death_e == 1 & !is.na(ons_dod) & !is.na(cprddeathdate) & 
#     ons_dod < ons_earliest | ons_dod > ons_latest | ons_match_rank >=3 ~ cprddeathdate)) 
# 
# # Keep only patients in the index file, remove records prior to birth or after death
# load(file="Data files/CPRD-2023/SMIindex.Rdata")

pat_smi_final <- pat_smi_clean_deaths %>%
  mutate(event_year = year(eventdate)) %>%
  # Remove prescriptions in or prior to birth year (n=514)
  filter(event_year > yob) %>% 
  # Remove diagnoses after death date (n=1,824)
  filter(deathdate > eventdate | is.na(deathdate)) %>% 
  select(-yob, -deathdate, -event_year)

# Number of unique patients with condition
n_distinct(pat_smi_final$patid) # 592,055


# PatSMI_C <- SMIindex %>%
#   select(patid, yob, deathdate) %>%
#   inner_join(PatSMI_C, by = "patid") %>% # keep only included SMI patients
#   mutate(event_year = year(eventdate)) %>%
#   filter(event_year > yob) %>% # remove prescriptions in or prior to birth year (n=514)
#   filter(deathdate > eventdate | is.na(deathdate)) %>% # remove diagnoses after death date (n=1,824)
#   select(-yob, -deathdate, -event_year)
# 
# #Save to file
# n_distinct(PatSMI_C$patid) # 592,055
# save(PatSMI_C, file="Data files/CPRD-2023/PatSMI_C.Rdata")
# 
# remove(PatSMI_C, SMIindex)

