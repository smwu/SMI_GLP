# ==============================================================================
# Check feasibility by examining the number of patients satisfying conditions
# Author: SM Wu
# Date Created: 2025/06/20
# Date Updated: 2025/09/29
# 
# Details:
# 1) Read in extracted data
# 2) Patients with T2DM and SMI
# 3) Patients with T2DM and SMI and GLP-1RA
# 4) Patients with T2DM and SMI and other antidiabetics
#
# Inputs:
# 1) Data/Extraction_Files/pat_smi_comb.RData: Patients with SMI diagnosis
# 2) Data/Extraction_Files/pat_t2dm_comb.RData: Patients with T2DM diagnosis
# 3) Data/Extraction_Files/pat_glp_comb.RData: Patients taking GLP-1RA meds
# 4) Data/Extraction_Files/pat_su_comb.RData: Patients taking sulfonylurea meds
# 5) Data/Extraction_Files/pat_sglt2i_comb.RData: Patients taking SGLT2-inhibitor meds
# 6) Data/Extraction_Files/pat_dpp4i_comb.RData: Patients taking DPP4-inhibitor meds
# 
# Final Outputs:
# 1) Number of individuals with SMI and T2DM diagnoses and taking various 
# anti-diabetic medications

# ==============================================================================


# ====================== 1) Read in extracted data =============================

# Clear memory
rm(list = ls())

# Set working directory
wd <- "/Volumes/ritd-ag-project-rd00qv-jfhay18/" # VPN connection
# wd <- "//live.rd.ucl.ac.uk/ritd-ag-project-rd00qv-jfhay18/" #Desktop@UCL
setwd(wd)

# Set input and output paths
path_input <- "Stephanie/SMI_GLP/Data/Extraction_Files/"

# Read in patients with SMI diagnosis
load(paste0(wd, path_input, "pat_smi_comb.RData"))

# Read in patients with T2DM diagnosis
load(paste0(wd, path_input, "pat_t2dm_comb.RData"))

# Read in patients taking GLP-1RA medication
load(paste0(wd, path_input, "pat_glp_comb.RData"))
# Read in patients taking other second-line oral antidiabetics, including 
# sulfonylureas (SUs), SGLT-2 inhibitors, and DPP-4 inhibitors
load(paste0(wd, path_input, "pat_su_comb.RData"))
load(paste0(wd, path_input, "pat_sglt2i_comb.RData"))
load(paste0(wd, path_input, "pat_dpp4i_comb.RData"))

# ====================== 2) Patients with T2DM and SMI =========================

### Patients with SMI (excluding depression) and T2DM diagnosis

# Get entries: 916,766 
pat_smi_t2dm <- pat_smi_comb %>%
  filter(patid %in% pat_t2dm_comb$patid) %>%
  bind_rows(pat_t2dm_comb %>% filter(patid %in% pat_smi_comb$patid))

# Number of unique patients: 70,381
n_distinct(pat_smi_t2dm$patid)


# ====================== 3) Patients with T2DM and SMI and GLP-1RA =============


### Patients with SMI and T2DM and taking GLP-1RAs

# Get entries: 107,851 
pat_smi_t2dm_glp <- pat_smi_t2dm %>%
  filter(patid %in% pat_glp_comb$patid) %>%
  bind_rows(pat_glp_comb %>% filter(patid %in% pat_smi_t2dm$patid))

# Number of unique patients: 4098
n_distinct(pat_smi_t2dm_glp$patid)

# =================== 4) Patients with T2DM and SMI and other antidiabetics ====

### Patients with SMI and T2DM taking SUs, SGLT-2is, or DPP-4is

# Get entries: 673,242 entries
pat_smi_t2dm_su_sglt2_dpp4 <- pat_smi_t2dm %>%
  filter(patid %in% pat_su_comb$patid | patid %in% pat_sglt2i_comb$patid | 
           patid %in% pat_dpp4i_comb$patid) %>%
  bind_rows(pat_su_comb %>% filter(patid %in% pat_smi_t2dm$patid)) %>%
  bind_rows(pat_sglt2i_comb %>% filter(patid %in% pat_smi_t2dm$patid)) %>%
  bind_rows(pat_dpp4i_comb %>% filter(patid %in% pat_smi_t2dm$patid))

# Number of unique patients: 32,710
n_distinct(pat_smi_t2dm_su_sglt2_dpp4$patid)

# How many of those on GLP-1RAs are also on SU/SGLT2/DPP4? 
unique_smi_t2dm_glp <- unique(pat_smi_t2dm_glp$patid)
unique_smi_t2dm_su_sglt2_dpp4 <- unique(pat_smi_t2dm_su_sglt2_dpp4$patid)
both <- intersect(unique_smi_t2dm_glp, unique_smi_t2dm_su_sglt2_dpp4)
length(both) # 3543 on both types
