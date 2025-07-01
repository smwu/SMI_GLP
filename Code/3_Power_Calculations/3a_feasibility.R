# ==============================================================================
# Check feasibility by examining the number of patients satisfying conditions
# Author: SM Wu
# Date Created: 2025/06/20
# Date Updated: 2025/06/27
# 
# Details:
# 1) 
#
# Inputs:
# 1) # 
# 
# Intermediate outputs:
# 1) 
# 
# Final Outputs:
# 1) 

# ==============================================================================


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
# Read in patients taking GLP-1RA medication
load(paste0(wd, path_input, "pat_glp_comb.RData"))
# Read in patients taking other second-line oral antidiabetics, including 
# sulfonylureas (SUs), SGLT-2 inhibitors, and DPP-4 inhibitors
load(paste0(wd, path_input, "pat_su_comb.RData"))
load(paste0(wd, path_input, "pat_sglt2i_comb.RData"))
load(paste0(wd, path_input, "pat_dpp4i_comb.RData"))


### Patients with SMI and taking GLP-1RAs

# Get entries: 188,353 
pat_smi_glp <- pat_smi_comb %>%
  filter(patid %in% pat_glp_comb$patid) %>%
  bind_rows(pat_glp_comb %>% filter(patid %in% pat_smi_comb$patid))

# Number of unique patients: 4613
n_distinct(pat_smi_glp$patid)


### Patients with SMI taking SUs, SGLT-2is, or DPP-4is

# Get entries: 2,973,126 entries
pat_smi_su_sglt2_dpp4 <- pat_smi_comb %>%
  filter(patid %in% pat_su_comb$patid | patid %in% pat_sglt2i_comb$patid | 
           patid %in% pat_dpp4i_comb$patid) %>%
  bind_rows(pat_su_comb %>% filter(patid %in% pat_smi_comb$patid)) %>%
  bind_rows(pat_sglt2i_comb %>% filter(patid %in% pat_smi_comb$patid)) %>%
  bind_rows(pat_dpp4i_comb %>% filter(patid %in% pat_smi_comb$patid))

# Number of unique patients: 33,537
n_distinct(pat_smi_su_sglt2_dpp4$patid)

# How many of those on GLP-1RAs are also on SU/SGLT2/DPP4? 
unique_smi_glp <- unique(pat_smi_glp$patid)
unique_smi_su_sglt2_dpp4 <- unique(pat_smi_su_sglt2_dpp4$patid)
both <- intersect(unique_smi_glp, unique_smi_su_sglt2_dpp4)
length(both) # 3801 on both types
