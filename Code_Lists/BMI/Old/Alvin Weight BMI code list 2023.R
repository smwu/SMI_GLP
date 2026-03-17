# --------------
# Generate weight and BMI code lists
# --------------
 # CPRD 2023 data

# Last run 24/07/23

#Clear memory
rm(list = ls())

# Packages
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(data.table)
library(tidylog)

# Set correct file path
path <- "//live.rd.ucl.ac.uk" #Desktop@UCL
path <- "/Volumes/" # VPN connection

# Set working directory
setwd(paste0(path, "/ritd-ag-project-rd00qv-jfhay18/Alvin"))

# Load data

# GOLD
CPRDGoldMedical <- read_delim("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/PhD project/Data sources/CPRD/Documentation/CPRD-2023/CPRDGoldMedical.txt", 
                              delim = "\t", escape_double = FALSE, 
                              col_types = cols(medcode = col_character()), 
                              trim_ws = TRUE) %>%
  select(medcode, readterm = desc)

# AURUM
CPRDAurumMedical <- read_delim("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/PhD project/Data sources/CPRD/Documentation/CPRD-2023/CPRDAurumMedical.txt", 
                               delim = "\t", escape_double = FALSE, 
                               col_types = cols(MedCodeId = col_character(), 
                                                Observations = col_skip(), OriginalReadCode = col_skip(), 
                                                CleansedReadCode = col_skip(), SnomedCTConceptId = col_skip(), 
                                                SnomedCTDescriptionId = col_skip(), 
                                                Release = col_skip(), EmisCodeCategoryId = col_skip()), 
                               trim_ws = TRUE) %>%
  select(medcodeid = MedCodeId, readterm = Term)

# Code lists

# GOLD

weightcodes_g <- CPRDGoldMedical %>%
  filter(grepl("(?i)weight|body mass|bmi|obesity|obese|overweight|underweight", readterm)) %>%
  filter(!grepl("(?i)target|sample|birth|baby|pregnancy|child|premature|heparin|bear|weightless|overestimates|sweat|
                |eye|subscale|molecular|placental|down's|score|fpc|massage|fracture|ideal weight|7pcl|ran|diffusion|fetal|lift|centile|percent|maternal|Ideal body weight|fear|
                |Obesity resolved", readterm))

# havent included codes for height, as these will be extracted directly without a code list

# save as text file
write.table(weightcodes_g, file = "Code lists/BMI/CPRD-2023/weightbmi_gold_240723.txt",
            sep = "\t", row.names = FALSE)

#AURUM

weightcodes_a <- CPRDAurumMedical %>%
  filter(grepl("(?i)weight|body mass|bmi|obesity|obese|overweight|underweight", readterm)) %>%
  filter(!grepl("(?i)target|sample|birth|baby|pregnancy|child|premature|heparin|bear|weightless|overestimates|sweat|
                |eye|subscale|molecular|placental|down's|score|fpc|massage|fracture|ideal weight|7pcl|ran|diffusion|fetal|lift|centile|percent|maternal|Ideal body weight|
                |Obesity resolved", readterm))

heightcodes_a <- CPRDAurumMedical %>%
  filter(grepl("(?i)height", readterm)) %>%
  filter(!grepl("(?i)child|heightened|loss|uter|fundal|score|sitting|limit|injury|jump|unfit|ratio|down's|furniture|to|centile|step|knee|fall", readterm))

weightcodes_a <- weightcodes_a %>%
  bind_rows(heightcodes_a) %>%
  distinct()

# save as text file
write.table(weightcodes_a, file = "Code lists/BMI/CPRD-2023/weightbmi_aurum_240723.txt",
            sep = "\t", row.names = FALSE)

