# Ethnicity code lists

# Language spoken codes

# Clear memory
rm(list = ls())

# Packages
library(dplyr)
library(gtsummary)
library(lubridate)
library(readr)
library(readxl)
library(reshape2)
library(purrr)
library(forcats)
library(stringr)
library(tidyr)
library(data.table)

# Set correct file path
path <- "//live.rd.ucl.ac.uk" #Desktop@UCL
path <- "/Volumes/" # VPN connection

# Set working directory
setwd(paste0(path, "/ritd-ag-project-rd00qv-jfhay18/Alvin"))


# Identify additional codes

# GOLD
CPRDGoldMedical <- read_delim("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/PhD project/CPRD/Documentation/CPRDGoldmedical.txt", 
                              delim = "\t", escape_double = FALSE, 
                              col_types = cols(Medcode = col_character(), 
                                               `Read Code` = col_character(), `Clinical Events` = col_number(), 
                                               `Test Events` = col_number(), `Referral Events` = col_number(), 
                                               `Immunisation Events` = col_number()), 
                              trim_ws = TRUE) %>%
  rename(readterm = `Read Term`, medcode = Medcode) %>%
  select(-`First Appeared`)

languages <- CPRDGoldMedical %>%
  filter(grepl("(?i)language", readterm)) %>%
  filter(!grepl("(?i)child|therapy|sign|english|clinic|second|impairment|nos|referral|refused|therapist|difficulty|disorders?
                |delay|scale|test|written", readterm))

interpretter <- CPRDGoldMedical %>%
  filter(grepl("(?i)Interpreter", readterm)) %>%
  filter(!grepl("(?i)not needed|alphabet", readterm))

languagesinterpretter_gold <- rbind(languages, interpretter)

write_csv(languagesinterpretter_gold, file = "Code lists/Ethnicity/languagesinterpretter_gold140223.csv")

# save as text file
write.table(languagesinterpretter_gold, file = "Code lists/Ethnicity/languagesinterpretter_gold140223.txt",
            sep = "\t", row.names = FALSE)

#AURUM

CPRDAurumMedical <- read_delim("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/PhD project/CPRD/Documentation/CPRDAurumMedical.txt", 
                               delim = "\t", escape_double = FALSE, 
                               col_types = cols(MedCodeId = col_character(), 
                                                OriginalReadCode = col_character(), 
                                                SnomedCTConceptId = col_character(), 
                                                SnomedCTDescriptionId = col_character(), 
                                                Release = col_character()), trim_ws = TRUE) %>%
  rename(readterm = Term) %>%
  select(-Release)

languages <- CPRDAurumMedical %>%
  filter(grepl("(?i)language", readterm)) %>%
  filter(!grepl("(?i)child|therapy|sign|english|clinic|second|impairment|nos|referral|refused|therapist|difficulty|disorders?
                |delay|scale|test|written", readterm))

interpretter <- CPRDAurumMedical %>%
  filter(grepl("(?i)Interpreter", readterm)) %>%
  filter(!grepl("(?i)not needed|alphabet", readterm))

languagesinterpretter_aurum <- rbind(languages, interpretter)

write_csv(combined, file = "Code lists/Ethnicity/languagesinterpretter_aurum210423.csv")

write.table(combined, file = "Code lists/Ethnicity/combined.txt",
            sep = "\t", row.names = FALSE)


# Load and merge with existing ethnicity codes

# AURUM
#original list
Ethnicity_A_210423 <- read_delim("Code lists/Ethnicity/Original lists/Ethnicity_A_210423.txt", 
                                 delim = "\t", escape_double = FALSE, 
                                 col_types = cols(medcodeid = col_character()), 
                                 trim_ws = TRUE)

#list of languages/intepretter
aurum_languagecodes_310523 <- read_delim("Code lists/Ethnicity/Languages/aurum_languagecodes_310523.txt", 
                                         delim = "\t", escape_double = FALSE, 
                                         col_types = cols(MedCodeId = col_character()), 
                                         trim_ws = TRUE) %>%
  rename(medcodeid = MedCodeId)


aurum_ethnicity_310525 <- Ethnicity_A_210423 %>%
  bind_rows(aurum_languagecodes_310523) %>%
  select(1:6) %>%
  distinct(medcodeid, readterm, ethnicgroup, .keep_all = TRUE) %>%
  group_by(medcodeid) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(remove = case_when(n > 1 & is.na(ethnicgroup) ~ 1,
                            TRUE ~ 0)) %>%
  filter(remove == 0) %>%
  group_by(medcodeid) %>%
  mutate(n = n()) %>%
  select(1:6)

write_csv(aurum_ethnicity_310525, file = "Code lists/Ethnicity/aurum_ethnicity_310525.csv")


write.table(aurum_ethnicity_310525, file = "Code lists/Ethnicity/aurum_ethnicityF_310525.txt",
            sep = "\t", row.names = FALSE)


# GOLD
# Original list
Ethnicity_G_210423 <- read_delim("Code lists/Ethnicity/Original lists/Ethnicity_G_210423.txt", 
                                 delim = "\t", escape_double = FALSE, 
                                 col_types = cols(`Read code` = col_character(), 
                                                  medcode = col_character()), trim_ws = TRUE)


#list of languages/intepretter
gold_languagecodes_310523 <- read_delim("Code lists/Ethnicity/Languages/gold_languagecodes_310523.txt", 
                                                delim = "\t", escape_double = FALSE, 
                                                col_types = cols(medcode = col_character()), 
                                                trim_ws = TRUE) %>%
  rename(ethnicgroup = Ethnicity)

gold_ethnicity_310525 <- Ethnicity_G_210423 %>%
  bind_rows(gold_languagecodes_310523) %>%
  select(2:7) %>%
  distinct(medcode, readterm, ethnicgroup, .keep_all = TRUE) %>%
  group_by(medcode) %>%
  mutate(n = n())

write_csv(gold_ethnicity_310525, file = "Code lists/Ethnicity/gold_ethnicity_310525.csv")


write.table(aurum_ethnicity_310525, file = "Code lists/Ethnicity/aurum_ethnicity_310525.txt",
            sep = "\t", row.names = FALSE)




head(aurum_languagecodes_210423)
# Aurum

aurum_languagecodes_210423 <- aurum_languagecodes_210423 %>%
  filter(!is.na(ethnicgroup) | ethnicgroup != "Unknown") %>%
  rename(medcodeid = MedCodeId)

Ethnicity_A_130423 <- Ethnicity_A_130423 %>%
  mutate(CodeType = "Ethnicity")


common_cols <- intersect(colnames(Ethnicity_A_130423), colnames(aurum_languagecodes_210423))

SMIEthnicity_A <- bind_rows(
  Ethnicity_A_130423 %>% select(all_of(common_cols)),
  aurum_languagecodes_210423 %>% select(all_of(common_cols)))

write.table(SMIEthnicity_A, file = "Code lists/Ethnicity/SMIEthnicity_A_210423.txt",
            sep = "\t", row.names = FALSE)



