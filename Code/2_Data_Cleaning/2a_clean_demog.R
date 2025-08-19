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


# ================= 1) Set up and load data ====================================

# Clear memory
rm(list = ls())

# Packages
library(readxl)
library(openxlsx)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# Set working directory
wd <- "/Volumes/ritd-ag-project-rd00qv-jfhay18/" # VPN connection
# wd <- "//live.rd.ucl.ac.uk/ritd-ag-project-rd00qv-jfhay18/" #Desktop@UCL
setwd(wd)

# Set input and output paths
path_input <- "2023 CPRD/"
path_output <- "Data/Final_Data_Files/"

## Load data

# Read in patient and practice files from GOLD and Aurum
# GOLD patient
gold_pat <- read.table(
  file = paste0(wd, path_input, "GOLD/Patient/SMI_GOLD_Extract_Patient_001.txt"),
  header = TRUE, fill = TRUE, sep = "\t", quote = "", 
  colClasses = c(patid = "character"))
# GOLD practice
gold_prac <- read.table(
  file = paste0(wd, path_input, "GOLD/Practice/SMI_GOLD_Extract_Practice_001.txt"),
  header = TRUE, sep = "\t", quote = "")
# Aurum patient
aurum_pat <- read.table(
  file = paste0(wd, path_input, "Aurum/Patient/SMI_AURUM_Extract_Patient_001.txt"),
  header = TRUE, fill = TRUE, sep = "\t", quote = "", 
  colClasses = c(patid = "character"))
# Aurum practice
aurum_prac <- read.table(
  file = paste0(wd, path_input, "Aurum/Practice/SMI_AURUM_Extract_Practice_001.txt"),
  header = TRUE, sep = "\t", quote = "")


# Read in Aurum medical dictionary
cprd_aurum_medical_raw <- 
  read_delim(
    paste0(wd, "Stephanie/SMI_GLP/Code_Lists/MASTER_Lists/",
           "CPRD_Aurum_Medical_10Feb2025.txt"), 
    delim = "\t", escape_double = FALSE, 
    col_types = cols(MedCodeId = col_character(), 
                     OriginalReadCode = col_character(), 
                     CleansedReadCode = col_character(), 
                     SnomedCTConceptId = col_character(), 
                     SnomedCTDescriptionId = col_character()), 
    trim_ws = TRUE)
cprd_aurum_medical <- cprd_aurum_medical_raw %>%
  select(-Release) %>%
  dplyr::rename(term = Term, medcodeid = MedCodeId) %>%
  mutate(term = str_to_lower(term))

# Read in Gold medical dictionary
cprd_gold_medical_raw <- 
  read_delim(
    paste0(wd, "Stephanie/SMI_GLP/Code_Lists/MASTER_Lists/",
           "CPRD_GOLD_Medical_23Feb2025.txt"), 
    delim = "\t", escape_double = FALSE, 
    col_types = cols(medcode = col_character(), 
                     readcode = col_character()), 
    trim_ws = TRUE) 
cprd_gold_medical <- cprd_gold_medical_raw %>%
  dplyr::rename(term = readterm) %>%
  mutate(term = str_to_lower(term))


# ================= 2) Set up and load data ====================================

# Generate practice ID for GOLD using the first four digits of the patient ID
gold_pat$pracid <- substring(gold_pat$patid, nchar(gold_pat$patid) - 4)

# Append 'G' or 'A' to the patient and practice IDs to distinguish GOLD and Aurum
gold_pat$patid <- paste0(gold_pat$patid, "-", "G")
gold_pat$pracid <- paste0(gold_pat$pracid, "-", "G")
gold_prac$pracid <- paste0(gold_prac$pracid, "-", "G")
aurum_pat$patid <- paste0(aurum_pat$patid, "-", "A")
aurum_pat$pracid <- paste0(aurum_pat$pracid, "-", "A")
aurum_prac$pracid <- paste0(aurum_prac$pracid, "-", "A")

# Outer join the patient and practice files together, merging by 'pracid' and 
# keeping all entries from either dataset
gold_pat <- merge(x = gold_pat, y = gold_prac, by = "pracid", all.x = TRUE, 
                  all.y = TRUE)
aurum_pat<-merge(x = aurum_pat, y = aurum_prac, by = "pracid", all.x = TRUE, 
                 all.y = TRUE)

# Add in variable denoting dataset source
gold_pat$source <- "Gold"
aurum_pat$source <- "Aurum"

####re-order so datasets are comparable####
ls(aurum_pat)
ls(gold_pat)

gold_pat <- gold_pat %>%
  select(-c(vmid, mob, CHSreg, CHSdate, capsup, famnum, frd, internal, reggap, 
            prescr, marital, regstat, uts))
aurum_pat <- aurum_pat %>%
  select(-c(mob, patienttypeid, usualgpstaffid, -ts)) %>%
  rename(accept = acceptable)

#Make name same as Gold
aurum_pat<-rename(aurum_pat, accept=acceptable)

#Take CPRD_ddate, but if null take emis_date and then rename and drop others
aurum_pat$emis_ddate<-as.Date(aurum_pat$emis_ddate, format= "%d/%m/%Y")
aurum_pat$cprd_ddate<-as.Date(aurum_pat$cprd_ddate, format= "%d/%m/%Y")

length(which(aurum_pat$emis_ddate!=aurum_pat$cprd_ddate))
length(which(is.na(aurum_pat$cprd_ddate)))
length(which(is.na(aurum_pat$emis_ddate)))

aurum_pat<-rename(aurum_pat, deathdate=cprd_ddate)
aurum_pat<-select(aurum_pat, -emis_ddate)

#Create died
aurum_pat$died<-0
aurum_pat$died[!is.na(aurum_pat$deathdate)]<-1

#Create died in Gold: check none have toreason as death and no death date!
gold_pat$deathdate<-as.Date(gold_pat$deathdate, format= "%d/%m/%Y")
table(gold_pat$toreason)
length(which(gold_pat$toreason==1 & is.na(gold_pat$deathdate)))

gold_pat$died<-0
gold_pat$died[!(is.na(gold_pat$deathdate))]<-1

gold_pat<-select(gold_pat, -toreason)

#Sort start and end dates
#Sort registration
aurum_pat$regenddate<-as.Date(aurum_pat$regenddate, format= "%d/%m/%Y")
aurum_pat$lcd<-as.Date(aurum_pat$lcd, format= "%d/%m/%Y")
aurum_pat$regstartdate<-as.Date(aurum_pat$regstartdate, format= "%d/%m/%Y")

summary(aurum_pat$regenddate)

gold_pat$tod<-as.Date(gold_pat$tod, format= "%d/%m/%Y")
gold_pat$lcd<-as.Date(gold_pat$lcd, format= "%d/%m/%Y")
gold_pat$crd<-as.Date(gold_pat$crd, format= "%d/%m/%Y")

summary(gold_pat$tod)

gold_pat<-rename(gold_pat, regstartdate=crd)
gold_pat<-rename(gold_pat, regenddate=tod)

#Reorder date
aurum_pat<-aurum_pat[, c(2,1,3,4,5,6,12,8,10, 9,11,7)]
gold_pat<-gold_pat[, c(2,1,3,4,5,6,12,7,9,10,11,8)]

#Year 18
aurum_pat$Date18<-paste0(aurum_pat$yob+18,"-01-01")
gold_pat$Date18<-paste0(gold_pat$yob+18,"-01-01")

#Generate follow up variables
aurum_pat$Start2000<-pmax(aurum_pat$regstartdate, aurum_pat$Date18, as.Date("2000-01-01"),  na.rm=TRUE)
aurum_pat$End2022<-pmin(aurum_pat$regenddate, aurum_pat$deathdate, as.Date("2022-12-31"),  na.rm=TRUE)
aurum_pat$Active0022<-as.numeric(aurum_pat$End2022-aurum_pat$Start2000)

gold_pat$Start2000<-pmax(gold_pat$regstartdate, gold_pat$Date18, as.Date("2000-01-01"),  na.rm=TRUE)
gold_pat$End2022<-pmin(gold_pat$regenddate, gold_pat$deathdate, as.Date("2022-12-31"),  na.rm=TRUE)
gold_pat$Active0022<-as.numeric(gold_pat$End2022-gold_pat$Start2000)

#Combine as one data set
AllSMI2023<-rbind(aurum_pat, gold_pat)

#Set region
AllSMI2023<- AllSMI2023%>% 
  mutate(region=case_when(region==1 ~ "North East",
                          region==2 ~ "North West",
                          region==3 ~ "Yorkshire & The Humber",
                          region==4 ~ "East Midlands",
                          region==5 ~ "West Midlands",
                          region==6 ~ "East of England",
                          region==7 ~ "London",
                          region==8 ~ "South East",
                          region==9 ~ "South West",
                          region==10 ~ "Wales",
                          region==11 ~ "Scotland",
                          region==12 ~ "Northern Ireland",
                          TRUE~ "NA"))

AllSMI2023$region[AllSMI2023$region=="NA"]<-NA
table(AllSMI2023$region, AllSMI2023$source, useNA="ifany")
table(aurum_prac$region, useNA="ifany")

#Check our old data

aurum_prac2018<-read.table("R:/pathfinder/Data/CPRD Aurum/18_288R_Aurum_Extract_Practice_001.txt", header=TRUE, fill=TRUE, sep="\t", quote="")
table(aurum_prac2018$region, useNA="ifany")

MissingRegion<-subset(aurum_prac, is.na(region))
OldCheck<-subset(aurum_prac2018, pracid %in% MissingRegion$pracid)

save(AllSMI2023, file="R:/2023 CPRD/MergedFile/RawCombined.Rdata")

