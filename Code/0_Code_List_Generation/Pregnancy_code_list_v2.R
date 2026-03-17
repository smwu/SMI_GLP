# Generate code list for Pregnancy 
# Author: S Picton & S Wu
# Date created: 2026/02/24
# Date updated: 2026/03/10

# 
# Details:
# 1) Set up and load data
# 2) Search for new relevant med codes
# 3) Create updated code lists
# 4) Adjust formatting for extraction
#
# Inputs:
# 1) Code_Lists/MASTER_Lists/CPRD_Aurum_Medical_14Oct2025.txt: Aurum medical master code list
# 2) Code_Lists/MASTER_Lists/CPRD_GOLD_Medical_14Oct2025.txt: GOLD medical master code list


# Final Outputs:

# 1) Code_Lists/Pregnancy/Aurum_Pregnancy_codelist_20260304.txt :  Aurum pregnancy code list 
# 2) Code_Lists/Pregnancy/Gold_Pregnancy_codelist_20260304.txt : Gold pregnancy code list 
# 3) Code_Lists/Pregnancy/Aurum_Gold_Pregnancy_codelist_20260304.txt : Combined Aurum & Gold pregnancy code list


# ================= 1) Set up and load data ====================================

# Clear memory
rm(list = ls())

# Packages
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(writexl)


#  If working in Data Safe Haven - manually install packages using Artifactory:
#   bit, bit64, cli, crayon, dplyr, generics, glue, hms,
#   lifecycle, magrittr, pillar, pkgconfig, purrr, R6, readr, rlang, stringi, 
#   stringr, tibble, tidyr, tidyselect, tzdb, utf8, vctrs, vroom, withr, writexl

# ### For running locally
# # Set working directory
# wd <- "/Volumes/ritd-ag-project-rd00qv-jfhay18/" # VPN connection
# # wd <- "//live.rd.ucl.ac.uk/ritd-ag-project-rd00qv-jfhay18/" #Desktop@UCL
# setwd(wd)
# 
# # Set input and output paths
# path_input <- "Code_Lists/"
# path_output <- "Code_Lists/Pregnancy/"


### For running in Data Safe Haven
# Set working directory
wd <- "S:/CDSTP_CPRD_25_005368/" 
setwd(wd)

# Set input and output paths
path_input <- "SMI_GLP/Code_Lists/"
path_output <- "SMI_GLP/Code_Lists/Pregnancy/"

## Load data

# Read in Aurum medical dictionary
cprd_aurum_medical_raw <- 
  read_delim(
    paste0(wd, path_input, "MASTER_Lists/CPRD_Aurum_Medical_14Oct2025.txt"), 
    delim = "\t", escape_double = FALSE, 
    col_types = cols(MedCodeId = col_character(), 
                     OriginalReadCode = col_character(), 
                     CleansedReadCode = col_character(), 
                     SnomedCTConceptId = col_character(), 
                     SnomedCTDescriptionId = col_character()), 
    trim_ws = TRUE)
cprd_aurum_medical <- cprd_aurum_medical_raw %>%
  select(-Release) %>%
  rename(term = Term, medcodeid = MedCodeId) %>%
  mutate(term = str_to_lower(term))

# Read in Gold medical dictionary
cprd_gold_medical_raw <- 
  read_delim(
    paste0(wd, path_input, "MASTER_Lists/CPRD_GOLD_Medical_14Oct2025.txt"), 
    delim = "\t", escape_double = FALSE, 
    col_types = cols(medcode = col_character(), 
                     readcode = col_character()), 
    trim_ws = TRUE) 
cprd_gold_medical <- cprd_gold_medical_raw %>%
  rename(term = readterm) %>%
  mutate(term = str_to_lower(term))


# Read in old pregnancy code lists, setting all col types to character

# Aurum
pregnancy_aurum_old <- read_delim(
  paste0(wd, path_input, "Pregnancy/Old/Aurum_Pregnancy_codelist_20260304.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character()), trim_ws = TRUE)



# Gold
pregnancy_gold_old <- read_delim(
  paste0(wd, path_input, "Pregnancy/Old/Gold_Pregnancy_codelist_20260304.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(V1 = col_character()), trim_ws = TRUE)

# ================= 2) Search for relevant med codes =======================
# Aurum

aurum_pregnancy <- cprd_aurum_medical %>%
  # Inclusion - Pregnancy related terms
  
  
  # Early pregnancy 
  
  
  filter(grepl(paste0("(?i)hyperemesis|serum pregnancy test positive|urine pregnancy test positive|
pregnant|antenatal",
                      
# Late preg
  "polyhydramnios|oligohydramnios|amniotic|placenta|antepartum",

# Postnatal

"postpartum|postnatal|puerperium|childbirth|labour|delivery|puerperal|postpartum", 

# Unspecified 

"primigravida|pregnancy|obstetric|gestation|pregnant|multigravida|maternal care"),
  term, per = TRUE)) %>%
               
   
  
  
  
  # Exclusion - not pregnancy related 
  filter(!grepl(paste0("(?i)Builders|civil engineer|labourer|fear of pregnancy|delivery of rehabilitation|radiotherapy delivery|
delivery of oral chemotherapy|delivery of a fraction|oral delivery of radiotherapy|
consultant gynaecology and obstetrics|obstetrics & gynaecology|obstetrics and gynaecology|
obstetrics illustrated|medication delivery|at risk of unplanned pregnancy|
not at risk of unplanned pregnancy|adverse reaction to drugs for obstetric,gynaecology, and urinar|
hibitaine obstetric|hibitane obstetric|insulin delivery system|
intrathecal drug delivery|complex chemotherapy|parenteral chemotherapy|
chemotherapy|rehabilitation|paediatric/ obstetrics and gynaecology|forced labour|	
international federation of|bipolar i disorder|fear of childbirth|
ultrasound procedure on female genital system and/or|proton|
 trying to achieve a pregnancy|laboured breathing|labouring activities|
 proton beam|nitric oxide|obstetrical/gynaecological device|fear of|prostate|cannabis hyperemesis|
 increased risk for unplanned",
                       
# Negations 

"pregnancy test negative|pre-pregnancy|non-obstetric|non-puerperal|nonpuerperal|
  unspecified whether in pregnancy|non obstetric|unspec whether during|
  no history of ectopic pregnancy|	
  pregnancy prevention programme|recurrent pregnancy loss|
  pregnancy test negative|pregnancy prevention|pregnancy not yet confirmed|
  no history of|urine pregnancy test negative", "test negative","pregnancy test negative",
 "not pregnant", "non puerperal",
                       
# Related to previous pregnancy
                       
"h/o:|history of|previous surgery in pregnancy|bladder: incontinence due to childbirth|
previous induced terminations|number of miscarriages or induced terminations of pregnancy|
number of induced terminations of pregnancy|past pregnancy history|past pregnancy|
obstetric history",
                       
# Related to family
                       
"fh|family history","fh: obstetric problem", "wife pregnant",
                       
# Process of care
                       
"pregnancy test requested|high sensitivity urine pregnancy test|test request : pregnancy test|
  pregnancy test kit given|pregnancy advice for patients with epilepsy not indicated|
  failed encounter - short message service|test kit|	
  failed encounter - sms|home delivery of urinary catheters|oxygen delivery|
  standard pregnancy test|sensitive pregnancy test|	
  obstetrics and gynaecology sample|obstetrics and gynecology sample|drug delivery|rate of delivery|
  admission to obstetrics and gynaecology department|	
  admission to obstetrics and gynecology department|admission by obstetrician & gynaecologist|
  under care of obstetrician and gynaecologist|seen by obstetrics and gynaecology service|
  seen by obstetrics and gynecology service|referral to obstetrics and gynaecology service|
  referral to obstetrician and gynecologist|discharge by obstetrician and gynecologist|
  discharge by obstetrician and gynaecologist|discharge from obstetrics and gynaecology service|
  discharge from obstetrics and gynecology service|obstetrician and gynaecologist|obstetrician and gynecologist|
  obstetrics and gynaecology department|obstetrics and gynecology department|obstetrics and gynaecology service|
  home pregnancy testing kit|international federation| 
  obstetrics and gynecology|finding related to care delivery|stopped smoking before pregnancy|
  obstetrics and gynaecology clinic|obstetrics and gynecology clinic|obstetrical/gynaecological device|
  obstetrics and gynaecology discharge summary|obstetrics and gynecology discharge summary|prior to radiation|
  drug delivery device|admission by obstetrician and gynaecologist|insulin delivery"),
                term, perl = TRUE))    %>%
  
  
  # Remove family history codes 
  filter(!grepl("(?i)(fh: obstetric problem|	
fh: raised b.p. in pregnancy|fh: diabetes in pregnancy|fh: puerperal depression|
fh: obstetric problem nos|fh: multiple pregnancy|	
fh: twin pregnancy|fh: raised b.p. in pregnancy|fh: twin pregnancy|family history:)", term, perl = TRUE)) %>%
  
  
  # Remove history of codes 
  filter(!grepl("(?i)(h/o:|h/o|obstetric history)", term, perl = TRUE)) %>%
  
  
  # Remove negative pregnancy test / test request codes / not pregnant code
  
  filter(!grepl("(?i)(pregnancy test negative|pregnancy test requested|b-hcg|beta|patient advised)",
                term, perl = TRUE)) %>%
  
  filter(!grepl("^urine pregnancy test(?! positive)|^pregnancy test(?! positive)",
                term, perl = TRUE)) %>%
  
  # Remove codes for NOT pregnant, NON PUERPERAL and WIFE/PARTNER and TRYING TO GET pregnant 
  
  filter(!grepl("not[-_ ]*pregnant", term, ignore.case = TRUE, perl =  TRUE)) %>%

  filter(!grepl("wife[-_ ]*pregnant", term, ignore.case = TRUE, perl =  TRUE)) %>%

filter(!grepl("partner[-_ ]*pregnant", term, ignore.case = TRUE, perl =  TRUE)) %>%

filter(!grepl("trying to get[-_ ]*pregnant", term, ignore.case = TRUE, perl =  TRUE)) %>%


filter(!grepl("non-[-_ ]*pregnant", term, ignore.case = TRUE, perl =  TRUE))  %>%

  
filter(!grepl("non[-_ ]*puerperal", term, ignore.case = TRUE, perl =  TRUE))

#### Gold

gold_pregnancy <- cprd_gold_medical %>%
  # Inclusion - Pregnancy related terms
  filter(grepl(paste0("(?i)pregnancy|obstetric|hyperemesis|antenatal|gestation|
  pregnant|maternal care|polyhydramnios|oligohydramnios|
  amniotic|placenta|antepartum|labour|postpartum|delivery|puerperal|
  puerperium|postpartum|childbirth|primigravida|multigravida|postnatal|serum pregnancy test positive|urine pregnancy test positive"),
               term)) %>% 
  
  
  # Exclusion - not pregnancy related 
  filter(!grepl(paste0("(?i)Builders|civil engineer|labourer|fear of pregnancy|delivery of rehabilitation|radiotherapy delivery|
delivery of oral chemotherapy|delivery of a fraction|oral delivery of radiotherapy|
consultant gynaecology and obstetrics|obstetrics & gynaecology|obstetrics and gynaecology|
obstetrics illustrated|medication delivery|at risk of unplanned pregnancy|
not at risk of unplanned pregnancy|adverse reaction to drugs for obstetric,gynaecology, and urinar|
hibitaine obstetric|hibitane obstetric|insulin delivery system|
intrathecal drug delivery|complex chemotherapy|parenteral chemotherapy|
chemotherapy|rehabilitation|paediatric/ obstetrics and gynaecology|forced labour|	
international federation of|bipolar i disorder|fear of childbirth|
ultrasound procedure on female genital system and/or|proton|
 trying to achieve a pregnancy|laboured breathing|labouring activities|
 proton beam|nitric oxide|obstetrical/gynaecological device|fear of|prostate|cannabis hyperemesis|
 increased risk for unplanned",
                       
                       # Negations 
                       "pregnancy test negative|pre-pregnancy|non-obstetric|non-puerperal|nonpuerperal|
  unspecified whether in pregnancy|non obstetric|unspec whether during|
  no history of ectopic pregnancy|	
  pregnancy prevention programme|recurrent pregnancy loss|not pregnant|
  pregnancy test negative|pregnancy prevention|pregnancy not yet confirmed|
  no history of|urine pregnancy test negative", "test negative","pregnancy test negative",
                       
                       # Related to previous pregnancy
                       
                       "h/o:|history of|previous surgery in pregnancy|bladder: incontinence due to childbirth|
  previous induced terminations|number of miscarriages or induced terminations of pregnancy|
  number of induced terminations of pregnancy|past pregnancy history|past pregnancy|
obstetric history",
                       
                       # Related to family
                       
                       "fh|family history","fh: obstetric problem",
                       
                       # Process of care
                       
                       "pregnancy test requested|high sensitivity urine pregnancy test|test request : pregnancy test|
  pregnancy test kit given|pregnancy advice for patients with epilepsy not indicated|
  failed encounter - short message service|test kit|	
  failed encounter - sms|home delivery of urinary catheters|oxygen delivery|
  standard pregnancy test|sensitive pregnancy test|	
  obstetrics and gynaecology sample|obstetrics and gynecology sample|drug delivery|rate of delivery|
  admission to obstetrics and gynaecology department|	
  admission to obstetrics and gynecology department|admission by obstetrician & gynaecologist|
  under care of obstetrician and gynaecologist|seen by obstetrics and gynaecology service|
  seen by obstetrics and gynecology service|referral to obstetrics and gynaecology service|
  referral to obstetrician and gynecologist|discharge by obstetrician and gynecologist|
  discharge by obstetrician and gynaecologist|discharge from obstetrics and gynaecology service|
  discharge from obstetrics and gynecology service|obstetrician and gynaecologist|obstetrician and gynecologist|
  obstetrics and gynaecology department|obstetrics and gynecology department|obstetrics and gynaecology service|
  home pregnancy testing kit|international federation| 
  obstetrics and gynecology|finding related to care delivery|stopped smoking before pregnancy|
  obstetrics and gynaecology clinic|obstetrics and gynecology clinic|obstetrical/gynaecological device|
  obstetrics and gynaecology discharge summary|obstetrics and gynecology discharge summary|prior to radiation|
  drug delivery device|admission by obstetrician and gynaecologist|insulin delivery"),
                term, perl = TRUE))    %>%
  
  
  # Remove family history codes 
  filter(!grepl("(?i)(fh: obstetric problem|	
fh: raised b.p. in pregnancy|fh: diabetes in pregnancy|fh: puerperal depression|
fh: obstetric problem nos|fh: multiple pregnancy|	
fh: twin pregnancy|fh: raised b.p. in pregnancy|fh: twin pregnancy|family history:)", term, perl = TRUE)) %>%
  
  
  # Remove history of codes 
  filter(!grepl("(?i)(h/o:|h/o|obstetric history)", term, perl = TRUE)) %>%
  
  
  # Remove negative pregnancy test / test request codes 
  
  filter(!grepl("(?i)(pregnancy test negative|pregnancy test requested|b-hcg|standard|beta|patient advised)",
                term, perl = TRUE)) %>%
  
  filter(!grepl("^urine pregnancy test(?! positive)|^pregnancy test(?! positive)",
                term, perl = TRUE))

###

## Comparing with older codelists

# New codes not in old list
new_aurum <- aurum_pregnancy %>%
  filter(!medcodeid %in% pregnancy_aurum_old$medcodeid)

new_gold <- gold_pregnancy %>%
  filter(!medcode %in% pregnancy_gold_old$medcode)

# Old codes not in new old list

miss_new_aurum <- pregnancy_aurum_old %>%
  filter(!medcodeid %in% aurum_pregnancy$medcodeid)


miss_new_gold <- pregnancy_gold_old %>%
  filter(!medcode %in% gold_pregnancy$medcode)

# ================= 3) Create updated code lists ===============================

# No previous code lists for comparison 

# Save updated code lists

write.table(aurum_pregnancy,
            file = paste0(wd, path_output, "Aurum_Pregnancy_codelist_20260304.txt"),
            sep = "\t", row.names = FALSE)

write.table(gold_pregnancy,
            file = paste0(wd, path_output, "Gold_Pregnancy_codelist_20260304.txt"),
            sep = "\t", row.names = FALSE)

# Combine Aurum and GOLD updated code lists
temp_aurum <- aurum_pregnancy %>%
  rename(medcode = medcodeid, readcode = CleansedReadCode) %>%
  select(medcode, term)
temp_gold <- gold_pregnancy %>%
  select(medcode, term)
temp_both <- rbind(temp_aurum, temp_gold)
aurum_gold_pregnancy <- temp_both %>% distinct()


# Save lists of new combined codelist into one .xlsx file



write_xlsx(aurum_gold_pregnancy,
           file = paste0(wd, path_output, "Aurum_Gold_Pregnancy_codelist_20260304.xlsx"))


# # Combine Aurum and GOLD into one file with a column specifying database


aurum_pregnancy$database <- "Aurum"
gold_pregnancy$database <- "Gold"
aurum_gold_pregnancy <- rbind(
  aurum_pregnancy %>% 
    rename(medcode = medcodeid) %>%
    select(medcode, term, database), 
  gold_pregnancy %>%
    select(medcode, term, database))

# # Save combined code list
write.table(aurum_gold_pregnancy,
            file = paste0(wd, path_output, "Aurum_Gold_Pregnancy_codelist_20260304.txt"),
            sep = "\t", row.names = FALSE)  
