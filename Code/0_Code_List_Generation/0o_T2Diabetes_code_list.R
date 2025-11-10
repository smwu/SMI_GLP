# ==============================================================================
# Generate code lists for Type 2 Diabetes Mellitus (T2DM) diagnoses
# Author: SM Wu
# Date Created: 2025/08/11
# Date Updated: 2025/10/27
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
# 3) Code_Lists/T2Diabetes/Old/Aurum_T2Diabetes_codelist_20240227_Alvin.txt: Old Aurum T2DM code list
# 4) Code_Lists/T2Diabetes/Old/Gold_T2Diabetes_codelist_20240227_Alvin.txt: Old GOLD T2DM code list
# 
# Intermediate outputs:
# 1) Code_Lists/T2Diabetes/Aurum_other_codes.csv: Potential codes to add for Aurum T2DM
# 2) Code_Lists/T2Diabetes/Gold_other_codes.csv: Potential codes to add for Gold T2DM
# 
# Final Outputs:
# 1) Code_Lists/T2Diabetes/Aurum_Gold_T2Diabetes_new_codes_20250819.txt: Newly added T2DM codes for Aurum and GOLD
# 2) Code_Lists/T2Diabetes/Aurum_T2Diabetes_codelist_20251027.txt: Updated Aurum T2DM code list
# 3) Code_Lists/T2Diabetes/Gold_T2Diabetes_codelist_20251027.txt: Updated GOLD T2DM code list
# 4) Code_Lists/T2Diabetes/Aurum_Gold_T2Diabetes_codelist_20251027.txt: Updated Aurum and GOLD T2DM code list
# 5) Code_Lists/T2Diabetes/Aurum_T2Diabetes_codelist_medcode_20251027.txt: Aurum T2DM comma-separated medcodes only
# 6) Code_Lists/T2Diabetes/Aurum_T2Diabetes_codelist_processed_20251027.txt: Aurum T2DM reformatted for CPRD extraction
# 7) Code_Lists/T2Diabetes/Gold_T2Diabetes_codelist_medcode_20251027.txt: GOLD T2DM comma-separated medcodes only
# 8) Code_Lists/T2Diabetes/Gold_T2Diabetes_codelist_processed_20251027.txt: GOLD T2DM reformatted for CPRD extraction

# ==============================================================================


# ================= 1) Set up and load data ====================================

# Clear memory
rm(list = ls())

# Packages
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(openxlsx)

# Set working directory
wd <- "/Volumes/ritd-ag-project-rd00qv-jfhay18/Stephanie/SMI_GLP/" # VPN connection
# wd <- "//live.rd.ucl.ac.uk/ritd-ag-project-rd00qv-jfhay18/Stephanie/SMI_GLP/" #Desktop@UCL
setwd(wd)

# Set input and output paths
path_input <- "Code_Lists/"
path_output <- "Code_Lists/T2Diabetes/"


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

# Read in old T2Diabetes code list from 2024/03/28, setting all col types to character
# Aurum
t2dm_codelist_aurum_old <- read_delim(
  paste0(wd, path_input, "T2Diabetes/Old/Aurum_T2Diabetes_codelist_20240227_Alvin.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character()), trim_ws = TRUE)
# Gold
t2dm_codelist_gold_old <- read_delim(
  paste0(wd, path_input, "T2Diabetes/Old/Gold_T2Diabetes_codelist_20240227_Alvin.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcode = col_character()), trim_ws = TRUE)

# Also cross-checked with T2DM code list in Jeffery et al. (2025) BJGP

# ================= 2) Search for new relevant med codes =======================

# Aurum

aurum_t2dm <- cprd_aurum_medical %>%
  # Inclusions: diabetes
  filter(grepl("(?i)diabet|diab mel|t2d|d\\s+m\\s+", 
               term)) %>%
  # Exclusions: family history, other disorders, substance, no symptom, treatment,
  # mild T2DM (if not recurrent)
  filter(!grepl(paste0(
    # Family history
    "(?i)fh:|family history|child|infant|maternal history|family|member|relative|",
    # Maternal, inherited, and pregnancy-related
    "gestational|mother|maternal|newborn|fetus|partum|natal|genetic|", 
    "diabetes mellitus in the puerperium|pueperium|pregnancy|preg.|",
    # Counselling and care
    "absent|administration|programme|education|review|advice|prevention|protocol|",
    "learning|diet|management|diabetic care|diabetes care|care plan|referral|program|",
    "interpretation|medicine|exam|advised|record|service|date|d.v.|clinic|nurse|",
    "diabetologist|pilot|triage|identity card|signposting|leaflet|admission|",
    "contact|therapy|check|health promotion|participant|study|treatment type|",
    "information|visit|side effects|nursing|register|diabetic treatment changed|",
    "diabetic stabilisation|diabetology|admit diabetic emergency|diabetes distress|",
    # Other disorders
    "nephrogenic|dwarfism|malnutrition|malnutrit|due to pancreatic injury|disease|",
    # Other non-related
    "induced|jam|vaccine|erectile|department|cha2ds2|endocrinology|poison|", 
    "injection|adverse|reaction|photography|insulin-glucose infus|photocoagulation|",
    # Pre-diabetic, latent, pre-existing, and history of diabetes
    "high risk of diabetes|pre-diab|prediab|risk|history|latent|suspect|h/o|",
    "pre-existing|",
    # Other types of diabetes
    "iddm-insulin dependent diabetes|^insulin dependent diabetes|type 1|type i diab|",
    "^insulin dependent diab mell|^insulin-dependent|control of insulin-dependent|",
    "unstable insulin dependent diabetes|diabetes mellitus type i|due to insulin|",
    "juvenile|youth|young|anemia|anaemia|brittle diabetes|labile diabetes|", 
    "cystic fibrosis|bronzed diabetes|lipodystrophy|other specified diabetes mellitus|", 
    "insipidus|monogenic|secondary|type 3c|transplant|hyperplasia|ketosis-prone|",
    "caused by chemical|pancreatic diabetes|lipoatrophic|diabetes type$|",
    "atypical diabetes|protein-deficient|maturity onset diabetes|",
    "hormonal aetiology|caused by insulin receptor antibodies|",
    # Diabetes complications
    "due to diabetes mellitus|due to type 2 diabetes mellitus|due to diabet|",
    "cataract|retinopathy|renal|kidney|diabetic derm|urine|associated with diabetes|", 
    "with diabetes mellitus|with type 2 diabetes|with type ii diabetes|",
    "in type 2 diabetes|diabetes-related|in type ii diabetes|in diabetes|",
    "polyneuropathy|diabetic peripheral vascular disease|diabetic cheir|",
    "rubeosis|thick skin|diabetic neurotrophic keratitis|diarrhoea in diabetes|",
    "papillopathy|microvascular anomaly|diabetic nephropathy|diabetic neuropathy|",
    "diabetes complication|diabetic complication|acute complication with diabetes|",
    "diabetes related|diabetic hyperosmolar|diabetic retinal disease|",
    "diabetic autonomic|diabeticorum|pretibial pigmental patches|diabetic hand|",
    "maculopathy|diabetic iritis|diabetic mononeuritis|diabetic foot|diabetic leg|",
    "diabetic glomerulo|diabetic ulcer|gangrene of toe in diabetic|",
    "diabetic severe hyperglycemia|^hyperglyc|diabetic peripheral|",
    "diabetic ketoacidosis|diabetic skin|diabetic ophthalmo|diabetic macular|",
    "diabetic radiculoplexus neuropathy|diabetic oculopathy|diabetic embryopathy|",
    "diabetic mastopathy|diabetic ketoacidosis|diabetic acidosis|",
    "diabetic vitreous hemorrhage|diabetic charcot's arthropathy|",
    "diabetic charcot arthropathy|diabetic severe hyperglycaemia|",
    "diabetic amyotrophy|diabetic mononeuropathy|diabetic retinal|diabetic vitreous|",
    "diabetic wet gangrene|panretinal photocoagulation|diabetic lumbosacral|",
    "diabetic neuropathic arthropathy|diabetic traction retinal detachment|",
    # Measurement without diagnosis
    "score|screen|at risk of diabetes|hba1|haemoglobin a1c|hb. a1c|assessment|",
    "fundoscopy|auras-af|calculator|qdiabetes|special interest in diabetes|",
    "assess|monitor|scale|audit|test|questionnaire|indicator|",
    # Negations
    "no evidence|not indicated|non-diab|deleted|resolved|excluded|no left|",
    "no right|no h/o|nondiab|no diab|remission"),
    term, perl = TRUE))

### Core T2DM definition composed of type-specific T2DM codes
# Only include entries that specify Type 2 Diabetes (exclude those with 
# unspecified diabetes type)
aurum_t2dm_v2 <- aurum_t2dm %>%
  filter(grepl(
    "(?i)\\b(type\\s*2|type\\s*ii|t2dm|non[- ]?insulin[- ]?dependent|niddm)\\b",
    term))

# Which terms were in the old list but are not included in the new list,
# filtering to those terms in the code dictionary
aurum_t2dm_miss_from_new <- t2dm_codelist_aurum_old %>%
  filter(!(medcodeid %in% aurum_t2dm$medcodeid)) %>% 
  filter(medcodeid %in% cprd_aurum_medical$medcodeid)


# # Removing specific terms due to SNOMED mapping issues
# exclusions <- c()
# aurum_t2dm <- aurum_t2dm %>%
#   filter(!(medcodeid %in% exclusions))

# Subset to terms not already included in old T2Diabetes code list
aurum_new_t2dm <- aurum_t2dm %>%
  filter(!(medcodeid %in% t2dm_codelist_aurum_old$medcodeid))


# Gold

gold_t2dm <- cprd_gold_medical %>%
  # Inclusions: diabetes
  filter(grepl("(?i)diabet|diab mel|t2d|d\\s+m\\s+", 
               term)) %>%
  # Exclusions: family history, other disorders, substance, no symptom, treatment,
  # mild T2DM (if not recurrent)
  filter(!grepl(paste0(
    # Family history
    "(?i)fh:|family history|child|infant|maternal history|family|member|relative|",
    # Maternal, inherited, and pregnancy-related
    "gestational|mother|maternal|newborn|fetus|partum|natal|genetic|", 
    "diabetes mellitus in the puerperium|pueperium|pregnancy|preg.|",
    # Counselling and care
    "absent|administration|programme|education|review|advice|prevention|protocol|",
    "learning|diet|management|diabetic care|diabetes care|care plan|referral|program|",
    "interpretation|medicine|exam|advised|record|service|date|d.v.|clinic|nurse|",
    "diabetologist|pilot|triage|identity card|signposting|leaflet|admission|",
    "contact|therapy|check|health promotion|participant|study|treatment type|",
    "information|visit|side effects|nursing|register|diabetic treatment changed|",
    "diabetic stabilisation|diabetology|admit diabetic emergency|diabetes distress|",
    # Other disorders
    "nephrogenic|dwarfism|malnutrition|malnutrit|due to pancreatic injury|disease|",
    # Other non-related
    "induced|jam|vaccine|erectile|department|cha2ds2|endocrinology|poison|", 
    "injection|adverse|reaction|photography|insulin-glucose infus|photocoagulation|",
    # Pre-diabetic, latent, pre-existing, and history of diabetes
    "high risk of diabetes|pre-diab|prediab|risk|history|latent|suspect|h/o|",
    "pre-existing|",
    # Other types of diabetes
    "iddm-insulin dependent diabetes|^insulin dependent diabetes|type 1|type i diab|",
    "^insulin dependent diab mell|^insulin-dependent|control of insulin-dependent|",
    "unstable insulin dependent diabetes|diabetes mellitus type i|due to insulin|",
    "juvenile|youth|young|anemia|anaemia|brittle diabetes|labile diabetes|", 
    "cystic fibrosis|bronzed diabetes|lipodystrophy|other specified diabetes mellitus|", 
    "insipidus|monogenic|secondary|type 3c|transplant|hyperplasia|ketosis-prone|",
    "caused by chemical|pancreatic diabetes|lipoatrophic|diabetes type$|",
    "atypical diabetes|protein-deficient|maturity onset diabetes|",
    "hormonal aetiology|caused by insulin receptor antibodies|",
    # Diabetes complications
    "due to diabetes mellitus|due to type 2 diabetes mellitus|due to diabet|",
    "cataract|retinopathy|renal|kidney|diabetic derm|urine|associated with diabetes|", 
    "with diabetes mellitus|with type 2 diabetes|with type ii diabetes|",
    "in type 2 diabetes|diabetes-related|in type ii diabetes|in diabetes|",
    "polyneuropathy|diabetic peripheral vascular disease|diabetic cheir|",
    "rubeosis|thick skin|diabetic neurotrophic keratitis|diarrhoea in diabetes|",
    "papillopathy|microvascular anomaly|diabetic nephropathy|diabetic neuropathy|",
    "diabetes complication|diabetic complication|acute complication with diabetes|",
    "diabetes related|diabetic hyperosmolar|diabetic retinal disease|",
    "diabetic autonomic|diabeticorum|pretibial pigmental patches|diabetic hand|",
    "maculopathy|diabetic iritis|diabetic mononeuritis|diabetic foot|diabetic leg|",
    "diabetic glomerulo|diabetic ulcer|gangrene of toe in diabetic|",
    "diabetic severe hyperglycemia|^hyperglyc|diabetic peripheral|",
    "diabetic ketoacidosis|diabetic skin|diabetic ophthalmo|diabetic macular|",
    "diabetic radiculoplexus neuropathy|diabetic oculopathy|diabetic embryopathy|",
    "diabetic mastopathy|diabetic ketoacidosis|diabetic acidosis|",
    "diabetic vitreous hemorrhage|diabetic charcot's arthropathy|",
    "diabetic charcot arthropathy|diabetic severe hyperglycaemia|",
    "diabetic amyotrophy|diabetic mononeuropathy|diabetic retinal|diabetic vitreous|",
    "diabetic wet gangrene|panretinal photocoagulation|diabetic lumbosacral|",
    "diabetic neuropathic arthropathy|diabetic traction retinal detachment|",
    # Measurement without diagnosis
    "score|screen|at risk of diabetes|hba1|haemoglobin a1c|hb. a1c|assessment|",
    "fundoscopy|auras-af|calculator|qdiabetes|special interest in diabetes|",
    "assess|monitor|scale|audit|test|questionnaire|indicator|",
    # Negations
    "no evidence|not indicated|non-diab|deleted|resolved|excluded|no left|",
    "no right|no h/o|nondiab|no diab|remission"),
    term, perl = TRUE))

# Which terms were in the old list but are not included in the new list,
# filtering to those terms in the code dictionary
gold_t2dm_miss_from_new <- t2dm_codelist_gold_old %>%
  filter(!(medcode %in% gold_t2dm$medcode)) %>% 
  filter(medcode %in% cprd_gold_medical$medcode)

# # Removing specific terms due to SNOMED mapping issues
# gold_t2dm <- gold_t2dm %>%
#   filter(!(medcode %in% exclusions))

# Subset to terms not already included in old T2Diabetes code list
gold_new_t2dm <- gold_t2dm %>%
  filter(!(medcode %in% t2dm_codelist_gold_old$medcode))


# Combine Aurum and GOLD new T2Diabetes codes
aurum_gold_t2dm_new_codes <- list(
  Aurum = aurum_new_t2dm,
  Gold = gold_new_t2dm)
# # Save lists of newly added codes into one .xlsx file on separate tabs
# write.xlsx(aurum_gold_t2dm_new_codes,
#            file = paste0(wd, path_output, "Aurum_Gold_T2Diabetes_new_codes_29Sept2025.xlsx"),
#            overwrite = TRUE)


# ## Comparing with older codelists
# aurum_t2dm_old <- read_delim(
#   "Code_Lists/T2Diabetes/Old/Aurum_T2Diabetes_codelist_20250929.txt",
#   delim = "\t", col_types = cols(medcodeid = col_character()))
# 
# gold_t2dm_old <- read_delim(
#   "Code_Lists/T2Diabetes/Old/Gold_T2Diabetes_codelist_20250929.txt",
#   delim = "\t", col_types = cols(medcode = col_character()))
# 
# # New codes not in old list
# new_aurum <- aurum_t2dm %>%
#   filter(!medcodeid %in% aurum_t2dm_old$medcodeid)
# new_gold <- gold_t2dm %>%
#   filter(!medcode %in% gold_t2dm_old$medcode)
# 
# # Old codes not in new old list
# miss_new_aurum <- aurum_t2dm_old %>%
#   filter(!medcodeid %in% aurum_t2dm$medcodeid)
# miss_new_gold <- gold_t2dm_old %>%
#   filter(!medcode %in% gold_t2dm$medcode)




### OLDER CODE [DO NOT RUN]

# # Save lists of potential codes to add for Aurum and GOLD
# # These were last reviewed by JH on Jul 23, 2025
# write.csv(aurum_new_t2dm, file = paste0(wd, path_output, "Aurum_Diabetes_other_codes.csv"))
# write.csv(gold_new_t2dm, file = paste0(wd, path_output, "Gold_Diabetes_other_codes.csv"))

# # Any terms in older lists but not in searched terms
# aurum_gold_t2dm_miss_codes <- list(
#   Aurum = aurum_t2dm_miss_from_new,
#   Gold = gold_t2dm_miss_from_new)
# 
# # Save lists of missing codes into one .xlsx file on separate tabs
# # These were last reviewed by JH on Jul 23, 2025
# write.xlsx(aurum_gold_t2dm_miss_codes,
#            file = paste0(wd, path_output, 
#                          "Aurum_Gold_Diabetes_missing_codes_14Jul2025.xlsx"),
#            overwrite = TRUE)


# ================= 3) Create updated code lists ===============================

# Create updated code lists

# Aurum
t2dm_codelist_aurum_new <- aurum_t2dm
# Gold
t2dm_codelist_gold_new <- gold_t2dm

# # Save updated code lists
# write.table(t2dm_codelist_aurum_new,
#             file = paste0(wd, path_output, "Aurum_T2Diabetes_codelist_20251027.txt"),
#             sep = "\t", row.names = FALSE)
# 
# write.table(t2dm_codelist_gold_new,
#             file = paste0(wd, path_output, "Gold_T2Diabetes_codelist_20251027.txt"),
#             sep = "\t", row.names = FALSE)


# Combine Aurum and GOLD updated code lists
temp_aurum <- t2dm_codelist_aurum_new %>%
  rename(medcode = medcodeid, readcode = CleansedReadCode) %>%
  select(medcode, term)
temp_gold <- t2dm_codelist_gold_new %>%
  select(medcode, term)
temp_both <- rbind(temp_aurum, temp_gold)
aurum_gold_t2dm_new <- temp_both %>% distinct()
# # Save lists of new combined codelist into one .xlsx file
# write.xlsx(aurum_gold_t2dm_new,
#            file = paste0(wd, path_output, "Aurum_Gold_T2Diabetes_codelist_20251027.xlsx"),
#            overwrite = TRUE)



# Combine Aurum and GOLD into one file with a column specifying database
t2dm_codelist_aurum_new$database <- "Aurum"
t2dm_codelist_gold_new$database <- "Gold"
t2dm_codelist_aurum_gold_new <- rbind(
  t2dm_codelist_aurum_new %>% 
    rename(medcode = medcodeid) %>%
    select(medcode, term, database), 
  t2dm_codelist_gold_new %>%
    select(medcode, term, database))
# # Save combined code list
# write.table(t2dm_codelist_aurum_gold_new,
#             file = paste0(wd, path_output, "Aurum_Gold_T2Diabetes_codelist_20251027.txt"),
#             sep = "\t", row.names = FALSE)

# ================= 4) Adjust formatting for extraction ========================

## Adjust format of code lists to assist in CPRD data extraction
# The 'medcode' will be a simple file with the medcode ids separated by commas.
# The 'processed' file will contain two columns: one for the medcode ID and 
# one for the term definition

# Aurum
t2dm_codelist_aurum_medcode <- t2dm_codelist_aurum_new$medcodeid |>
  as.character() |>
  trimws() |>
  unique() |>
  na.omit()

# # Write to .txt file with medcodes separated by commas
# write.table(t(t2dm_codelist_aurum_medcode), 
#             paste0(wd, path_output, 
#                    "Aurum_T2Diabetes_codelist_medcode_20251027.txt"), 
#             sep=", ", row.names=FALSE, col.names=FALSE, quote=FALSE)

t2dm_codelist_aurum_processed <- t2dm_codelist_aurum_new %>%
  select(medcodeid, term)

# # Write to .txt file with columns separated by tab
# write.table(t2dm_codelist_aurum_processed,
#             paste0(wd, path_output,
#                    "Aurum_T2Diabetes_codelist_processed_20251027.txt"),
#             sep = "\t", row.names = FALSE,
#             quote=FALSE)


# GOLD
t2dm_codelist_gold_medcode <- t2dm_codelist_gold_new$medcode |>
  as.character() |>
  trimws() |>
  unique() |>
  na.omit()

# # Write to .txt file with medcodes separated by commas
# write.table(t(t2dm_codelist_gold_medcode), 
#             paste0(wd, path_output, 
#                    "Gold_T2Diabetes_codelist_medcode_20251027.txt"), 
#             sep=", ", row.names=FALSE, col.names=FALSE, quote=FALSE)

t2dm_codelist_gold_processed <- t2dm_codelist_gold_new %>%
  select(medcode, term)

# # Write to .txt file with columns separated by tab
# write.table(t2dm_codelist_gold_processed,
#             paste0(wd, path_output,
#                    "Gold_T2Diabetes_codelist_processed_20251027.txt"),
#             sep = "\t", row.names = FALSE,
#             quote=FALSE)

