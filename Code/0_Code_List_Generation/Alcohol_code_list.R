# Generate code list for alcohol  
# Author: S Picton & S Wu
# Date created: 2026/02/06
# Date updated: 2026/03/04

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
# 3) Code_Lists/Alcohol_Misuse/Old/Aurum_Alcohol_Misuse_codelist_Naomi.txt : Old Aurum alcohol code list 
# 4) Code_Lists/Alcohol_Misuse/Old/Gold_Alcohol_Misuse_codelist_Naomi.txt: Old Gold alcohol code list 
#
# Final Outputs:

# 1) Code_Lists/Alcohol_Misuse/Aurum_Alcohol_codelist_20260304.txt : Updated Aurum alcohol code list 
# 2) Code_Lists/Alcohol-Misuse/Gold_Alcohol_codelist_20260304.txt : Updated Gold alcohol code list 
# 3) Code_Lists/Alcohol-Misuse/Aurum_Gold_Alcohol_misuse_codelist_20260304.txt : Combined Aurum & Gold alcohol code list

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
# path_output <- "Code_Lists/Alcohol/"


### For running in Data Safe Haven
# Set working directory
wd <- "S:/CDSTP_CPRD_25_005368/" 
setwd(wd)

# Set input and output paths
path_input <- "SMI_GLP/Code_Lists/"
path_output <- "SMI_GLP/Code_Lists/Alcohol_Misuse/"



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

# Read in old Alcohol Misuse code lists, setting all col types to character

# Aurum
alcohol_misuse_aurum_old <- read_delim(
  paste0(wd, path_input, "Alcohol_Misuse/Old/Aurum_Alcohol_Misuse_codelist_Naomi.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(medcodeid = col_character()), trim_ws = TRUE)



# Gold
alcohol_misuse_gold_old <- read_delim(
  paste0(wd, path_input, "Alcohol_Misuse/Old/Gold_Alcohol_Misuse_codelist_Naomi.txt"),
  delim = "\t", escape_double = FALSE, 
  col_types = cols(V1 = col_character()), trim_ws = TRUE)


### Old gold column is not called medcode

alcohol_misuse_gold_old <- read_delim("Alcohol_Misuse/Old/Gold_Alcohol_Misuse_codelist_Naomi.txt")
names(alcohol_misuse_gold_old)
# Medcode appears to be V1 - no column titles 


# ================= 2) Search for new relevant med codes =======================

# Aurum

aurum_alcohol_misuse <- cprd_aurum_medical %>%
  # Inclusion - Alcohol misuse related terms 
  filter(grepl(paste0("(?i)alcohol|heavy drinker|moderate drinker|very heavy drinker|alcohol use|alcoholism|alcohol disorder|alcohol consumption|alcohol abuse|
  alcoholic|alc misuse|drinks in morning|feels should cut down drinking|binge drinker|harmful alcohol use|
            risk drinking|hazardous alcohol"),
  term)) %>%
  

  # Exclusion     
  filter(!grepl(paste0(
    # Alcohol codes not related to misuse 
  "(?i)alcohol use|alcohol consumption|alcohol consumption nos|
    alcohol intake|alcohol units consumed per week|drinks alcohol|current drinker of alcohol|alcohol user|alcoholic beverage|alcoholic drink|social drinker|light drinker - 1-2u/day|
  	ex-trivial drinker <1u/day|ex-light drinker",


# Codes relating to household
"alcoholic spouse|alcoholic spouse|alcoholic offspring|alcoholic in the family|husband alcoholic|family history of alcoholism|
  carer of a person with alcohol misuse|fh - alcoholism|	
  family history of alcohol misuse|cpu risk factor parent/carer - alcohol abuse|child witness to adult alcohol abuse|	
  witness to adult alcohol misuse|alcohol misuser in household|paternal alcohol abuse|alcoholic parent|alcoholism in family|
  no history of alcohol use in household|alcohol abuse by mother|alcoholic husband|
  alcoholic spouse|alcoholic parent|carer of a person with alcohol misuse|witness|carer|family history of alcohol misuse|cpu",
  
  
  
  # Not related to alcohol misuse (non alcoholic liver disease, allergy to alcohol, adverse reactions to alcohols
#accidental poisoning)
  "alcohol consumption within recommended sensible limits|non-alcoholic fatty liver|other non-alcoholic chronic liver disease nos|accidental poisoning by alcohol, nec|accidental poisoning by alcoholic beverages|
accidental poisoning by other ethyl alcohol and its products|accidental poisoning by denatured alcohol|
accidental poisoning by grain alcohol nos|accidental poisoning by ethyl alcohol nos|
accidental poisoning by methyl alcohol|accidental poisoning by wood alcohol|accidental poisoning by methyl alcohol nos|
accidental poisoning by isopropyl alcohol|accidental poisoning by rubbing alcohol substitute|accidental poisoning by secondary propyl alcohol|accidental poisoning by isopropyl alcohol nos|accidental poisoning by other alcohols|
accidental poisoning by alcohol|laennec's cirrhosis, non-alcoholic|non-alcoholic cirrhosis nos|stereotactic injection of alcohol to gasserian ganglion|pain in lymph nodes after alcohol consumption|non-alcoholic amnestic syndrome|
alcohol deterrent poisoning|nonalcoholic steatohepatitis|ho/rts - police: venesection alcohol|police:venesect-alcohol|
ho/rts-police:venesect alcohol|cirrhosis - non-alcoholic|acc pois/expos alcohol indust/construct area|acc poison/expos alcohol school/pub admin area|accid pois/expos alcohol in sport/athletic area|accid pois/expos to alcohol other spec place|accid poison/expos alcohol in street/highway|accid poison/expos alcohol trade/service area|
accid poison/expos to alcohol at res institut|accid poison/expos to alcohol unspecif place|
accident poison/exposure to alcohol at home|accident poison/exposure to alcohol on farm|
accident poisoning/exposure to alcohol|acute / subacute confusional state, nonalcoholic|delirium, not induced by alcohol+other psychoactive subs|korsakov's psychosis, nonalcoholic|poisoning by and exposure to alcohol, occurrence at sports and athletics area, undetermined intent|poisoning by and exposure to alcohol, occurrence at school, other institution and public administrative area, undetermined intent|
poisoning by and exposure to alcohol, occurrence at other specified place, undetermined intent|
poisoning by and exposure to alcohol, occurrence on street and highway, undetermined intent|
poisoning by and exposure to alcohol, occurrence at trade and service area, undetermined intent|
poisoning by and exposure to alcohol, occurrence in residential institution, undetermined intent|
poisoning by and exposure to alcohol, occurrence at unspecified place, undetermined intent|
poisoning by and exposure to alcohol, occurrence at home, undetermined intent|
poisoning by and exposure to alcohol, occurrence on farm, undetermined intent|
poisoning by and exposure to alcohol, occurrence at industrial and construction area, undetermined intent|
poisoning by and exposure to alcohol, undetermined intent|korsakoff's syndrome - non-alcoholic|
accident poisoning - alcohol|accident poison- ethyl alcohol|
accident poison-methyl alcohol|accident poison - alcohol nos|
adverse reaction to nicotinyl alcohol|adverse reaction to polyvinyl alcohol|adverse reaction to alcoholic coal tar extract|adverse reaction to wool alcohols|adverse reaction to liquid paraffin and acetylated wool alcohols|
adverse reaction to lanolin alcohol|adverse reaction to oleyl alcohol|adverse reaction to oleyl alcohol|
adverse reaction to phenethyl alcohol|adverse reaction to 2,4-dichlorobenzyl alcohol|
adverse reaction to alcoholic coal tar extract|adverse reaction to benzyl alcohol|adverse reaction to cetostearyl alcohol|
adverse reaction to cetyl alcohol|adverse reaction to cetyl alcohol-coal tar distillate|adverse reaction to acetylated wool alcohols|adverse reaction to alcohol|adverse reaction to alcohol 90 %|
adverse reaction to wool alcohols ointment|adverse reaction to polyvinyl alcohol liquifilm|
adverse reaction to stearyl alcohol|adverse reaction to dichlorobenzyl alcohol and amylmetacresol|
adverse reaction to dichlorobenzyl alcohol|adverse reaction to isopropyl alcohol and chlorhexidine gluconate|
hypertension, abnormal renal/liver function, stroke, bleeding history or predisposition, labile international normalized ratio, elderly over 65, and drugs/alcohol concomitantly bleeding risk score|
non-alcoholic fatty liver disease nafld fibrosis score|psychiatry/ toxicology/ drug and alcohol primary group|
non-alcoholic korsakoff's psychosis|korsakov's syndrome - non-alcoholic|benzyl alcohol|spinal injection of alcohol|
drug interaction with alcohol|alcohol interaction with drug|alcohol intolerance|nafld - nonalcoholic fatty liver disease|
accidental poisoning by alcoholic beverage|restricted alcohol diet|alcohol-free diet|
accidental exposure to alcohol|acid and alcohol-fast bacillus|aafb - acid and alcohol-fast bacillus|methyl alcohol|
isopropyl alcohol|cirrhosis of liver not due to alcohol|warning. causes drowsiness which may continue the next day. if affected do not drive or operate machinery. avoid alcoholic drink|
ethyl alcohol|etoh - alcohol|warning. avoid alcoholic drink|allergy to alcohol|alcohol septal ablation|
nash - nonalcoholic steatohepatitis|drinks alcoholic cider|non-alcoholic fatty liver disease without non-alcoholic steatohepatitis|nafld non-alcoholic fatty liver disease fibrosis score|
nafld non-alcoholic fatty liver disease fibrosis score|
allergy to wool alcohol|adverse reaction to alcohol|
non-alcoholic fatty liver disease|
accidental poisoning by substance with alcohol structure|
adverse reaction to alcoholic beverage|accidental poisoning|accidental poisoning by alcohol|excessive coffee drinker|
chronic nonalcoholic liver disease|accident|alcohol-induced flushing|
  ingestible alcohol|drinking alcohol|low alcohol beer|low alcohol lager|high alcohol lager|etoh - low alcohol wine|
  accidental poisoning with ethyl alcohol|alcohol products adverse reaction|
  alcohol metabolism modifier allergy|isopropyl|chronic nonalcoholic liver disease|wood|butyl|propyl|amyl|rubbing|
 accid|adverse reaction to|primary group|drug interaction with alcohol|denatured|not induced|
cooking oils|cooking|burning",




#Negations
"does not misuse|alcohol consumption unknown|non-drinker alcohol|
  denies alcohol abuse|non drinker alcohol|reduced alcohol intake|
  does not drink alcohol|non - drinker alcohol|current non-drinker of alcohol|
  alcohol intake within recommended daily limit|denies alcohol use|
  decreased alcohol consumption|
  declined to provide information about alcohol use|
  does not misuse alcohol|lifetime non-drinker of alcohol|no alcohol abuse|no misuse of alcohol|light drinker|
  ex-light drinker|ex-trivial drinker|trivial drinker - <1u/day|	
  non - drinker|not induced by alcohol+other|occasional|grade a none drinker or rare|
misuse free - alcohol free|free - alcohol free|not misuse|
non-drinker for personal reasons|lifetime non-drinker|drinks rarely",
"non - drinker", "alcohol free",



  
  # Related to pregnancy
  "maternal alcohol abuse|foetal| 	
pregnancy alcohol advice|pregnancy alcohol education|maternal care for suspected damage to fetus from alcohol|
fetal alcohol syndrome|fetus and newborn affected by maternal use of alcohol|suspected foetal damage from maternal alcohol|fetal alcohol syndrome|fetus or neonate affected by placental or breast transfer of alcohol|
pregnancy alcohol advice|foetal alcohol syndrome|fetal or neonatal effect of maternal use of alcohol|
fetal or neonatal effect of placental or breast transfer of alcohol|alcohol consumption during pregnancy|
suspected foetal damage from maternal alcohol addiction|suspected fetal damage from maternal alcohol addiction|
foetal alcohol spectrum disorder|fetal alcohol spectrum disorder|
fetal exposure to alcohol|discussion about maternal wellbeing - alcohol|
foetal or neonatal effect of maternal use of alcohol|fetal alcohol spectrum disorder fasd without sentinel facial features|fetal alcohol spectrum disorder fasd with sentinel facial features|
fetal disorder caused by maternal alcohol dependence in pregnancy|
at increased risk of fasd fetal alcohol spectrum disorder|maternal alcohol abuse|
damage to fetus from alcohol|maternal|maternal care for damage to fetus from alcohol|
fetal or neonatal effect of placental or breast transfer of alcohol|
foetal alcohol spectrum disorder|pregnancy alcohol|placental|breast",



# Process of care / education about alcohol / blood tests / screening tools 
"alcohol leaflet given|alcohol use disorders identification test score|severity of alcohol dependence questionnaire score|
  sadq - severity of alcohol dependence questionnaire|blood-alcohol and blood-drug test|screening for alcohol abuse|
  medicolegal blood alcohol test|alcohol questionnaire completed|alcohol consumption counselling|alcohol counselling by other agencies|alcohol consumption screening test declined|education about alcohol consumption|alcohol consumption screen|
  alcohol consumption screening|alcohol screen - fast alcohol screening test completed|fast fast alcohol screening test score|single alcohol screening questionnaire|alcohol assessment declined|	
  declined referral to specialist alcohol treatment service|number of alcohol units consumed on heaviest drinking day|
  health education - alcohol|alcohol audit score|other alcohol related information|heart beat alcohol consumption|
  health ed. - alcohol|advice relating to alcohol consumption|alcohol consumption in the week before custody|
  seminars in alcohol & drug misuse|responding to drug & alcohol problems in the community|breath alcohol level|
  alcohol screen - alcohol use disorder identification test completed|alcohol screen - alcohol use disorder identification test consumption questions completed|alcohol screen - alcohol use disorder identification test piccinelli consumption questions completed|alcohol use disorders identification test - piccinelli consumption score|audit-c alcohol use disorders identification test - consumption score|alcohol assesment declined - enhanced services admin|advised to abstain from alcohol consumption|referral to community alcohol team declined|epworth score - sitting quietly after lunch without alcohol|audit score - frequency of drinking alcohol|audit score - units of alcohol drunk on a typical day|
  audit score - freq needs morning alcoholic drink in last year|audit-c score - frequency of drinking alcohol|
  audit-c score - units of alcohol drunk on a typical day|honosca-cr health of the nation outcome scales for children and adolescents - clinician-rated scale 4 score - alcohol, substance/solvent misuse|clinical institute withdrawal of alcohol scale, revised score|addiction research foundation clinical institute withdrawal assessment for alcohol|
  alcohol problems questionnaire|keele enhance trial - anxiety/depression verbal advice - alcohol|keele enhance trial- anxiety/depression written advice - alcohol|alcohol units consumed in last 48 hours|declines to state current alcohol consumption|alcohol use disorders identification test declined|family wellbeing discussion about alcohol|alcohol measurement, breath|audit-pc alcohol use disorders identification test - primary care score|
  alcohol measurement|alcohol level|alcohol intake|ai - alcohol intake|etoh - alcohol intake|
  alcoholic drink intake|blood alcohol level|recommended alcohol intake|alcohol drinking behaviour|alcohol drinking behavior|pattern of alcohol consumption through week|drinks alcohol evenly through week|drinks alcohol unevenly through week|total time drunk alcohol|alcohol units|alcohol units/day|alcohol units/week|
  alcohol dependence scale|audit - alcohol use disorders identification test|alcohol use inventory|
  michigan alcoholism screening test|brief michigan alcoholism screening test|short alcohol dependence data|
  health of the nation outcome scale item 3 - alcohol/drug problem|
  % energy intake from alcohol|details of alcohol drinking behaviour|alcohol intake - finding|
  finding of alcohol intake|alcohol abuse prevention assessment|alcohol abuse prevention education|
  alcohol consumption counseling|alcohol withdrawal scale|
  self-monitoring of alcohol intake|measurement of alcohol in blood specimen|
  audit alcohol use disorders identification test score|number of alcohol units consumed on typical drinking day|
  sadq severity of alcohol dependence questionnaire score|alcohol withdrawal scale score|
  addiction research foundation ciwa-a clinical institute withdrawal assessment for alcohol|
  addiction research foundation clinical institute withdrawal assessment for alcohol score|
  assessment using alcohol use disorders identification test|
  has-bled hypertension, abnormal renal and/or liver function, stroke, bleeding history or predisposition, labile inr international normalised ratio, elderly over 65, and drugs and/or alcohol concomitantly score|
  hypertension, abnormal renal/liver function, stroke, bleeding history or predisposition, labile international normalized ratio, elderly over 65, and drugs/alcohol concomitantly score|
  has-bled hypertension, abnormal renal and/or liver function, stroke, bleeding history or predisposition, labile international normalised ratio, elderly over 65, and drugs and/or alcohol concomitantly bleeding risk score|
  hypertension, abnormal renal/liver function, stroke, bleeding history or predisposition, labile international normalised ratio, elderly over 65, and drugs/alcohol concomitantly bleeding risk score|
  exposure to alcohol|assessment of alcohol use|
  home office road traffic statistics - police venesection alcohol|ho/rts-police:venesect alcohol|
  alcohol blood elevated|
  assessment using clinical institute withdrawal assessment of alcohol scale, revised|
  assessment using ciwa-ar clinical institute withdrawal assessment of alcohol scale, revised|
  current view provisional problem description item 13 score - drug and alcohol difficulties|
  honosca-sr health of the nation outcome scales for children and adolescents - self-rated scale 4 score - alcohol, substance/solvent misuse|
  honosca health of the nation outcome scales for children and adolescents - parent's assessment scale 4 score - alcohol, substance/solvent misuse|
emergency care drug or alcohol use related to injury simple reference set|alcohol units consumed per day|
breath alcohol concentration|assessment using audit-c alcohol use disorders identification test - consumption|
assessment using fast fast alcohol screening test|
fast - fast alcohol screening test|
alcohol units per week|
alcohol use disorder identification test piccinelli consumption questionnaire|
alcohol use disorders identification test|
alcohol use disorder identification test consumption questionnaire|
sadq - severity of alcohol dependence questionnaire|
severity of alcohol dependence questionnaire|
ciwa-ar - clinical institute withdrawal assessment for alcohol scale, revised|
alcohol use disorders identification test - consumption score|
assessment using audit-c alcohol use disorders identification test - consumption|
assessment using single alcohol screening question|
audit-c alcohol use disorders identification test - consumption score|
adapted assist-lite alcohol, smoking and substance involvement screening test lite|
provision of information about alcohol use|
blood-alcohol and blood-drug test|alcohol use disorders identification test for primary care score|
alcohol use education declined|education about alcohol consumption declined|
estimated volume of intake of alcoholic cider in 24 hours|alcohol intake in 1 week|
alcoholic beverage intake|alcoholic drink intake|spirit drinker|beer drinker|sadq - severity of alcohol dependence questionnaire|
alcohol leaflet given|audit-c score|honosca-cr|	
health education|fast fast alcohol screening test score|health education - alcohol|health ed. - alcohol|
seminars|hypertension, abnormal renal/liver function, stroke, bleeding history or predisposition, labile international normalized ratio, elderly over 65, and drugs/alcohol concomitantly bleeding risk score|
alcohol measurement|michigan|screening question|health of the nation|energy intake|sadq|ciwa-a|
has-bled*|questionnaire|revised|res institut|adapted|hypoglycaemia|hypoglycemia|fast alcohol screening test|cycle of change|renal|
hypertension, abnormal renal/liver function, stroke, bleeding history or predisposition, labile international normalized ratio, elderly over 65, and drugs/alcohol concomitantly score|
alcohol test"),
term, perl = TRUE))



# Gold

gold_alcohol_misuse <- cprd_gold_medical %>%
  # Inclusion - Alcohol misuse related terms,  
  filter(grepl(paste0("(?i)alcohol misuse|heavy drinker|moderate drinker|very heavy drinker|alcoholism|alcohol disorder|alcohol abuse|
                 alcoholic|alcohol|binge|feels should cut down|nondependent alcohol abuse, unspecified|
                      drinks in morning|higher risk drinking"), 
               term)) %>%

  # Exclusion - Not related to alcohol misuse  
  filter(!grepl(paste0(
    
    # Not related to alcohol misuse
   "(?i)accident|denatured|unspecif|street/highway|wood|poisoning|methyl|
      sport/athletic|res institut|rubbing|indust/construct|non-alcoholic chronic liver disease|
      school/pub|accidental poisoning|lymph nodes|isopropyl|butyl|spec place|at home|amyl|
      propyl|non-alcoholic fatty liver|nonalcoholic steatohepatitis|alcohol consumption|
   alcohol consumption|gasserian ganglion|korsakov's psychosis, nonalcoholic|sport/athletic|
  expos alcohol school/pub admin area|int self poison|admin area",



# Related to household
"carer|fh:|family wellbeing|family history|in household|offspring|in the family|
      husband|spouse|witness|in family|husband alcoholic|	
carer of a person|carer of a person with alcohol misuse|of a person",


# Process of care
"screening for|screening test|health ed. - alcohol|lifestyle advice|michigan|medicolegal|
      injection|screen|audit c|counselling|questionnaire|advice|scale|advised|
      test|breath alcohol level|fast alcohol screening test|leaflet|sadq|blood-drug|
      sadd|bmast|inventory|munich|honos|clinical institute|ciwa|alcohol units per week|
      police:venesect-alcohol|single alcohol|honosca|cycle of change stage|police:venesect-alcohol|
      alcohol use disorder identificatn test consumptn questionnre|
alcohol blood level excessive|identificatn|blood level excessive|alcohol units consumed on heaviest drinking day|
audit - alcohol use disorders identification test|alcohol use disorders identification test|audit|
sadd|data|short|blood alcohol test",




# Related to pregnancy / foetus 
"fetal|newborn|maternal|pregnancy|fetus/neonate|	
fetal alcohol syndrome|fetal alcohol syndrome|fetal alcohol syndrome",

# Negations
"non-alcoholic|alcohol intake within recommended sensible limits|
      declined|denied|non-drinker|declines|alcohol consumption unknown|
      non drinker alcohol|alcohol assessment declined|
      alcohol use disorders identification test declined|removal of alcohol|
declined referral|consumptn declined|referral to community alcohol team declined"),
term)) 

## Comparing with older codelists

# New codes not in old list
new_aurum <- aurum_alcohol_misuse %>%
  filter(!medcodeid %in% alcohol_misuse_aurum_old$medcodeid)

new_gold <- gold_alcohol_misuse %>%
  filter(!medcode %in% alcohol_misuse_gold_old$V1)

# Old codes not in new old list
miss_new_aurum <- alcohol_misuse_aurum_old %>%
  filter(!medcodeid %in% aurum_alcohol_misuse$medcodeid)



miss_new_gold <- alcohol_misuse_gold_old %>%
  filter(!V1 %in% gold_alcohol_misuse$medcode)

## # ================= 3) Create updated code lists ===============================

# Create updated code lists

# Aurum
alcohol_misuse_codelist_aurum_new <- aurum_alcohol_misuse

# Gold
alcohol_misuse_codelist_gold_new <- gold_alcohol_misuse

# Save updated code lists

write.table(alcohol_misuse_codelist_aurum_new,
            file = paste0(wd, path_output, "Aurum_Alcohol_misuse_codelist_20260304.txt"),
            sep = "\t", row.names = FALSE)

write.table(alcohol_misuse_codelist_gold_new,
            file = paste0(wd, path_output, "Gold_Alcohol_misuse_codelist_20260304.txt"),
            sep = "\t", row.names = FALSE)

# Combine Aurum and GOLD updated code lists
temp_aurum <- alcohol_misuse_codelist_aurum_new %>%
  rename(medcode = medcodeid, readcode = CleansedReadCode) %>%
  select(medcode, term)
temp_gold <- alcohol_misuse_codelist_gold_new %>%
  select(medcode, term)
temp_both <- rbind(temp_aurum, temp_gold)
aurum_gold_alcohol_misuse_new <- temp_both %>% distinct()


# Save lists of new combined codelist into one .xlsx file

library(writexl)

write_xlsx(aurum_gold_alcohol_misuse_new,
           file = paste0(wd, path_output, "Aurum_Gold_Alcohol_misuse_codelist_20260304.xlsx")
           
# # Combine Aurum and GOLD into one file with a column specifying database
alcohol_misuse_codelist_aurum_new$database <- "Aurum"
alcohol_misuse_codelist_gold_new$database <- "Gold"
alcohol_misuse_codelist_aurum_gold_new <- rbind(
alcohol_misuse_codelist_aurum_new %>% 
     rename(medcode = medcodeid) %>%
      select(medcode, term, database), 
   alcohol_misuse_codelist_gold_new %>%
select(medcode, term, database))
# 
# # Save combined code list
 write.table(alcohol_misuse_codelist_aurum_gold_new,
            file = paste0(wd, path_output, "Aurum_Gold_Alcohol_misuse_codelist_20260304.txt"),
             sep = "\t", row.names = FALSE)
