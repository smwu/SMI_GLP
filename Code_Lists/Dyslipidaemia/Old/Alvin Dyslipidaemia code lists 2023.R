# --------------
# Generate dyslipidaemia code lists
# CPRD 2023 data
# --------------

# Last run: 24/07/23

# Clear memory
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
dyslipidcodes_g <- CPRDGoldMedical %>%
  filter(grepl("(?i)cholest|lipid|trigl|lipoprotein|glycerid|xanthoma", readterm)) %>%
  filter(!grepl("(?i)tubular|cerebral|granuloma|triglycerides normal|lipids normal|cholesterol normal|electroph. normal|FH|plasma lipids|genotyping|level|Retinal|cholesteatoma|ratio|storage|
                |cholesteatosis|cholestyramine|calculus|Lipoprotein electrophoresis|electroph. -|electroph. NOS|apheresis|possible|Phospholipid|supplementation|Apolipoprotein|Fibroxanthoma|Lipidoses|liver|
                |Mercaptoacetyltriglycine|xanthomatosis|screen|assess|not indicated|Xanthomatous|pregnancy|gall|deficiencies|Mucolipidosis|renal|pseudoxanthoma|tumour", readterm)) %>%
  filter(!medcode %in% c(37, 37206, 23124, 30870, 15195, 12, 12821, 62, 26902, 30638, 53538, 6363, 18147))

# save as text file
write.table(dyslipidcodes_g, file = "Code lists/Dyslipidaemia/CPRD-2023/dyslipid_gold_240723.txt",
            sep = "\t", row.names = FALSE)


# AURUM

dyslipidcodes_a <- CPRDAurumMedical %>%
  filter(grepl("(?i)cholest|lipid|trigl|lipoprotein|glycerid|xanthoma", readterm)) %>%
  filter(!grepl("(?i)tubular|cerebral|granuloma|triglycerides normal|lipids normal|cholesterol normal|electroph. normal|FH|plasma lipids|genotyping|level|Retinal|cholesteatoma|ratio|storage|
                |cholesteatosis|cholestyramine|calculus|electroph. -|electroph. NOS|apheresis|possible|Phospholipid|supplementation|Apolipoprotein|Fibroxanthoma|Lipidoses|liver|
                |Mercaptoacetyltriglycine|xanthomatosis|screen|assess|not indicated|Xanthomatous|pregnancy|gall|deficiencies|Mucolipidosis|renal|pseudoxanthoma|Sphingolipidosis|carcinoma|phenotype|tumour|jaundice|
                |Cholestasis|lipase|family history|drug induced", readterm)) %>%
  filter(!medcodeid %in% c(186091018, 259209011, 259239018, 259252011, 405414014, 460116012, 2478521013, 34521000033114, 107301000006115, 109931000006117, 310251000000112,
                           667191000006111, 854481000006110, 854781000006119, 854781000006119, 1019311000006115, 1019331000006114, 1060061000006111, 1116931000006112, 1118321000006112, 
                           1122611000006115, 1127261000006118, 1131971000006114, 1137261000006115, 1591751000006117, 1630681000006110, 1631261000006110, 1639281000006116, 1640161000006111, 
                           1663241000006117, 1743961000006112, 1856411000006112, 1864161000006119, 1865801000006111, 1877381000006114, 1879211000006117, 1933151000006117, 2572861000006113, 
                           2650121000006116, 2733261000000112, 2733541000006119, 2750051000006115, 2757781000006114, 2757791000006112, 2854331000006115, 2949971000006113, 3002261000006111,
                           987321000006112, 1842101000006119, 3259431000006111, 3281711000006116, 3639851000006115, 3864411000006119, 3877651000006116, 3891501000006117, 3891531000006113, 
                           3891541000006115, 4058511000006118, 4275041000006114, 4079571000006118, 4079601000006113, 4079621000006115, 4079651000006112, 4079741000006116, 4110971000006117, 4217651000006116,
                           4600201000006115, 4600231000006111, 5029121000006117, 5311171000006114, 5311181000006112, 5415651000006116, 5415671000006114, 5415701000006110, 5742491000006119, 5899681000006116, 
                           5899711000006115, 6014411000006114, 6014421000006118, 5928571000006112, 5500571000006113, 6831591000006111, 7054871000006111, 7263101000006113, 7612381000006110, 8036201000006119, 
                           8330391000006119, 8333521000006116, 14357531000006113, 8883010, 259264015, 259269013, 4600561000006117))

# save as text file
write.table(dyslipidcodes_a, file = "Code lists/Dyslipidaemia/CPRD-2023/dyslipid_aurum_240723.txt",
            sep = "\t", row.names = FALSE)

