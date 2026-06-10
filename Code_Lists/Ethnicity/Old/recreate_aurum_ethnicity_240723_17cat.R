# Recreate Aurum ethnicity/language code list from the Aurum medical dictionary
# using 17-category ethnicity coding
# Author: generated for Stephanie Wu
#
# Input required in the R environment:
#   cprd_aurum_medical: CPRD Aurum medical dictionary with at least a medcode
#                       column (e.g. MedCodeId/medcodeid) and a term column
#                       (e.g. Term/readterm).
#
# Output:
#   aurum_ethnicity_240723_recreated_17cat.txt
#
# Notes:
# - The code list is generated from scratch by searching dictionary terms and
#   selected medcodes using grepl/!grepl rules. It does not import the old code list.
# - CodeType is preserved as either "Ethnicity" or "Language".
# - ethnicgroup uses the requested 17-category scheme and is deliberately non-missing.
# - Language-derived records are weaker evidence than direct ethnicity records, so
#   keep CodeType for sensitivity analyses.

library(dplyr)

# -----------------------------
# User parameters
# -----------------------------
outfile <- "aurum_ethnicity_240723_recreated_17cat.txt"

# Optional: set this to the original list path for an audit after generation.
# Leave as NULL when generating the list from scratch.
target_path <- NULL

# -----------------------------
# Helpers
# -----------------------------
stopifnot(exists("cprd_aurum_medical"))

find_col <- function(dat, candidates) {
  hit <- candidates[tolower(candidates) %in% tolower(names(dat))]
  if (length(hit) == 0L) {
    stop("Could not find any of these columns in cprd_aurum_medical: ",
         paste(candidates, collapse = ", "))
  }
  names(dat)[match(tolower(hit[1]), tolower(names(dat)))]
}

rx <- function(x) paste0("(", paste(x, collapse = "|"), ")")
has <- function(x, pattern) grepl(pattern, x, ignore.case = TRUE, perl = TRUE)
not_has <- function(x, pattern) !has(x, pattern)

medcode_col <- find_col(cprd_aurum_medical,
                        c("medcodeid", "MedCodeId", "medcode", "Medcode"))
term_col <- find_col(cprd_aurum_medical,
                     c("readterm", "Term", "ReadTerm", "Read Term", "term"))

medical <- cprd_aurum_medical %>%
  transmute(
    medcodeid = as.character(.data[[medcode_col]]),
    readterm  = trimws(as.character(.data[[term_col]]))
  ) %>%
  filter(!is.na(medcodeid), !is.na(readterm), readterm != "") %>%
  distinct(medcodeid, readterm, .keep_all = TRUE)

# -----------------------------
# 17-category labels and search criteria
# -----------------------------
ethnicgroup_levels <- c(
  "1=White British",
  "2=White Irish",
  "3=Other White",
  "4=White and Black Caribbean",
  "5=White and Black African",
  "6=White and Asian",
  "7=Other Mixed",
  "8=Indian",
  "9=Pakistani",
  "10=Bangladeshi",
  "11=Other Asian",
  "12=Caribbean",
  "13=African",
  "14=Other Black",
  "15=Chinese",
  "16=Other",
  "17=Unknown"
)

# Original/direct ethnicity inclusion terms are deliberately aligned to the 17
# categories. The broad demographic terms capture census-style labels; the
# category-specific terms capture short dictionary terms without "ethnic".
ethnicity_include_by_group <- list(
  white_british = c(
    "white british", "british or mixed british", "eng/?welsh/?scot", "english",
    "welsh", "scottish", "northern irish", "ulster scots", "cornish"
  ),
  white_irish = c(
    "white irish", "\\birish\\b", "irish traveller"
  ),
  other_white = c(
    "other white", "white:", "white -", "white ethnic", "white background",
    "caucasian", "caucasoid", "european", "europeanoid", "polish",
    "romanian", "bulgarian", "czech", "slovak", "sloven", "albanian",
    "bosnian", "croatian", "serbian", "kosovan", "greek", "cypriot",
    "italian", "portuguese", "jewish", "israeli", "gypsy", "romany",
    "roma", "traveller", "australian", "new zealand european", "pakeha",
    "north american", "baltic", "estonian", "latvian", "lithuanian",
    "russian", "ukrainian", "commonwealth \\(russian\\) indep states"
  ),
  white_black_caribbean = c(
    "white.*black caribbean", "black caribbean.*white"
  ),
  white_black_african = c(
    "white.*black african", "black african.*white"
  ),
  white_asian = c(
    "white.*asian", "asian.*white", "chinese and white"
  ),
  other_mixed = c(
    "mixed", "multiple", "black and asian", "black and chinese", "black and white",
    "afro-caucasian", "afro-caribbean", "other ethnic.*mixed",
    "other ethnic.*black/white", "other ethnic.*asian/white"
  ),
  indian = c(
    "\\bindian\\b", "british indian", "indian sub-continent", "punjabi",
    "kashmiri"
  ),
  pakistani = c(
    "pakistani", "mirpuri"
  ),
  bangladeshi = c(
    "bangladeshi", "bengali", "sylheti"
  ),
  other_asian = c(
    "other asian", "asian", "japanese", "korean", "filipino", "malaysian",
    "sri lankan", "tamil", "sinhalese", "nepali", "thai", "vietnamese",
    "burmese", "myanmar", "indonesian", "javanese", "sundanese", "malay",
    "tagalog", "pashto", "pushto", "urdu", "guj[ae]rati", "hindi",
    "marathi", "konkani", "telugu", "malayalam", "uighur", "tibetan",
    "far eastern", "mongoloid", "oriental", "sikh", "hindu", "buddhist"
  ),
  caribbean = c(
    "caribbean", "west indian", "guyana", "indo-caribbean"
  ),
  african = c(
    "african", "nigerian", "somali", "ethiopian", "eritrean", "amharic",
    "tigrinya", "oromo", "swahili", "yoruba", "igbo", "lingala", "akan",
    "twi", "zulu", "shona", "ndebele", "brawa", "kongo", "hausa",
    "fulah", "kanuri", "bambara", "ewe", "herero", "kuanyama", "luba-katanga",
    "chewa", "nyanja", "chichewa", "southern sotho"
  ),
  other_black = c(
    "other black", "black", "negroid", "black british", "black arab",
    "black iranian", "black north african", "black indian sub-continent",
    "black east african asian", "black e afric asia"
  ),
  chinese = c(
    "chinese", "cantonese", "mandarin"
  ),
  other = c(
    "other ethnic", "other group", "arab", "arabic", "iranian", "farsi",
    "persian", "dari", "kurdish", "turkish", "azerbaijani", "turkmen",
    "tajik", "tatar", "kazakh", "kirghiz", "middle eastern", "north african",
    "moroccan", "yemeni", "latin american", "south american", "australoid",
    "fijian", "samoan", "tongan", "tokelauan", "niuean", "cook island",
    "maori", "pacific", "nauru", "tahitian", "marshallese", "hiri motu",
    "chamorro", "bislama", "inuktitut", "inupiaq", "greenlandic", "kalaallisut",
    "quechua", "guarani", "aymara", "navajo", "cree", "ojibwa", "ojibwe",
    "divehi", "dhivehi", "malagasy", "tetum", "haitian", "creole",
    "mauritian", "seychellois", "maldivian", "st helena", "armenian", "muslim"
  ),
  unknown = c(
    "unknown", "not recorded", "not given", "not stated", "refused", "declined",
    "ethnic group nos", "ethnic groups \\(census\\) nos", "patient ethnicity unknown"
  ),
  demographic_anchors = c(
    "ethnic", "race", "census", "\\borigin\\b", "\\bNMO\\b", "O/E -",
    "brit\\. ethnic minor", "non-white"
  )
)

ethnicity_include <- rx(unlist(ethnicity_include_by_group, use.names = FALSE))

# Exclude clearly clinical/non-demographic uses when broad terms are searched.
ethnicity_exclude <- rx(c(
  "culture", "microbiology", "antigen", "antibody", "anaemia", "sickle",
  "blackout", "black stool", "white blood", "white cell", "white matter",
  "asian flu", "indian ink", "west nile", "middle ear", "guinea pig",
  "racecadotril", "travel to", "holiday in", "foreign body", "black eye",
  "white spot", "white coat", "blackhead", "blackwater fever", "white piedra"
))

# Language/interpreter search inspired by the 2023 code, but kept separate from
# direct ethnicity codes through CodeType == "Language".
language_include <- rx(c(
  "language", "interpreter", "translator", "linguist",
  "preferred communication language", "main spoken language"
))

language_exclude <- rx(c(
  "alphabet", "body language", "programming language", "language of medicine",
  "speech and language therap", "language therapy", "language therapist",
  "specific language impairment", "language delay", "language disorder",
  "language development", "language scale", "language test", "written language",
  "english language assessment", "english language training"
))

# Medcodes whose source in the historic list was Naomi rather than Shiekh.
naomi_medcodes <- c(
  "459731017", "286003011", "412016016", "253634013", "253635014",
  "253629014", "402434017", "253630016"
)

# -----------------------------
# Candidate selection
# -----------------------------
ethnicity_codes <- medical %>%
  filter(has(readterm, ethnicity_include), not_has(readterm, ethnicity_exclude)) %>%
  mutate(CodeType = "Ethnicity")

language_codes <- medical %>%
  filter(has(readterm, language_include), not_has(readterm, language_exclude)) %>%
  mutate(CodeType = "Language")

# -----------------------------
# 17-category ethnicgroup inference
# -----------------------------
assign_ethnicgroup_17 <- function(term) {
  t <- tolower(term)

  case_when(
    # 17 Unknown / not stated / refused / not recorded. Put first so these do
    # not get pulled into an ethnicity category simply because they contain the
    # word "ethnic".
    has(t, rx(ethnicity_include_by_group$unknown)) |
      has(t, "other main spoken language|additional main spoken language|supplemental main language|^main spoken language$|language barrier|interpreter required|interpreter was|required indicator|need for interpreter|interpreter needed$|interpreter booked|interpreter declined|interpreter present|interpreter not present|not available|translator/interpreter|linguist/interpreter|language interpreter$|other interpreter|further interpreter|interpreter/translator services|consent .* interpreter|information sharing.*interpreter|practice member interpreter|family member interpreter|telephone interpreter") ~ "17=Unknown",

    # Mixed groups before single-group categories.
    has(t, rx(ethnicity_include_by_group$white_black_caribbean)) ~ "4=White and Black Caribbean",
    has(t, rx(ethnicity_include_by_group$white_black_african)) ~ "5=White and Black African",
    has(t, rx(ethnicity_include_by_group$white_asian)) ~ "6=White and Asian",
    has(t, rx(ethnicity_include_by_group$other_mixed)) ~ "7=Other Mixed",

    # Asian subcategories before broader Asian.
    has(t, rx(ethnicity_include_by_group$indian)) &
      not_has(t, "west indian|indo-caribbean|black indian") ~ "8=Indian",
    has(t, rx(ethnicity_include_by_group$pakistani)) ~ "9=Pakistani",
    has(t, rx(ethnicity_include_by_group$bangladeshi)) ~ "10=Bangladeshi",
    has(t, rx(ethnicity_include_by_group$chinese)) ~ "15=Chinese",

    # Black subcategories before broader Black.
    has(t, rx(ethnicity_include_by_group$caribbean)) &
      not_has(t, "white.*black caribbean|black caribbean.*white") ~ "12=Caribbean",
    has(t, rx(ethnicity_include_by_group$african)) &
      not_has(t, "white.*black african|black african.*white|east african asian") ~ "13=African",
    has(t, rx(ethnicity_include_by_group$other_black)) ~ "14=Other Black",

    # White subcategories before other white.
    has(t, rx(ethnicity_include_by_group$white_british)) ~ "1=White British",
    has(t, rx(ethnicity_include_by_group$white_irish)) &
      not_has(t, "gypsy|traveller.*gypsy") ~ "2=White Irish",
    has(t, rx(ethnicity_include_by_group$other_white)) ~ "3=Other White",

    # Remaining Asian and Other groups.
    has(t, rx(ethnicity_include_by_group$other_asian)) ~ "11=Other Asian",
    has(t, rx(ethnicity_include_by_group$other)) ~ "16=Other",

    # Less-specific demographic labels.
    has(t, "non-white|brit\\. ethnic minor") ~ "16=Other",

    TRUE ~ "17=Unknown"
  )
}

# -----------------------------
# Build recreated code list
# -----------------------------
aurum_ethnicity_240723_recreated <- bind_rows(ethnicity_codes, language_codes) %>%
  mutate(
    ethnicgroup = assign_ethnicgroup_17(readterm),
    ethnicgroup = factor(ethnicgroup, levels = ethnicgroup_levels),
    status = NA_character_,
    Source = case_when(
      CodeType == "Language" ~ "Direct",
      medcodeid %in% naomi_medcodes ~ "Naomi",
      TRUE ~ "Shiekh"
    )
  ) %>%
  # If a code is picked up as both ethnicity and language, keep the ethnicity row
  # because direct ethnicity records are stronger evidence than language records.
  arrange(match(CodeType, c("Ethnicity", "Language")), medcodeid, readterm) %>%
  distinct(medcodeid, .keep_all = TRUE) %>%
  select(medcodeid, readterm, ethnicgroup, status, Source, CodeType) %>%
  arrange(match(CodeType, c("Ethnicity", "Language")), ethnicgroup, readterm, medcodeid)

# -----------------------------
# Validation and export
# -----------------------------
stopifnot(!anyNA(aurum_ethnicity_240723_recreated$ethnicgroup))
stopifnot(all(as.character(aurum_ethnicity_240723_recreated$ethnicgroup) %in% ethnicgroup_levels))
stopifnot(setequal(unique(aurum_ethnicity_240723_recreated$CodeType),
                  c("Ethnicity", "Language")))

message("Rows generated: ", nrow(aurum_ethnicity_240723_recreated))
message("CodeType counts:")
print(table(aurum_ethnicity_240723_recreated$CodeType, useNA = "ifany"))
message("17-category ethnicgroup counts:")
print(table(aurum_ethnicity_240723_recreated$ethnicgroup, useNA = "ifany"))

write.table(
  aurum_ethnicity_240723_recreated,
  file = outfile,
  sep = "\t",
  row.names = FALSE,
  quote = TRUE,
  na = ""
)

# Optional audit against the old file, without using it to create the list.
if (!is.null(target_path) && file.exists(target_path)) {
  target <- read.delim(target_path, stringsAsFactors = FALSE, colClasses = "character") %>%
    mutate(medcodeid = as.character(medcodeid))

  audit_missing_from_recreated <- anti_join(
    target, aurum_ethnicity_240723_recreated,
    by = c("medcodeid", "CodeType")
  )
  audit_added_by_recreated <- anti_join(
    aurum_ethnicity_240723_recreated, target,
    by = c("medcodeid", "CodeType")
  )
  audit_ethnicgroup_changes <- inner_join(
    target %>% select(medcodeid, readterm_old = readterm, CodeType,
                      ethnicgroup_old = ethnicgroup),
    aurum_ethnicity_240723_recreated %>% select(medcodeid, readterm_new = readterm,
                                                CodeType, ethnicgroup_new = ethnicgroup),
    by = c("medcodeid", "CodeType")
  ) %>%
    filter(is.na(ethnicgroup_old) | ethnicgroup_old != as.character(ethnicgroup_new))

  message("Target rows not found by rules: ", nrow(audit_missing_from_recreated))
  message("Rows found by rules but absent from target: ", nrow(audit_added_by_recreated))
  message("Rows with changed or newly non-missing ethnicgroup: ", nrow(audit_ethnicgroup_changes))

  write.table(audit_missing_from_recreated,
              file = sub("\\.txt$", "_audit_missing_from_recreated.txt", outfile),
              sep = "\t", row.names = FALSE, quote = TRUE, na = "")
  write.table(audit_added_by_recreated,
              file = sub("\\.txt$", "_audit_added_by_recreated.txt", outfile),
              sep = "\t", row.names = FALSE, quote = TRUE, na = "")
  write.table(audit_ethnicgroup_changes,
              file = sub("\\.txt$", "_audit_ethnicgroup_changes.txt", outfile),
              sep = "\t", row.names = FALSE, quote = TRUE, na = "")
}
