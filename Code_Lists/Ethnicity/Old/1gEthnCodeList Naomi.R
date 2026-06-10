rm(list = ls(all.names = TRUE))
####Libraries####
library(haven)
library(psych)
library(tidyverse)
library(tableone)
library(stringr)

####Load old ethnicity lists####
EthnAurum<-read.table("CodeLists/Ethnicity/EthnicityGroupsAurum.txt", header=TRUE, sep="\t", colClasses=c(MedCodeId="character"))
EthnGold<-read.table("CodeLists/Ethnicity/EthnicityGroupsGold.txt", header=TRUE, sep="\t", colClasses=c(medcode="character"))

#Remove blanks and remove
EthnAurum<-subset(EthnAurum, Group!="Remove" & !is.na(Group))
EthnGold<-subset(EthnGold, Group!="Remove" & !is.na(Group))

#Look ups
AurumMed<-read.table("LookUps/202205_Lookups_CPRDAurum/202205_EMISMedicalDictionary.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".", colClasses = c(MedCodeId="character"))
GoldMed<-read.table("LookUps/202303_Lookups_CPRDGOLD/medical.txt", header=TRUE, fill=TRUE, sep="\t", quote = "", dec = ".",colClasses = c(medcode="character"))
AurumMed$Term<-tolower(AurumMed$Term)
AurumMed$Term<-gsub('"', '', AurumMed$Term)
GoldMed$desc<-tolower(GoldMed$desc)

####Check for new ethnicity codes Aurum####
NewEthnAurum<-subset(AurumMed, !(MedCodeId %in% EthnAurum$MedCodeId))

NewEthnAurum<-NewEthnAurum%>%
  mutate(Ethn = case_when(Term=="patient ethnicity unknown"|Term=="ethnicity not stated" ~1,
                          grepl("o\\/e|^ethn|consultation|seen by|physical|mutilation|encounter|seen in|difficult|neo|english|tracer|function|scheme|assess|use|usage|cancer|congenital|morbidity|using|regime|stim|scale|delay|nmo|adverse reaction|\\[|ability|able|advice|speech and language|sign language|nationality data|americanus|africanum|frog|mother|father|not main|carer|parent|benign|impairment|finding|score|religion|exam|language barrier|test|development|repetitious|disorder|therap", Term, ignore.case=TRUE) ~0,
                          Term=="english as a second language"|Term=="language spoken"|Term=="supplemental main language spoken"|Term=="additional main spoken language"|Term=="second language"|Term=="language read"|Term=="main spoken language"|Term=="world languages"|Term=="preferred written language"|Term=="blacksmith"|Term=="language interpreter"|Term=="country"|Term=="country of birth"|Term=="country of origin"|Term=="language"|Term=="preferred spoken language"|Term=="main spoken language nos"|Term=="other main spoken language" ~ 0,
                          grepl("^country", Term, ignore.case=TRUE) ~1,
                          grepl("country", Term, ignore.case=TRUE) ~0,
                          CleansedReadCode=="" & grepl("language|indian|african|american|ethnic", Term, ignore.case=TRUE) ~1,
                          CleansedReadCode=="" ~ 0,
                          grepl("^13c|^13m|^13o|^13i|^13p|^13q|^13r|^13x|^13X|^13y|^13z|^9N[b-l]|^9N[o-z]|^9N[A-T]|^9N[V-Z]|^9N[0-9]", CleansedReadCode, ignore.case=FALSE) ~0,
                          grepl("^13[a-z]|^134|^226|^9i|^9s|^9t|^9T|^9N", CleansedReadCode, ignore.case=FALSE) ~1,
                         TRUE ~ 0))%>%
  subset(Ethn==1)

EthnAurum<-select(EthnAurum, MedCodeId, CleansedReadCode, Term)
NewEthnAurum<-select(NewEthnAurum, MedCodeId, CleansedReadCode, Term)
EthnAurum<-rbind(EthnAurum, NewEthnAurum)
EthnAurum<-subset(EthnAurum, MedCodeId!="")

####Check for new ethnicity codes Gold####
NewEthnGold<-subset(GoldMed, !(medcode %in% EthnGold$medcode))

NewEthnGold<-NewEthnGold%>%
  mutate(Ethn = case_when(desc=="patient ethnicity unknown"|desc=="ethnicity not stated" ~1,
                          grepl("o\\/e|^ethn|diabetes|consultation|seen by|physical|mutilation|encounter|seen in|difficult|neo|english|tracer|function|scheme|assess|use|usage|cancer|congenital|morbidity|using|regime|stim|scale|delay|nmo|adverse reaction|\\[|ability|able|advice|speech and language|sign language|nationality data|americanus|africanum|frog|mother|father|not main|carer|parent|benign|impairment|finding|score|religion|exam|language barrier|test|development|repetitious|disorder|therap", desc, ignore.case=TRUE) ~0,
                          desc=="english as a second language"|desc=="language spoken"|desc=="supplemental main language spoken"|desc=="additional main spoken language"|desc=="second language"|desc=="language read"|desc=="main spoken language"|desc=="world languages"|desc=="preferred written language"|desc=="blacksmith"|desc=="language interpreter"|desc=="country"|desc=="country of birth"|desc=="country of origin"|desc=="language"|desc=="preferred spoken language"|desc=="main spoken language nos"|desc=="other main spoken language" ~ 0,
                          grepl("^country", desc, ignore.case=TRUE) ~1,
                          grepl("country", desc, ignore.case=TRUE) ~0,
                          readcode=="" & grepl("language|indian|african|american|ethnic", desc, ignore.case=TRUE) ~1,
                          readcode=="" ~ 0,
                          grepl("^13c|^13m|^13i|^13sl|^13wd|^13o|^13p|^13q|^13r|^13x|^13X|^13y|^13z|^9N[b-l]|^9N[o-z]|^9N[A-T]|^9N[V-Z]|^9N[0-9]", readcode, ignore.case=FALSE) ~0,
                          grepl("^13[a-z]|^134|^226|^9i|^9s|^9t|^9T|^9N", readcode, ignore.case=FALSE) ~1,
                          TRUE ~ 0))%>%
  subset(Ethn==1)

EthnGold<-select(EthnGold, medcode, readcode, desc=readterm)
NewEthnGold<-select(NewEthnGold, medcode, readcode, desc)
EthnGold<-rbind(EthnGold, NewEthnGold)

####Compare Gold and Aurum####
EthnAurum$Term<-tolower(EthnAurum$Term)
EthnAurum$Term<-gsub('"', '', EthnAurum$Term)
EthnGold$desc<-tolower(EthnGold$desc)

GoldEthnCheck<-subset(EthnGold, !(desc %in% EthnAurum$Term)& desc!="")
AddtoAurumT<-subset(AurumMed, Term %in% GoldEthnCheck$desc) #6 to add

AurumEthnCheck<-subset(EthnAurum, !(Term %in% EthnGold$desc))
AddtoGoldT<-subset(GoldMed, desc %in% AurumEthnCheck$Term) #None to add on term

GoldEthnCheck<-subset(EthnGold, !(readcode %in% EthnAurum$CleansedReadCode))
AddtoAurumR<-subset(AurumMed, CleansedReadCode %in% GoldEthnCheck$readcode) #None to add

AurumEthnCheck<-subset(EthnAurum, !(CleansedReadCode %in% EthnGold$readcode) & CleansedReadCode!="")
AddtoGoldR<-subset(GoldMed, readcode %in% AurumEthnCheck$CleansedReadCode) #5 to add on readcode

AddtoAurumT<-select(AddtoAurumT, MedCodeId, CleansedReadCode, Term)
EthnAurum<-rbind(EthnAurum, AddtoAurumT)

####Save final lists####
write.table(EthnAurum, file = "Codelists/Ethnicity22/EthnAurum.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")
write.table(EthnGold, file = "Codelists/Ethnicity22/EthnGold.txt", col.names=TRUE, row.names=FALSE, sep="\t", dec = ".")
