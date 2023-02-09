rm(list = ls(all.names = TRUE))
####Libraries####
library(tidyverse)
library(tableone)
library(sqldf)
library(inborutils)
library(DBI)
library(RSQLite)

####Load initial patient file####

load("FILEPATH/Data/Patients_SMI.Rdata")

####Query SQL db####
conn <- dbConnect(RSQLite::SQLite(),"R:/Datamind_Repurposing/TriNetX/Data/Diag.sqlite" )
MetabQuery <- dbSendQuery(conn, "SELECT * FROM Diag WHERE code LIKE 'E65%' OR code LIKE 'E66%' OR code LIKE 'E67%' OR code LIKE 'E68%' OR code LIKE 'E10%'
OR code LIKE 'E11%' OR code LIKE 'E12%' OR code LIKE 'E13%' OR code LIKE 'E14%' OR code LIKE 'E78%'
OR code LIKE '278%' OR code LIKE '249%' OR code LIKE '250%' OR code LIKE '272%'")
MetabDiag <- dbFetch(MetabQuery)
write.csv(MetabDiag, "R:/Datamind_Repurposing/TriNetX/Data/MetabDiag.csv", quote = FALSE, row.names = FALSE)

####Load metabolic diagnoses####
MetabDiag<-read.table("R:/Datamind_Repurposing/TriNetX/Data/MetabDiag.csv", header=TRUE, sep=",")

length(unique(MetabDiag$patient_id))
length(which(!(SMI$patient_id %in% MetabDiag$patient_id)))
#Include only those who are in the patient file
MetabDiag<-subset(MetabDiag, patient_id %in% SMI$patient_id)

#Check the codes brought in by the broad logic of the SQL query, and exclude those which aren't in the code list
table(MetabDiag$code)
MetabDiag<-subset(MetabDiag, code!="250514000" &  code!="278414003" & code !="278.1" & code !="278.2" & code !="278.3" & code !="278.4"& code !="278.8"
                  & code !="272.5" & code != "272.6" & code !="272.7" & code !="E65" & !startsWith(code, "E67")& !startsWith(code, "E68") &
                    code !="E78.6")

#Code the diagnoses as obesity, diabetes or dyslipidaemia
MetabDiag1<-MetabDiag%>%
  mutate(Diag = case_when(startsWith(code, "E66") ~ "Obesity",
                          
                             startsWith(code, "E10") ~ "Diabetes I",
                          startsWith(code, "E11") ~ "Diabetes II",
                          startsWith(code, "E12") ~ "Diabetes",
                          startsWith(code, "E13") ~ "Diabetes",
                          startsWith(code, "E14") ~ "Diabetes",
                          
                          code =="E78.8" ~ "Maybe dyslipidaemia",
                          code == "E78.9" ~ "Maybe dyslipidaemia",
                          startsWith(code, "E78") ~ "Dyslipidaemia",
                          
                          code=="278.00" ~ "Obese",
                          code=="278.01" ~ "Obese",
                          code=="278.02" ~ "Overweight",
                          code=="278.03" ~ "Obese",
                          code=="278.0" ~ "Overweight/Obesity",
                          code=="278" ~ "Overweight/Obesity",
                          
                          nchar(code)==6 & startsWith(code, "249.") & (endsWith(code, "0") | endsWith(code, "2")) ~ "Diabetes II",
                          nchar(code)==6 & startsWith(code, "249.") & (endsWith(code, "1") | endsWith(code, "3")) ~ "Diabetes I",   
                          startsWith(code, "249") ~ "Secondary diabetes",                          
                          nchar(code)==6 & startsWith(code, "250.") & (endsWith(code, "0") | endsWith(code, "2")) ~ "Diabetes II",
                          nchar(code)==6 & startsWith(code, "250.") & (endsWith(code, "1") | endsWith(code, "3")) ~ "Diabetes I",  
                          startsWith(code, "250") ~ "Diabetes", 

                          code=="272.9" ~ "Maybe dyslipidaemia",
                          code=="272.8" ~ "Maybe dyslipidaemia",
                          startsWith(code, "272") ~ "Dyslipidaemia",
                             TRUE ~ "Check"))
  
table(MetabDiag1$Diag)

MetabDiag1$TrueDate<-as.character(MetabDiag$date)
MetabDiag1$TrueDate<-as.Date(MetabDiag$TrueDate, "%Y %m %d")

#####Focus on lipids####

Metabsub<-select(MetabDiag1, patient_id, TrueDate, Diag)
Metabsub<-subset(Metabsub, Diag=="Dyslipidaemia"|Diag=="Maybe dyslipidaemia")
length(unique(Metabsub$patient_id))

#Check how many "maybe codes"
Maybe<-subset(Metabsub, Diag=="Maybe dyslipidaemia")
length(unique(Maybe$patient_id))
Definite<-subset(Metabsub, Diag=="Dyslipidaemia")
length(unique(Definite$patient_id))

#Take first diagnosis, if more than one on the same date just take any as only need the date and group         
FirstAnyLipid<-Metabsub%>%
  group_by(patient_id)%>%
  mutate(First=min(TrueDate))%>%
  subset(TrueDate==First)%>%
  distinct(patient_id, Diag, .keep_all = TRUE)%>%
  mutate(obs_count=n()) %>% 
  ungroup()

#Take dyslipidaemia, if dyslipidaemia and maybe dyslipidaemia occur on the same day

FirstAnyLipid<-FirstAnyLipid%>%
  mutate(Select=case_when(Diag=="Dyslipidaemia" ~ 1,
                          Diag=="Maybe dyslipidaemia" ~ 2,
                          TRUE ~ 0))%>%
  group_by(patient_id)%>%
  mutate(Choose=min(Select))%>%
  subset(Choose==Select)%>%
  distinct(patient_id, Diag, .keep_all = TRUE) 

length(which(FirstAnyLipid$Diag=="Maybe dyslipidaemia"))

FirstAnyLipid<-select(FirstAnyLipid, patient_id, FirstAnyDysLipid = First)

#Merge first lipid to patient table
SMI<-merge(x=SMI, y=FirstAnyLipid, by="patient_id", all.x=TRUE, all.y=FALSE)                     

####Inclusion criteria####
#If dyslipidaemia is prior to SMI diagnosis then exclude from the cohort
length(which(SMI$FirstSMIdate>SMI$FirstAnyDysLipid))
SMI<-subset(SMI, FirstSMIdate<=FirstAnyDysLipid | is.na(FirstAnyDysLipid))

#Eventually, if first dyslipidaemia is after 01/01/2021 we will exclude. Need to check cholesterol values first though so they stay in
length(which(SMI$FirstAnyDysLipid>"2020-12-31"))
length(which(SMI$FirstSMIdate==SMI$FirstAnyDysLipid))
length(which(!is.na(SMI$FirstAnyDysLipid)))

#Then bring in first definite dyslipidaemia date
FirstTrueLipid<-Metabsub%>%
  subset(Diag=="Dyslipidaemia") %>% 
  group_by(patient_id)%>%
  mutate(First=min(TrueDate))%>%
  subset(TrueDate==First)%>%
  distinct(patient_id, Diag, .keep_all = TRUE)%>%
  mutate(obs_count=n()) %>% 
  ungroup()

FirstTrueLipid<-select(FirstTrueLipid, patient_id, FirstTrueDysLipid = First)

#Add first definite dyslipidaemia to the patient table. This, or cholesterol date will be the date of study entry
SMI<-merge(x=SMI, y=FirstTrueLipid, by="patient_id", all.x=TRUE, all.y=FALSE)                     

length(which(SMI$FirstSMIdate>SMI$FirstTrueDysLipid))
length(which(SMI$FirstTrueDysLipid>"2020-12-31"))
length(which(SMI$FirstSMIdate==SMI$FirstTrueDysLipid))
length(which(!is.na(SMI$FirstTrueDysLipid)))

#Definite dyslipidaemia
SMI$Lipids<-0
SMI$Lipids[!is.na(SMI$FirstTrueDysLipid)]<-1

save(SMI, file="FILEPATH/Data/Patients_SMILipid.Rdata")

####Create a table of main variables in those with and without a diagnosis of dyslipidaemia
SMI$died<-as.factor(SMI$died)

MyVars<-c("LastDiag", "patient_regional_location","sex", "marital_status", "died", "AgeAtDiagnosis", "AgeAtDeath", "RaceEthn")

Table1<-CreateTableOne(vars=MyVars,  strata="Lipids", data=SMI)
print(Table1, nonnormal=TRUE)

Table1Exp <- print(Table1, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, catDigits=2)

write.csv(Table1Exp, file = "FILEPATH/Tables/TableLipids.csv")

####Pull out diabetes####
load("FILEPATH/Data/Patients_SMILipid.Rdata")

#Make sure only have current patients given lipids exclusions
MetaSub<-subset(MetabDiag1, patient_id %in% SMI$patient_id)

#Limit to diabetes
Diabetes<-select(MetaSub, patient_id, TrueDate, Diag)
Diabetes<-subset(Diabetes, Diag=="Diabetes"|Diag=="Diabetes I"| Diag=="Diabetes II" | Diag=="Secondary diabetes")

length(unique(Diabetes$patient_id))
table(Diabetes$Diag)

#Expore diabetes type
DiabetesType<-Diabetes%>%
  select(-TrueDate)%>%
  group_by (patient_id, Diag) %>% 
  mutate(Count = n()) %>% 
  distinct(patient_id, Diag, .keep_all = TRUE) %>% 
  pivot_wider(names_from = Diag, values_from = Count)

DiabetesType$`Diabetes II`[DiabetesType$`Diabetes II`>1]<- 1
DiabetesType$`Diabetes II`[is.na(DiabetesType$`Diabetes II`)]<- 0

DiabetesType$`Diabetes I`[DiabetesType$`Diabetes I`>1]<- 1
DiabetesType$`Diabetes I`[is.na(DiabetesType$`Diabetes I`)]<- 0

DiabetesType$`Diabetes`[DiabetesType$`Diabetes`>1]<- 1
DiabetesType$`Diabetes`[is.na(DiabetesType$`Diabetes`)]<- 0

DiabetesType$`Secondary diabetes`[DiabetesType$`Secondary diabetes`>1]<- 1
DiabetesType$`Secondary diabetes`[is.na(DiabetesType$`Secondary diabetes`)]<- 0

length(which(DiabetesType$`Diabetes II`==1 & DiabetesType$`Diabetes I`==1))

DiabetesType<-DiabetesType%>%
    mutate(Type = case_when(`Diabetes II`==1 & `Diabetes I`==1 ~ "Mixed",
                          `Diabetes II`==1 & `Diabetes I`==0 ~ "Type II",
                          `Diabetes II`==0 & `Diabetes I`==1 ~ "Type I",
                          `Diabetes II`==0 & `Diabetes I`==0 & `Secondary diabetes`==1 ~ "Secondary",
                          TRUE ~ "Unknown"))
table(DiabetesType$Type)

#Take first any diabetes
FirstDiab<-Diabetes%>%
  group_by(patient_id)%>%
  mutate(First=min(TrueDate), ObsCount=n())%>%
  subset(TrueDate==First)%>%
  distinct(patient_id, .keep_all = TRUE)%>%
  ungroup()

FirstDiab<-select(FirstDiab, patient_id, FirstDiabetes = First)

#Merge first diabetes to patient table
SMI<-merge(x=SMI, y=FirstDiab, by="patient_id", all.x=TRUE, all.y=FALSE)

#Create a binary diabetes variable
SMI$Diabetes<-0
SMI$Diabetes[!is.na(SMI$FirstDiabetes)]<-1
table(SMI$Diabetes)

####Pull out obesity####
Obesity<-subset(MetaSub, Diag=="Obesity"|Diag=="Overweight/Obesity"|Diag=="Overweight")
table(Obesity$code, Obesity$Diag)

Obesity<-select(Obesity, patient_id, TrueDate, Diag)
length(unique(Obesity$patient_id))

#Investigate type of obesity/overweight
ObeseType<-Obesity%>%
  select(-TrueDate)%>%
  group_by (patient_id, Diag) %>% 
  mutate(Count = n()) %>% 
  distinct(patient_id, .keep_all = TRUE)%>%
  pivot_wider(names_from = Diag, values_from = Count, values_fill = 0)

length(which(ObeseType$Obesity==0 & ObeseType$Overweight>0))
length(which(ObeseType$`Overweight/Obesity`>0 & ObeseType$Obesity==0 & ObeseType$Overweight==0))

#Limit to just obesity records and take the first
FirstObese<-Obesity%>%
  subset(Diag =="Obesity") %>% 
  group_by(patient_id)%>%
  mutate(First=min(TrueDate), ObsCount=n())%>%
  subset(TrueDate==First)%>%
  distinct(patient_id, .keep_all = TRUE)%>%
  ungroup()

FirstObese<-select(FirstObese, patient_id, FirstObesity = First)

#Merge first obesity record to the patient table
SMI<-merge(x=SMI, y=FirstObese, by="patient_id", all.x=TRUE, all.y=FALSE)   

#Create a binary obesity variable, but note this needs to be supplemented with obesity codes in other tables
SMI$Obesity<-0
SMI$Obesity[!is.na(SMI$FirstObesity)]<-1
table(SMI$Obesity)

save(SMI, file="FILEPATH/Data/Patients_SMIMetab.Rdata")
