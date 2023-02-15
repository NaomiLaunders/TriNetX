rm(list = ls(all.names = TRUE))
####Libraries####
library(tidyverse)
library(tableone)
library(sqldf)
library(inborutils)
library(DBI)
library(RSQLite)

####Load initial patient file####

load("FILEPATH/Data/Patients.Rdata")

#Use SQL to query the database
conn <- dbConnect(RSQLite::SQLite(),"FILEPATH/Diag.sqlite" )
SMIQuery <- dbSendQuery(conn, "SELECT * FROM Diag WHERE code LIKE 'F2%' OR code LIKE 'F30%' OR code LIKE 'F31%' OR code LIKE '29%' OR code = '301.22'")
SMIDiag <- dbFetch(SMIQuery)
write.csv(SMIDiag, "FILEPATH/Diag.csv", quote = FALSE, row.names = FALSE)
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------dbDisconnect(conn)

####Pull first and last SMI diagnosis####
SMIDiag<-read.csv(file="FILEPATH/Diag.csv")

#Check they all match
length(unique(SMIDiag$patient_id))
length(which(!(NewSMI$patient_id %in% SMIDiag$patient_id)))

#Drop any diagnosis records not in cohort
SMIDiag<-subset(SMIDiag,(patient_id %in% NewSMI$patient_id))

#Drop those where code = 290-294
SMIDiag2<-subset(SMIDiag, !(startsWith(code, "290")|startsWith(code, "291")|startsWith(code, "292")|
                              startsWith(code, "293")| startsWith(code, "294")| startsWith(code, "29.")|startsWith(code, "299")|
                              startsWith(code, "296.2")|startsWith(code, "296.3")|code=="296.82"|startsWith(code, "296.9")|
                              startsWith(code, "298.2")|code=="296"))
                              
#Check they all match
length(unique(SMIDiag2$patient_id))

#Convert date field
SMIDiag2$TrueDate<-as.character(SMIDiag2$date)
SMIDiag2$TrueDate<-as.Date(SMIDiag2$TrueDate, "%Y %m %d")

#Find minimum and code up using logic presented in the codelist file [To be uploaded]

SMIMin<-SMIDiag2%>%
  group_by(patient_id)%>%
  mutate(First=min(TrueDate), n=n())%>%
  subset(TrueDate==First)%>%
  mutate(FullDiag = case_when(code == "297" ~ "Other psychotic disorders",
                              code == "298" ~ "Other psychotic disorders",
                              
                   startsWith(code, "295.7") ~ "Schizoaffective disorder",
                   startsWith(code, "295") ~ "Schizophrenia",
                   
                   startsWith(code, "296.0") ~ "Bipolar affective disorder",
                   startsWith(code, "296.1") ~ "Manic episode",
                   startsWith(code, "296.4") ~ "Bipolar affective disorder",
                   startsWith(code, "296.5") ~ "Bipolar affective disorder",
                   startsWith(code, "296.6") ~ "Bipolar affective disorder",
                   startsWith(code, "296.7") ~ "Bipolar affective disorder",
                   code == "296.8" ~ "Bipolar affective disorder",
                   startsWith(code, "296.80") ~ "Bipolar affective disorder",
                   startsWith(code, "296.81") ~ "Bipolar affective disorder",
                   startsWith(code, "296.89") ~ "Bipolar affective disorder",
                   
                   startsWith(code, "297.0")~ "Persistent delusional disorder",
                   startsWith(code, "297.1")~ "Persistent delusional disorder",
                   startsWith(code, "297.2")~ "Persistent delusional disorder",
                   startsWith(code, "297.3")~ "Induced delusional disorder",
                   startsWith(code, "297.8") ~ "Other psychotic disorders",
                   startsWith(code, "297.9") ~ "Other psychotic disorders",
                   
                   startsWith(code, "298.0") ~ "Other psychotic disorders",
                   startsWith(code, "298.1") ~ "Other psychotic disorders",
                   startsWith(code, "298.3")~ "Transient psychotic disorder",
                   startsWith(code, "298.4")~ "Other psychotic disorders",
                   startsWith(code, "298.8")~ "Other psychotic disorders",
                   startsWith(code, "298.9") ~ "Other psychotic disorders",
                   code=="301.22" ~ "Schizotypal disorder",
                   startsWith(code, "F20") ~ "Schizophrenia",
                   startsWith(code, "F21") ~ "Schizotypal disorder",
                   startsWith(code, "F22") ~ "Persistent delusional disorder",
                   startsWith(code, "F23") ~ "Transient psychotic disorder",
                   startsWith(code, "F24") ~ "Induced delusional disorder",
                   startsWith(code, "F25") ~ "Schizoaffective disorder",
                   startsWith(code, "F28") ~ "Other psychotic disorders",
                   startsWith(code, "F29") ~ "Other psychotic disorders",
                   startsWith(code, "F30") ~ "Manic episode",
                   startsWith(code, "F31") ~ "Bipolar affective disorder",
                   TRUE ~ "Check"))

#Check every patient has a diagnosis
table(SMIMin$FullDiag)

####Inclusion - first SMI diagnosis on or before 31 December 2020 to allow for 1 year follow up####
length(which(SMIMin$First>"2020-12-31"))
#Only include their first diagnosis if before 01/01/2021
SMIMin<-subset(SMIMin, First<="2020-12-31")
length(unique(SMIMin$patient_id))

#If they don't now have a minimum diagnosis, remove from the diagnosis table and patient table
SMIDiag2<-subset(SMIDiag2, patient_id %in% SMIMin$patient_id)
NewSMI<-subset(NewSMI, patient_id %in% SMIMin$patient_id)

#A few have two or more diagnoses on the first day of diagnosis - if these are the same full diagnosis, just keep one
SMIMin<-distinct(SMIMin, patient_id, FullDiag, .keep_all = TRUE)

#Code the broad diagnostic grouping and if they have the two or more of the same broad group, only keep one
SMIMin<-SMIMin%>%
  mutate(Diag=case_when(FullDiag=="Schizophrenia" ~ "Schizophrenia",
                        FullDiag=="Schizoaffective disorder" ~ "Other",
                        FullDiag=="Persistent delusional disorder" ~ "Other",
                        FullDiag=="Transient psychotic disorder" ~ "Other",
                        FullDiag=="Induced delusional disorder" ~ "Other",
                        FullDiag=="Other psychotic disorders" ~ "Other",
                        FullDiag=="Manic episode" ~ "Bipolar",
                        FullDiag=="Bipolar affective disorder" ~ "Bipolar",
                        FullDiag=="Schizotypal disorder" ~ "Other",
                        TRUE ~ "Check"))%>%
  distinct(patient_id, Diag, .keep_all = TRUE)

#Check everyone has a broad group
table(SMIMin$FullDiag[SMIMin$Diag=="Check"])

#Limit the fields to those we need
SMIMinSub<-select(SMIMin, patient_id, FullDiag, Diag, First)

#Sort out those that occur on the same day with different broad diagnoses - take schizophrenia, then bipolar, then other
SMIMinSub<-SMIMinSub%>%
    mutate(Select=case_when(Diag=="Schizophrenia" ~ 1,
                          Diag=="Bipolar" ~ 2,
                          Diag=="Other" ~ 3,
                          TRUE ~ 0))%>%
    group_by(patient_id)%>%
    mutate(Choose=min(Select))%>%
    subset(Choose==Select)%>%
    distinct(patient_id, .keep_all = TRUE)

SMIMinSub<-select(SMIMinSub, patient_id, FullDiag, FirstDiag = Diag, FirstSMIdate = First)

#Merge first diagnosis onto the main patient table
SMI<-merge(x=NewSMI, y=SMIMinSub, by="patient_id", all.x=TRUE, all.y=TRUE)                     
  
save(SMI, file="FILEPATH/Data/Patients_SMI.Rdata")

####Find last date of diagnosis####

SMIMax<-SMIDiag2%>%
  group_by(patient_id)%>%
  mutate(Last=max(TrueDate), n=n())%>%
  subset(TrueDate==Last)%>%
  
  mutate(FullDiag = case_when(code == "297" ~ "Other psychotic disorders",
                                        code == "298" ~ "Other psychotic disorders",
                                        
                                        startsWith(code, "295.7") ~ "Schizoaffective disorder",
                                        startsWith(code, "295") ~ "Schizophrenia",
                                        
                                        startsWith(code, "296.0") ~ "Bipolar affective disorder",
                                        startsWith(code, "296.1") ~ "Manic episode",
                                        startsWith(code, "296.4") ~ "Bipolar affective disorder",
                                        startsWith(code, "296.5") ~ "Bipolar affective disorder",
                                        startsWith(code, "296.6") ~ "Bipolar affective disorder",
                                        startsWith(code, "296.7") ~ "Bipolar affective disorder",
                                        code == "296.8" ~ "Bipolar affective disorder",
                                        startsWith(code, "296.80") ~ "Bipolar affective disorder",
                                        startsWith(code, "296.81") ~ "Bipolar affective disorder",
                                        startsWith(code, "296.89") ~ "Bipolar affective disorder",
                                        
                                        startsWith(code, "297.0")~ "Persistent delusional disorder",
                                        startsWith(code, "297.1")~ "Persistent delusional disorder",
                                        startsWith(code, "297.2")~ "Persistent delusional disorder",
                                        startsWith(code, "297.3")~ "Induced delusional disorder",
                                        startsWith(code, "297.8") ~ "Other psychotic disorders",
                                        startsWith(code, "297.9") ~ "Other psychotic disorders",
                                        
                                        startsWith(code, "298.0") ~ "Other psychotic disorders",
                                        startsWith(code, "298.1") ~ "Other psychotic disorders",
                                        startsWith(code, "298.3")~ "Transient psychotic disorder",
                                        startsWith(code, "298.4")~ "Other psychotic disorders",
                                        startsWith(code, "298.8")~ "Other psychotic disorders",
                                        startsWith(code, "298.9") ~ "Other psychotic disorders",
                                        code=="301.22" ~ "Schizotypal disorder",
                                        startsWith(code, "F20") ~ "Schizophrenia",
                                        startsWith(code, "F21") ~ "Schizotypal disorder",
                                        startsWith(code, "F22") ~ "Persistent delusional disorder",
                                        startsWith(code, "F23") ~ "Transient psychotic disorder",
                                        startsWith(code, "F24") ~ "Induced delusional disorder",
                                        startsWith(code, "F25") ~ "Schizoaffective disorder",
                                        startsWith(code, "F28") ~ "Other psychotic disorders",
                                        startsWith(code, "F29") ~ "Other psychotic disorders",
                                        startsWith(code, "F30") ~ "Manic episode",
                                        startsWith(code, "F31") ~ "Bipolar affective disorder",
                                        TRUE ~ "Check")) %>%
                                        distinct(patient_id, Diag, .keep_all = TRUE)
                                        
table(SMIMax$FullDiag)
table(SMIMax$code[SMIMax$FullDiag=="Check"])

####Inclusion - last SMI diagnosis on or after 01/01/2000 so are timely####
length(which(SMIMax$Last<"2000-01-01"))
#If last diagnosis is earlier than 01/01/2000 then drop it
SMIMax<-subset(SMIMax, Last>="2000-01-01")
#If there is now no last diagnosis, remove from diagnosis and patient tables
NewSMI<-subset(NewSMI, patient_id %in% SMIMax$patient_id)
SMI<-subset(SMI, patient_id %in% SMIMax$patient_id)

#Code broad groups, and if two broad groups on the same day then only take one
SMIMax<-SMIMax%>%
  mutate(Diag=case_when(FullDiag=="Schizophrenia" ~ "Schizophrenia",
                        FullDiag=="Schizoaffective disorder" ~ "Other",
                        FullDiag=="Persistent delusional disorder" ~ "Other",
                        FullDiag=="Transient psychotic disorder" ~ "Other",
                        FullDiag=="Induced delusional disorder" ~ "Other",
                        FullDiag=="Other psychotic disorders" ~ "Other",
                        FullDiag=="Manic episode" ~ "Bipolar",
                        FullDiag=="Bipolar affective disorder" ~ "Bipolar",
                        FullDiag=="Schizotypal disorder" ~ "Other",
                        TRUE ~ "Check"))%>%
  distinct(patient_id, Diag, .keep_all = TRUE)

table(SMIMax$FullDiag[SMIMax$Diag=="Check"])

SMIMaxSub<-select(SMIMax, patient_id, LastFullDiag=FullDiag, LastDiag = Diag, Last)
                        
#Sort out those that have different broad diagnoses on the same day - take schizophrenia, then bipolar, then other

SMIMaxSub<-SMIMaxSub%>%
  mutate(Select=case_when(LastDiag=="Schizophrenia" ~ 1,
                          LastDiag=="Bipolar" ~ 2,
                          LastDiag=="Other" ~ 3,
                          TRUE ~ 0))%>%
  group_by(patient_id)%>%
  mutate(Choose=min(Select))%>%
  subset(Choose==Select)%>%
  distinct(patient_id, .keep_all = TRUE)                     
                        
SMIMaxSub<-select(SMIMaxSub, patient_id, LastFullDiag, LastDiag, LastSMIdate = Last)                  

#Merge last SMI diagnosis to the patient table
SMI<-merge(x=SMI, y=SMIMaxSub, by="patient_id", all.x=TRUE, all.y=TRUE)

#Create age at first diagnosis and age at death
SMI$AgeAtDiagnosis<-as.numeric(year(SMI$FirstSMIdate))-SMI$year_of_birth
SMI$AgeAtDeath<-as.numeric(SMI$DeathYear)-SMI$year_of_birth

#Save the patient table
save(SMI, file="FILEPATH/Data/Patients_SMI.Rdata")

####Investigate gaps between first and last diagnosis####
prop.table(table(SMI$FirstDiag))
prop.table(table(SMI$LastDiag))

summary(year(SMI$LastSMIdate))
summary(year(SMI$FirstSMIdate))

SMI$DiagGap<-as.numeric(SMI$LastSMIdate-SMI$FirstSMIdate)
summary(SMI$DiagGap)
length(which(SMI$DiagGap==0))

DiagGap<-SMI%>%
  mutate(DiagGapCat = case_when(DiagGap==0 ~ "No gap",
                                DiagGap>0 & DiagGap<14 ~ "Under 2 weeks",
                                DiagGap>=14 &DiagGap<31 ~ "Two weeks - one month",
                                DiagGap>=31 & DiagGap<365.25 ~ "One month to a year",
                                DiagGap>=365.25 & DiagGap<730.5 ~ "One to two years",
                                DiagGap>=730.5 ~ "Over two years",
                                TRUE ~ "Check"))
DiagGap$DiagGapCat<-factor(DiagGap$DiagGapCat, levels = c("No gap", "Under 2 weeks","Two weeks - one month", "One month to a year", "One to two years",
                                                                   "Over two years" ))

table(DiagGap$LastDiag, DiagGap$DiagGapCat)

prop.table(table(DiagGap$LastDiag, DiagGap$DiagGapCat),1)*100

tapply(SMI$DiagGap, SMI$LastDiag, summary)

####Table 1 - compare key demographic variables in people with schizophrenia, bipolar disorder and other psychoses####
SMI$died<-as.factor(SMI$died)
MyVars<-c("patient_regional_location","sex", "marital_status", "died", "DeathYear", "AgeAtDiagnosis", "AgeAtDeath", "RaceEthn")

Table1<-CreateTableOne(vars=MyVars,  strata="LastDiag", data=SMI)
print(Table1, nonnormal=TRUE)

Table1Exp <- print(Table1, nonnormal=TRUE, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, catDigits=2)

write.csv(Table1Exp, file = "FILEPATH/Outputs/Tables/TableOne.csv")
