rm(list = ls(all.names = TRUE))
####Libraries####
library(tidyverse)

####Load initial patient file####

#Import SMI list
SMI<-read.csv("FILEPATH/patient.csv", header=TRUE, fill=TRUE, dec = ".")

####Cleaning####

#Double check unique
length(unique(SMI$patient_id))

#Drop invalid data and log in table called consort
NewSMI<-SMI
Consort<-data.frame("Count" = length(NewSMI$patient_id), "Description" = c("Original"))

#Year of birth
table(NewSMI$year_of_birth, useNA="ifany")
NewSMI<-subset(NewSMI, !is.na(year_of_birth))
Consort1<-data.frame("Count" =length(NewSMI$patient_id), "Description" = c("Missing YoB"))
Consort<- rbind(Consort,Consort1)

#Over 90 at end of study
table(NewSMI$year_of_birth[NewSMI$year_of_birth<1931])
NewSMI<-subset(NewSMI, NewSMI$year_of_birth>=1931)
Consort1<-data.frame("Count" =length(NewSMI$patient_id), "Description" = c("Over 90 at study end"))
Consort<- rbind(Consort,Consort1)

#Under 18 at end of study
table(NewSMI$year_of_birth[NewSMI$year_of_birth>2003])
NewSMI<-subset(NewSMI, NewSMI$year_of_birth<=2003)
Consort1<-data.frame("Count" =length(NewSMI$patient_id), "Description" = c("Under 18 at study end"))
Consort<- rbind(Consort,Consort1)

#Unknown sex
table(NewSMI$sex, useNA="ifany")
NewSMI<-subset(NewSMI, NewSMI$sex!="Unknown")
Consort1<-data.frame("Count" =length(NewSMI$patient_id), "Description" = c("Unknown sex"))
Consort<- rbind(Consort,Consort1)

#Deaths
NewSMI$died<-0
NewSMI$died[!is.na(NewSMI$month_year_death)]<-1
table(NewSMI$died)
prop.table(table(NewSMI$died))*100

NewSMI$DeathYear<-substr(NewSMI$month_year_death, 1,4)
table(NewSMI$DeathYear)

NewSMI<-subset(NewSMI, NewSMI$DeathYear>=2000|is.na(NewSMI$DeathYear))
Consort1<-data.frame("Count" =length(NewSMI$patient_id), "Description" = c("Died before 2000"))
Consort<- rbind(Consort,Consort1)

#create final data log
Consort<-Consort %>%
  mutate (Lag = lag(Count), Excluded = Lag-Count)
Consort<-select(Consort, -Lag)
write.csv(Consort, file = "FILEPATH/Analysis/Consort.csv")

#If died in 2022, did not die during follow up
length(which(NewSMI$DeathYear==2022))
NewSMI$died[NewSMI$DeathYear==2022]<-0
NewSMI$death_date_source_id[NewSMI$DeathYear==2022]<-""
NewSMI$month_year_death[NewSMI$DeathYear==2022]<-NA
NewSMI$DeathYear[NewSMI$DeathYear==2022]<-NA

####Basic descriptives of patient table####

#Create age at end of follow up
NewSMI$AgeAtEnd<-2021-NewSMI$year_of_birth
summary(NewSMI$AgeAtEnd)
sd(NewSMI$AgeAtEnd)

prop.table(table(NewSMI$sex))*100

#Merge ethnicity and race to create basic descriptives
table(NewSMI$race)
table(NewSMI$ethnicity)

RaceEthn<-as.data.frame(table(NewSMI$ethnicity, NewSMI$race))
NewSMI<-NewSMI%>%
  mutate(RaceEthn=case_when(race=="White"&(ethnicity=="Not Hispanic or Latino"|ethnicity=="Unknown")~ "White NH",
                            race=="White"&ethnicity=="Hispanic or Latino"~ "White H",
                            race=="Black or African American"&(ethnicity=="Not Hispanic or Latino"|ethnicity=="Unknown")~ "Black NH",
                            race=="Black or African American"&ethnicity=="Hispanic or Latino"~ "Black H",
                            race=="Asian" ~ "Asian",
                            race=="American Indian or Alaska Native" ~ "American Indian/Alaska Native",
                            race=="Native Hawaiian or Other Pacific Islander" ~ "Pacific Islander",
                            race=="Unknown"&ethnicity=="Hispanic or Latino"~ "Unknown H",
                            race=="Unknown"&(ethnicity=="Not Hispanic or Latino"|ethnicity=="Unknown")~ "Unknown",
                            TRUE ~ "CHECK"))
table(NewSMI$RaceEthn)
prop.table(table(NewSMI$RaceEthn))*100
prop.table(table(NewSMI$ethnicity))*100
prop.table(table(NewSMI$race))*100

#marital status
table(NewSMI$marital_status, useNA="ifany")
prop.table(table(NewSMI$marital_status))*100

#Deaths
prop.table(table(NewSMI$died))*100
Deaths<-subset(NewSMI, died==1)
prop.table(table(Deaths$death_date_source_id))*100

#Plot deaths per year
ggplot()+
  geom_bar(data = Deaths, stat = "count", aes(x=DeathYear))+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, angle = 90))+
  theme(axis.text.y = element_text(size = 14))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  labs( x="Year", y = "Number of patients died")

#location
table(NewSMI$patient_regional_location, useNA="ifany")
prop.table(table(NewSMI$patient_regional_location))*100

save(NewSMI, file="FILEPATH/Data/Patients.Rdata")
