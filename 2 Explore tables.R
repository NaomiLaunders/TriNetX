#Note, other than patient file, these files are too large to load into R in one go. Therefore, convert to SQL and query from there.
#This is best performed in an overnight run

####Explore the individual tables####
rm(list = ls(all.names = TRUE))
####Libraries####
library(haven)
library(psych)
library(dplyr)
library(lubridate)
library(tidyverse)
library(tableone)
library(sqldf)
library(inborutils)
library(DBI)
library(RSQLite)

####Lab table####
csv_to_sqlite(csv_file="R:/TrinetX/62cc23daa207fa03eb201ee0_20220711_133000927/lab_result.csv", "R:/Datamind_Repurposing/TriNetX/Data/Labs.sqlite", "Labs", 
              pre_process_size=1000, chunk_size=50000)

conn <- dbConnect(RSQLite::SQLite(),"R:/Datamind_Repurposing/TriNetX/Data/Labs.sqlite" )

LabQuery <- dbSendQuery(conn, "SELECT DISTINCT code, code_system
                        FROM Labs") 
Lab <- dbFetch(LabQuery)
dbClearResult(LabQuery)
write.csv(Lab, "R:/Datamind_Repurposing/TriNetX/Data/AllLabCodes.csv", quote = FALSE, row.names = FALSE)

####Encounters table####
csv_to_sqlite(csv_file="R:/TrinetX/62cc23daa207fa03eb201ee0_20220711_133000927/encounter.csv", "R:/Datamind_Repurposing/TriNetX/Data/Encounters.sqlite", "Encounters", 
              pre_process_size=1000, chunk_size=50000)

conn <- dbConnect(RSQLite::SQLite(),"R:/Datamind_Repurposing/TriNetX/Data/Encounters.sqlite" )
DiagQuery <- dbSendQuery(conn, "SELECT COUNT(type), type FROM Encounters GROUP BY type")
Diag <- dbFetch(DiagQuery)
dbClearResult(DiagQuery)
write.csv(Diag, "R:/Datamind_Repurposing/TriNetX/Data/AllEncounterCodes.csv", quote = FALSE, row.names = FALSE)

####Procedure####
csv_to_sqlite(csv_file="R:/TrinetX/62cc23daa207fa03eb201ee0_20220711_133000927/procedure.csv", "R:/Datamind_Repurposing/TriNetX/Data/Procedure.sqlite", "Procedure", 
              pre_process_size=1000, chunk_size=50000)

conn <- dbConnect(RSQLite::SQLite(),"R:/Datamind_Repurposing/TriNetX/Data/Procedure.sqlite" )
DiagQuery <- dbSendQuery(conn, "SELECT COUNT(*), code, code_system FROM Procedure GROUP BY code, code_system")
Diag <- dbFetch(DiagQuery)
dbClearResult(DiagQuery)
write.csv(Diag, "R:/Datamind_Repurposing/TriNetX/Data/AllProcedureCodes.csv", quote = FALSE, row.names = FALSE)

####Vital signs####
csv_to_sqlite(csv_file="R:/TrinetX/62cc23daa207fa03eb201ee0_20220711_133000927/Vital_signs.csv", "R:/Datamind_Repurposing/TriNetX/Data/vitals.sqlite", "Vitals", 
              pre_process_size=1000, chunk_size=50000)

conn <- dbConnect(RSQLite::SQLite(),"R:/Datamind_Repurposing/TriNetX/Data/vitals.sqlite" )
DiagQuery <- dbSendQuery(conn, "SELECT COUNT(*), code, code_system FROM Vitals GROUP BY code, code_system")
Diag <- dbFetch(DiagQuery)
dbClearResult(DiagQuery)
write.csv(Diag, "R:/Datamind_Repurposing/TriNetX/Data/AllVitalsCodes.csv", quote = FALSE, row.names = FALSE)

####Medicine tables####
csv_to_sqlite(csv_file="R:/TrinetX/62cc23daa207fa03eb201ee0_20220711_133000927/medication_drug.csv", "R:/Datamind_Repurposing/TriNetX/Data/Medical.sqlite", "Drug", 
              pre_process_size=1000, chunk_size=50000)

conn <- dbConnect(RSQLite::SQLite(),"R:/Datamind_Repurposing/TriNetX/Data/Medical.sqlite" )
DiagQuery <- dbSendQuery(conn, "SELECT DISTINCT code_system, code FROM Drug")
Diag <- dbFetch(DiagQuery)
dbClearResult(DiagQuery)
write.csv(Diag, "R:/Datamind_Repurposing/TriNetX/Data/AllDrugsCodes.csv", quote = FALSE, row.names = FALSE)

csv_to_sqlite(csv_file="R:/TrinetX/62cc23daa207fa03eb201ee0_20220711_133000927/medication_ingredient.csv", "R:/Datamind_Repurposing/TriNetX/Data/Medical.sqlite", "Ingredient", 
              pre_process_size=1000, chunk_size=50000)

conn <- dbConnect(RSQLite::SQLite(),"R:/Datamind_Repurposing/TriNetX/Data/Medical.sqlite" )
DiagQuery <- dbSendQuery(conn, "SELECT DISTINCT code_system, code FROM Ingredient")
Diag <- dbFetch(DiagQuery)
dbClearResult(DiagQuery)
write.csv(Diag, "R:/Datamind_Repurposing/TriNetX/Data/AllIngredientsCodes.csv", quote = FALSE, row.names = FALSE)

####Diagnosis table####
csv_to_sqlite(csv_file="R:/TrinetX/62cc23daa207fa03eb201ee0_20220711_133000927/diagnosis.csv", "R:/Datamind_Repurposing/TriNetX/Data/Diag.sqlite", "Diag", 
              pre_process_size=1000, chunk_size=50000)

conn <- dbConnect(RSQLite::SQLite(),"R:/Datamind_Repurposing/TriNetX/Data/Diag.sqlite" )
DiagQuery <- dbSendQuery(conn, "SELECT DISTINCT code, code_system FROM Diag")
Diag <- dbFetch(DiagQuery)
dbClearResult(DiagQuery)
write.csv(Diag, "R:/Datamind_Repurposing/TriNetX/Data/AllDiagCodes.csv", quote = FALSE, row.names = FALSE)
