#Note, other than patient file, these files are too large to load into R in one go. Therefore, convert to SQL and query from there.
#This is best performed in an overnight run

#In hindsight, it would have been better to load all tables into one database, but I don't need them to be for this analysis, and it would take a long time to go back and re run them.

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
csv_to_sqlite(csv_file="FILENAME/lab_result.csv", "FILENAME/Labs.sqlite", "Labs", 
              pre_process_size=1000, chunk_size=50000)

conn <- dbConnect(RSQLite::SQLite(),"FILENAME/Labs.sqlite" )

LabQuery <- dbSendQuery(conn, "SELECT DISTINCT code, code_system
                        FROM Labs") 
Lab <- dbFetch(LabQuery)
dbClearResult(LabQuery)
write.csv(Lab, "FILENAME/AllLabCodes.csv", quote = FALSE, row.names = FALSE)

####Encounters table####
csv_to_sqlite(csv_file="FILENAME/encounter.csv", "FILENAME/Encounters.sqlite", "Encounters", 
              pre_process_size=1000, chunk_size=50000)

conn <- dbConnect(RSQLite::SQLite(),"FILENAME/Encounters.sqlite" )
DiagQuery <- dbSendQuery(conn, "SELECT COUNT(type), type FROM Encounters GROUP BY type")
Diag <- dbFetch(DiagQuery)
dbClearResult(DiagQuery)
write.csv(Diag, "FILENAME/AllEncounterCodes.csv", quote = FALSE, row.names = FALSE)

####Procedure####
csv_to_sqlite(csv_file="FILENAME/procedure.csv", "FILENAME/Procedure.sqlite", "Procedure", 
              pre_process_size=1000, chunk_size=50000)

conn <- dbConnect(RSQLite::SQLite(),"FILENAME/Procedure.sqlite" )
DiagQuery <- dbSendQuery(conn, "SELECT COUNT(*), code, code_system FROM Procedure GROUP BY code, code_system")
Diag <- dbFetch(DiagQuery)
dbClearResult(DiagQuery)
write.csv(Diag, "FILENAME/AllProcedureCodes.csv", quote = FALSE, row.names = FALSE)

####Vital signs####
csv_to_sqlite(csv_file="FILENAME/Vital_signs.csv", "FILENAME/vitals.sqlite", "Vitals", 
              pre_process_size=1000, chunk_size=50000)

conn <- dbConnect(RSQLite::SQLite(),"FILENAME/vitals.sqlite" )
DiagQuery <- dbSendQuery(conn, "SELECT COUNT(*), code, code_system FROM Vitals GROUP BY code, code_system")
Diag <- dbFetch(DiagQuery)
dbClearResult(DiagQuery)
write.csv(Diag, "FILENAME/AllVitalsCodes.csv", quote = FALSE, row.names = FALSE)

####Medicine tables####
csv_to_sqlite(csv_file="FILENAME/medication_drug.csv", "FILENAME/Medical.sqlite", "Drug", 
              pre_process_size=1000, chunk_size=50000)

conn <- dbConnect(RSQLite::SQLite(),"FILENAME/Medical.sqlite" )
DiagQuery <- dbSendQuery(conn, "SELECT DISTINCT code_system, code FROM Drug")
Diag <- dbFetch(DiagQuery)
dbClearResult(DiagQuery)
write.csv(Diag, "FILENAME/AllDrugsCodes.csv", quote = FALSE, row.names = FALSE)

csv_to_sqlite(csv_file="FILENAME/medication_ingredient.csv", "FILENAME/Medical.sqlite", "Ingredient", 
              pre_process_size=1000, chunk_size=50000)

conn <- dbConnect(RSQLite::SQLite(),"FILENAME/Medical.sqlite" )
DiagQuery <- dbSendQuery(conn, "SELECT DISTINCT code_system, code FROM Ingredient")
Diag <- dbFetch(DiagQuery)
dbClearResult(DiagQuery)
write.csv(Diag, "FILENAME/AllIngredientsCodes.csv", quote = FALSE, row.names = FALSE)

####Diagnosis table####
csv_to_sqlite(csv_file="FILENAME/diagnosis.csv", "FILENAME/Diag.sqlite", "Diag", 
              pre_process_size=1000, chunk_size=50000)

conn <- dbConnect(RSQLite::SQLite(),"FILENAME/Diag.sqlite" )
DiagQuery <- dbSendQuery(conn, "SELECT DISTINCT code, code_system FROM Diag")
Diag <- dbFetch(DiagQuery)
dbClearResult(DiagQuery)
write.csv(Diag, "FILENAME/AllDiagCodes.csv", quote = FALSE, row.names = FALSE)
