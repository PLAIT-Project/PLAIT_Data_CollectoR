setwd("D:/PLAIT_Data_CollectoR")

# install.packages("RSQLite")
library(RSQLite)
# install.packages("sqldf")
library(sqldf)
# install.packages("RODBC")
library(RODBC)
# The easiest way to get dplyr is to install the whole tidyverse:
# install.packages("tidyverse")
library(tidyr)
# Alternatively, install just dplyr:
# install.packages("dplyr")
library(dplyr)
library(purrr)
# if dplyr makes some trouble --> https://github.com/tidyverse/dplyr/issues/5214#issuecomment-810636358
library(jsonlite)
#install.packages("rjson")
library(rjson)
#install.packages("writexl")
library("writexl")
#install.packages("openxlsx")
library(openxlsx)
#install.packages("xlsx")
library("xlsx")

# 1. Read Django Database (SQLite)
# see: https://db.rstudio.com/databases/sqlite/
# see: https://cran.r-project.org/web/packages/RSQLite/vignettes/RSQLite.html
# see: https://www.datacamp.com/community/tutorials/sqlite-in-r

# Create a connection to our database
conn2DjangoDB <- dbConnect(RSQLite::SQLite(), "C:/inetpub/wwwroot/project_plait_0/db.sqlite3")
# List all the tables available in the database
Django_tables_listed <- dbListTables(conn2DjangoDB)
# relevant tables are "ticket_ticket" and "auth_user"
Django_tables_listed

# 2. Extract Django Database to a data frame 
## a) ticket_ticket
tickets_db <- tbl(conn2DjangoDB, "ticket_ticket")
tickets_db_df <- as.data.frame(tickets_db)
View(tickets_db_df)
## b) users_profile
user_db <- tbl(conn2DjangoDB, "auth_user")
user_db_df <- as.data.frame(user_db)
View(user_db_df)

# for safety reasons: close connection
dbDisconnect(conn2DjangoDB)

# 3. Create a human-friendly, information-rich table
# Create new, empty data frame
PLAITdataCollectoR <- data.frame()
# Begin filling it with ticket_ticket and auth_users columns
## a) ticket_ticket.id as TicketPK (PK for Primary Key)
PLAITdataCollectoR <- data.frame(tickets_db_df$id)
colnames(PLAITdataCollectoR)[1] <- "TicketPK"
## b) ticket_ticket.ticket_id as TicketName
PLAITdataCollectoR[2] <- data.frame(tickets_db_df$ticket_id)
colnames(PLAITdataCollectoR)[2] <- "TicketName"
## c) ticket_ticket.user_diagnosis as UserDiagnosis
PLAITdataCollectoR[3] <- data.frame(tickets_db_df$user_diagnosis)
colnames(PLAITdataCollectoR)[3] <- "UserDiagnosis"
## d) ticket_ticket.ticket_id as UserID
PLAITdataCollectoR[4] <- data.frame(tickets_db_df$initiator_id)
colnames(PLAITdataCollectoR)[4] <- "UserID"
## e) auth_users.username as UserName where initiatorID == userID
user_id2name <- data.frame(tickets_db_df$initiator_id)
colnames(user_id2name) <- "ID"
user_id2name[2] <- user_db_df[ match(user_id2name[['ID']], user_db_df[['id']]), 'username']
colnames(user_id2name)[2] <- "UserName"
#View(user_id2name)
PLAITdataCollectoR[5] <- data.frame(user_id2name$UserName)
colnames(PLAITdataCollectoR)[5] <- "UserName"
## f) ticket_ticket.date_submitted as TicketSubmittedAt
PLAITdataCollectoR[6] <- data.frame(tickets_db_df$date_submitted)
colnames(PLAITdataCollectoR)[6] <- "TicketSubmittedAt"
## g) ticket_ticket.file_uploaded as UploadedFilePath
PLAITdataCollectoR[7] <- data.frame(tickets_db_df$file_uploaded)
colnames(PLAITdataCollectoR)[7] <- "UploadedFilePath"
## h) Extract filename and file type
file_name_extractoR <- data.frame(tickets_db_df$file_uploaded)
colnames(file_name_extractoR)[1] <- "UploadedFilePath"
#View(file_name_extractoR)
full_file_name_extractoR_tmp <- cbind(file_name_extractoR %>% separate(UploadedFilePath, c("ParentFolder","FullFileName"), "/"))
#View(full_file_name_extractoR_tmp)
full_file_name_extractoR_tmp_tmp <- data.frame(full_file_name_extractoR_tmp$FullFileName)
colnames(full_file_name_extractoR_tmp_tmp) <- "FullFileName"
#View(full_file_name_extractoR_tmp_tmp)
file_type_extractoR_tmp <- data.frame(do.call("rbind", strsplit(as.character(full_file_name_extractoR_tmp_tmp$FullFileName), ".", fixed = TRUE)))
colnames(file_type_extractoR_tmp) <- c("FileName", "FileType")
#View(file_type_extractoR_tmp)
file_name_type_extractoR <- cbind(file_name_extractoR, 
                                  full_file_name_extractoR_tmp$ParentFolder, 
                                  full_file_name_extractoR_tmp$FullFileName, 
                                  file_type_extractoR_tmp$FileName, 
                                  tolower(file_type_extractoR_tmp$FileType))
colnames(file_name_type_extractoR) <- c("UploadedFilePath", 
                                         "ParentFolder", 
                                         "FullFileName", 
                                         "FileName", 
                                         "FileType")
#View(file_name_type_extractoR)
### I) file name
PLAITdataCollectoR[8] <- data.frame(file_name_type_extractoR$FileName)
colnames(PLAITdataCollectoR)[8] <- "FileName"
### II) file type
PLAITdataCollectoR[9] <- data.frame(file_name_type_extractoR$FileType)
colnames(PLAITdataCollectoR)[9] <- "FileType"
### III) full file name
PLAITdataCollectoR[10] <- data.frame(file_name_type_extractoR$FullFileName)
colnames(PLAITdataCollectoR)[10] <- "FullFileName"
## i) LRN exists
lrn_path <- "C:/inetpub/wwwroot/project_plait_0/files/lrn_files"
lrn_files <- list.files(path=lrn_path)
lrn_files
lrn_files_extracted_names <- data.frame(do.call("rbind", strsplit(as.character(lrn_files), ".", fixed = TRUE)))
colnames(lrn_files_extracted_names) <- c("FileName", "FileType")
View(lrn_files_extracted_names)
lrn_exists <- file_name_type_extractoR$FileName %in% lrn_files_extracted_names$FileName
lrn_exists
PLAITdataCollectoR[11] <- data.frame(lrn_exists)
colnames(PLAITdataCollectoR)[11] <- "LRNexists"
## j) Error exists
err_path <- "C:/inetpub/wwwroot/project_plait_0/files/err_files_sent"
err_files <- list.files(path=err_path)
err_files
err_files_extracted_names <- data.frame(do.call("rbind", strsplit(as.character(err_files), ".", fixed = TRUE)))
colnames(err_files_extracted_names) <- c("FileName", "FileType")
View(err_files_extracted_names)
err_exists <- file_name_type_extractoR$FileName %in% err_files_extracted_names$FileName
err_exists
PLAITdataCollectoR[12] <- data.frame(err_exists)
colnames(PLAITdataCollectoR)[12] <- "Errexists"
## k) JSON exists
sol_path <- "C:/inetpub/wwwroot/project_plait_0/files/sent_files"
sol_files <- list.files(path = sol_path)
sol_files
sol_files_extracted_names <- data.frame(do.call("rbind", strsplit(as.character(sol_files), ".", fixed = TRUE)))
colnames(sol_files_extracted_names) <- c("FileName", "FileType")
View(sol_files_extracted_names)
sol_exists <- file_name_type_extractoR$FileName %in% sol_files_extracted_names$FileName
sol_exists
PLAITdataCollectoR[13] <- data.frame(sol_exists)
colnames(PLAITdataCollectoR)[13] <- "JSONexists"
## l) read JSON file containing diagnosis
sol_files <- list.files(path = sol_path, pattern = "*.json", full.names = TRUE) 
sol_files
json_collectoR_full_file_names <- data.frame(do.call("rbind", strsplit(as.character(sol_files), "/", fixed = TRUE)))
json_collectoR_full_file_names <- json_collectoR_full_file_names[ -c(1:6) ]
colnames(json_collectoR_full_file_names) <- "FullFileName"
json_collectoR_full_file_names
json_collectoR_file_name_type <- data.frame(do.call("rbind", strsplit(as.character(json_collectoR_full_file_names$FullFileName), ".", fixed = TRUE)))
colnames(json_collectoR_file_name_type) <- c("FileName", "FileType")
json_collectoR_file_name_type
myJSON <- lapply(sol_files, function(x) fromJSON(file = x)) 
#View(myJSON)
myJSON[[1]]
json_df <- data.frame(matrix(unlist(myJSON), nrow = length(myJSON), byrow = TRUE), stringsAsFactors = FALSE)
colnames(json_df) <- c("PLAITDiagnosis", "PLAITProbability")
json_df[3] <- paste(json_df$PLAITDiagnosis, json_df$PLAITProbability)
colnames(json_df)[3] <- "PLAITDiagProb"
#View(json_df)
json_collectoR <- cbind(#json_collectoR_full_file_names$FullFileName, 
                        json_collectoR_file_name_type$FileName,
                        #json_collectoR_file_name_type$FileType,
                        json_df$PLAITDiagnosis,
                        json_df$PLAITProbability,
                        json_df$PLAITDiagProb)
colnames(json_collectoR) <- c(#"FullFileName", 
                              "FileName", 
                              #"FileType", 
                              "PLAITDiagnosis", 
                              "PLAITProbability", 
                              "PLAITDiagProb")
View(json_collectoR)

PLAITdataCollectoR <- merge(PLAITdataCollectoR, json_collectoR, by = "FileName", all=TRUE)
# m) LRN path
lrn_exists_yn <- c("TRUE", "FALSE")
set_lrn_path <- c("C:/inetpub/wwwroot/project_plait_0/files/lrn_files/", "Check for Error")
lrn_path_df <- data.frame(lrn_exists_yn, set_lrn_path)
colnames(lrn_path_df) <- c("LRNexists", "LRNPath")
lrn_path_df
PLAITdataCollectoR <- merge(PLAITdataCollectoR, lrn_path_df, by = "LRNexists", all=TRUE)
# Re-order data frame
col_order <- c("TicketPK",
               "FileName",
               "FullFileName", 
               "LRNexists",
               "JSONexists",
               "Errexists",
               "UserDiagnosis",
               "PLAITDiagnosis",
               "PLAITProbability",
               "PLAITDiagProb",
               "TicketName",
               "UserID",
               "UserName",
               "TicketSubmittedAt",
               "UploadedFilePath",
               "LRNPath",
               "FileType"
               )
PLAITdataCollectoR <- PLAITdataCollectoR[, col_order]
View(PLAITdataCollectoR)
# Export to XLS
xls_name <- paste0("D:/PLAIT_Data_CollectoR/data_as_XLS/PLAITdataCollectoR_", Sys.Date() , ".xlsx" )
xls_name
write_xlsx(PLAITdataCollectoR, xls_name)
# Export to CSV
csv_name <- paste0("D:/PLAIT_Data_CollectoR/data_as_CSV/PLAITdataCollectoR_", Sys.Date() , ".csv" )
csv_name
write.csv(PLAITdataCollectoR, csv_name, row.names = FALSE)

# Export to SQLite DB
conn2collectoR <- dbConnect(RSQLite::SQLite(), "PLAITdataCollectoR.db")
dbWriteTable(conn2collectoR, "PLAITdataCollectoR", PLAITdataCollectoR)

# List all the Tables
dbListTables(conn2collectoR)

# Examples: 
dbGetQuery(conn2collectoR, "SELECT * FROM Cars_and_Makes")
dbGetQuery(conn, "SELECT * FROM cars_data LIMIT 10")
dbGetQuery(conn2collectoR,"SELECT car_names, hp, cyl FROM cars_data WHERE cyl = 8")
dbGetQuery(conn2collectoR,"SELECT car_names, hp, cyl FROM cars_data WHERE car_names LIKE 'M%' AND cyl IN (6,8)")
dbGetQuery(conn2collectoR,"SELECT cyl, AVG(hp) AS 'average_hp', AVG(mpg) AS 'average_mpg' FROM cars_data GROUP BY cyl ORDER BY average_hp")
## Parameterised Queries:
mpg <-  18
cyl <- 6
Result <- dbGetQuery(conn2collectoR, 'SELECT car_names, mpg, cyl FROM cars_data WHERE mpg >= ? AND cyl >= ?', params = c(mpg,cyl))
Result



# Close the database connection
dbDisconnect(conn2collectoR)
