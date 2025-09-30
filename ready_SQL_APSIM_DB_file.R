### How to read in apsim database files.

install.packages(c("DBI", "RSQLite"))
#library(DBI)
#library(RSQLite)


# Replace "path/to/your/database.db" with the actual path to your file
file_path1  <- "N:/work/Riskwise/early_sowing/"

#file_path1  <- "D:/work/Riskwise/early_sowing/" #on my machine
site        <- "Lock"
file_path2  <- "/APSIM_runs/Wheat/"
file_name   <- "Lock_fixed_Pengcheng_HF_rule_test1.db"



con <- dbConnect(SQLite(), paste0(file_path1, site,file_path2, file_name ))
dbListTables(con)
data <- dbGetQuery(con, "SELECT * FROM Daily")
dbDisconnect(con)
rm(con)


names(data)
