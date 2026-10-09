# run_all_sql.R  -  one click: Source this file in RStudio.
# 1) loads the 7 original tables into SQL Server (database AuroraMarket)
# 2) builds the order-level dataset and uploads it as order_level_dataset
# Needs .Renviron in this folder with SQLSERVER_UID / SQLSERVER_PWD.

needed <- c("readr", "dplyr", "tidyr", "stringr", "DBI", "odbc")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)

readRenviron(".Renviron")
t0 <- Sys.time()
message("\n==== 1/2  Loading the 7 original tables ====")
source("R/00_load_to_sqlserver.R")
message("\n==== 2/2  Building and uploading order_level_dataset ====")
source("R/03_build_order_dataset.R")

con <- sql_connect("AuroraMarket")
message("\n==== Tables now in AuroraMarket ====")
print(dbGetQuery(con, "SELECT t.name AS table_name, SUM(p.rows) AS rows
                       FROM sys.tables t JOIN sys.partitions p ON p.object_id = t.object_id AND p.index_id IN (0,1)
                       GROUP BY t.name ORDER BY t.name"))
dbDisconnect(con)
message(sprintf("\nDONE in %.1f minutes", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
