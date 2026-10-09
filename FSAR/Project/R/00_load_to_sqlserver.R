# 00_load_to_sqlserver.R
# Loads the 7 ORIGINAL Aurora Market tables, unchanged, into SQL Server
# (database AuroraMarket). Run from the Project.Rproj root.
# Packages: DBI, odbc, readr, dplyr. Connection: R/sql_connect.R (+ .Renviron)

suppressPackageStartupMessages({ library(readr); library(dplyr) })
source("R/sql_connect.R")
database <- "AuroraMarket"

# ---- 1. Create the database if needed ---------------------------------------
con <- sql_connect("master")
dbExecute(con, sprintf("IF DB_ID('%s') IS NULL CREATE DATABASE [%s];", database, database))
dbDisconnect(con)
con <- sql_connect(database)

# ---- 2. Read each CSV with explicit types (timestamps as UTC date-times) -----
dt <- col_datetime(format = "")
spec <- list(
  customers = cols(.default = col_character()),
  orders = cols(.default = col_character(), purchase_timestamp = dt, approval_timestamp = dt,
                carrier_timestamp = dt, delivery_timestamp = dt, estimated_delivery_timestamp = dt),
  order_items = cols(.default = col_character(), item_number = col_integer(),
                     shipping_deadline = dt, item_price = col_double(), freight_cost = col_double()),
  products = cols(.default = col_double(), product_id = col_character(), product_category = col_character()),
  sellers = cols(.default = col_character()),
  payments = cols(.default = col_character(), payment_sequence = col_integer(),
                  installments = col_integer(), payment_amount = col_double()),
  customer_feedback = cols(.default = col_character(), rating = col_integer(),
                           has_title = col_logical(), has_comment = col_logical(),
                           comment_length = col_integer(),
                           feedback_created_timestamp = dt, feedback_answered_timestamp = dt))

# Primary keys where the data supports one (order_items has duplicate rows, so none there)
keys <- list(customers = "customer_record_id", orders = "order_id", products = "product_id",
             sellers = "seller_id", payments = c("order_id", "payment_sequence"),
             customer_feedback = c("feedback_id", "order_id"), order_items = NULL)

# ---- 3. Write each table ------------------------------------------------------
for (t in names(spec)) {
  df <- read_csv(file.path("Data", paste0(t, ".csv")), col_types = spec[[t]], na = c("", "NA"))
  types <- sql_types(df)
  dbWriteTable(con, t, sql_safe(df), overwrite = TRUE, field.types = types)
  k <- keys[[t]]
  if (!is.null(k)) {
    for (col in k) dbExecute(con, sprintf("ALTER TABLE [%s] ALTER COLUMN [%s] %s NOT NULL;", t, col, types[[col]]))
    dbExecute(con, sprintf("ALTER TABLE [%s] ADD CONSTRAINT [PK_%s] PRIMARY KEY (%s);",
                           t, t, paste0("[", k, "]", collapse = ", ")))
  }
  n <- dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM [%s]", t))$n
  message(sprintf("%-18s %7s rows in CSV | %7s rows in SQL %s", t, format(nrow(df), big.mark = ","),
                  format(n, big.mark = ","), if (n == nrow(df)) "OK" else "MISMATCH"))
}
dbDisconnect(con)
