# sql_connect.R
# Shared SQL Server connection helper (SQL Server authentication).
# Credentials come from .Renviron in the project root (git-ignored):
#   SQLSERVER_UID=...
#   SQLSERVER_PWD=...
#   SQLSERVER_SERVER=...   (optional; otherwise common local instance names are tried)
# The first driver/server combination that works is remembered for the session.

suppressPackageStartupMessages({ library(DBI); library(odbc) })

sql_connect <- function(db = "master") {
  uid <- Sys.getenv("SQLSERVER_UID"); pwd <- Sys.getenv("SQLSERVER_PWD")
  if (uid == "" || pwd == "") {
    readRenviron(".Renviron")   # in case R was not restarted after creating the file
    uid <- Sys.getenv("SQLSERVER_UID"); pwd <- Sys.getenv("SQLSERVER_PWD")
  }
  if (uid == "" || pwd == "") stop("SQLSERVER_UID / SQLSERVER_PWD not found in .Renviron")

  open <- function(drv, srv) dbConnect(odbc(), Driver = drv, Server = srv, Database = db,
                                       UID = uid, PWD = pwd, TrustServerCertificate = "Yes",
                                       timeout = 10)
  found <- getOption("aurora.sql")
  if (!is.null(found)) return(open(found$driver, found$server))

  installed <- unique(odbcListDrivers()$name)
  preferred <- c("ODBC Driver 18 for SQL Server", "ODBC Driver 17 for SQL Server",
                 "ODBC Driver 13 for SQL Server", "SQL Server Native Client 11.0", "SQL Server")
  drivers <- intersect(preferred, installed)
  if (length(drivers) == 0) stop("No SQL Server ODBC driver installed. Installed drivers: ",
                                 paste(installed, collapse = ", "))
  servers <- if (Sys.getenv("SQLSERVER_SERVER") != "") Sys.getenv("SQLSERVER_SERVER") else
    c("localhost", "localhost\\SQLEXPRESS", "localhost\\MSSQLSERVER", "(local)")

  errors <- character()
  for (d in drivers) for (s in servers) {
    con <- tryCatch(open(d, s), error = function(e) {
      errors <<- c(errors, sprintf("  - %s @ %s: %s", d, s, conditionMessage(e))); NULL })
    if (!is.null(con)) {
      options(aurora.sql = list(driver = d, server = s))
      message(sprintf("Connected to SQL Server '%s' with driver '%s'", s, d))
      return(con)
    }
  }
  stop("Could not connect to SQL Server. Attempts:\n", paste(errors, collapse = "\n"))
}

# SQL column types for a data frame (ids short, long text as max, dates as datetime2, flags as bit)
sql_types <- function(df, short_text = character()) {
  t <- vapply(df, function(x) {
    if (inherits(x, "POSIXct")) "datetime2(0)"
    else if (is.logical(x))     "bit"
    else if (is.integer(x))     "int"
    else if (is.numeric(x))     "float"
    else if (max(nchar(x), 0, na.rm = TRUE) <= 64) "nvarchar(64)"
    else                        "nvarchar(max)"
  }, character(1))
  t[intersect(short_text, names(t))] <- "nvarchar(64)"
  t
}
