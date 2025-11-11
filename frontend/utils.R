# utils_db.R (for example)

library(DBI)
library(RPostgres)

# Read environment variables (with defaults)
DB_USER     = Sys.getenv("DB_USER", "user")
DB_PASSWORD = Sys.getenv("DB_PASSWORD", "password")
DB_HOST     = Sys.getenv("DB_HOST", "postgres")
DB_PORT     = Sys.getenv("DB_PORT", "5432")
DB_NAME     = Sys.getenv("DB_NAME", "churn_db")

get_db_connection = function() {
  dbConnect(
    RPostgres::Postgres(),
    dbname   = DB_NAME,
    host     = DB_HOST,
    port     = as.integer(DB_PORT),
    user     = DB_USER,
    password = DB_PASSWORD
  )
}

load_data = function() {
  con = get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)
  
  df = dbGetQuery(con, "SELECT * FROM customers")
  df
}
