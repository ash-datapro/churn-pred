# data/load_data.R



# --- Packages ---
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(DBI)
  library(RPostgres)
})

# --- Configuration ---
db_user = "user"
db_password = "password"
db_host = "localhost"
db_port = 5432
db_name = "churn_db"
csv_file_path = "Desktop/Project/postsales-churn-pred/data/Telco-Customer-Churn.csv"
stopifnot(file.exists(csv_file_path))

# --- Create database connection ---
conn = NULL
tryCatch({
  conn = dbConnect(
    Postgres(),
    user = db_user,
    password = db_password,
    host = db_host,
    port = db_port,
    dbname = db_name
  )
  on.exit({ if (!is.null(conn) && dbIsValid(conn)) dbDisconnect(conn) }, add = TRUE)
}, error = function(e) {
  stop("Error creating DB connection: ", conditionMessage(e))
})

# --- Load CSV data ---
df = NULL
tryCatch({
  df = read_csv(csv_file_path, show_col_types = FALSE, progress = FALSE)
  message("CSV loaded successfully.")
}, error = function(e) {
  stop("Error loading CSV: ", conditionMessage(e))
})

# --- Clean column names (trim any whitespace) ---
names(df) = trimws(names(df))

# --- Rename columns to match the database schema ---
df = df |>
  rename(
    customer_id = customerID,
    senior_citizen = SeniorCitizen,
    phone_service = PhoneService,
    multiple_lines = MultipleLines,
    internet_service = InternetService,
    online_security = OnlineSecurity,
    online_backup = OnlineBackup,
    device_protection = DeviceProtection,
    tech_support = TechSupport,
    streaming_tv = StreamingTV,
    streaming_movies = StreamingMovies,
    paperless_billing = PaperlessBilling,
    monthly_charges = MonthlyCharges,
    total_charges = TotalCharges,
    payment_method = PaymentMethod,
    churn = Churn,
    partner = Partner,
    dependents = Dependents,
    contract = Contract
  )

# --- Trim whitespace in all string columns ---
df = df |>
  mutate(across(where(is.character), ~ trimws(.x)))

# --- Check for missing values ---
missing_counts = sapply(df, function(col) sum(is.na(col)))
message("Missing values per column:")
print(missing_counts)

# --- Convert 'total_charges' to numeric and handle non-numeric issues ---
df$total_charges = suppressWarnings(as.numeric(df$total_charges))
initial_rows = nrow(df)
df = df |>
  filter(!is.na(total_charges))
dropped_rows = initial_rows - nrow(df)
if (dropped_rows > 0) {
  message(sprintf("Dropped %d rows due to non-numeric TotalCharges.", dropped_rows))
}

# --- Optionally, impute or fill missing values for other columns if needed ---
# For example, fill missing values in 'payment_method' with the mode:
# mode_value = df$payment_method[which.max(tabulate(match(df$payment_method, unique(df$payment_method))))]
# df$payment_method = ifelse(is.na(df$payment_method), mode_value, df$payment_method)

# --- Remove duplicate records based on the primary key ('customer_id') ---
initial_rows = nrow(df)
df = df |>
  distinct(customer_id, .keep_all = TRUE)
duplicates_removed = initial_rows - nrow(df)
if (duplicates_removed > 0) {
  message(sprintf("Removed %d duplicate rows based on customer_id.", duplicates_removed))
}

# --- Convert data types ---
if ("senior_citizen" %in% names(df)) {
  df$senior_citizen = as.integer(df$senior_citizen)
}

# --- Insert Data into the Database ---
tryCatch({
  dbWriteTable(conn, "customers", df, append = TRUE, row.names = FALSE)
  message("Data loaded into the 'customers' table successfully.")
}, error = function(e) {
  message("Error inserting data into the database: ", conditionMessage(e))
})

