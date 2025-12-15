setwd("~/Desktop/Project/postsales-churn-pred/backend/api")
library(plumber)
pr = plumb("main.R")

pr$run(host = "0.0.0.0", port = 8000)

library(httr)
library(jsonlite)

url = "http://127.0.0.1:8000/predict"
body = list(
  tenure = 15,
  monthly_charges = 70.5,
  contract = "Month-to-month",
  gender = "Female",
  senior_citizen = 0,
  partner = "No",
  dependents = "No",
  internet_service = "Fiber optic",
  paperless_billing = "Yes",
  payment_method = "Electronic check",
  services_list = c("Multiple Lines","Streaming TV","Online Backup")
)

res = POST(url,
           body = toJSON(body, auto_unbox = TRUE),
           encode = "raw",
           add_headers("Content-Type" = "application/json")
)
status_code(res)
content(res, "parsed", simplifyVector = TRUE)

setwd("~/Desktop/Project/postsales-churn-pred/frontend")

library(shiny)

Sys.setenv(DB_HOST = "localhost")
Sys.setenv(DB_PORT = "5432")
Sys.setenv(DB_USER = "user")
Sys.setenv(DB_PASSWORD = "password")
Sys.setenv(DB_NAME = "churn_db")

#Sys.setenv(PREDICT_API_URL = "http://127.0.0.1:8000/predict")
shiny::runApp(".", host = "0.0.0.0", port = 8501)

Sys.getenv("PREDICT_API_URL")  # should show http://127.0.0.1:8000/predict

