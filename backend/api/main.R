# backend/main.R

setwd("/Users/ash/Desktop/Project/postsales-churn-pred/backend/api")

# Required packages
suppressPackageStartupMessages({
  library(plumber)
  library(jsonlite)
  library(dplyr)
  library(workflows)  # NEW: to read levels from the workflow
  library(parsnip)    # NEW: predict() method support
})

# --- Load the Trained Model ---
model = NULL
tryCatch({
  model = readRDS("model.rds")
}, error = function(e) {
  model <<- NULL
  message("Error loading the model: ", e$message)
})

# --- Helpers: input validation & feature engineering ---

validate_input = function(body) {
  req = function(name) {
    if (is.null(body[[name]])) stop(paste0("Missing field: ", name))
    body[[name]]
  }
  
  out = list(
    tenure            = as.numeric(req("tenure")),
    monthly_charges   = as.numeric(req("monthly_charges")),
    contract          = as.character(req("contract")),
    gender            = as.character(req("gender")),
    senior_citizen    = as.integer(req("senior_citizen")),
    partner           = as.character(req("partner")),
    dependents        = as.character(req("dependents")),
    internet_service  = as.character(req("internet_service")),
    paperless_billing = as.character(req("paperless_billing")),
    payment_method    = as.character(req("payment_method")),
    services_list     = body[["services_list"]]
  )
  
  if (is.null(out$services_list)) out$services_list = list()
  if (is.na(out$tenure) || out$tenure < 0) stop("tenure must be >= 0")
  if (is.na(out$monthly_charges) || out$monthly_charges < 0) stop("monthly_charges must be >= 0")
  if (!(out$senior_citizen %in% c(0L, 1L))) stop("senior_citizen must be 0 or 1")
  
  out
}

make_feature_df = function(data) {
  d = data
  
  # 1) Derived features
  d$total_charges = d$tenure * d$monthly_charges
  d$avg_charge_per_month = d$total_charges / (d$tenure + 1e-6)
  
  # 2) Tenure bucket
  t = d$tenure
  if (t <= 12) {
    d$tenure_bucket = "0-12"
  } else if (t <= 24) {
    d$tenure_bucket = "12-24"
  } else if (t <= 48) {
    d$tenure_bucket = "24-48"
  } else {
    d$tenure_bucket = "48+"
  }
  
  # 3) Service flags from list
  svc_map = c(
    "Multiple Lines"    = "multiple_lines",
    "Online Security"   = "online_security",
    "Online Backup"     = "online_backup",
    "Device Protection" = "device_protection",
    "Tech Support"      = "tech_support",
    "Streaming TV"      = "streaming_tv",
    "Streaming Movies"  = "streaming_movies"
  )
  for (col in unname(svc_map)) d[[col]] = "No"
  for (svc in d$services_list) {
    key = unname(svc_map[names(svc_map) == as.character(svc)])
    if (length(key) == 1) d[[key]] = "Yes"
  }
  
  # 4) services_count
  d$services_count = sum(vapply(unname(svc_map), function(col) isTRUE(d[[col]] == "Yes"), logical(1)))
  
  # 5) Assemble data frame (column order to match training)
  cols = c(
    "tenure", "monthly_charges", "total_charges", "avg_charge_per_month",
    "services_count", "contract", "gender", "senior_citizen", "partner",
    "dependents", "internet_service", "paperless_billing", "payment_method",
    "multiple_lines", "online_security", "online_backup",
    "device_protection", "tech_support", "streaming_tv", "streaming_movies",
    "tenure_bucket"
  )
  df = as.data.frame(d[cols], stringsAsFactors = FALSE)
  
  # Match training factor levels used in train_model.R
  df$contract = factor(df$contract, levels = c("Month-to-month","One year","Two year"))
  df$gender = factor(df$gender, levels = c("Female","Male"))
  df$partner = factor(df$partner, levels = c("No","Yes"))
  df$dependents = factor(df$dependents, levels = c("No","Yes"))
  df$internet_service = factor(df$internet_service, levels = c("DSL","Fiber optic","No"))
  df$paperless_billing = factor(df$paperless_billing, levels = c("No","Yes"))
  df$payment_method = factor(df$payment_method, levels = c(
    "Electronic check","Mailed check","Bank transfer (automatic)","Credit card (automatic)"
  ))
  df$multiple_lines   = factor(df$multiple_lines,   levels = c("No","Yes"))
  df$online_security  = factor(df$online_security,  levels = c("No","Yes"))
  df$online_backup    = factor(df$online_backup,    levels = c("No","Yes"))
  df$device_protection= factor(df$device_protection,levels = c("No","Yes"))
  df$tech_support     = factor(df$tech_support,     levels = c("No","Yes"))
  df$streaming_tv     = factor(df$streaming_tv,     levels = c("No","Yes"))
  df$streaming_movies = factor(df$streaming_movies, levels = c("No","Yes"))
  df$tenure_bucket    = factor(df$tenure_bucket,    levels = c("0-12","12-24","24-48","48+"))
  df$senior_citizen   = as.integer(df$senior_citizen)
  
  df
}

# --- Robust probability extractor (uses the workflow's class levels) ---
get_positive_proba = function(model, newdata) {
  # 1) Ask the model what its outcome levels are
  lvl = NULL
  try({
    lvl = workflows::extract_fit_parsnip(model)$lvl
  }, silent = TRUE)
  
  # 2) Predict class probabilities the tidymodels way
  pred_prob = tryCatch(
    predict(model, new_data = newdata, type = "prob"),
    error = function(e) e
  )
  if (inherits(pred_prob, "error")) stop(pred_prob$message)
  
  cols = colnames(pred_prob)
  
  # If we know the positive class (second level by convention), use that column
  if (!is.null(lvl) && length(lvl) >= 2) {
    pos_name = paste0(".pred_", lvl[2])
    if (pos_name %in% cols) return(as.numeric(pred_prob[[pos_name]][1]))
  }
  
  # Common fallbacks
  if (".pred_1" %in% cols)   return(as.numeric(pred_prob$.pred_1[1]))
  if (".pred_Yes" %in% cols) return(as.numeric(pred_prob$.pred_Yes[1]))
  if ("Yes" %in% cols)       return(as.numeric(pred_prob[["Yes"]][1]))
  
  # Last resort: take max prob among columns that start with .pred_
  pred_cols = grep("^\\.pred_", cols, value = TRUE)
  if (length(pred_cols) >= 1) {
    row = as.numeric(pred_prob[1, pred_cols, drop = TRUE])
    return(max(row, na.rm = TRUE))
  }
  
  stop("Could not derive probability from model prediction")
}

#* Health check
#* @get /health
function() {
  list(status = "ok")
}

#* Prediction Endpoint
#* @post /predict
function(req, res) {
  if (is.null(model)) {
    res$status = 503
    return(list(detail = "Model not loaded"))
  }
  
  body = tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(body)) {
    res$status = 400
    return(list(detail = "Invalid JSON body"))
  }
  
  data = tryCatch(validate_input(body), error = function(e) e)
  if (inherits(data, "error")) {
    res$status = 400
    return(list(detail = data$message))
  }
  
  df = tryCatch(make_feature_df(data), error = function(e) e)
  if (inherits(df, "error")) {
    res$status = 500
    return(list(detail = paste0("Feature engineering error: ", df$message)))
  }
  
  out = tryCatch({
    proba = get_positive_proba(model, df)
    threshold = 0.5
    prediction = ifelse(proba >= threshold, "Yes", "No")
    list(prediction = prediction, probability = jsonlite::unbox(proba))
  }, error = function(e) e)
  
  if (inherits(out, "error")) {
    res$status = 500
    return(list(detail = paste0("Prediction error: ", out$message)))
  }
  
  out
}
