# backend/api/train_model.R

# ── Packages ───────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(recipes)
  library(rsample)
  library(parsnip)
  library(workflows)
  library(tune)
  library(dials)
  library(yardstick)
  library(ggplot2)
  library(purrr)
  library(readr)
})

# ── 0) Database Connection (matches load-data.R) ───────────────────────────────
db_user = "user"
db_password = "password"
db_host = "localhost"
db_port = 5432
db_name = "churn_db"

con = NULL
tryCatch({
  con = dbConnect(
    Postgres(),
    user = db_user,
    password = db_password,
    host = db_host,
    port = db_port,
    dbname = db_name
  )
  on.exit({ if (!is.null(con) && dbIsValid(con)) dbDisconnect(con) }, add = TRUE)
}, error = function(e) {
  stop("Error creating DB connection: ", conditionMessage(e))
})

# ── 1) Load & Feature–Engineer ────────────────────────────────────────────────
load_and_engineer = function() {
  df = dbGetQuery(con, "SELECT * FROM customers")
  
  # Normalize churn to factor 0/1 (Yes/No in Telco)
  df = df |>
    mutate(
      churn = str_to_lower(as.character(churn)),
      churn = recode(churn, "yes" = "1", "no" = "0"),
      churn = factor(churn, levels = c("0","1")),
      total_charges = suppressWarnings(as.numeric(total_charges))
    )
  
  # Base/features you loaded in load-data.R (present in Telco)
  base = c(
    "tenure","monthly_charges","total_charges",
    "contract","gender","senior_citizen","partner","dependents",
    "internet_service","multiple_lines","online_security","online_backup",
    "device_protection","tech_support","streaming_tv","streaming_movies",
    "paperless_billing","payment_method"
  )
  
  # Keep only needed columns (+ target), require a few key fields present
  df = df |>
    select(any_of(c(base, "churn"))) |>
    drop_na(tenure, monthly_charges, total_charges, contract, internet_service)
  
  # Engineered features (handle "Yes"/"No" strings; ignore "No internet service")
  svc_cols = c(
    "multiple_lines","online_security","online_backup",
    "device_protection","tech_support","streaming_tv","streaming_movies"
  )
  
  df = df |>
    mutate(
      avg_charge_per_month = total_charges / (tenure + 1e-3),
      services_count = pmap_int(across(all_of(svc_cols)), function(...) {
        vals = str_to_lower(as.character(c(...)))
        sum(vals == "yes", na.rm = TRUE)
      }),
      tenure_bucket = cut(
        tenure,
        breaks = c(-Inf, 12, 24, 48, Inf),
        labels = c("0-12","12-24","24-48","48+")
      )
    )
  
  return(df)
}

# ── 2) Preprocess (recipes) ───────────────────────────────────────────────────
build_recipe = function(df_train) {
  num_feats = c("tenure","monthly_charges","total_charges",
                "avg_charge_per_month","services_count")
  cat_feats = c("contract","gender","senior_citizen","partner","dependents",
                "internet_service","paperless_billing","payment_method",
                "tenure_bucket")
  
  recipe(churn ~ ., data = df_train) |>
    # no step_select(): we already curated columns upstream
    step_impute_median(all_numeric_predictors()) |>
    step_impute_mode(all_nominal_predictors()) |>
    step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
    step_normalize(all_numeric_predictors())
}

# ── 3) Train + Randomized Tuning (xgboost GBM analogue) ───────────────────────
train_and_tune = function(df_train) {
  rec = build_recipe(df_train)
  
  model_spec = boost_tree(
    trees       = tune(),   # n_estimators
    learn_rate  = tune(),   # learning_rate
    tree_depth  = tune(),   # max_depth
    sample_size = tune(),   # subsample
    min_n       = tune()    # min leaf size
  ) |>
    set_engine("xgboost") |>
    set_mode("classification")
  
  wf = workflow() |>
    add_model(model_spec) |>
    add_recipe(rec)
  
  set.seed(42)
  folds = vfold_cv(df_train, v = 5, strata = churn)
  
  grid = grid_random(
    finalize(trees(), df_train),              # yields a reasonable trees range
    learn_rate(range = c(0.01, 0.1)),
    tree_depth(range = c(3L, 7L)),
    sample_prop(range = c(0.6, 1.0)),
    min_n(range = c(1L, 10L)),
    size = 20
  )
  
  ctrl = control_grid(save_pred = TRUE, verbose = TRUE, allow_par = TRUE)
  metrics = metric_set(roc_auc)
  
  message("🔍 Tuning Gradient Boosting...")
  tuned = tune_grid(
    wf,
    resamples = folds,
    grid = grid,
    metrics = metrics,
    control = ctrl
  )
  
  best = select_best(tuned, metric = "roc_auc")
  best_mean = tryCatch({
    show_best(tuned, metric = "roc_auc", n = 1)$mean[[1]]
  }, error = function(e) NA_real_)
  if (!is.na(best_mean)) {
    message(sprintf("▶️ Best CV ROC AUC: %.4f", best_mean))
  }
  
  wf_final = finalize_workflow(wf, best)
  fit(wf_final, data = df_train)
}

# ── 4) Threshold Optimization (maximize F1) ───────────────────────────────────
find_best_threshold = function(y_true, y_prob) {
  thrs = seq(0.1, 0.9, length.out = 81)
  f1s = map_dbl(thrs, function(t) {
    preds = ifelse(y_prob >= t, "1", "0")
    f_meas_vec(
      truth = y_true,
      estimate = factor(preds, levels = c("0","1")),
      event_level = "second"
    )
  })
  best_idx = which.max(f1s)
  list(threshold = thrs[best_idx], f1 = f1s[best_idx])
}

# ── 5) Evaluate & Report ─────────────────────────────────────────────────────
evaluate_and_report = function(fitted, df_test, threshold) {
  dir.create("reports", showWarnings = FALSE, recursive = TRUE)
  
  probs = predict(fitted, df_test, type = "prob")$.pred_1
  preds = ifelse(probs >= threshold, "1", "0")
  truth = df_test$churn
  pred_factor = factor(preds, levels = c("0","1"))
  
  # Vector metrics (positive class = "1")
  prec = precision_vec(truth = truth, estimate = pred_factor, event_level = "second")
  rec  = recall_vec(   truth = truth, estimate = pred_factor, event_level = "second")
  f1v  = f_meas_vec(   truth = truth, estimate = pred_factor, event_level = "second")
  
  cr_tbl = tibble::tibble(
    metric = c("precision", "recall", "f1"),
    value  = c(prec, rec, f1v)
  )
  
  write_lines(
    paste(capture.output(print(cr_tbl, n = Inf, width = Inf)), collapse = "\n"),
    "reports/classification_report.txt"
  )
  
  # Confusion Matrix
  cm = conf_mat(
    data.frame(truth, pred = pred_factor),
    truth = truth, estimate = pred
  )
  cm_df = as.data.frame(cm$table)
  p_cm = ggplot(cm_df, aes(x = Prediction, y = Truth, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq)) +
    scale_x_discrete(labels = c("0" = "Stay", "1" = "Churn")) +
    scale_y_discrete(labels = c("0" = "Stay", "1" = "Churn")) +
    labs(x = "Predicted", y = "Actual", title = "Confusion Matrix") +
    theme_minimal()
  ggsave("reports/confusion_matrix.png", p_cm, width = 6, height = 5, dpi = 120)
  
  # ROC
  roc_df = roc_curve(
    data.frame(truth, .pred_1 = probs),
    truth = truth, .pred_1, event_level = "second"
  )
  p_roc = ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
    geom_path() + geom_abline(linetype = "dashed") +
    coord_equal() + theme_minimal() +
    labs(title = "ROC Curve")
  ggsave("reports/roc_curve.png", p_roc, width = 6, height = 5, dpi = 120)
  
  auc = roc_auc_vec(truth = truth, estimate = probs, event_level = "second")
  
  md = paste0(
    "# Gradient Boosting Churn Model Report\n\n",
    "**Test ROC AUC:** ", sprintf("%.4f", auc), "  \n",
    "**Test F1 (@thr=", sprintf("%.2f", threshold), "):** ",
    sprintf("%.3f", f1v), "\n\n",
    "## Classification Report\n\n",
    paste(capture.output(print(cr_tbl, n = Inf, width = Inf)), collapse = "\n"),
    "\n\n",
    "![Confusion Matrix](confusion_matrix.png)  ",
    "![ROC Curve](roc_curve.png)\n"
  )
  write_lines(md, "reports/model_report.md")
}

# ── 6) Main ───────────────────────────────────────────────────────────────────
set.seed(42)

df = load_and_engineer()

split = initial_split(df, prop = 0.8, strata = churn)
train = training(split)
test  = testing(split)

fitted = train_and_tune(train)

message("🔧 Finding best classification threshold...")
probs_test = predict(fitted, test, type = "prob")$.pred_1
thr_info = find_best_threshold(test$churn, probs_test)
message(sprintf("👉 Best threshold = %.2f, F1 = %.3f",
                thr_info$threshold, thr_info$f1))

message("📊 Generating reports...")
evaluate_and_report(fitted, test, thr_info$threshold)

saveRDS(fitted, file = "model.rds")
message("✅ Done. Model and reports are saved.")
