library(shiny)
library(plotly)
library(dplyr)
library(stats)

# Reuse the dark theming helper from plots.R
# apply_dark(p)

insights_ui = function(id) {
  ns = NS(id)
  tagList(
    h2("⬆️ Insights"),
    # Small toolbar
    fluidRow(
      column(6, tags$span(class = "badge-dark", "Interactive KPIs & relationships")),
      column(6, div(style = "text-align:right;",
                    actionButton(ns("refresh_insights"), "↻ Recompute (sampled)")))
    ),
    br(),
    # Row 1: Correlation + Contract vs Churn
    fluidRow(
      column(
        6,
        h4("Correlation (numeric)"),
        plotlyOutput(ns("correlation"), height = "380px"),
        uiOutput(ns("corr_note"))
      ),
      column(
        6,
        h4("Churn by Contract"),
        plotlyOutput(ns("contract_churn"), height = "380px")
      )
    ),
    br(),
    # Row 2: Tenure vs Charges + Payment Method
    fluidRow(
      column(
        6,
        h4("Tenure vs Monthly Charges"),
        plotlyOutput(ns("tenure_vs_charges"), height = "380px")
      ),
      column(
        6,
        h4("Payment Method Breakdown"),
        plotlyOutput(ns("payment_donut"), height = "380px")
      )
    ),
    br(),
    # Row 3: Demographics tiles
    fluidRow(
      column(6, h4("Gender vs Churn"), plotlyOutput(ns("gender_churn"), height = "320px")),
      column(6, h4("Senior Citizen vs Churn"), plotlyOutput(ns("senior_churn"), height = "320px"))
    ),
    br(),
    # Optional Feature Importance (if model available)
    fluidRow(
      column(
        12,
        h4("Feature Importance (quick permutation on sample)"),
        tags$small(class = "text-muted",
                   "Computed on a small sample for responsiveness; treat as directional."),
        plotlyOutput(ns("feature_importance"), height = "380px")
      )
    )
  )
}

insights_server = function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Sample the incoming data for faster interactivity
    sampled = reactive({
      df = data()
      if (nrow(df) > 3000) df = df[sample(seq_len(nrow(df)), 3000), , drop = FALSE]
      df
    })
    observeEvent(input$refresh_insights, { invisible(sampled()) })
    
    # ---- Correlation heatmap (numeric columns only) ----
    output$correlation = renderPlotly({
      df = sampled()
      if (is.null(df) || !nrow(df)) return(NULL)
      
      num_df = df |> select(where(is.numeric))
      if (ncol(num_df) < 2) return(NULL)
      
      cor_mat = tryCatch(cor(num_df, use = "pairwise.complete.obs"), error = function(e) NULL)
      if (is.null(cor_mat)) return(NULL)
      
      p = plot_ly(
        x = colnames(cor_mat), y = rownames(cor_mat),
        z = cor_mat, type = "heatmap", colorscale = "Viridis",
        hovertemplate = "X: %{x}<br>Y: %{y}<br>ρ: %{z:.2f}<extra></extra>"
      )
      apply_dark(p)
    })
    
    output$corr_note = renderUI({
      df = sampled()
      num_cols = names(df)[sapply(df, is.numeric)]
      if (length(num_cols) >= 2) {
        HTML(sprintf("<span class='text-muted'>Using %d numeric features.</span>", length(num_cols)))
      } else {
        HTML("<span class='text-muted'>Need at least two numeric columns to compute correlations.</span>")
      }
    })
    
    # ---- Churn by Contract (normalized) ----
    output$contract_churn = renderPlotly({
      df = sampled()
      req("contract" %in% names(df), "churn" %in% names(df))
      tmp = df |>
        mutate(churn = tolower(as.character(churn))) |>
        mutate(churn = ifelse(churn %in% c("yes","1","true"), "Yes", "No")) |>
        count(contract, churn) |>
        group_by(contract) |>
        mutate(pct = n / sum(n) * 100) |>
        ungroup()
      
      p = plot_ly(
        tmp, x = ~contract, y = ~pct, color = ~churn, type = "bar",
        hovertemplate = "Contract: %{x}<br>Churn: %{color}<br>%{y:.1f}%%<extra></extra>"
      ) |>
        layout(barmode = "stack", yaxis = list(title = "Percent"))
      apply_dark(p)
    })
    
    # ---- Tenure vs Monthly Charges (scatter w/ smooth) ----
    output$tenure_vs_charges = renderPlotly({
      df = sampled()
      req(all(c("tenure", "monthly_charges") %in% names(df)))
      # loess smooth (quietly fail if not enough rows)
      fit_y = tryCatch({
        if (nrow(df) >= 10) predict(loess(monthly_charges ~ tenure, data = df))
      }, error = function(e) NULL)
      
      p = plot_ly(df, x = ~tenure, y = ~monthly_charges, type = "scatter", mode = "markers",
                  opacity = 0.6, marker = list(size = 6),
                  hovertemplate = "Tenure: %{x}<br>Monthly: $%{y:.2f}<extra></extra>")
      if (!is.null(fit_y)) {
        p = add_lines(p, x = df$tenure, y = fit_y, name = "Smooth", hoverinfo = "skip")
      }
      apply_dark(p)
    })
    
    # ---- Payment method donut ----
    output$payment_donut = renderPlotly({
      df = sampled()
      req("payment_method" %in% names(df))
      agg = df |> count(payment_method) |> arrange(desc(n))
      p = plot_ly(
        agg, labels = ~payment_method, values = ~n, type = "pie",
        hole = 0.45, sort = FALSE,
        textinfo = "label+percent",
        hovertemplate = "%{label}<br>Count: %{value}<extra></extra>"
      )
      apply_dark(p)
    })
    
    # ---- Gender vs Churn ----
    output$gender_churn = renderPlotly({
      df = sampled()
      req(all(c("gender","churn") %in% names(df)))
      tmp = df |>
        mutate(churn = tolower(as.character(churn)),
               churn = ifelse(churn %in% c("yes","1","true"), "Yes", "No")) |>
        count(gender, churn)
      p = plot_ly(tmp, x = ~gender, y = ~n, color = ~churn, type = "bar",
                  hovertemplate = "Gender: %{x}<br>Churn: %{color}<br>Count: %{y}<extra></extra>") |>
        layout(barmode = "stack")
      apply_dark(p)
    })
    
    # ---- Senior Citizen vs Churn ----
    output$senior_churn = renderPlotly({
      df = sampled()
      req(all(c("senior_citizen","churn") %in% names(df)))
      tmp = df |>
        mutate(senior = ifelse(as.integer(senior_citizen) == 1, "Yes", "No"),
               churn = tolower(as.character(churn)),
               churn = ifelse(churn %in% c("yes","1","true"), "Yes", "No")) |>
        count(senior, churn)
      p = plot_ly(tmp, x = ~senior, y = ~n, color = ~churn, type = "bar",
                  hovertemplate = "Senior: %{x}<br>Churn: %{color}<br>Count: %{y}<extra></extra>") |>
        layout(barmode = "stack", xaxis = list(title = "Senior Citizen"))
      apply_dark(p)
    })
    
    # ---- Lightweight permutation importance (on sample) ----
    # Uses a simple logistic regression as a quick surrogate for ranking.
    output$feature_importance = renderPlotly({
      df = sampled()
      req("churn" %in% names(df))
      
      # Prepare: target as 0/1, select a small set of helpful predictors if present
      df = df |>
        mutate(
          churn_bin = ifelse(tolower(as.character(churn)) %in% c("yes","1","true"), 1, 0),
          services_count = if ("services_count" %in% names(df)) services_count else NA_real_
        )
      
      base_cols = intersect(
        c("tenure","monthly_charges","total_charges","services_count"),
        names(df)
      )
      
      # add a few categorical dummies if present
      cat_cols = intersect(
        c("contract","internet_service","payment_method","paperless_billing"),
        names(df)
      )
      
      if (length(base_cols) < 1 && length(cat_cols) < 1) return(NULL)
      
      work = df[, c("churn_bin", base_cols, cat_cols), drop = FALSE]
      # one-hot encode simple categorical columns
      for (cc in cat_cols) {
        work[[cc]] = factor(work[[cc]])
      }
      # model frame with 0/1 dummies (R will handle)
      form = as.formula(paste("churn_bin ~", paste(setdiff(names(work), "churn_bin"), collapse = " + ")))
      # fit quick glm (ignore warnings)
      fit = tryCatch(glm(form, data = work, family = binomial()), error = function(e) NULL)
      if (is.null(fit)) return(NULL)
      
      # permutation importance: drop-column approximation
      predictors = setdiff(names(work), "churn_bin")
      # baseline logloss
      pred0 = suppressWarnings(predict(fit, type = "response"))
      clip = pmax(pmin(pred0, 1 - 1e-9), 1e-9)
      logloss0 = -mean(work$churn_bin * log(clip) + (1 - work$churn_bin) * log(1 - clip))
      
      imp = lapply(predictors, function(col) {
        w = work
        # shuffle one column
        w[[col]] = sample(w[[col]])
        f2 = tryCatch(glm(form, data = w, family = binomial()), error = function(e) NULL)
        if (is.null(f2)) return(data.frame(feature = col, delta = NA_real_))
        p = suppressWarnings(predict(f2, type = "response"))
        p = pmax(pmin(p, 1 - 1e-9), 1e-9)
        ll = -mean(w$churn_bin * log(p) + (1 - w$churn_bin) * log(1 - p))
        data.frame(feature = col, delta = ll - logloss0)
      })
      imp = bind_rows(imp) |> filter(is.finite(delta)) |> arrange(desc(delta))
      
      if (!nrow(imp)) return(NULL)
      
      p = plot_ly(
        imp, x = ~delta, y = ~reorder(feature, delta), type = "bar", orientation = "h",
        hovertemplate = "%{y}<br>ΔLogLoss: %{x:.4f}<extra></extra>"
      ) |>
        layout(xaxis = list(title = "Increase in log-loss (worse = more important)"),
               yaxis = list(title = NULL))
      apply_dark(p)
    })
  })
}
