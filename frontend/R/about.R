library(shiny)
library(httr)
library(jsonlite)

about_ui = function(id) {
  ns = NS(id)
  tagList(
    h2("️ℹ️ About"),
    div(class = "text-muted",
        "A compact, production-style dashboard for exploring churn KPIs, insights, and predictions."
    ),
    br(),
    
    # Top badges / quick facts
    fluidRow(
      column(
        3,
        tags$div(class = "value-box",
                 h4(class = "vb-title", "App"),
                 h2(textOutput(ns("about_app_name"), inline = TRUE), class = "vb-value"),
                 tags$div(textOutput(ns("about_build")), class = "text-muted")
        )
      ),
      column(
        3,
        tags$div(class = "value-box",
                 h4(class = "vb-title", "R & Shiny"),
                 h2(textOutput(ns("about_r_version"), inline = TRUE), class = "vb-value"),
                 tags$div(textOutput(ns("about_shiny_version")), class = "text-muted")
        )
      ),
      column(
        3,
        tags$div(class = "value-box",
                 h4(class = "vb-title", "API Health"),
                 h2(textOutput(ns("about_api_status"), inline = TRUE), class = "vb-value"),
                 tags$div(textOutput(ns("about_api_latency")), class = "text-muted")
        )
      ),
      column(
        3,
        tags$div(class = "value-box",
                 h4(class = "vb-title", "Data Rows (current view)"),
                 h2(textOutput(ns("about_rows"), inline = TRUE), class = "vb-value"),
                 tags$div(textOutput(ns("about_timestamp")), class = "text-muted")
        )
      )
    ),
    br(),
    
    # Collapsible sections
    fluidRow(
      column(
        12,
        tags$details(
          open = TRUE,
          tags$summary("What is this app?"),
          tags$p("This dashboard explores the Telco Customer Churn dataset. It provides:"),
          tags$ul(
            tags$li("Overview KPIs and quick trends"),
            tags$li("Insights: correlations, segment breakdowns, and relationships"),
            tags$li("Prediction: send a customer profile to the API and receive churn probability"),
            tags$li("Download: export the filtered data as CSV")
          )
        ),
        tags$details(
          tags$summary("Tech stack"),
          tags$ul(
            tags$li("Frontend: Shiny + bslib + Plotly + DT"),
            tags$li("Backend API: plumber (R) serving a tidymodels workflow"),
            tags$li("Database: PostgreSQL (customers table)"),
            tags$li("Containerization: Docker Compose-ready (frontend, backend, postgres)")
          ),
          tags$p(class = "text-muted",
                 "Tip: in Docker, set ",
                 tags$code("PREDICT_API_URL=http://backend:8000/predict"),
                 " for the frontend container.")
        ),
        tags$details(
          tags$summary("Endpoints"),
          tags$p("Prediction endpoint and health check used by this app:"),
          tags$pre(style = "white-space: pre-wrap; font-size: 12px;",
                   textOutput(ns("about_endpoints")))
        ),
        tags$details(
          tags$summary("Data dictionary (selected fields)"),
          tableOutput(ns("about_schema"))
        ),
        tags$details(
          tags$summary("How to use"),
          tags$ol(
            tags$li("Use the filters on the left to subset the population."),
            tags$li("Explore KPIs and charts in the Overview & Insights tabs."),
            tags$li("Switch to Prediction, pick a preset or enter values, and press ",
                    tags$strong("Predict"), "."),
            tags$li("Export the filtered dataset via the download button on Overview.")
          )
        ),
        tags$details(
          tags$summary("Credits"),
          tags$p("Dataset: Telco Customer Churn (Kaggle). UI inspired by modern BI dashboards.")
        )
      )
    )
  )
}

about_server = function(id, data_reactive = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # --- Basic app facts
    output$about_app_name = renderText("Churn Dashboard")
    output$about_build = renderText({
      paste0("Build: ", format(Sys.time(), "%Y-%m-%d %H:%M"))
    })
    output$about_r_version = renderText({
      paste0("R ", getRversion())
    })
    output$about_shiny_version = renderText({
      v = tryCatch(as.character(utils::packageVersion("shiny")), error = function(e) NA)
      paste0("shiny ", v)
    })
    
    # --- API health ping
    output$about_api_status = renderText({
      api = Sys.getenv("PREDICT_API_URL", "http://127.0.0.1:8000/predict")
      health = sub("/predict$", "/health", api)
      t0 = Sys.time()
      ok = tryCatch({
        res = httr::GET(health, timeout(2))
        httr::status_code(res) == 200
      }, error = function(e) FALSE)
      if (ok) "OK" else "Down"
    })
    
    output$about_api_latency = renderText({
      api = Sys.getenv("PREDICT_API_URL", "http://127.0.0.1:8000/predict")
      health = sub("/predict$", "/health", api)
      t0 = Sys.time()
      ms = tryCatch({
        res = httr::GET(health, timeout(2))
        as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
      }, error = function(e) NA_real_)
      if (is.na(ms)) "Timeout" else paste0(round(ms), " ms")
    })
    
    # --- Rows visible (if the server passed a reactive)
    output$about_rows = renderText({
      if (is.null(data_reactive)) return("—")
      df = tryCatch(data_reactive(), error = function(e) NULL)
      if (is.null(df)) return("—")
      format(nrow(df), big.mark = ",")
    })
    output$about_timestamp = renderText({
      paste0("As of ", format(Sys.time(), "%Y-%m-%d %H:%M"))
    })
    
    # --- Endpoints text
    output$about_endpoints = renderText({
      api = Sys.getenv("PREDICT_API_URL", "http://127.0.0.1:8000/predict")
      health = sub("/predict$", "/health", api)
      paste0(
        "PREDICT_API_URL = ", api, "\n",
        "HEALTH = ", health, "\n"
      )
    })
    
    # --- Tiny schema table (static cheat-sheet)
    output$about_schema = renderTable({
      data.frame(
        Field = c(
          "customer_id","gender","senior_citizen","partner","dependents",
          "tenure","internet_service","contract","paperless_billing",
          "payment_method","monthly_charges","total_charges","churn"
        ),
        Type = c(
          "string","string","integer (0/1)","string","string",
          "integer","string","string","string",
          "string","numeric","numeric","string (Yes/No)"
        ),
        Description = c(
          "Unique customer identifier",
          "Customer gender",
          "Whether the customer is a senior citizen",
          "Has a partner",
          "Has dependents",
          "Months the customer has stayed",
          "DSL / Fiber optic / No",
          "Month-to-month / One year / Two year",
          "Paperless billing flag",
          "Payment method category",
          "Monthly charges in USD",
          "Lifetime total charges",
          "Target label"
        ),
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, bordered = TRUE, spacing = "s")
  })
}
