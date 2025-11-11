library(shiny)
library(DT)
library(plotly)
library(bslib)
library(dplyr)

# source("R/db.R")
source("R/sidebar.R")
source("R/metrics.R")
source("R/plots.R")       # contains apply_dark()
source("R/prediction.R")
source("R/insights.R")
source("R/about.R")
source("R/retrain.R")

ui = fluidPage(
  theme = bs_theme(bootswatch = "darkly", bg = "#000000", fg = "#FFFFFF"),
  
  # ---------- Pro styling ----------
  tags$head(
    tags$style(HTML("
      /* Base sizing: smaller UI text, large title */
      body { font-size: 14px !important; }
      h1 { font-size: 44px !important; line-height: 1.1; margin-bottom:6px; }
      h2, h3, h4, h5, h6, label, .control-label { color:#fff !important; }

      /* Global bg/fg */
      body, .container-fluid, .tab-content { background-color:#000 !important; color:#fff !important; }

      /* Sticky sidebar + card look */
      .well { background:#111 !important; color:#fff !important; border-color:#333 !important; }
      .sidebar-stick { position: sticky; top: 12px; }

      /* Tabs – compact, pill-like */
      .nav-tabs>li>a {
        background:#121212 !important; color:#e0e0e0 !important; border:1px solid #2f2f2f !important;
        font-size: 13px !important; padding: 8px 12px !important; border-radius:10px !important; margin-right:6px;
      }
      .nav-tabs>li.active>a, .nav-tabs>li>a:hover {
        background:#222 !important; color:#fff !important; border-color:#444 !important;
      }

      /* Inputs – compact controls */
      .form-control, .selectize-input, .selectize-dropdown, .modal-content {
        background:#111 !important; color:#fff !important; border-color:#333 !important;
        font-size: 13px !important;
      }
      .selectize-dropdown .active { background:#222 !important; }
      .btn, .btn-default {
        background:#222 !important; color:#fff !important; border-color:#444 !important;
        font-size: 13px !important; padding: 6px 10px !important; border-radius:10px;
      }
      .btn:hover { background:#333 !important; }

      /* Toolbar badges */
      .badge-dark {
        display:inline-block; padding:4px 8px; margin-left:8px; border:1px solid #333;
        background:#111; color:#bbb; border-radius:8px; font-size:12px;
      }

      /* Value boxes (metrics) – compact */
      .value-box {
        background:#111 !important; color:#fff !important; border:1px solid #333 !important;
        border-radius:14px; padding:12px 14px; margin-bottom:14px; box-shadow:0 2px 6px rgba(0,0,0,0.4);
      }
      .value-box .vb-title { color:#bbb !important; margin:0 0 4px 0; font-size:12px !important; }
      .value-box .vb-value { color:#fff !important; margin:0; font-weight:700; font-size:26px !important; }

      /* DataTable – dark + small */
      .dataTables_wrapper { font-size: 12px !important; }
      .dataTables_wrapper .dataTables_length select,
      .dataTables_wrapper .dataTables_filter input {
        background:#111 !important; color:#fff !important; border:1px solid #333 !important; font-size:12px !important;
      }
      table.dataTable, table.dataTable tbody td, table.dataTable thead th {
        color:#fff !important; background:#000 !important; font-size:12px !important;
      }

      /* Plotly containers */
      .plotly, .js-plotly-plot, .plot-container, .svg-container { background:#000 !important; }

      /* === Selectize dark-theme fix: make selected items visible === */
      .selectize-input { background:#111 !important; color:#fff !important; border:1px solid #333 !important; }
      .selectize-input > input { color:#fff !important; }
      .selectize-control.multi .selectize-input .item {
        background:#1f1f1f !important; color:#ffffff !important; border:1px solid #555 !important;
        border-radius:8px; padding:2px 6px; margin:2px 4px 2px 0;
      }
      .selectize-control.multi .selectize-input .item.active {
        background:#2b2b2b !important; border-color:#777 !important;
      }
      .selectize-control.multi .selectize-input > div .remove { color:#bbb !important; }
      .selectize-control.multi .selectize-input > div .remove:hover { color:#fff !important; }
      .selectize-dropdown {
        background:#0f0f0f !important; color:#fff !important; border:1px solid #333 !important;
      }
      .selectize-dropdown .option { color:#fff !important; }
      .selectize-dropdown .active, .selectize-dropdown .option.selected {
        background:#222 !important; color:#fff !important;
      }
      .selectize-control .selectize-input::placeholder { color:#8a8a8a !important; }
      .selectize-control .selectize-input.has-items { padding:6px 8px !important; }

      /* Insight chips */
      .insight-chip {
        display:inline-block; margin:4px 6px 0 0; padding:6px 10px; border-radius:12px;
        background:#141414; border:1px solid #2e2e2e; color:#cfcfcf; font-size:12px;
      }
    "))
  ),
  
  # ---------- Header ----------
  fluidRow(
    column(
      12,
      titlePanel("💎 Post-Sales Customer Churn Dashboard"),
      div(
        "An interactive, production-style dashboard for exploring churn KPIs, insights, and predictions.",
        class = "text-muted", style = "margin-top:-10px;margin-bottom:16px;"
      )
    )
  ),
  
  # ---------- Body layout ----------
  sidebarLayout(
    sidebarPanel(
      div(class = "sidebar-stick",
          h4("Filter Options"),
          p(class = "text-muted", "Use the options below to refine the data view."),
          sidebar_ui("sidebar"),
          hr(),
          actionButton("refresh_btn", "↻ Refresh Data"),
          tags$span(class = "badge-dark", textOutput("last_updated", inline = TRUE))
      ),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "📶 Overview",
          # Toolbar
          fluidRow(
            column(6, tags$span(class = "badge-dark", textOutput("row_count", inline = TRUE))),
            column(6, div(style = "text-align:right",
                          downloadButton("download_filtered", "⬇️ Download Filtered CSV")))
          ),
          br(),
          
          # KPI row
          metrics_ui("metrics"),
          br(),
          
          # Quick insights chips
          fluidRow(
            column(
              12,
              h4("Quick Insights"),
              uiOutput("insight_chips")
            )
          ),
          br(),
          
          # Visual grid 1 (2x2): churn mix, tenure dist, monthly charges box, payment donut
          fluidRow(
            column(6, plotlyOutput("churn_plot", height = "340px")),
            column(6, plotlyOutput("tenure_hist", height = "340px"))
          ),
          fluidRow(
            column(6, plotlyOutput("monthly_box", height = "340px")),
            column(6, plotlyOutput("payment_donut", height = "340px"))
          ),
          br(),
          
          # Segments table & top categories
          fluidRow(
            column(
              6,
              h4("Churn by Contract"),
              plotlyOutput("contract_churn", height = "360px")
            ),
            column(
              6,
              h4("Top Segments (Count)"),
              DTOutput("segment_table")
            )
          )
        ),
        tabPanel("⬆️ Insights", insights_ui("insights")),
        tabPanel("🔄 Prediction", prediction_ui("predict")),
        tabPanel("️ℹ️ About", about_ui("about"))
      ),
      width = 9
    )
  )
)

server = function(input, output, session) {
  # Reactive data
  data_all = reactive(load_data())
  data_filtered = sidebar_server("sidebar", data_all)
  
  # Modules
  metrics_server("metrics", data_filtered)
  insights_server("insights", data_filtered)
  prediction_server("predict", api_url = "http://127.0.0.1:8000/predict")
  about_server("about")
  
  # Toolbar info badges
  output$row_count = renderText({
    n = tryCatch(nrow(data_filtered()), error = function(e) 0)
    paste0("Rows: ", format(n, big.mark = ","))
  })
  output$last_updated = renderText({
    paste0("Last updated: ", format(Sys.time(), "%Y-%m-%d %H:%M"))
  })
  observeEvent(input$refresh_btn, { invisible(data_all()) })
  
  # ----------- Overview visuals -----------
  # 1) Churn mix (already themed in plots.R via apply_dark)
  output$churn_plot = renderPlotly({
    apply_dark(plot_churn_distribution(data_filtered()))
  })
  
  # 2) Tenure histogram
  output$tenure_hist = renderPlotly({
    df = data_filtered()
    req("tenure" %in% names(df))
    p = plot_ly(df, x = ~tenure, type = "histogram", nbinsx = 40,
                hovertemplate = "Tenure: %{x}<br>Count: %{y}<extra></extra>")
    apply_dark(p)
  })
  
  # 3) Monthly charges box (by churn)
  output$monthly_box = renderPlotly({
    df = data_filtered()
    req(all(c("monthly_charges", "churn") %in% names(df)))
    p = plot_ly(df, x = ~churn, y = ~monthly_charges, type = "box",
                hovertemplate = "Churn: %{x}<br>Monthly: $%{y:.2f}<extra></extra>")
    apply_dark(p)
  })
  
  # 4) Payment method donut
  output$payment_donut = renderPlotly({
    df = data_filtered()
    req("payment_method" %in% names(df))
    agg = df %>% count(payment_method) %>% arrange(desc(n))
    p = plot_ly(agg, labels = ~payment_method, values = ~n, type = "pie",
                hole = 0.45, sort = FALSE, textinfo = "label+percent",
                hovertemplate = "%{label}<br>Count: %{value}<extra></extra>")
    apply_dark(p)
  })
  
  # 5) Contract vs churn (stacked %)
  output$contract_churn = renderPlotly({
    df = data_filtered()
    req(all(c("contract","churn") %in% names(df)))
    tmp = df %>%
      mutate(churn = ifelse(tolower(as.character(churn)) %in% c("yes","1","true"), "Yes", "No")) %>%
      count(contract, churn) %>%
      group_by(contract) %>%
      mutate(pct = n/sum(n)*100) %>%
      ungroup()
    p = plot_ly(tmp, x = ~contract, y = ~pct, color = ~churn, type = "bar",
                hovertemplate = "Contract: %{x}<br>Churn: %{color}<br>%{y:.1f}%%<extra></extra>") |>
      layout(barmode = "stack", yaxis = list(title = "Percent"))
    apply_dark(p)
  })
  
  # 6) Segment table (top combinations)
  output$segment_table = renderDT({
    df = data_filtered()
    keep = intersect(c("contract","internet_service","payment_method","gender","churn"), names(df))
    if (length(keep) < 2 || nrow(df) == 0) return(DT::datatable(data.frame()))
    tmp = df %>%
      mutate(churn = ifelse(tolower(as.character(churn)) %in% c("yes","1","true"), "Yes", "No")) %>%
      count(contract, internet_service, payment_method, churn, name = "count") %>%
      arrange(desc(count)) %>% head(12)
    DT::datatable(tmp, rownames = FALSE, options = list(dom = "t", pageLength = 12))
  })
  
  # 7) Quick Insights chips (simple heuristics on filtered data)
  output$insight_chips = renderUI({
    df = data_filtered()
    chips = list()
    safe_pct = function(n, d) ifelse(d > 0, sprintf("%.1f%%", 100*n/d), "—")
    
    if (all(c("contract","churn") %in% names(df))) {
      tmp = df %>%
        mutate(churn = tolower(as.character(churn)) %in% c("yes","1","true")) %>%
        group_by(contract) %>% summarise(rate = mean(churn), .groups = "drop") %>%
        arrange(desc(rate))
      if (nrow(tmp) >= 2) {
        chips <- append(chips, list(
          tags$span(class = "insight-chip",
                    sprintf("Highest churn: %s (%s)",
                            tmp$contract[1], safe_pct(tmp$rate[1], 1)))
        ))
      }
    }
    
    if (all(c("internet_service","churn") %in% names(df))) {
      tmp = df %>%
        mutate(churn = tolower(as.character(churn)) %in% c("yes","1","true")) %>%
        group_by(internet_service) %>% summarise(rate = mean(churn), .groups = "drop") %>%
        arrange(desc(rate))
      if (nrow(tmp) >= 1) {
        chips <- append(chips, list(
          tags$span(class = "insight-chip",
                    sprintf("Risky service: %s", tmp$internet_service[1]))
        ))
      }
    }
    
    if (all(c("tenure","churn") %in% names(df))) {
      early = df %>% filter(tenure <= 6)
      if (nrow(early) > 0) {
        rate = mean(tolower(as.character(early$churn)) %in% c("yes","1","true"))
        chips <- append(chips, list(
          tags$span(class = "insight-chip",
                    sprintf("≤6 mo churn: %s", safe_pct(rate, 1)))
        ))
      }
    }
    
    if (length(chips) == 0) tags$span(class = "text-muted", "No insights available for current filter.")
    else do.call(tagList, chips)
  })
  
  # Download filtered data
  output$download_filtered = downloadHandler(
    filename = function() paste0("filtered_churn_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      write.csv(data_filtered(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
