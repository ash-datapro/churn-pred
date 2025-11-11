library(shiny)

metrics_ui = function(id) {
  ns = NS(id)
  fluidRow(
    column(3, uiOutput(ns("total_customers"))),
    column(3, uiOutput(ns("churn_rate"))),
    column(3, uiOutput(ns("avg_tenure"))),
    column(3, uiOutput(ns("avg_monthly_charges")))
  )
}

metrics_server = function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    value_box = function(title, value) {
      tags$div(
        class = "value-box",
        tags$h4(title, class = "vb-title"),
        tags$h2(value, class = "vb-value")
      )
    }
    
    output$total_customers = renderUI({
      df = data()
      value_box("Total Customers", nrow(df))
    })
    
    output$churn_rate = renderUI({
      df = data()
      if (!"churn" %in% names(df) || nrow(df) == 0) return(NULL)
      rate = mean(tolower(as.character(df$churn)) %in% c("yes","1","true"), na.rm = TRUE) * 100
      value_box("Churn Rate", sprintf("%.1f%%", rate))
    })
    
    output$avg_tenure = renderUI({
      df = data()
      if (!"tenure" %in% names(df) || nrow(df) == 0) return(NULL)
      value_box("Avg Tenure", sprintf("%.1f", mean(df$tenure, na.rm = TRUE)))
    })
    
    output$avg_monthly_charges = renderUI({
      df = data()
      if (!"monthly_charges" %in% names(df) || nrow(df) == 0) return(NULL)
      value_box("Avg Monthly Charges", sprintf("$%.2f", mean(df$monthly_charges, na.rm = TRUE)))
    })
  })
}
