library(shiny)
library(httr)
library(jsonlite)
library(plotly)

# UI ---------------------------------------------------------------------------
prediction_ui = function(id) {
  ns = NS(id)
  tagList(
    h3("🔄 Prediction"),
    # Top toolbar (preset + reset + predict)
    fluidRow(
      column(
        8,
        selectInput(
          ns("preset"),
          label = NULL,
          choices = c(
            "Choose an example…" = "",
            "New month-to-month user (higher risk)" = "p1",
            "Long-tenure two-year contract (lower risk)" = "p2"
          ),
          width = "100%"
        )
      ),
      column(
        4,
        div(style = "text-align:right;",
            actionButton(ns("reset"), "Reset"),
            actionButton(ns("predict"), "Predict", class = "btn-primary"))
      )
    ),
    br(),
    
    # Inputs on the left, results on the right
    fluidRow(
      column(
        7,
        fluidRow(
          column(6, numericInput(ns("tenure"), "Tenure (months)", 12, min = 0, step = 1)),
          column(6, numericInput(ns("monthly_charges"), "Monthly Charges ($)", 70, min = 0, step = 0.5))
        ),
        fluidRow(
          column(6, selectInput(ns("contract"), "Contract", c("Month-to-month","One year","Two year"))),
          column(6, selectInput(ns("gender"), "Gender", c("Female","Male")))
        ),
        fluidRow(
          column(6, selectInput(ns("senior_citizen"), "Senior Citizen", c("0","1"))),
          column(6, selectInput(ns("partner"), "Partner", c("Yes","No")))
        ),
        fluidRow(
          column(6, selectInput(ns("dependents"), "Dependents", c("Yes","No"))),
          column(6, selectInput(ns("internet_service"), "Internet Service", c("DSL","Fiber optic","No")))
        ),
        fluidRow(
          column(6, selectInput(ns("paperless_billing"), "Paperless Billing", c("Yes","No"))),
          column(6, selectInput(
            ns("payment_method"), "Payment Method",
            c("Electronic check","Mailed check","Bank transfer (automatic)","Credit card (automatic)")
          ))
        ),
        checkboxGroupInput(
          ns("services_list"), "Services",
          choices = c(
            "Multiple Lines","Online Security","Online Backup",
            "Device Protection","Tech Support","Streaming TV","Streaming Movies"
          )
        )
      ),
      column(
        5,
        # Result card
        tags$div(
          class = "value-box",
          h4(class = "vb-title", "Prediction"),
          h2(textOutput(ns("result_title"), inline = TRUE), class = "vb-value"),
          tags$div(textOutput(ns("result_subtitle"), inline = TRUE), class = "text-muted")
        ),
        plotlyOutput(ns("prob_gauge"), height = "280px"),
        tags$div(
          style = "margin-top:8px;",
          tags$small(class = "text-muted",
                     "Threshold: 0.50  •  API: ", textOutput(ns("api_label"), inline = TRUE))
        ),
        br(),
        # Raw JSON preview for debugging/transparency
        tags$details(
          tags$summary("Request payload (JSON)"),
          tags$pre(style = "white-space: pre-wrap; font-size: 12px;", textOutput(ns("payload_preview")))
        )
      )
    )
  )
}

# SERVER -----------------------------------------------------------------------
prediction_server = function(id, api_url = "http://backend:8000/predict") {
  moduleServer(id, function(input, output, session) {
    
    # Show which endpoint we’re using
    output$api_label = renderText(api_url)
    
    # Helpers ------------------------------------------------------------------
    set_preset = function(code) {
      if (identical(code, "p1")) {
        updateNumericInput(session, "tenure", value = 2)
        updateNumericInput(session, "monthly_charges", value = 85)
        updateSelectInput(session, "contract", selected = "Month-to-month")
        updateSelectInput(session, "gender", selected = "Female")
        updateSelectInput(session, "senior_citizen", selected = "0")
        updateSelectInput(session, "partner", selected = "No")
        updateSelectInput(session, "dependents", selected = "No")
        updateSelectInput(session, "internet_service", selected = "Fiber optic")
        updateSelectInput(session, "paperless_billing", selected = "Yes")
        updateSelectInput(session, "payment_method", selected = "Electronic check")
        updateCheckboxGroupInput(session, "services_list",
                                 selected = c("Multiple Lines","Streaming TV"))
      } else if (identical(code, "p2")) {
        updateNumericInput(session, "tenure", value = 48)
        updateNumericInput(session, "monthly_charges", value = 55)
        updateSelectInput(session, "contract", selected = "Two year")
        updateSelectInput(session, "gender", selected = "Male")
        updateSelectInput(session, "senior_citizen", selected = "0")
        updateSelectInput(session, "partner", selected = "Yes")
        updateSelectInput(session, "dependents", selected = "Yes")
        updateSelectInput(session, "internet_service", selected = "DSL")
        updateSelectInput(session, "paperless_billing", selected = "No")
        updateSelectInput(session, "payment_method", selected = "Bank transfer (automatic)")
        updateCheckboxGroupInput(session, "services_list",
                                 selected = c("Online Security","Online Backup","Tech Support"))
      }
    }
    
    assemble_body = reactive({
      list(
        tenure = input$tenure,
        monthly_charges = input$monthly_charges,
        contract = input$contract,
        gender = input$gender,
        senior_citizen = as.integer(input$senior_citizen),
        partner = input$partner,
        dependents = input$dependents,
        internet_service = input$internet_service,
        paperless_billing = input$paperless_billing,
        payment_method = input$payment_method,
        services_list = input$services_list
      )
    })
    
    output$payload_preview = renderText({
      toJSON(assemble_body(), auto_unbox = TRUE, pretty = TRUE)
    })
    
    # Preset/Reset handlers ----------------------------------------------------
    observeEvent(input$preset, ignoreInit = TRUE, handlerExpr = {
      set_preset(input$preset)
    })
    
    observeEvent(input$reset, {
      updateSelectInput(session, "preset", selected = "")
      set_preset("p1")  # put a sensible default back in
    })
    
    # Core prediction ----------------------------------------------------------
    predict_once = function(body) {
      tryCatch({
        POST(
          api_url,
          body = toJSON(body, auto_unbox = TRUE),
          encode = "raw",
          add_headers("Content-Type" = "application/json")
        )
      }, error = function(e) e)
    }
    
    # Nice result renderers ----------------------------------------------------
    render_result = function(pred, proba) {
      pct = round(proba * 100, 1)
      output$result_title = renderText(paste0(pred, "  (", pct, "%)"))
      output$result_subtitle = renderText(ifelse(pred == "Yes",
                                                 "Higher churn likelihood",
                                                 "Lower churn likelihood"))
      
      # Gauge
      g_col = if (pred == "Yes") "#d9534f" else "#5cb85c"
      p = plot_ly(
        type = "indicator", mode = "gauge+number",
        value = pct,
        number = list(suffix = "%", font = list(color = "#FFFFFF")),
        gauge = list(
          axis = list(range = list(NULL, 100), tickcolor = "#AAAAAA"),
          bar = list(color = g_col),
          bgcolor = "#000000",
          borderwidth = 1, bordercolor = "#333333",
          steps = list(
            list(range = c(0, 40), color = "#123d12"),
            list(range = c(40, 70), color = "#403312"),
            list(range = c(70, 100), color = "#3d1212")
          ),
          threshold = list(line = list(color = "#FFFFFF", width = 2),
                           thickness = 0.75, value = 50)
        )
      ) |>
        layout(paper_bgcolor = "#000000", plot_bgcolor = "#000000",
               margin = list(l = 10, r = 10, t = 10, b = 10),
               font = list(color = "#FFFFFF"))
      output$prob_gauge = renderPlotly(p)
    }
    
    render_error = function(msg) {
      output$result_title = renderText("Error")
      output$result_subtitle = renderText(msg)
      output$prob_gauge = renderPlotly(NULL)
    }
    
    # Click -> call API --------------------------------------------------------
    observeEvent(input$predict, {
      body = assemble_body()
      
      # minimal client-side validation
      if (is.null(body$tenure) || is.null(body$monthly_charges) ||
          is.na(body$tenure) || is.na(body$monthly_charges) ||
          body$tenure < 0 || body$monthly_charges < 0) {
        render_error("Please enter valid Tenure and Monthly Charges.")
        return()
      }
      
      res = predict_once(body)
      
      if (inherits(res, "error")) {
        render_error(res$message)
        return()
      }
      if (status_code(res) != 200) {
        txt = content(res, "text", encoding = "UTF-8")
        render_error(txt)
        return()
      }
      
      ct = content(res, as = "parsed", simplifyVector = TRUE)
      proba = suppressWarnings(as.numeric(ct$probability))
      pred = as.character(ct$prediction)
      if (is.na(proba) || !nzchar(pred)) {
        render_error("Invalid API response.")
        return()
      }
      render_result(pred, proba)
    })
    
    # Initialize with a good-looking preset
    observeEvent(TRUE, { set_preset("p1") }, once = TRUE)
  })
}
