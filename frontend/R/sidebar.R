library(shiny)
library(dplyr)

sidebar_ui = function(id) {
  ns = NS(id)
  tagList(
    h3("Filter Options"),
    p(class = "text-muted", "Use the options below to refine the data view."),
    
    h4("Filters"),
    # Multi-selects (empty = all)
    selectizeInput(
      ns("contract"), "Contract",
      choices = NULL, multiple = TRUE,
      options = list(placeholder = "All contracts")
    ),
    selectizeInput(
      ns("gender"), "Gender",
      choices = NULL, multiple = TRUE,
      options = list(placeholder = "All genders")
    ),
    selectizeInput(
      ns("internet_service"), "Internet Service",
      choices = NULL, multiple = TRUE,
      options = list(placeholder = "All services")
    ),
    selectizeInput(
      ns("payment_method"), "Payment Method",
      choices = NULL, multiple = TRUE,
      options = list(placeholder = "All payment methods")
    ),
    
    # Tenure range (auto-scaled from data)
    sliderInput(ns("tenure_range"), "Tenure (months)", min = 0, max = 72, value = c(0, 72), step = 1),
    
    checkboxInput(ns("only_churn"), "Only churned customers", value = FALSE),
    
    # Quick segment presets
    h4("Quick Segments"),
    radioButtons(
      ns("segment"), label = NULL,
      choices = c(
        "None" = "none",
        "High Risk (M2M • Fiber • E-check)" = "hi_risk",
        "Low Risk (2-yr • DSL • Auto-pay)" = "lo_risk"
      ),
      selected = "none"
    ),
    
    # Active filter pills + actions
    h4("Active"),
    uiOutput(ns("active_filters")),
    div(
      actionButton(ns("reset"), "Reset Filters"),
      style = "margin-top:6px;"
    ),
    div(
      tags$span(class = "badge-dark", textOutput(ns("rows_badge"), inline = TRUE)),
      style = "margin-top:6px;"
    )
  )
}

sidebar_server = function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # --- Populate choices and slider from data() ---
    observeEvent(data(), {
      df = data()
      nm = names(df)
      
      opts = function(col) {
        if (!col %in% nm) return(character(0))
        vals = sort(unique(as.character(df[[col]])))
        vals[!is.na(vals) & nzchar(vals)]
      }
      
      updateSelectizeInput(session, "contract", choices = opts("contract"), server = TRUE)
      updateSelectizeInput(session, "gender", choices = opts("gender"), server = TRUE)
      updateSelectizeInput(session, "internet_service", choices = opts("internet_service"), server = TRUE)
      updateSelectizeInput(session, "payment_method", choices = opts("payment_method"), server = TRUE)
      
      if ("tenure" %in% nm) {
        rng = range(df$tenure, na.rm = TRUE)
        if (is.finite(rng[1]) && is.finite(rng[2])) {
          updateSliderInput(
            session, "tenure_range",
            min = floor(rng[1]), max = ceiling(rng[2]),
            value = c(floor(rng[1]), ceiling(rng[2]))
          )
        }
      }
    }, ignoreInit = FALSE)
    
    # --- Reset button ---
    observeEvent(input$reset, {
      updateSelectizeInput(session, "contract", selected = character(0))
      updateSelectizeInput(session, "gender", selected = character(0))
      updateSelectizeInput(session, "internet_service", selected = character(0))
      updateSelectizeInput(session, "payment_method", selected = character(0))
      updateCheckboxInput(session, "only_churn", value = FALSE)
      updateRadioButtons(session, "segment", selected = "none")
    })
    
    # --- Core filtering (returns filtered data) ---
    filtered = reactive({
      df = data()
      nm = names(df)
      if (nrow(df) == 0) return(df)
      
      to_chr = function(x) as.character(x)
      
      # Apply quick segment presets first (they just pre-filter; users can refine further)
      if (input$segment == "hi_risk") {
        if ("contract" %in% nm) df = df[to_chr(df$contract) == "Month-to-month", , drop = FALSE]
        if ("internet_service" %in% nm) df = df[to_chr(df$internet_service) == "Fiber optic", , drop = FALSE]
        if ("payment_method" %in% nm) df = df[to_chr(df$payment_method) == "Electronic check", , drop = FALSE]
      } else if (input$segment == "lo_risk") {
        if ("contract" %in% nm) df = df[to_chr(df$contract) == "Two year", , drop = FALSE]
        if ("internet_service" %in% nm) df = df[to_chr(df$internet_service) == "DSL", , drop = FALSE]
        if ("payment_method" %in% nm) {
          auto = c("Bank transfer (automatic)", "Credit card (automatic)")
          df = df[to_chr(df$payment_method) %in% auto, , drop = FALSE]
        }
      }
      
      # contract
      if ("contract" %in% nm && length(input$contract)) {
        df = df[to_chr(df$contract) %in% input$contract, , drop = FALSE]
      }
      # gender
      if ("gender" %in% nm && length(input$gender)) {
        df = df[to_chr(df$gender) %in% input$gender, , drop = FALSE]
      }
      # internet service
      if ("internet_service" %in% nm && length(input$internet_service)) {
        df = df[to_chr(df$internet_service) %in% input$internet_service, , drop = FALSE]
      }
      # payment method
      if ("payment_method" %in% nm && length(input$payment_method)) {
        df = df[to_chr(df$payment_method) %in% input$payment_method, , drop = FALSE]
      }
      # tenure range
      if ("tenure" %in% nm && length(input$tenure_range) == 2) {
        df = df[df$tenure >= input$tenure_range[1] & df$tenure <= input$tenure_range[2], , drop = FALSE]
      }
      # churn
      if (isTRUE(input$only_churn) && "churn" %in% nm) {
        ch = tolower(to_chr(df$churn))
        df = df[ch %in% c("yes", "1", "true"), , drop = FALSE]
      }
      
      df
    })
    
    # --- Row badge + active filter chips (for a premium feel) ---
    output$rows_badge = renderText({
      n = tryCatch(nrow(filtered()), error = function(e) 0)
      paste0("Rows: ", format(n, big.mark = ","))
    })
    
    output$active_filters = renderUI({
      chips = list()
      
      add_chip = function(label, values) {
        if (length(values) && all(nzchar(values))) {
          chips <<- append(chips, list(
            tags$span(
              label, ": ",
              paste(values, collapse = ", "),
              class = "badge-dark",
              style = "display:inline-block;margin:2px 6px 2px 0;"
            )
          ))
        }
      }
      
      if (length(input$contract)) add_chip("Contract", input$contract)
      if (length(input$gender)) add_chip("Gender", input$gender)
      if (length(input$internet_service)) add_chip("Internet", input$internet_service)
      if (length(input$payment_method)) add_chip("Payment", input$payment_method)
      if (isTRUE(input$only_churn)) add_chip("Churn", "Yes")
      if (!is.null(input$tenure_range)) {
        add_chip("Tenure",
                 paste0(input$tenure_range[1], "–", input$tenure_range[2]))
      }
      if (input$segment != "none") add_chip("Segment", input$segment)
      
      if (length(chips) == 0) tags$span(class = "text-muted", "No active filters")
      else do.call(tagList, chips)
    })
    
    # Return filtered reactive to the app
    return(filtered)
  })
}
