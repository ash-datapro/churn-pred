library(shiny)
library(httr)

retrain_ui = function(id) {
  ns = NS(id)
  tagList(
    h3("🔁 Retrain Model"),
    actionButton(ns("retrain"), "Start Retraining"),
    verbatimTextOutput(ns("status"))
  )
}

retrain_server = function(id, retrain_url = "http://backend:8000/retrain") {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$retrain, {
      res = tryCatch(POST(retrain_url), error = function(e) e)
      
      if (inherits(res, "error")) {
        output$status = renderText(paste("Error:", res$message))
      } else {
        output$status = renderText(content(res, "text", encoding = "UTF-8"))
      }
    })
  })
}
