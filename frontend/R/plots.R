library(plotly)
library(dplyr)

# Universal dark theming for plotly figures
apply_dark = function(p) {
  if (is.null(p)) return(NULL)
  p |>
    layout(
      template = "plotly_dark",
      paper_bgcolor = "#000000",
      plot_bgcolor  = "#000000",
      font = list(color = "#FFFFFF"),
      xaxis = list(gridcolor = "#333333", zerolinecolor = "#333333"),
      yaxis = list(gridcolor = "#333333", zerolinecolor = "#333333")
    )
}

plot_churn_distribution = function(df) {
  if (!"churn" %in% names(df)) return(NULL)
  p = df |>
    count(churn) |>
    arrange(desc(n)) |>
    plot_ly(x = ~churn, y = ~n, type = "bar", hovertemplate = "Churn: %{x}<br>Count: %{y}<extra></extra>")
  apply_dark(p)
}

plot_tenure_boxplot = function(df) {
  if (!"tenure" %in% names(df)) return(NULL)
  p = plot_ly(df, y = ~tenure, type = "box", hovertemplate = "Tenure: %{y}<extra></extra>")
  apply_dark(p)
}

# ...add other plot_* functions and end each with apply_dark(p) ...

expander_explanation = function(key) {
  # same text mapping as your Python version
}
