# utils.R
# Helper functions for the Child Poverty Explorer Shiny app

#' Create an inline bar for reactable cells
#' @param value The numeric value
#' @param max_value The maximum value (for scaling)
#' @param color The bar color
#' @param height Bar height in pixels
bar_cell <- function(value, max_value, color = "#006878", height = 14) {
  if (is.na(value) || max_value == 0) {
    return(htmltools::div())
  }

  width <- paste0(value / max_value * 100, "%")

  htmltools::div(
    style = list(display = "flex", alignItems = "center"),
    htmltools::div(style = list(
      background = color,
      width = width,
      height = paste0(height, "px"),
      marginRight = "8px",
      borderRadius = "2px"
    )),
    format(value, big.mark = ",")
  )
}

#' Format large numbers with commas
format_number <- function(x) {
  format(x, big.mark = ",", scientific = FALSE)
}

#' Calculate Jenks natural breaks
#' @param values Numeric vector
#' @param n Number of breaks
jenks_breaks <- function(values, n = 5) {
  values <- values[!is.na(values)]
  if (length(values) < n) {
    return(quantile(values, probs = seq(0, 1, length.out = n)))
  }

  # Use quantiles as a simple approximation of Jenks
  # (Full Jenks requires classInt package)
  quantile(values, probs = seq(0, 1, length.out = n))
}
