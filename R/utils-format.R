#' Format cancer registry rates
#'
#' @description
#' Format numeric rate values, such as crude rates or age-standardized rates.
#'
#' @param x Numeric vector to format.
#' @param digits Number of decimal places.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' format_rate(c(12.345, 67.891), digits = 1)
format_rate <- function(x, digits = 2) {
  .check_digits(digits)
  scales::number(x, accuracy = 10^-digits)
}


#' Format percentages
#'
#' @description
#' Format percentage values. By default `scale = 1`, which is suitable when the
#' data are already stored as percentages, such as `23.5`. Use `scale = 100`
#' when values are stored as proportions, such as `0.235`.
#'
#' @param x Numeric vector to format.
#' @param digits Number of decimal places.
#' @param scale Multiplier applied before formatting.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' format_percent(c(23.45, 67.89), digits = 1)
#' format_percent(c(0.2345, 0.6789), digits = 1, scale = 100)
format_percent <- function(x, digits = 1, scale = 1) {
  .check_digits(digits)
  if (!is.numeric(scale) || length(scale) != 1 || is.na(scale)) {
    stop("`scale` must be a single numeric value.", call. = FALSE)
  }

  scales::number(x * scale, accuracy = 10^-digits, suffix = "%")
}


#' Format counts
#'
#' @description
#' Format case counts, death counts, or population counts using comma grouping.
#'
#' @param x Numeric vector to format.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' format_count(c(1234, 567890))
format_count <- function(x) {
  scales::comma(x, accuracy = 1)
}
