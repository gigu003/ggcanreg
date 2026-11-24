#' Draw a big-value indicator card
#'
#' @description
#' `gg_big_value()` creates a simple big-number card for highlighting one key
#' indicator in cancer registry reports, dashboards, and presentation graphics.
#' It returns a ggplot object and can be combined with other plots using
#' patchwork or other ggplot composition tools.
#'
#' @param value Value to display as the main big number. Can be numeric or
#'   character. Numeric values are formatted with `scales::number()`.
#' @param text Descriptive text shown above the value.
#' @param value_color Color of the main value.
#' @param text_color Color of the descriptive text.
#' @param value_size Font size of the main value.
#' @param text_size Font size of the descriptive text.
#' @param value_digits Number of decimal places for numeric values. If `NULL`,
#'   `scales::number()` chooses the default display.
#' @param value_suffix Optional suffix appended to the formatted value, such as
#'   `" cases"`, `"%"`, or `"/100,000"`.
#' @param value_fun Optional formatting function applied to `value`. When
#'   supplied, it takes precedence over `value_digits` and `value_suffix`.
#' @param title_case Logical. Whether to convert `text` to title case. Defaults
#'   to `FALSE` to avoid changing Chinese text or abbreviations such as `MV`,
#'   `DCO`, and `ASR`.
#' @param hjust Horizontal justification for both text elements.
#' @param base_family Base font family. Keep as `""` to use the default device
#'   font, or pass a Chinese-capable font family when needed.
#' @param bg_color Background color of the card.
#' @param plot_margin Plot margin created by `ggplot2::margin()`.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' gg_big_value(12345, "New cases", value_suffix = " cases")
#'
#' gg_big_value(45.2, "Incidence rate", value_digits = 1, value_suffix = "/100,000")
gg_big_value <- function(
  value,
  text,
  value_color = "#B2182B",
  text_color = "grey35",
  value_size = 20,
  text_size = 6,
  value_digits = NULL,
  value_suffix = NULL,
  value_fun = NULL,
  title_case = FALSE,
  hjust = 0,
  base_family = "",
  bg_color = "transparent",
  plot_margin = ggplot2::margin(5, 5, 5, 5)
) {
  if (length(value) != 1 || is.na(value)) {
    stop("`value` must be a single non-missing value.", call. = FALSE)
  }
  if (length(text) != 1 || is.na(text)) {
    stop("`text` must be a single non-missing value.", call. = FALSE)
  }
  .check_single_logical(title_case, "title_case")
  if (!is.null(value_digits)) {
    .check_digits(value_digits, "value_digits")
  }
  .check_single_number(value_size, "value_size", min = 0)
  .check_single_number(text_size, "text_size", min = 0)
  .check_single_number(hjust, "hjust")
  .check_single_character(base_family, "base_family")
  .check_single_character(bg_color, "bg_color")

  value_label <- .format_big_value(
    value = value,
    digits = value_digits,
    suffix = value_suffix,
    value_fun = value_fun
  )

  text_label <- as.character(text)
  if (isTRUE(title_case)) {
    text_label <- stringr::str_to_title(text_label)
  }

  ggplot2::ggplot() +
    ggplot2::geom_text(
      ggplot2::aes(x = 1, y = 1.25, label = value_label),
      family = base_family,
      fontface = "bold",
      size = value_size,
      color = value_color,
      hjust = hjust
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = 1, y = 1.85, label = text_label),
      family = base_family,
      color = text_color,
      size = text_size,
      hjust = hjust
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0.8, 2.1),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0.9, 2),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_void(base_family = base_family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = bg_color, colour = NA),
      panel.background = ggplot2::element_rect(fill = bg_color, colour = NA),
      plot.margin = plot_margin
    )
}


.format_big_value <- function(
  value,
  digits = NULL,
  suffix = NULL,
  value_fun = NULL
) {
  if (!is.null(value_fun)) {
    if (!is.function(value_fun)) {
      stop("`value_fun` must be NULL or a function.", call. = FALSE)
    }
    value <- value_fun(value)
    if (length(value) != 1 || is.na(value)) {
      stop(
        "`value_fun` must return a single non-missing value.",
        call. = FALSE
      )
    }
  } else if (is.numeric(value)) {
    accuracy <- if (is.null(digits)) NULL else 10^-digits
    value <- scales::number(value, accuracy = accuracy)
  } else {
    value <- as.character(value)
  }

  paste0(value, if (is.null(suffix)) "" else suffix)
}
