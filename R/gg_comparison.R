#' Highlight selected items in a one-dimensional comparison plot
#'
#' @description
#' `gg_comparison()` displays all observations along one numeric axis and
#' highlights one or more selected items. It is useful for comparing one cancer,
#' registry, area, or other unit against a reference distribution.
#'
#' @param data A data frame.
#' @param x Numeric variable used for comparison.
#' @param item Variable identifying comparison items, such as cancer site,
#'   registry, or area.
#' @param highlight Value or values in `item` to highlight.
#' @param x_breaks Optional numeric vector specifying x-axis break positions.
#'   If `NULL`, breaks are generated automatically and include the minimum
#'   value of `x`.
#' @param x_labels Label function for the x axis. Defaults to
#'   `scales::number_format(accuracy = 0.1)`. Use
#'   `scales::percent_format()` for proportions stored as 0--1.
#' @param point_color Color for non-highlighted items.
#' @param highlight_color Color for highlighted items.
#' @param point_size Size of non-highlighted item marks.
#' @param highlight_size Size of highlighted item marks.
#' @param add_label Logical. Whether to add labels for highlighted items.
#' @param label_digits Number of decimal places used in highlighted labels.
#' @param label_size Size of highlighted item labels.
#' @param label_y Y position of highlighted item labels.
#' @param check_overlap Logical. Whether to check for and omit overlapping text
#'   labels.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param x_lab X-axis title.
#' @param font_family Font family used by plot text and highlighted labels.
#' @param base_size Base font size in points. Defaults to 12 for coordinated
#'   output across ggcanreg charts.
#' @param bg_color Background color for the entire figure, including the plot
#'   panel. Use `"transparent"` for slide overlays.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' cancer_data <- data.frame(
#'   cancer = c("Lung", "Breast", "Stomach", "Liver"),
#'   incidence = c(45.2, 38.1, 22.5, 28.7)
#' )
#'
#' gg_comparison(cancer_data, incidence, cancer, "Lung")
gg_comparison <- function(
  data,
  x,
  item,
  highlight,
  x_breaks = NULL,
  x_labels = scales::number_format(accuracy = 0.1),
  point_color = "grey80",
  highlight_color = "#D55E00",
  point_size = 4,
  highlight_size = 8,
  add_label = TRUE,
  label_digits = 1,
  label_size = 3.5,
  label_y = 1.12,
  check_overlap = FALSE,
  title = NULL,
  subtitle = NULL,
  x_lab = NULL,
  font_family = "sans",
  base_size = 12,
  bg_color = "white"
) {
  x_name <- rlang::as_name(rlang::ensym(x))
  item_name <- rlang::as_name(rlang::ensym(item))

  .check_data_frame(data)

  missing_vars <- setdiff(c(x_name, item_name), names(data))
  if (length(missing_vars) > 0) {
    stop(
      sprintf(
        "Variable(s) not found in `data`: %s.",
        paste(missing_vars, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!is.numeric(data[[x_name]])) {
    stop(sprintf("Variable `%s` must be numeric.", x_name), call. = FALSE)
  }
  if (anyNA(data[[x_name]]) || any(!is.finite(data[[x_name]]))) {
    stop(
      sprintf(
        "Variable `%s` must contain only finite, non-missing values.",
        x_name
      ),
      call. = FALSE
    )
  }

  if (!is.null(x_breaks) && !is.numeric(x_breaks)) {
    stop("`x_breaks` must be a numeric vector or NULL.", call. = FALSE)
  }

  if (is.null(x_breaks)) {
    x_range <- range(data[[x_name]], na.rm = TRUE)
    pretty_breaks <- sort(unique(scales::breaks_pretty()(x_range)))
    if (length(pretty_breaks) > 1) {
      min_spacing <- min(diff(pretty_breaks))
      pretty_breaks <- pretty_breaks[
        abs(pretty_breaks - x_range[1]) >= min_spacing / 2
      ]
    }
    x_breaks <- sort(unique(c(x_range[1], pretty_breaks)))
  }

  .check_single_logical(add_label, "add_label")
  .check_single_logical(check_overlap, "check_overlap")
  .check_digits(label_digits, "label_digits")
  .check_single_number(point_size, "point_size", min = 0)
  .check_single_number(highlight_size, "highlight_size", min = 0)
  .check_single_number(label_size, "label_size", min = 0)
  .check_single_number(label_y, "label_y")

  highlight_data <- data[data[[item_name]] %in% highlight, , drop = FALSE]

  if (nrow(highlight_data) == 0) {
    stop("No rows match `highlight` in the `item` variable.", call. = FALSE)
  }

  highlight_data <- highlight_data[
    order(highlight_data[[x_name]]),
    ,
    drop = FALSE
  ]
  highlight_data$.ggcanreg_comparison_label_y <- label_y +
    ((seq_len(nrow(highlight_data)) - 1) %% 3) * 0.035
  y_upper <- max(
    1.18,
    highlight_data$.ggcanreg_comparison_label_y + 0.05
  )

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[[x_name]], y = 1)
  ) +
    ggplot2::geom_point(
      shape = 124,
      color = point_color,
      size = point_size
    ) +
    ggplot2::geom_point(
      data = highlight_data,
      shape = 124,
      color = highlight_color,
      size = highlight_size
    ) +
    ggplot2::scale_x_continuous(breaks = x_breaks, labels = x_labels) +
    ggplot2::scale_y_continuous(limits = c(0.95, y_upper)) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_lab,
      y = NULL
    ) +
    .journal_chart_theme(
      base_size = base_size,
      font_family = font_family,
      legend_position = "none",
      grid = "none",
      subtitle = subtitle,
      bg_color = bg_color
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.text.y.left = ggplot2::element_blank(),
      axis.text.y.right = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.title.y.left = ggplot2::element_blank(),
      axis.title.y.right = ggplot2::element_blank()
    )

  if (isTRUE(add_label)) {
    label_accuracy <- 10^-label_digits

    p <- p +
      ggplot2::geom_text(
        data = highlight_data,
        ggplot2::aes(
          x = .data[[x_name]],
          y = .data$.ggcanreg_comparison_label_y,
          label = paste0(
            .data[[item_name]],
            ": ",
            scales::number(.data[[x_name]], accuracy = label_accuracy)
          )
        ),
        color = highlight_color,
        size = label_size,
        family = font_family,
        vjust = 0,
        check_overlap = check_overlap,
        inherit.aes = FALSE
      )
  }

  p
}


# Backward-compatible internal alias for the earlier prototype.
comparison <- gg_comparison
