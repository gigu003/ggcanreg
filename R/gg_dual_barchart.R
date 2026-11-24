#' Create a dual bar chart
#'
#' @param data The input data frame.
#' @param x The variable for the categories (quoted or unquoted).
#' @param y The variable for the values (quoted or unquoted).
#' @param group The grouping variable (quoted or unquoted), must have exactly two unique values.
#' @param legend_labels Labels for the two groups.
#' @param fill Optional fill/grouping variable passed to [gg_barchart()] inside
#'   each side. If `NULL`, bars are drawn with `highlight_color` and `bar_color`.
#' @param highlight_color Color for the first group's bars (if fill is NULL).
#' @param bar_color Color for the second group's bars (if fill is NULL).
#' @param top_n Number of top categories to display on each side. Selection is
#'   performed independently within each of the two `group` values.
#' @param palette Palette name passed to [gg_barchart()] when `fill` is used.
#' @param add_label Whether to add value labels.
#' @param label_digits Number of decimal places used for value labels.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param legend_title Optional legend title when `fill` is used.
#' @param legend_position Position of legend. Passed to `ggplot2::theme()`.
#' @param legend_direction Optional legend direction: `"horizontal"` or
#'   `"vertical"`.
#' @param legend_nrow,legend_ncol Optional number of rows or columns in the
#'   legend, passed to `ggplot2::guide_legend()`.
#' @param legend_title_size,legend_text_size Optional font size for legend
#'   title and text.
#' @param legend_key_width,legend_key_height Optional legend key width and
#'   height. Use a `grid::unit()` object, or provide a single numeric value
#'   interpreted as lines.
#' @param font_family Font family used by plot text and value labels. The
#'   default `"sans"` is portable; use an installed family required by the
#'   target journal when necessary.
#' @param base_size Base font size in points. Defaults to 12 for coordinated
#'   output across ggcanreg charts.
#' @param bg_color Background color for the complete combined figure and both
#'   panels. Use `"transparent"` for slide overlays or transparent PNG export.
#'
#' @return A ggplot object combining two bar charts side by side.
#' @export
#'
gg_dual_barchart <- function(
  data,
  x,
  y,
  group,
  fill = NULL,
  top_n = 10,
  legend_labels = c("Incidence", "Mortality"),
  highlight_color = "#F18222",
  bar_color = "#01847C",
  palette = "default",
  add_label = FALSE,
  label_digits = 1,
  title = NULL,
  subtitle = NULL,
  legend_title = NULL,
  legend_position = "top",
  legend_direction = NULL,
  legend_nrow = NULL,
  legend_ncol = NULL,
  legend_title_size = NULL,
  legend_text_size = NULL,
  legend_key_width = NULL,
  legend_key_height = NULL,
  font_family = "sans",
  base_size = 12,
  bg_color = "white"
) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  fill <- rlang::enquo(fill)
  group <- rlang::enquo(group)
  has_fill <- !rlang::quo_is_null(fill)

  .check_data_frame(data)
  .check_dual_barchart_variable(data, x, "x")
  .check_dual_barchart_variable(data, y, "y")
  .check_dual_barchart_variable(data, group, "group")
  if (has_fill) {
    .check_dual_barchart_variable(data, fill, "fill")
  }

  y_values <- dplyr::pull(data, !!y)
  if (!is.numeric(y_values)) {
    stop("`y` must be a numeric column.", call. = FALSE)
  }
  if (anyNA(y_values) || any(!is.finite(y_values))) {
    stop("`y` must contain only finite, non-missing values.", call. = FALSE)
  }
  shared_y_lim <- value_axis_limits(data, !!y)

  if (
    !is.numeric(top_n) ||
      length(top_n) != 1 ||
      is.na(top_n) ||
      !is.finite(top_n) ||
      top_n < 1 ||
      top_n != floor(top_n)
  ) {
    stop("`top_n` must be a single positive integer.", call. = FALSE)
  }
  .check_single_logical(add_label, "add_label")
  .check_digits(label_digits, "label_digits")
  if (
    !is.character(legend_labels) ||
      length(legend_labels) != 2 ||
      anyNA(legend_labels)
  ) {
    stop(
      "`legend_labels` must be a character vector of length 2.",
      call. = FALSE
    )
  }
  .check_single_character(highlight_color, "highlight_color")
  .check_single_character(bar_color, "bar_color")

  group_values <- dplyr::pull(data, !!group) |> unique()
  if (anyNA(group_values) || length(group_values) != 2) {
    stop(
      "The `group` variable must contain exactly two non-missing unique values.",
      call. = FALSE
    )
  }
  duldata <- dplyr::group_by(data, !!group) |> group_split()
  if (has_fill) {
    plot_left <- gg_barchart(
      duldata[[1]],
      !!x,
      !!y,
      y_lim = shared_y_lim,
      group = !!fill,
      top_n = top_n,
      reverse = TRUE,
      x_label_position = "left",
      bar_color = highlight_color,
      palette = palette,
      add_label = add_label,
      label_digits = label_digits,
      legend = FALSE,
      legend_title = legend_title,
      legend_position = legend_position,
      legend_direction = legend_direction,
      legend_nrow = legend_nrow,
      legend_ncol = legend_ncol,
      legend_title_size = legend_title_size,
      legend_text_size = legend_text_size,
      legend_key_width = legend_key_width,
      legend_key_height = legend_key_height,
      font_family = font_family,
      base_size = base_size,
      bg_color = bg_color
    )
    plot_right <- gg_barchart(
      duldata[[2]],
      !!x,
      !!y,
      y_lim = shared_y_lim,
      group = !!fill,
      top_n = top_n,
      x_label_position = "right",
      bar_color = bar_color,
      palette = palette,
      add_label = add_label,
      label_digits = label_digits,
      legend = TRUE,
      legend_title = legend_title,
      legend_position = legend_position,
      legend_direction = legend_direction,
      legend_nrow = legend_nrow,
      legend_ncol = legend_ncol,
      legend_title_size = legend_title_size,
      legend_text_size = legend_text_size,
      legend_key_width = legend_key_width,
      legend_key_height = legend_key_height,
      font_family = font_family,
      base_size = base_size,
      bg_color = bg_color
    )
  } else {
    plot_left <- gg_barchart(
      duldata[[1]],
      !!x,
      !!y,
      y_lim = shared_y_lim,
      top_n = top_n,
      reverse = TRUE,
      x_label_position = "left",
      bar_color = highlight_color,
      palette = palette,
      add_label = add_label,
      label_digits = label_digits,
      font_family = font_family,
      base_size = base_size,
      bg_color = bg_color
    )
    plot_right <- gg_barchart(
      duldata[[2]],
      !!x,
      !!y,
      y_lim = shared_y_lim,
      top_n = top_n,
      x_label_position = "right",
      bar_color = bar_color,
      palette = palette,
      add_label = add_label,
      label_digits = label_digits,
      font_family = font_family,
      base_size = base_size,
      bg_color = bg_color
    )
  }

  plot_left <- plot_left +
    xlab(legend_labels[1]) +
    theme(
      legend.position = "none",
      axis.title.x.top = element_text(face = "bold"),
      axis.title.y = element_blank(),
      axis.line.y = element_blank(),
      axis.line.y.left = element_blank(),
      axis.line.y.right = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.y.left = element_blank(),
      axis.ticks.y.right = element_blank(),
      plot.margin = margin(t = 4, r = 10, b = 8, l = 12)
    )

  plot_right <- plot_right +
    xlab(legend_labels[2]) +
    theme(
      axis.title.x.top = element_text(face = "bold"),
      axis.title.y = element_blank(),
      axis.text.y.right = element_text(
        hjust = 0,
        margin = margin(l = 4)
      ),
      axis.line.y = element_blank(),
      axis.line.y.left = element_blank(),
      axis.line.y.right = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.y.left = element_blank(),
      axis.ticks.y.right = element_blank(),
      plot.margin = margin(t = 4, r = 12, b = 8, l = 10)
    )

  annotation_theme <- .journal_chart_theme(
    base_size = base_size,
    font_family = font_family,
    legend_position = "none",
    grid = "none",
    subtitle = subtitle,
    bg_color = bg_color
  )

  combined <- plot_left +
    plot_right +
    patchwork::plot_layout(
      widths = c(1, 1),
      guides = if (has_fill) "collect" else "keep"
    ) +
    patchwork::plot_annotation(
      title = title,
      subtitle = subtitle,
      theme = annotation_theme
    )

  if (has_fill) {
    combined <- combined & theme(legend.position = legend_position)
  }

  combined
}


.check_dual_barchart_variable <- function(data, var, arg) {
  if (rlang::quo_is_missing(var) || rlang::quo_is_null(var)) {
    stop(sprintf("`%s` must be supplied.", arg), call. = FALSE)
  }

  var_name <- rlang::as_name(var)
  if (!var_name %in% names(data)) {
    stop(
      sprintf("Variable `%s` was not found in `data`.", var_name),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Create a dual bar chart
#'
#' @description
#' `gg_dul_barchart()` is kept as a backward-compatible alias for
#' [gg_dual_barchart()]. New code should use `gg_dual_barchart()`.
#'
#' @inheritParams gg_dual_barchart
#'
#' @return A ggplot object combining two bar charts side by side.
#' @export
gg_dul_barchart <- function(
  data,
  x,
  y,
  group,
  fill = NULL,
  top_n = 10,
  legend_labels = c("Incidence", "Mortality"),
  highlight_color = "#F18222",
  bar_color = "#01847C",
  palette = "default",
  add_label = FALSE,
  label_digits = 1,
  title = NULL,
  subtitle = NULL,
  legend_title = NULL,
  legend_position = "top",
  legend_direction = NULL,
  legend_nrow = NULL,
  legend_ncol = NULL,
  legend_title_size = NULL,
  legend_text_size = NULL,
  legend_key_width = NULL,
  legend_key_height = NULL,
  font_family = "sans",
  base_size = 12,
  bg_color = "white"
) {
  gg_dual_barchart(
    data = data,
    x = {{ x }},
    y = {{ y }},
    group = {{ group }},
    fill = {{ fill }},
    top_n = top_n,
    legend_labels = legend_labels,
    highlight_color = highlight_color,
    bar_color = bar_color,
    palette = palette,
    add_label = add_label,
    label_digits = label_digits,
    title = title,
    subtitle = subtitle,
    legend_title = legend_title,
    legend_position = legend_position,
    legend_direction = legend_direction,
    legend_nrow = legend_nrow,
    legend_ncol = legend_ncol,
    legend_title_size = legend_title_size,
    legend_text_size = legend_text_size,
    legend_key_width = legend_key_width,
    legend_key_height = legend_key_height,
    font_family = font_family,
    base_size = base_size,
    bg_color = bg_color
  )
}
