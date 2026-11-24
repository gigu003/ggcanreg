#' Create a heatmap with optional facets
#'
#' @description
#' Generate a standardized heatmap using ggplot2. The fill color represents a
#' numeric indicator and the plot can optionally be faceted by a categorical
#' variable.
#'
#' @param data A data frame to be used for plot.
#' @param x \code{character} or \code{factor} column of \code{data}.
#' @param y  \code{character} or \code{factor} column of \code{data}.
#' @param fill \code{numeric} A numeric variable whose values will be
#'        represented by the fill color of the heatmap tiles.
#' @param facet An optional categorical variable used for faceting the plot
#'        into multiple subplots.
#' @param palette Palette name passed to [scale_fill_canreg()].
#' @param add_label Logical. Whether to show fill values on tiles.
#' @param label_digits Number of decimal places used for labels.
#' @param label_format Label format. Use `"number"` for numeric labels or
#'        `"percent"` for percentage labels.
#' @param label_scale Multiplier applied before percent formatting. The default
#'        `1` is suitable when percentages are already stored as `23.5`.
#' @param label_size Label font size.
#' @param label_color Label color.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param x_lab,y_lab Preferred axis labels.
#' @param legend_title Optional legend title.
#' @param legend Logical. Whether to show the legend.
#' @param legend_position Position of legend. Passed to `ggplot2::theme()`.
#' @param legend_direction Optional legend direction: `"horizontal"` or
#'        `"vertical"`.
#' @param legend_title_size,legend_text_size Optional font size for legend
#'        title and text.
#' @param legend_key_width,legend_key_height Optional legend key width and
#'        height. Use a `grid::unit()` object, or provide a single numeric value
#'        interpreted as lines.
#' @param font_family Font family used by plot text and tile labels.
#' @param base_size Base font size in points. Defaults to 12 for coordinated
#'        output across ggcanreg charts.
#' @param bg_color Background color for the entire figure, including the plot
#'        panel and facet strips. Use `"transparent"` for slide overlays.
#'
#' @return A ggplot object representing the heatmap.
#' @export
#'
gg_heatmap <- function(
  data,
  x,
  y,
  fill,
  facet = NULL,
  palette = "map_blue",
  add_label = TRUE,
  label_digits = 1,
  label_format = c("number", "percent"),
  label_scale = 1,
  label_size = 3,
  label_color = "black",
  title = NULL,
  subtitle = NULL,
  x_lab = NULL,
  y_lab = NULL,
  legend_title = NULL,
  legend = TRUE,
  legend_position = "top",
  legend_direction = NULL,
  legend_title_size = NULL,
  legend_text_size = NULL,
  legend_key_width = NULL,
  legend_key_height = NULL,
  font_family = "sans",
  base_size = 12,
  bg_color = "white"
) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  fill_var <- rlang::enquo(fill)
  facet_var <- rlang::enquo(facet)
  label_format <- match.arg(label_format)
  has_facet <- !rlang::quo_is_null(facet_var)

  .check_data_frame(data)
  .check_data_variable(data, x_var, "x")
  .check_data_variable(data, y_var, "y")
  .check_data_variable(data, fill_var, "fill")
  .check_data_variable(data, facet_var, "facet", optional = TRUE)

  fill_values <- dplyr::pull(data, !!fill_var)
  if (!is.numeric(fill_values)) {
    stop("`fill` must be a numeric column.", call. = FALSE)
  }
  if (anyNA(fill_values) || any(!is.finite(fill_values))) {
    stop("`fill` must contain only finite, non-missing values.", call. = FALSE)
  }

  .check_single_logical(add_label, "add_label")
  .check_single_logical(legend, "legend")
  .check_digits(label_digits, "label_digits")
  .check_single_number(label_size, "label_size", min = 0)
  .check_single_character(label_color, "label_color")
  legend_options <- .legend_options(
    legend_position = legend_position,
    legend_direction = legend_direction,
    legend_title_size = legend_title_size,
    legend_text_size = legend_text_size,
    legend_key_width = legend_key_width,
    legend_key_height = legend_key_height
  )

  if (
    !is.numeric(label_scale) ||
      length(label_scale) != 1 ||
      is.na(label_scale) ||
      !is.finite(label_scale)
  ) {
    stop("`label_scale` must be a single numeric value.", call. = FALSE)
  }

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = !!x_var, y = !!y_var, fill = !!fill_var)
  ) +
    ggplot2::geom_tile(color = bg_color, linewidth = 0.3) +
    ggplot2::scale_x_discrete(position = "top") +
    scale_fill_canreg(palette = palette, discrete = FALSE) +
    .journal_chart_theme(
      base_size = base_size,
      font_family = font_family,
      legend_position = legend_position,
      grid = "none",
      subtitle = subtitle,
      x_angle = 45,
      bg_color = bg_color
    ) +
    .legend_theme(
      legend = legend,
      legend_title = legend_title,
      options = legend_options
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_lab,
      y = y_lab,
      fill = legend_title
    )

  legend_guides <- .legend_guides("fill", "colorbar", legend_options)
  if (!is.null(legend_guides)) {
    p <- p + legend_guides
  }

  if (isTRUE(add_label)) {
    label_expr <- if (label_format == "percent") {
      rlang::expr(format_percent(
        !!fill_var,
        digits = label_digits,
        scale = label_scale
      ))
    } else {
      rlang::expr(scales::number(!!fill_var, accuracy = 10^-label_digits))
    }
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = !!label_expr),
        vjust = 0.5,
        color = label_color,
        size = label_size,
        family = font_family
      )
  }

  if (has_facet) {
    p <- p +
      ggplot2::facet_wrap(
        ggplot2::vars(!!facet_var),
        strip.position = "top"
      ) +
      ggplot2::theme(strip.placement = "outside")
  }

  p
}
