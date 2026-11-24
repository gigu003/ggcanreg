#' Draw a stacked bar or area chart for compositional change
#'
#' @description
#' Create a stacked bar chart or stacked area chart to visualize compositional
#' changes over time or across categories. Common use cases in cancer registry
#' work include cancer site composition by year, age-group composition trends,
#' and quality indicator composition changes.
#'
#' @param data A data frame.
#' @param x Variable used for the x axis (e.g., `year`, `agegrp`).
#' @param y Numeric column for the bar/area height (e.g., `cases`, `incidence`).
#' @param fill Variable used for stack grouping (e.g., `cancer`, `sex`).
#' @param type Chart type: `"bar"` for stacked bar chart (default) or `"area"`
#'   for stacked area chart.
#' @param position Stack position: `"fill"` for 100% stacked composition
#'   (default), `"stack"` for raw-value stacking.
#' @param palette Palette name passed to [scale_fill_canreg()].
#' @param sort_by_y Character. How to order the fill segments within each
#'   x-category based on y values: `"none"` (default, keep data order),
#'   `"desc"` (descending y), or `"asc"` (ascending y).
#' @param add_label Logical. Whether to display value labels on the stack
#'   segments.
#' @param label_digits Number of decimal places for value labels.
#' @param label_size Numeric. Font size for value labels.
#' @param label_color Character. Color of value labels.
#' @param horizontal Logical. If `TRUE` (default), draw a horizontal bar chart.
#'   Only used when `type = "bar"`.
#' @param reverse Logical. If `TRUE`, reverse the value axis direction.
#' @param bar_width Numeric. Bar width, passed to `ggplot2::geom_col()`. Only
#'   used when `type = "bar"`.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param x_lab,y_lab Preferred axis labels. If supplied, they take precedence
#'   over `axis_title_x` and `axis_title_y`.
#' @param legend_title Optional legend title.
#' @param legend Logical. Whether to show the legend.
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
#' @param x_angle Numeric. Rotation angle for x-axis text labels.
#' @param axis_title_x Character or expression. Global title of the x axis. If
#'   `NULL`, the input x variable name is used.
#' @param axis_title_y Character or expression. Global title of the y axis. If
#'   `NULL`, meaningful defaults are chosen based on `position`.
#' @param font_family Font family used by plot text and value labels.
#' @param base_size Base font size in points. Defaults to 12 for coordinated
#'   output across ggcanreg charts.
#' @param bg_color Background color for the entire figure, including the plot
#'   panel. Use `"transparent"` for slide overlays.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' # Cancer site composition by year (100% stacked bar)
#' library(ggplot2)
#' trend_data <- data.frame(
#'   year = rep(2020:2022, each = 3),
#'   cancer = rep(c("Lung", "Stomach", "Liver"), 3),
#'   cases = c(45, 28, 31, 42, 25, 28, 40, 22, 30)
#' )
#'
#' gg_stack(trend_data, year, cases, fill = cancer) +
#'   labs(title = "Cancer site composition by year")
#'
#' # Raw-value stacked area chart
#' gg_stack(trend_data, year, cases, fill = cancer,
#'          type = "area", position = "stack") +
#'   labs(title = "Cancer cases trend by site")
gg_stack <- function(
  data,
  x,
  y,
  fill,
  type = c("bar", "area"),
  position = c("fill", "stack"),
  palette = "default",
  sort_by_y = c("none", "desc", "asc"),
  add_label = FALSE,
  label_digits = 1,
  label_size = 3,
  label_color = "white",
  horizontal = TRUE,
  reverse = FALSE,
  bar_width = 0.75,
  title = NULL,
  subtitle = NULL,
  x_lab = NULL,
  y_lab = NULL,
  legend_title = NULL,
  legend = TRUE,
  legend_position = "top",
  legend_direction = NULL,
  legend_nrow = NULL,
  legend_ncol = NULL,
  legend_title_size = NULL,
  legend_text_size = NULL,
  legend_key_width = NULL,
  legend_key_height = NULL,
  x_angle = 0,
  axis_title_x = NULL,
  axis_title_y = NULL,
  font_family = "sans",
  base_size = 12,
  bg_color = "white"
) {
  # --- Argument validation ---
  .check_data_frame(data)

  # --- Tidy evaluation ---
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  fill_var <- rlang::enquo(fill)
  .check_data_variable(data, x_var, "x")
  .check_data_variable(data, y_var, "y")
  .check_data_variable(data, fill_var, "fill")

  type <- match.arg(type)
  position <- match.arg(position)
  sort_by_y <- match.arg(sort_by_y)

  y_values <- dplyr::pull(data, !!y_var)
  if (!is.numeric(y_values)) {
    stop("`y` must be a numeric column.", call. = FALSE)
  }
  if (anyNA(y_values) || any(!is.finite(y_values))) {
    stop("`y` must contain only finite, non-missing values.", call. = FALSE)
  }
  if (position == "fill") {
    x_name <- rlang::as_name(x_var)
    group_totals <- tapply(y_values, data[[x_name]], sum)
    if (any(group_totals <= 0)) {
      stop(
        "Each `x` group must have a positive total when `position = \"fill\"`.",
        call. = FALSE
      )
    }
  }

  .check_single_logical(horizontal, "horizontal")
  .check_single_logical(reverse, "reverse")
  .check_single_logical(add_label, "add_label")
  .check_digits(label_digits, "label_digits")
  .check_single_number(label_size, "label_size", min = 0)
  .check_single_character(label_color, "label_color")
  if (
    !is.numeric(bar_width) ||
      length(bar_width) != 1 ||
      is.na(bar_width) ||
      bar_width <= 0 ||
      bar_width > 1
  ) {
    stop("`bar_width` must be a single number between 0 and 1.", call. = FALSE)
  }
  .check_single_number(x_angle, "x_angle")
  .check_single_logical(legend, "legend")
  .check_single_character(font_family, "font_family")
  .check_single_number(base_size, "base_size", min = 0)
  .check_bg_color(bg_color)
  legend_options <- .legend_options(
    legend_position = legend_position,
    legend_direction = legend_direction,
    legend_nrow = legend_nrow,
    legend_ncol = legend_ncol,
    legend_title_size = legend_title_size,
    legend_text_size = legend_text_size,
    legend_key_width = legend_key_width,
    legend_key_height = legend_key_height
  )

  if (is.null(x_lab)) {
    x_lab <- axis_title_x
  }
  if (is.null(y_lab)) {
    y_lab <- axis_title_y
  }

  # --- Pre-compute number of fill levels for colour scale ---
  fill_data <- dplyr::pull(data, !!fill_var)
  n_fill <- dplyr::n_distinct(fill_data, na.rm = TRUE)

  # --- Sort fill segments by y value ---
  if (sort_by_y != "none") {
    fill_levels <- data |>
      dplyr::group_by(!!fill_var) |>
      dplyr::summarise(
        .y_total = sum(!!y_var, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(
        if (sort_by_y == "desc") dplyr::desc(.data$.y_total) else .data$.y_total
      ) |>
      dplyr::pull(!!fill_var)

    data <- data |>
      dplyr::mutate(
        !!fill_var := factor(!!fill_var, levels = fill_levels)
      )
  }

  # --- Position helper ---
  pos_fun <- if (position == "fill") {
    ggplot2::position_fill(vjust = 0.5, reverse = !reverse)
  } else {
    ggplot2::position_stack(vjust = 0.5, reverse = !reverse)
  }

  # --- Build base plot ---
  if (type == "area") {
    # Stacked area chart
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = !!x_var,
        y = !!y_var,
        fill = !!fill_var,
        group = !!fill_var
      )
    ) +
      ggplot2::geom_area(
        position = if (position == "fill") "fill" else "stack",
        na.rm = TRUE
      )

    # Labels for area chart
    if (add_label) {
      p <- p +
        ggplot2::geom_text(
          data = function(d) {
            d |>
              dplyr::group_by(!!x_var) |>
              dplyr::mutate(
                .cumsum = cumsum(!!y_var),
                .ypos = .cumsum - !!y_var / 2,
                .label = scales::number(!!y_var, accuracy = 10^-label_digits)
              ) |>
              dplyr::ungroup()
          },
          ggplot2::aes(y = .data$.ypos, label = .data$.label),
          size = label_size,
          family = font_family,
          color = label_color,
          show.legend = FALSE
        )
    }
  } else {
    # Stacked bar chart
    if (horizontal) {
      p <- ggplot2::ggplot(
        data,
        ggplot2::aes(x = !!y_var, y = !!x_var, fill = !!fill_var)
      ) +
        ggplot2::geom_col(
          width = bar_width,
          position = if (position == "fill") "fill" else "stack"
        )

      if (add_label) {
        p <- p +
          ggplot2::geom_text(
            ggplot2::aes(
              label = scales::number(!!y_var, accuracy = 10^-label_digits)
            ),
            position = pos_fun,
            size = label_size,
            family = font_family,
            color = label_color,
            show.legend = FALSE
          )
      }
    } else {
      p <- ggplot2::ggplot(
        data,
        ggplot2::aes(x = !!x_var, y = !!y_var, fill = !!fill_var)
      ) +
        ggplot2::geom_col(
          width = bar_width,
          position = if (position == "fill") "fill" else "stack"
        )

      if (add_label) {
        p <- p +
          ggplot2::geom_text(
            ggplot2::aes(
              label = scales::number(!!y_var, accuracy = 10^-label_digits)
            ),
            position = pos_fun,
            size = label_size,
            family = font_family,
            color = label_color,
            show.legend = FALSE
          )
      }
    }
  }

  # --- Scales ---
  p <- p + scale_fill_canreg(palette = palette, n = n_fill)

  pct_expand <- ggplot2::expansion(mult = c(0, 0.05))

  if (position == "fill") {
    if (type == "bar" && horizontal) {
      if (reverse) {
        p <- p +
          ggplot2::scale_x_reverse(
            labels = scales::percent,
            expand = pct_expand
          )
      } else {
        p <- p +
          ggplot2::scale_x_continuous(
            labels = scales::percent,
            expand = pct_expand
          )
      }
    } else if (type == "bar" && !horizontal) {
      if (reverse) {
        p <- p +
          ggplot2::scale_y_reverse(
            labels = scales::percent,
            expand = pct_expand
          )
      } else {
        p <- p +
          ggplot2::scale_y_continuous(
            labels = scales::percent,
            expand = pct_expand
          )
      }
    } else {
      # Area chart
      if (reverse) {
        p <- p +
          ggplot2::scale_y_reverse(
            labels = scales::percent,
            expand = pct_expand
          )
      } else {
        p <- p +
          ggplot2::scale_y_continuous(
            labels = scales::percent,
            expand = pct_expand
          )
      }
    }
  }

  # --- Default axis labels ---
  x_label <- rlang::as_label(x_var)

  if (is.null(y_lab)) {
    y_lab <- if (position == "fill") "Proportion" else "Value"
  }

  # --- Theme ---
  p <- p +
    .journal_chart_theme(
      base_size = base_size,
      font_family = font_family,
      legend_position = legend_position,
      grid = "y",
      subtitle = subtitle,
      x_angle = x_angle,
      bg_color = bg_color
    ) +
    .legend_theme(
      legend = legend,
      legend_title = legend_title,
      options = legend_options
    )

  legend_guides <- .legend_guides("fill", "legend", legend_options)
  if (!is.null(legend_guides)) {
    p <- p + legend_guides
  }

  # --- Labels ---
  p <- p +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = if (is.null(x_lab)) x_label else x_lab,
      y = y_lab,
      fill = legend_title
    )

  return(p)
}
