#' Plot line chart
#'
#' Create a line chart from long-format data or from multiple y columns in
#' wide-format data.
#'
#' @param data Data set to be used for line chart.
#' @param x `numeric`, `character`, `factor`, or date-like column of `data` used
#'   as the x axis.
#' @param y `numeric` column of `data`, or multiple numeric columns such as
#'   `c(y1, y2)` for wide-format data.
#' @param group Optional grouping column. When supplied, one line is drawn for
#'   each group. If `y = c(y1, y2)` is used, `group` is created automatically
#'   from the selected y column names.
#' @param facet Optional faceting column.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param x_lab,y_lab Preferred axis labels.
#' @param legend_title Optional legend title.
#' @param palette Palette name passed to [canreg_palette()] when
#'   `line_color = "auto"`.
#' @param line_color Character vector of line colors, or `"auto"` to use the
#'   default palette.
#' @param line_size Numeric. Line width.
#' @param point Logical. Whether to add points to the lines.
#' @param point_size Numeric. Point size.
#' @param add_label Logical. Whether to display y-value labels.
#' @param label_digits Number of decimal places used for value labels.
#' @param label_size Numeric. Label font size.
#' @param y_lim Optional numeric vector of length 2 for y-axis limits.
#' @param x_breaks Optional vector of x-axis breaks. Useful for integer year
#'   axes in trend charts.
#' @param x_labels Optional labels for x-axis breaks.
#' @param x_angle Numeric. Rotation angle for x-axis labels.
#' @param legend Logical. Whether to show legend.
#' @param legend_position Position of legend. Passed to `ggplot2::theme()` when
#'   `facet_legend = FALSE`. When `facet_legend = TRUE`, use one of
#'   `"topright"`, `"topleft"`, `"bottomright"`, `"bottomleft"`, or a numeric
#'   vector of length 2 such as `c(0.1, 0.9)` for fine adjustment inside each
#'   facet panel.
#' @param legend_size Numeric. Font size of the repeated facet legend.
#' @param legend_spacing Numeric. Relative vertical spacing between repeated
#'   facet legend items. Smaller values make the legend more compact.
#' @param legend_direction Optional global legend direction: `"horizontal"` or
#'   `"vertical"`. Only used when `facet_legend = FALSE`.
#' @param legend_nrow,legend_ncol Optional number of rows or columns in the
#'   global legend, passed to `ggplot2::guide_legend()`.
#' @param legend_title_size,legend_text_size Optional font size for global
#'   legend title and text.
#' @param legend_key_width,legend_key_height Optional global legend key width
#'   and height. Use a `grid::unit()` object, or provide a single numeric value
#'   interpreted as lines.
#' @param facet_legend Logical. If `TRUE`, and both `facet` and `group` are
#'   supplied, draw a repeated legend inside each facet panel.
#' @param ncol Number of columns for facets.
#' @param font_family Font family used by plot text and in-panel labels. The
#'   default `"sans"` is portable; use an installed family such as `"Arial"`
#'   or a Chinese-capable family when required by a journal.
#' @param base_size Base font size in points. Defaults to 12 for coordinated
#'   output across ggcanreg charts.
#' @param bg_color Background color for the entire figure, including the plot,
#'   panels, and facet strips. Use `"transparent"` for slide overlays or
#'   transparent PNG export.
#'
#' @return An object of class `ggplot`.
#' @export
#'
gg_linechart <- function(
  data,
  x,
  y,
  group = NULL,
  facet = NULL,
  title = NULL,
  subtitle = NULL,
  x_lab = NULL,
  y_lab = NULL,
  legend_title = NULL,
  palette = "trend",
  line_color = "auto",
  line_size = 1,
  point = TRUE,
  point_size = 2,
  add_label = FALSE,
  label_digits = 1,
  label_size = 3,
  y_lim = NULL,
  x_breaks = NULL,
  x_labels = NULL,
  x_angle = 0,
  legend = TRUE,
  legend_position = "top",
  legend_size = 3,
  legend_spacing = 0.045,
  legend_direction = NULL,
  legend_nrow = NULL,
  legend_ncol = NULL,
  legend_title_size = NULL,
  legend_text_size = NULL,
  legend_key_width = NULL,
  legend_key_height = NULL,
  facet_legend = FALSE,
  ncol = NULL,
  font_family = "sans",
  base_size = 12,
  bg_color = "white"
) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  group_var <- rlang::enquo(group)
  facet_var <- rlang::enquo(facet)
  x_label <- rlang::as_label(x_var)
  y_label <- rlang::as_label(y_var)

  has_group <- !rlang::quo_is_missing(group_var) &&
    !rlang::quo_is_null(group_var)
  has_facet <- !rlang::quo_is_missing(facet_var) &&
    !rlang::quo_is_null(facet_var)

  .check_linechart_vars(data, x_var, y_var, group_var, facet_var)

  if (
    !is.null(y_lim) &&
      (!is.numeric(y_lim) || length(y_lim) != 2 || any(is.na(y_lim)))
  ) {
    stop(
      "`y_lim` must be `NULL` or a numeric vector of length 2.",
      call. = FALSE
    )
  }

  # Store evaluated x values in an internal column so expressions such as
  # x = factor(year) can also be used in plotting and facet legend positioning.
  data <- dplyr::mutate(data, .x_value = !!x_var)
  x_var <- rlang::sym(".x_value")

  # Support wide-format data, e.g. y = c(rate_male, rate_female).
  if (rlang::quo_is_call(y_var, "c")) {
    select_vars <- rlang::quos(!!x_var, !!y_var)
    if (has_facet) {
      select_vars <- c(select_vars, rlang::quos(!!facet_var))
    }

    data <- data |>
      dplyr::select(!!!select_vars) |>
      tidyr::pivot_longer(
        cols = !!y_var,
        names_to = ".series_name",
        values_to = ".y_value"
      )

    y_var <- rlang::sym(".y_value")
    group_var <- rlang::sym(".series_name")
    y_label <- "value"
    has_group <- TRUE
  } else {
    data <- dplyr::mutate(data, .y_value = !!y_var)
    y_var <- rlang::sym(".y_value")
  }

  if (identical(line_color, "auto")) {
    n_colors <- if (has_group) {
      length(unique(dplyr::pull(data, !!group_var)))
    } else {
      1
    }
    line_color <- canreg_palette(palette, n = n_colors)
  }

  if (has_group) {
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = !!x_var,
        y = !!y_var,
        color = !!group_var,
        group = !!group_var
      )
    ) +
      ggplot2::geom_line(linewidth = line_size)

    if (point) {
      p <- p + ggplot2::geom_point(size = point_size)
    }
    if (add_label) {
      p <- p +
        ggplot2::geom_text(
          ggplot2::aes(
            label = scales::number(!!y_var, accuracy = 10^-label_digits)
          ),
          size = label_size,
          family = font_family,
          vjust = -0.6,
          show.legend = FALSE
        )
    }

    p <- p + ggplot2::scale_color_manual(values = line_color, name = NULL)
  } else {
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(x = !!x_var, y = !!y_var, group = 1)
    ) +
      ggplot2::geom_line(linewidth = line_size, color = line_color[1])

    if (point) {
      p <- p + ggplot2::geom_point(size = point_size, color = line_color[1])
    }
    if (add_label) {
      p <- p +
        ggplot2::geom_text(
          ggplot2::aes(
            label = scales::number(!!y_var, accuracy = 10^-label_digits)
          ),
          size = label_size,
          family = font_family,
          vjust = -0.6,
          color = line_color[1]
        )
    }
  }

  if (facet_legend && (!has_facet || !has_group)) {
    warning(
      "`facet_legend = TRUE` only works when both `facet` and `group` are supplied.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(legend_spacing) ||
      length(legend_spacing) != 1 ||
      is.na(legend_spacing) ||
      legend_spacing < 0
  ) {
    stop(
      "`legend_spacing` must be a single non-negative number.",
      call. = FALSE
    )
  }
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

  show_global_legend <- legend && !(facet_legend && has_facet && has_group)

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
      options = legend_options,
      show = !(facet_legend && has_facet && has_group)
    )

  legend_guides <- .legend_guides("color", "legend", legend_options)
  if (!is.null(legend_guides)) {
    p <- p + legend_guides
  }

  p <- p +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = if (is.null(x_lab)) x_label else x_lab,
      y = if (is.null(y_lab)) y_label else y_lab,
      color = legend_title
    )
  if (!is.null(y_lim)) {
    p <- p + ggplot2::scale_y_continuous(limits = y_lim)
  }
  if (!is.null(x_breaks) || !is.null(x_labels)) {
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = x_breaks,
        labels = x_labels
      )
  }
  if (has_facet) {
    p <- p +
      ggplot2::facet_wrap(
        ggplot2::vars(!!facet_var),
        scales = "free",
        ncol = ncol
      )
  }

  if (facet_legend && has_facet && has_group) {
    if (is.numeric(legend_position) && length(legend_position) == 2) {
      if (
        any(is.na(legend_position)) ||
          any(legend_position < 0 | legend_position > 1)
      ) {
        stop(
          "Numeric `legend_position` must be a length-2 vector with values between 0 and 1 when `facet_legend = TRUE`.",
          call. = FALSE
        )
      }
    } else {
      legend_position_name <- match.arg(
        legend_position,
        choices = c("topright", "topleft", "bottomright", "bottomleft")
      )
      legend_position <- switch(
        legend_position_name,
        topright = c(0.95, 0.95),
        topleft = c(0.05, 0.95),
        bottomright = c(0.95, 0.05),
        bottomleft = c(0.05, 0.05)
      )
    }

    x_name <- rlang::as_name(x_var)
    y_name <- rlang::as_name(y_var)
    facet_name <- rlang::as_name(facet_var)
    group_name <- rlang::as_name(group_var)
    group_values <- unique(data[[group_name]])

    legend_data <- tidyr::expand_grid(
      .facet_value = unique(data[[facet_name]]),
      .group_value = group_values
    )
    legend_data[[facet_name]] <- legend_data$.facet_value
    legend_data[[group_name]] <- legend_data$.group_value
    legend_data$.legend_label <- as.character(legend_data$.group_value)
    legend_data$.legend_order <- match(legend_data$.group_value, group_values)

    facet_values <- unique(data[[facet_name]])
    x_data <- data[[x_name]]
    position_data <- dplyr::bind_rows(lapply(facet_values, function(f) {
      x_values <- data[[x_name]][data[[facet_name]] == f]
      if (!is.null(y_lim)) {
        y_range <- y_lim
      } else {
        y_values <- data[[y_name]][data[[facet_name]] == f]
        y_range <- range(y_values, na.rm = TRUE)
      }
      y_span <- diff(y_range)
      if (!is.finite(y_span) || y_span == 0) {
        y_span <- 1
      }
      y_anchor <- y_range[1] + y_span * legend_position[2]
      y_spacing <- y_span * legend_spacing
      y_dir <- if (legend_position[2] >= 0.5) -1 else 1

      if (
        inherits(x_data, "Date") ||
          inherits(x_data, "POSIXct") ||
          inherits(x_data, "POSIXt") ||
          is.numeric(x_data)
      ) {
        x_is_date <- inherits(x_data, "Date")
        x_is_posix <- inherits(x_data, "POSIXct") || inherits(x_data, "POSIXt")
        x_numeric <- if (x_is_date || x_is_posix) {
          as.numeric(x_values)
        } else {
          x_values
        }
        x_range <- range(x_numeric, na.rm = TRUE)
        x_span <- diff(x_range)
        if (!is.finite(x_span) || x_span == 0) {
          x_span <- 1
        }
        x_anchor <- x_range[1] + x_span * legend_position[1]
        line_len <- x_span * 0.06
        text_gap <- x_span * 0.08
        if (legend_position[1] >= 0.5) {
          x_text <- x_anchor
          x_start <- x_anchor - text_gap - line_len
          x_end <- x_anchor - text_gap
        } else {
          x_start <- x_anchor
          x_end <- x_anchor + line_len
          x_text <- x_anchor + line_len + text_gap
        }
        x_point <- (x_start + x_end) / 2
        if (x_is_date) {
          x_start <- as.Date(x_start, origin = "1970-01-01")
          x_end <- as.Date(x_end, origin = "1970-01-01")
          x_point <- as.Date(x_point, origin = "1970-01-01")
          x_text <- as.Date(x_text, origin = "1970-01-01")
        } else if (x_is_posix) {
          x_start <- as.POSIXct(x_start, origin = "1970-01-01")
          x_end <- as.POSIXct(x_end, origin = "1970-01-01")
          x_point <- as.POSIXct(x_point, origin = "1970-01-01")
          x_text <- as.POSIXct(x_text, origin = "1970-01-01")
        }
      } else {
        x_levels <- unique(as.character(x_values))
        x_index <- max(
          1,
          round(1 + (length(x_levels) - 1) * legend_position[1])
        )
        if (legend_position[1] >= 0.5) {
          x_text <- x_levels[x_index]
          x_start <- x_levels[max(1, x_index - 1)]
          x_end <- x_text
        } else {
          x_start <- x_levels[x_index]
          x_end <- x_levels[min(length(x_levels), x_index + 1)]
          x_text <- x_end
        }
        x_point <- x_end
      }

      data.frame(
        .facet_value = f,
        .legend_x_start = x_start,
        .legend_x_end = x_end,
        .legend_x_point = x_point,
        .legend_x_text = x_text,
        .legend_y = y_anchor,
        .legend_y_spacing = y_spacing,
        .legend_y_dir = y_dir
      )
    }))
    legend_data <- dplyr::left_join(
      legend_data,
      position_data,
      by = ".facet_value"
    )
    legend_data$.legend_y_item <- legend_data$.legend_y +
      (legend_data$.legend_order - 1) *
        legend_data$.legend_y_spacing *
        legend_data$.legend_y_dir

    hjust <- if (legend_position[1] >= 0.5) 1.05 else -0.05

    p <- p +
      ggplot2::geom_segment(
        data = legend_data,
        ggplot2::aes(
          x = .data$.legend_x_start,
          xend = .data$.legend_x_end,
          y = .data$.legend_y_item,
          yend = .data$.legend_y_item,
          color = !!group_var
        ),
        linewidth = line_size,
        inherit.aes = FALSE,
        show.legend = FALSE
      )

    if (point) {
      p <- p +
        ggplot2::geom_point(
          data = legend_data,
          ggplot2::aes(
            x = .data$.legend_x_point,
            y = .data$.legend_y_item,
            color = !!group_var
          ),
          size = point_size,
          inherit.aes = FALSE,
          show.legend = FALSE
        )
    }

    p <- p +
      ggplot2::geom_text(
        data = legend_data,
        ggplot2::aes(
          x = .data$.legend_x_text,
          y = .data$.legend_y_item,
          label = .data$.legend_label,
          color = !!group_var
        ),
        hjust = hjust,
        vjust = 0.5,
        size = legend_size,
        family = font_family,
        inherit.aes = FALSE,
        show.legend = FALSE
      )
  }

  return(p)
}


.check_linechart_vars <- function(data, x, y, group, facet) {
  .check_data_frame(data)

  .check_linechart_var(data, x, "x")
  .check_linechart_var(data, y, "y")
  .check_linechart_var(data, group, "group", optional = TRUE)
  .check_linechart_var(data, facet, "facet", optional = TRUE)

  invisible(TRUE)
}


.check_linechart_var <- function(data, var, arg, optional = FALSE) {
  if (optional && (rlang::quo_is_missing(var) || rlang::quo_is_null(var))) {
    return(invisible(TRUE))
  }

  expr <- rlang::quo_get_expr(var)
  missing_vars <- character()

  if (rlang::is_symbol(expr)) {
    var_name <- rlang::as_name(expr)
    if (!var_name %in% names(data)) {
      missing_vars <- var_name
    }
  } else if (identical(arg, "y") && rlang::is_call(expr, "c")) {
    y_vars <- as.list(expr)[-1]
    y_var_names <- vapply(
      y_vars,
      function(y_var) {
        if (rlang::is_symbol(y_var)) {
          rlang::as_name(y_var)
        } else {
          NA_character_
        }
      },
      character(1)
    )
    y_var_names <- stats::na.omit(y_var_names)
    missing_vars <- setdiff(y_var_names, names(data))
  }

  if (length(missing_vars) > 0) {
    stop(
      sprintf(
        "\u53d8\u91cf %s \u4e0d\u5b58\u5728\u4e8e `data` \u4e2d\u3002\u8bf7\u68c0\u67e5\u5217\u540d\uff0c\u6216\u4f7f\u7528 `names(data)` \u67e5\u770b\u53ef\u7528\u53d8\u91cf\u3002",
        paste(missing_vars, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
