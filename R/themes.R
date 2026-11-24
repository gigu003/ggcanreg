#' ggcanreg default ggplot2 theme
#'
#' @description
#' `theme_canreg()` provides a unified theme for the `gg_*` plotting functions
#' in ggcanreg. The default style is designed for cancer registry reports,
#' papers, and routine analysis charts. Use `"report"` for readable analytic
#' reports, `"publication"` for compact journal-ready figures, and `"slides"`
#' for projection-friendly presentations. The legacy `default`, `block`,
#' `grid`, and `pie` styles remain available for compatibility.
#'
#' @param style Theme style. Use `"report"` for analytic reports,
#'   `"publication"` for journal figures, or `"slides"` for presentations.
#'   Legacy options are `"default"`, `"block"`, `"grid"`, and `"pie"`.
#' @param base_size Base font size in points. If omitted, it is 12 for report
#'   and legacy styles, 8 for publication, and 18 for slides.
#' @param base_family Base font family. Keep as `""` to use the default device
#'   font, or pass a Chinese-capable font family when needed.
#' @param grid Major grid line direction. One of `"y"`, `"x"`, `"both"`, or
#'   `"none"`.
#' @param legend_position Legend position passed to `ggplot2::theme()`.
#' @param legend_title Logical. Whether to show legend titles.
#' @param title_position Plot title alignment. Use `"left"` or `"center"`.
#' @param title_face Font face for plot titles.
#' @param axis_title Logical. Whether to show axis titles.
#' @param axis_text Logical. Whether to show axis text.
#' @param x_text_angle Angle of x-axis text in degrees.
#' @param strip_fill Facet strip background fill color.
#' @param strip_text_face Font face for facet strip text.
#' @param panel_fill Panel background fill color. If `NULL`, a style-specific
#'   default is used.
#' @param plot_fill Plot background fill color.
#' @param grid_colour Major grid line color. If `NULL`, a style-specific default
#'   is used.
#' @param border_colour Panel border color used by `style = "grid"`. If `NULL`,
#'   a style-specific default is used.
#' @param margin Plot margin. Use a `ggplot2::margin()` object, a single number,
#'   or a numeric vector of length 4 in the order `c(top, right, bottom, left)`.
#'   Numeric values use points (`"pt"`) as the unit.
#'
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(factor(cyl), mpg)) +
#'   geom_boxplot() +
#'   theme_canreg()
theme_canreg <- function(
  style = c(
    "report",
    "publication",
    "slides",
    "default",
    "block",
    "grid",
    "pie"
  ),
  base_size = 12,
  base_family = "",
  grid = c("y", "x", "both", "none"),
  legend_position = "top",
  legend_title = FALSE,
  title_position = c("left", "center"),
  title_face = "bold",
  axis_title = TRUE,
  axis_text = TRUE,
  x_text_angle = 0,
  strip_fill = "grey95",
  strip_text_face = "bold",
  panel_fill = NULL,
  plot_fill = "white",
  grid_colour = NULL,
  border_colour = NULL,
  margin = ggplot2::margin()
) {
  base_size_missing <- missing(base_size)
  grid_missing <- missing(grid)
  legend_position_missing <- missing(legend_position)
  style <- match.arg(style)
  if (base_size_missing) {
    base_size <- switch(
      style,
      publication = 8,
      slides = 18,
      12
    )
  }
  if (grid_missing) {
    grid <- switch(
      style,
      publication = "none",
      slides = "y",
      "y"
    )
  }
  if (legend_position_missing) {
    legend_position <- switch(
      style,
      publication = "bottom",
      slides = "bottom",
      "top"
    )
  }
  grid <- match.arg(grid)
  title_position <- match.arg(title_position)

  .check_single_number(base_size, "base_size", min = 0)
  .check_single_number(x_text_angle, "x_text_angle")
  .check_single_logical(legend_title, "legend_title")
  .check_single_logical(axis_title, "axis_title")
  .check_single_logical(axis_text, "axis_text")

  if (!is.character(base_family) || length(base_family) != 1) {
    stop("`base_family` must be a single character string.", call. = FALSE)
  }
  if (!is.character(title_face) || length(title_face) != 1) {
    stop("`title_face` must be a single character string.", call. = FALSE)
  }
  if (!is.character(strip_text_face) || length(strip_text_face) != 1) {
    stop("`strip_text_face` must be a single character string.", call. = FALSE)
  }

  margin <- .as_plot_margin(margin)

  title_hjust <- if (identical(title_position, "center")) 0.5 else 0
  panel_fill <- panel_fill %||%
    if (identical(style, "grid")) "aliceblue" else "white"
  grid_colour <- grid_colour %||%
    switch(
      style,
      report = "#D9E2EC",
      slides = "#C9D6E4",
      grid = "steelblue",
      "grey85"
    )
  axis_text_colour <- switch(
    style,
    report = "#334E68",
    publication = "black",
    slides = "#243B53",
    "grey25"
  )
  grid_linewidth <- if (identical(style, "slides")) 0.6 else 0.4
  border_colour <- border_colour %||%
    if (identical(style, "grid")) "steelblue" else "grey80"

  legend_title_element <- if (legend_title) {
    ggplot2::element_text(face = "bold", colour = "grey20")
  } else {
    ggplot2::element_blank()
  }
  axis_title_element <- if (axis_title) {
    ggplot2::element_text(face = "bold", colour = "grey20")
  } else {
    ggplot2::element_blank()
  }
  axis_text_element <- if (axis_text) {
    ggplot2::element_text(colour = axis_text_colour)
  } else {
    ggplot2::element_blank()
  }
  axis_text_x_element <- if (axis_text) {
    ggplot2::element_text(
      angle = x_text_angle,
      hjust = if (x_text_angle == 0) 0.5 else 1,
      vjust = if (x_text_angle == 0) 0.5 else 1,
      colour = axis_text_colour
    )
  } else {
    ggplot2::element_blank()
  }

  grid_theme <- switch(
    grid,
    x = ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(
        colour = grid_colour,
        linewidth = grid_linewidth
      ),
      panel.grid.major.y = ggplot2::element_blank()
    ),
    y = ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(
        colour = grid_colour,
        linewidth = grid_linewidth
      )
    ),
    both = ggplot2::theme(
      panel.grid.major = ggplot2::element_line(
        colour = grid_colour,
        linewidth = grid_linewidth
      )
    ),
    none = ggplot2::theme(panel.grid.major = ggplot2::element_blank())
  )

  style_theme <- switch(
    style,
    report = ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = if (axis_title) {
        ggplot2::element_text(face = "bold", colour = "#243B53")
      } else {
        ggplot2::element_blank()
      },
      axis.text = if (axis_text) {
        ggplot2::element_text(colour = "#334E68")
      } else {
        ggplot2::element_blank()
      },
      plot.title = ggplot2::element_text(
        size = ggplot2::rel(1.35),
        face = "bold",
        colour = "#102A43",
        margin = ggplot2::margin(b = 5)
      ),
      plot.subtitle = ggplot2::element_text(
        colour = "#486581",
        margin = ggplot2::margin(b = 8)
      ),
      strip.text = ggplot2::element_text(
        face = strip_text_face,
        colour = "#243B53"
      ),
      panel.border = ggplot2::element_blank()
    ),
    publication = ggplot2::theme(
      axis.line = ggplot2::element_line(colour = "black", linewidth = 0.35),
      axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.35),
      axis.ticks.length = grid::unit(1.5, "mm"),
      axis.title = if (axis_title) {
        ggplot2::element_text(face = "plain", colour = "black")
      } else {
        ggplot2::element_blank()
      },
      axis.text = if (axis_text) {
        ggplot2::element_text(colour = "black", size = ggplot2::rel(0.9))
      } else {
        ggplot2::element_blank()
      },
      plot.title = ggplot2::element_text(
        size = ggplot2::rel(1.15),
        face = "bold",
        colour = "black",
        margin = ggplot2::margin(b = 3)
      ),
      plot.subtitle = ggplot2::element_text(colour = "black"),
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.75),
        colour = "black"
      ),
      legend.title = if (legend_title) {
        ggplot2::element_text(face = "plain", colour = "black")
      } else {
        ggplot2::element_blank()
      },
      legend.text = ggplot2::element_text(size = ggplot2::rel(0.9)),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        face = strip_text_face,
        colour = "black",
        size = ggplot2::rel(0.9)
      ),
      panel.border = ggplot2::element_blank()
    ),
    slides = ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = if (axis_title) {
        ggplot2::element_text(
          face = "bold",
          colour = "#102A43",
          size = ggplot2::rel(1.05)
        )
      } else {
        ggplot2::element_blank()
      },
      axis.text = if (axis_text) {
        ggplot2::element_text(colour = "#243B53", size = ggplot2::rel(0.95))
      } else {
        ggplot2::element_blank()
      },
      plot.title = ggplot2::element_text(
        size = ggplot2::rel(1.5),
        face = "bold",
        colour = "#0B2D63",
        margin = ggplot2::margin(b = 8)
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(1.1),
        colour = "#40577F",
        margin = ggplot2::margin(b = 12)
      ),
      legend.title = if (legend_title) {
        ggplot2::element_text(
          face = "bold",
          colour = "#102A43",
          size = ggplot2::rel(1.05)
        )
      } else {
        ggplot2::element_blank()
      },
      legend.text = ggplot2::element_text(size = ggplot2::rel(0.95)),
      strip.text = ggplot2::element_text(
        face = strip_text_face,
        colour = "#102A43",
        size = ggplot2::rel(1.05)
      ),
      panel.border = ggplot2::element_blank()
    ),
    default = ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(colour = "grey50", linewidth = 0.4),
      panel.border = ggplot2::element_blank()
    ),
    block = ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = if (axis_text) {
        ggplot2::element_text(face = "bold.italic", colour = "grey25")
      } else {
        ggplot2::element_blank()
      },
      strip.text = ggplot2::element_text(
        face = strip_text_face,
        colour = "grey15"
      ),
      panel.border = ggplot2::element_blank()
    ),
    grid = ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text = if (axis_text) {
        ggplot2::element_text(face = "bold.italic", colour = "grey25")
      } else {
        ggplot2::element_blank()
      },
      panel.border = ggplot2::element_rect(
        colour = border_colour,
        fill = NA,
        linetype = 2,
        linewidth = 0.5
      )
    ),
    pie = ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    )
  )

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = title_face,
        hjust = title_hjust,
        colour = "grey15"
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = title_hjust,
        colour = "grey30"
      ),
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.85),
        colour = "grey45"
      ),
      plot.background = ggplot2::element_rect(fill = plot_fill, colour = NA),
      plot.margin = margin,
      legend.position = legend_position,
      legend.title = legend_title_element,
      legend.background = ggplot2::element_rect(
        fill = "transparent",
        colour = NA
      ),
      legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
      strip.text = ggplot2::element_text(
        face = strip_text_face,
        colour = "grey15"
      ),
      strip.background = ggplot2::element_rect(
        fill = strip_fill,
        colour = "white",
        linewidth = 0
      ),
      panel.background = ggplot2::element_rect(fill = panel_fill, colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = axis_title_element,
      axis.text = axis_text_element,
      axis.text.x = axis_text_x_element
    ) +
    grid_theme +
    style_theme
}


.check_bg_color <- function(bg_color) {
  .check_single_character(bg_color, "bg_color")
  tryCatch(
    grDevices::col2rgb(bg_color, alpha = TRUE),
    error = function(cnd) {
      stop("`bg_color` must be a valid R color.", call. = FALSE)
    }
  )
}


.journal_chart_theme <- function(
  base_size = 12,
  font_family = "sans",
  legend_position = "top",
  grid = c("y", "x", "both", "none"),
  subtitle = NULL,
  x_angle = 0,
  bg_color = "white"
) {
  .check_single_number(base_size, "base_size", min = 0)
  .check_single_character(font_family, "font_family")
  .check_single_number(x_angle, "x_angle")
  .check_bg_color(bg_color)
  grid <- match.arg(grid)

  legend_margin <- if (identical(legend_position, "top")) {
    ggplot2::margin(b = 6)
  } else if (identical(legend_position, "bottom")) {
    ggplot2::margin(t = 6)
  } else {
    ggplot2::margin()
  }

  theme_canreg(
    style = "publication",
    base_size = base_size,
    base_family = font_family,
    grid = grid,
    grid_colour = "#D9D9D9",
    legend_position = legend_position,
    legend_title = TRUE,
    panel_fill = bg_color,
    plot_fill = bg_color,
    margin = ggplot2::margin(t = 14, r = 18, b = 12, l = 18)
  ) +
    ggplot2::theme(
      text = ggplot2::element_text(
        family = font_family,
        size = base_size,
        colour = "black"
      ),
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(
        size = ggplot2::rel(2),
        face = "bold",
        colour = "#0B2D63",
        lineheight = 0.95,
        margin = ggplot2::margin(b = if (is.null(subtitle)) 14 else 4)
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(1.15),
        colour = "#40577F",
        lineheight = 1.05,
        margin = ggplot2::margin(b = 14)
      ),
      plot.caption.position = "plot",
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.8),
        colour = "#40577F",
        hjust = 0,
        margin = ggplot2::margin(t = 6)
      ),
      axis.title = ggplot2::element_text(
        size = ggplot2::rel(0.95),
        face = "plain"
      ),
      axis.text = ggplot2::element_text(
        size = ggplot2::rel(0.85),
        colour = "black"
      ),
      axis.text.x.bottom = ggplot2::element_text(
        angle = x_angle,
        hjust = ifelse(x_angle == 0, 0.5, 1),
        vjust = ifelse(x_angle == 0, 0.5, 1),
        margin = ggplot2::margin(t = 4)
      ),
      axis.text.x.top = ggplot2::element_text(
        angle = x_angle,
        hjust = ifelse(x_angle == 0, 0.5, 1),
        vjust = ifelse(x_angle == 0, 0.5, 0),
        margin = ggplot2::margin(b = 4)
      ),
      axis.text.y.left = ggplot2::element_text(
        margin = ggplot2::margin(r = 4)
      ),
      axis.text.y.right = ggplot2::element_text(
        margin = ggplot2::margin(l = 4)
      ),
      axis.title.x.bottom = ggplot2::element_text(
        margin = ggplot2::margin(t = 7)
      ),
      axis.title.x.top = ggplot2::element_text(
        margin = ggplot2::margin(b = 7)
      ),
      axis.title.y.left = ggplot2::element_text(
        margin = ggplot2::margin(r = 7)
      ),
      axis.title.y.right = ggplot2::element_text(
        margin = ggplot2::margin(l = 7)
      ),
      axis.ticks.length = grid::unit(2, "pt"),
      panel.spacing = grid::unit(12, "pt"),
      strip.background = ggplot2::element_rect(
        fill = bg_color,
        colour = NA
      ),
      strip.text = ggplot2::element_text(
        size = ggplot2::rel(0.9),
        face = "bold",
        margin = ggplot2::margin(t = 4, r = 5, b = 4, l = 5)
      ),
      legend.title = ggplot2::element_text(
        size = ggplot2::rel(0.9),
        face = "bold",
        colour = "#0B2D63",
        lineheight = 0.95
      ),
      legend.text = ggplot2::element_text(
        size = ggplot2::rel(0.85),
        colour = "#0B2D63"
      ),
      legend.margin = legend_margin,
      legend.box.spacing = grid::unit(4, "pt")
    )
}


#' ggcanreg map theme
#'
#' @description
#' `theme_canreg_map()` provides a clean map-oriented theme for `geom_sf()`
#' charts. It removes axes, ticks, panel grids, and panel borders while keeping
#' titles, captions, and legends available for cancer registry maps.
#'
#' @inheritParams theme_canreg
#'
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot() +
#'   theme_canreg_map()
theme_canreg_map <- function(
  base_size = 12,
  base_family = "",
  legend_position = "right",
  title_position = c("left", "center"),
  margin = ggplot2::margin()
) {
  theme_canreg(
    style = "default",
    base_size = base_size,
    base_family = base_family,
    grid = "none",
    legend_position = legend_position,
    title_position = title_position,
    axis_title = FALSE,
    axis_text = FALSE,
    margin = margin
  ) +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank()
    )
}


#' ggcanreg block theme
#'
#' @description
#' A shortcut for `theme_canreg(style = "block")`. This theme is suitable for
#' block-like charts, heatmaps, and plots with dense x-axis categories.
#'
#' @inheritParams theme_canreg
#' @param ... Arguments passed to [theme_canreg()].
#'
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(factor(cyl), factor(gear), fill = mpg)) +
#'   geom_tile() +
#'   theme_block()
theme_block <- function(
  grid = "none",
  title_position = "center",
  x_text_angle = 90,
  ...
) {
  theme_canreg(
    style = "block",
    grid = grid,
    title_position = title_position,
    x_text_angle = x_text_angle,
    ...
  )
}


#' ggcanreg grid theme
#'
#' @description
#' A shortcut for `theme_canreg(style = "grid")`. This theme keeps a light
#' panel background and dashed panel border for report-style display charts.
#'
#' @inheritParams theme_canreg
#' @param ... Arguments passed to [theme_canreg()].
#'
#' @return A ggplot2 theme object.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_grid()
theme_grid <- function(
  grid = "both",
  legend_position = "bottom",
  title_position = "center",
  ...
) {
  theme_canreg(
    style = "grid",
    grid = grid,
    legend_position = legend_position,
    title_position = title_position,
    ...
  )
}


`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.as_plot_margin <- function(margin) {
  if (inherits(margin, "unit")) {
    return(margin)
  }

  if (is.numeric(margin) && length(margin) == 1 && !is.na(margin)) {
    return(ggplot2::margin(margin, margin, margin, margin))
  }

  if (is.numeric(margin) && length(margin) == 4 && !any(is.na(margin))) {
    return(ggplot2::margin(
      t = margin[[1]],
      r = margin[[2]],
      b = margin[[3]],
      l = margin[[4]]
    ))
  }

  stop(
    paste0(
      "`margin` must be a ggplot2 margin object, a single number, ",
      "or a numeric vector of length 4: c(top, right, bottom, left)."
    ),
    call. = FALSE
  )
}

theme_none <- function(
  base_size = 12,
  base_family = "",
  base_line_size = base_size / 22,
  base_rect_size = base_size / 22
) {
  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(b = 20, l = 20, r = 20, unit = "pt"),
      axis.title.y = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        size = base_size,
        margin = ggplot2::margin(t = 2, b = 2)
      ),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
}
