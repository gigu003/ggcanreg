#' Add a North Arrow to a ggplot
#'
#' `gg_north_arrow()` creates a fixed-size north-arrow annotation that can be
#' added to a ggplot with `+`. It positions the arrow relative to the plotting
#' panel and sizes it in centimeters.
#'
#' @param location Corner in which to place the arrow: `"tl"` (top left),
#'   `"tr"` (top right), `"bl"` (bottom left), or `"br"` (bottom right).
#' @param width,height Width and height of the arrow in centimeters. Both must
#'   be positive numbers.
#' @param pad_x,pad_y Horizontal and vertical padding from the selected corner,
#'   in centimeters. Both must be non-negative numbers.
#' @param line_width Width of the triangle outlines. Must be a non-negative
#'   number.
#' @param text_size Font size of the `N` label in points. Must be a positive
#'   number.
#'
#' @return A ggplot2 annotation layer.
#' @export
#'
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point() +
#'   gg_north_arrow()
gg_north_arrow <- function(
  location = "tr",
  width = 1,
  height = 1.4,
  pad_x = 0.3,
  pad_y = 0.3,
  line_width = 0.6,
  text_size = 9
) {
  if (
    !is.character(location) ||
      length(location) != 1L ||
      is.na(location) ||
      !location %in% c("tl", "tr", "bl", "br")
  ) {
    stop(
      '`location` must be one of "tl", "tr", "bl", or "br".',
      call. = FALSE
    )
  }
  .check_north_arrow_number(width, "width", positive = TRUE)
  .check_north_arrow_number(height, "height", positive = TRUE)
  .check_north_arrow_number(pad_x, "pad_x")
  .check_north_arrow_number(pad_y, "pad_y")
  .check_north_arrow_number(line_width, "line_width")
  .check_north_arrow_number(text_size, "text_size", positive = TRUE)

  right <- substr(location, 2, 2) == "r"
  top <- substr(location, 1, 1) == "t"

  x <- grid::unit(as.integer(right), "npc") +
    grid::unit(if (right) -pad_x else pad_x, "cm")
  y <- grid::unit(as.integer(top), "npc") +
    grid::unit(if (top) -pad_y else pad_y, "cm")

  arrow_symbol <- grid::grobTree(
    grid::polygonGrob(
      x = c(0.2, 0.5, 0.5),
      y = c(0.12, 0.72, 0.35),
      gp = grid::gpar(
        fill = "white",
        col = "black",
        lwd = line_width
      )
    ),
    grid::polygonGrob(
      x = c(0.8, 0.5, 0.5),
      y = c(0.12, 0.72, 0.35),
      gp = grid::gpar(
        fill = "black",
        col = "black",
        lwd = line_width
      )
    ),
    grid::textGrob(
      "N",
      x = 0.5,
      y = 0.9,
      gp = grid::gpar(
        fontsize = text_size,
        fontface = "bold"
      )
    ),
    vp = grid::viewport(
      x = x,
      y = y,
      width = grid::unit(width, "cm"),
      height = grid::unit(height, "cm"),
      just = c(
        if (right) "right" else "left",
        if (top) "top" else "bottom"
      )
    )
  )
  arrow <- grid::grobTree(arrow_symbol)

  ggplot2::annotation_custom(
    grob = arrow,
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf
  )
}


.check_north_arrow_number <- function(x, arg, positive = FALSE) {
  valid <- is.numeric(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    is.finite(x) &&
    (if (positive) x > 0 else x >= 0)

  if (!valid) {
    requirement <- if (positive) "positive" else "non-negative"
    stop(
      sprintf("`%s` must be a single %s finite number.", arg, requirement),
      call. = FALSE
    )
  }

  invisible(x)
}
