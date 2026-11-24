#' Cancer registry color palettes
#'
#' @description
#' `canreg_palette()` returns predefined color palettes for cancer registry
#' charts. These palettes are used by `scale_fill_canreg()` and
#' `scale_color_canreg()`, and can also be used directly in custom ggplot2 code.
#'
#' @param name Palette name. Available palettes include `"default"`,
#'   `"categorical"`, `"trend"`, `"highlight"`, `"sex"`, `"area"`,
#'   `"urban_rural"`, `"quality"`, `"quality_status"`, `"indicator"`,
#'   `"map_blue"`, `"map_red"`, `"map_green"`, and `"map_orange"`.
#' @param n Number of colors to return. If `NULL`, all colors in the palette are
#'   returned. If `n` is larger than the palette length, colors are interpolated.
#' @param reverse Logical. Whether to reverse the palette order.
#'
#' @return A character vector of colors.
#' @export
#'
#' @examples
#' canreg_palette("default")
#' canreg_palette("sex")
#' canreg_palette("map_blue", n = 7)
canreg_palette <- function(name = "default", n = NULL, reverse = FALSE) {
  if (!is.character(name) || length(name) != 1 || is.na(name)) {
    stop("`name` must be a single palette name.", call. = FALSE)
  }
  if (!is.null(n) && (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1)) {
    stop("`n` must be NULL or a single positive number.", call. = FALSE)
  }
  .check_single_logical(reverse, "reverse")

  palettes <- .canreg_palettes()
  name <- .standardize_palette_name(name)

  if (!name %in% names(palettes)) {
    stop(
      sprintf(
        "Unknown palette `%s`. Available palettes are: %s.",
        name,
        paste(names(palettes), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  colors <- palettes[[name]]
  if (isTRUE(reverse)) {
    colors <- rev(colors)
  }

  if (is.null(n)) {
    return(colors)
  }

  n <- as.integer(n)
  if (n <= length(colors)) {
    return(colors[seq_len(n)])
  }

  color_names <- names(colors)
  colors <- grDevices::colorRampPalette(unname(colors))(n)
  if (!is.null(color_names) && length(color_names) >= n) {
    names(colors) <- color_names[seq_len(n)]
  }

  colors
}


#' ggplot2 fill scale using ggcanreg palettes
#'
#' @description
#' `scale_fill_canreg()` applies ggcanreg palettes to the `fill` aesthetic. Use
#' discrete palettes for grouped bars, pie slices, and categorical fills, and
#' continuous palettes for maps or heatmaps.
#'
#' @inheritParams canreg_palette
#' @param palette Palette name passed to [canreg_palette()].
#' @param discrete Logical. If `TRUE`, use `ggplot2::scale_fill_manual()`. If
#'   `FALSE`, use `ggplot2::scale_fill_gradientn()`.
#' @param na.value Color used for missing values.
#' @param ... Additional arguments passed to the underlying ggplot2 scale.
#'
#' @return A ggplot2 scale object.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(factor(cyl), mpg, fill = factor(cyl))) +
#'   geom_boxplot() +
#'   scale_fill_canreg()
scale_fill_canreg <- function(
  palette = "default",
  discrete = TRUE,
  n = NULL,
  reverse = FALSE,
  na.value = "grey90",
  ...
) {
  colors <- canreg_palette(palette, n = n, reverse = reverse)

  if (isTRUE(discrete)) {
    ggplot2::scale_fill_manual(values = colors, na.value = na.value, ...)
  } else {
    ggplot2::scale_fill_gradientn(
      colors = unname(colors),
      na.value = na.value,
      ...
    )
  }
}


#' ggplot2 color scale using ggcanreg palettes
#'
#' @description
#' `scale_color_canreg()` applies ggcanreg palettes to the `color` aesthetic.
#' It is useful for lines, points, text, and borders.
#'
#' @inheritParams scale_fill_canreg
#'
#' @return A ggplot2 scale object.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_canreg()
scale_color_canreg <- function(
  palette = "default",
  discrete = TRUE,
  n = NULL,
  reverse = FALSE,
  na.value = "grey90",
  ...
) {
  colors <- canreg_palette(palette, n = n, reverse = reverse)

  if (isTRUE(discrete)) {
    ggplot2::scale_color_manual(values = colors, na.value = na.value, ...)
  } else {
    ggplot2::scale_color_gradientn(
      colors = unname(colors),
      na.value = na.value,
      ...
    )
  }
}


.standardize_palette_name <- function(name) {
  aliases <- c(
    urban_rural = "area",
    quality_status = "quality",
    categorical = "default"
  )

  name <- tolower(name)
  if (name %in% names(aliases)) {
    aliases[[name]]
  } else {
    name
  }
}


.canreg_palettes <- function() {
  list(
    default = c(
      "#0072B2",
      "#D55E00",
      "#009E73",
      "#CC79A7",
      "#F0E442",
      "#56B4E9",
      "#E69F00"
    ),
    trend = c("#0072B2", "#D55E00", "#009E73", "#CC79A7"),
    highlight = c(base = "grey80", highlight = "#D55E00"),
    sex = c(
      Male = "#0072B2",
      Female = "#D55E00",
      Total = "#4D4D4D",
      "\u7537" = "#0072B2",
      "\u5973" = "#D55E00",
      "\u5408\u8ba1" = "#4D4D4D"
    ),
    area = c(
      Urban = "#009E73",
      Rural = "#E69F00",
      Total = "#4D4D4D",
      "\u57ce\u5e02" = "#009E73",
      "\u519c\u6751" = "#E69F00",
      "\u5408\u8ba1" = "#4D4D4D"
    ),
    quality = c(
      Pass = "#009E73",
      Warning = "#E69F00",
      Fail = "#D55E00",
      "\u8fbe\u6807" = "#009E73",
      "\u9884\u8b66" = "#E69F00",
      "\u4e0d\u8fbe\u6807" = "#D55E00"
    ),
    indicator = c(
      Incidence = "#0072B2",
      Mortality = "#D55E00",
      Prevalence = "#009E73",
      Survival = "#CC79A7",
      "\u53d1\u75c5" = "#0072B2",
      "\u6b7b\u4ea1" = "#D55E00",
      "\u60a3\u75c5" = "#009E73",
      "\u751f\u5b58" = "#CC79A7"
    ),
    map_blue = c("#EFF6FB", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B"),
    map_red = c("#FFF5F0", "#FCBBA1", "#FB6A4A", "#CB181D", "#67000D"),
    map_green = c("#F7FCF5", "#C7E9C0", "#74C476", "#238B45", "#00441B"),
    map_orange = c("#FFF5EB", "#FDD0A2", "#FD8D3C", "#E6550D", "#7F2704")
  )
}
