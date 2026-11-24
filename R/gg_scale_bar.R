#' Add a Scale Bar to a ggplot2 Map
#'
#' @param location Position: "tl", "tr", "bl", or "br".
#' @param distance Scale bar distance. NULL for automatic selection.
#' @param units Distance units: "km" or "m".
#' @param bar_height Bar height in millimeters.
#' @param text_size Text size in points.
#' @param pad_x Horizontal padding in panel fractions.
#' @param pad_y Vertical padding in panel fractions.
#'
#' @return A ggplot2 layer.
#' @export
gg_scale_bar <- function(
  location = "bl",
  distance = NULL,
  units = "km",
  bar_height = 2,
  text_size = 8,
  pad_x = 0.05,
  pad_y = 0.06
) {
  location <- match.arg(
    location,
    c("bl", "br", "tl", "tr")
  )

  units <- match.arg(units, c("km", "m"))

  if (!is.null(distance)) {
    stopifnot(
      is.numeric(distance),
      length(distance) == 1,
      is.finite(distance),
      distance > 0
    )
  }

  stopifnot(
    bar_height > 0,
    text_size > 0,
    pad_x > 0,
    pad_x < 0.25,
    pad_y > 0,
    pad_y < 0.25
  )

  ggplot2::layer(
    data = data.frame(.annotation = 1),
    mapping = NULL,
    stat = "identity",
    geom = GeomScaleBar,
    position = "identity",
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = list(
      location = location,
      distance = distance,
      units = units,
      bar_height = bar_height,
      text_size = text_size,
      pad_x = pad_x,
      pad_y = pad_y
    )
  )
}


GeomScaleBar <- ggplot2::ggproto(
  "GeomScaleBar",
  ggplot2::Geom,

  required_aes = character(0),

  default_aes = ggplot2::aes(),

  draw_key = ggplot2::draw_key_blank,

  draw_panel = function(
    data,
    panel_params,
    coord,
    location,
    distance,
    units,
    bar_height,
    text_size,
    pad_x,
    pad_y
  ) {
    if (!inherits(coord, "CoordSf")) {
      stop(
        "gg_scale_bar() requires coord_sf().",
        call. = FALSE
      )
    }

    crs <- panel_params$crs

    if (is.null(crs) || is.na(crs)) {
      stop("Map CRS is missing.", call. = FALSE)
    }

    # Get panel ranges in map coordinates
    ranges <- coord$range(panel_params)

    xr <- ranges$x
    yr <- ranges$y

    # Position
    right <- location %in% c("tr", "br")
    top <- location %in% c("tl", "tr")

    anchor_x <- if (right) 1 - pad_x else pad_x

    bar_y <- if (top) {
      1 - pad_y - 0.06
    } else {
      pad_y + 0.02
    }

    map_y <- yr[1] + bar_y * diff(yr)

    # Calculate real-world distance between two
    # horizontal positions in the panel.
    distance_km <- function(x0, x1) {
      points <- sf::st_sfc(
        sf::st_point(c(
          xr[1] + x0 * diff(xr),
          map_y
        )),
        sf::st_point(c(
          xr[1] + x1 * diff(xr),
          map_y
        )),
        crs = crs
      )

      points <- sf::st_transform(points, 4326)

      as.numeric(
        sf::st_distance(points[1], points[2])
      ) /
        1000
    }

    direction <- if (right) -1 else 1

    from_anchor <- function(width) {
      distance_km(
        anchor_x,
        anchor_x + direction * width
      )
    }

    # Automatically choose a nice distance
    if (is.null(distance)) {
      target_km <- from_anchor(0.20)

      if (!is.finite(target_km) || target_km <= 0) {
        stop("Cannot calculate map distance.")
      }

      target <- if (units == "km") {
        target_km
      } else {
        target_km * 1000
      }

      magnitude <- 10^floor(log10(target))

      candidates <- c(1, 2, 5, 10) * magnitude

      distance <- max(
        candidates[candidates <= target]
      )
    }

    target_km <- if (units == "km") {
      distance
    } else {
      distance / 1000
    }

    # Maximum available width
    max_width <- 0.65

    if (target_km >= from_anchor(max_width)) {
      stop(
        "Scale bar is too long for the map panel.",
        call. = FALSE
      )
    }

    # Find the width corresponding to the distance
    bar_width <- stats::uniroot(
      function(w) from_anchor(w) - target_km,
      interval = c(0, max_width)
    )$root

    # Left and right endpoints
    x_left <- if (right) {
      anchor_x - bar_width
    } else {
      anchor_x
    }

    x_right <- x_left + bar_width

    # Find the geographic halfway point
    half_width <- stats::uniroot(
      function(w) {
        distance_km(x_left, x_left + w) -
          target_km / 2
      },
      interval = c(0, bar_width)
    )$root

    x_mid <- x_left + half_width

    breaks <- c(x_left, x_mid, x_right)

    # Two alternating segments
    centers <- (breaks[-1] + breaks[-3]) / 2

    widths <- diff(breaks)

    rectangles <- grid::rectGrob(
      x = grid::unit(centers, "npc"),
      y = grid::unit(bar_y, "npc"),
      width = grid::unit(widths, "npc"),
      height = grid::unit(bar_height, "mm"),
      gp = grid::gpar(
        fill = c("black", "white"),
        col = "black",
        lwd = 0.6
      )
    )

    # Tick marks
    ticks <- grid::segmentsGrob(
      x0 = grid::unit(breaks, "npc"),
      x1 = grid::unit(breaks, "npc"),

      y0 = grid::unit(bar_y, "npc") -
        grid::unit(bar_height / 2, "mm"),

      y1 = grid::unit(bar_y, "npc") +
        grid::unit(bar_height / 2 + 1, "mm"),

      gp = grid::gpar(
        col = "black",
        lwd = 0.6
      )
    )

    # Labels
    fmt <- function(x) {
      format(
        x,
        trim = TRUE,
        scientific = FALSE
      )
    }

    labels <- c(
      "0",
      fmt(distance / 2),
      paste(fmt(distance), units)
    )

    text <- grid::textGrob(
      label = labels,
      x = grid::unit(breaks, "npc"),

      y = grid::unit(bar_y, "npc") +
        grid::unit(
          bar_height / 2 + 2,
          "mm"
        ),

      just = c("centre", "bottom"),

      gp = grid::gpar(
        fontsize = text_size,
        col = "black"
      )
    )

    grid::grobTree(
      rectangles,
      ticks,
      text
    )
  }
)
