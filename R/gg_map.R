#' Create map.
#'
#' @description
#' Draw a cancer registry map using the bundled [china_map] boundaries by
#' default. On national maps, the South China Sea islands and dashed boundary
#' are removed from the main panel and shown in a framed, enlarged inset at the
#' lower right. A custom `sf` object can be supplied for other boundaries. For
#' backward compatibility, a regular data frame can still be supplied together
#' with `areacode`; its boundaries are now matched locally from [china_map].
#'
#' @param data `NULL` to draw the bundled national map, an `sf` object for a
#'   custom offline map, or a data frame containing area codes to join to the
#'   bundled boundaries.
#' @param map_level Boundary level used when `data = NULL`: `"province"`,
#'   `"city"`, or `"county"`.
#' @param facet Optional faceting variable. Use `"province"` to draw one panel
#'   per province or `"city"` to draw one panel per city, with labels derived
#'   from bundled boundary metadata. Alternatively, supply a column name in
#'   `data`, such as `"sex"`, to draw one complete statistical map for each
#'   value of that variable. When `facet = "city"`, `bg_level` defaults to
#'   `"city"`; each panel is independently scaled to its own spatial extent.
#' @param facet_nrow,facet_ncol Optional number of rows or columns used to
#'   arrange facet panels. Each must be a positive integer. If both are
#'   `NULL`, a compact landscape layout is calculated from the number of
#'   panels; for example, 18 panels use six columns and three rows.
#' @param areacode When `data` is a data frame, the variable containing region
#'   codes. When `data = NULL`, a character or numeric vector matched directly
#'   against `china_map$area_code`. Superseded county codes are matched to their
#'   current bundled boundaries when an official successor is available.
#' @param value Variable for values to visualize.
#' @param fill_missing Optional numeric value used to fill regions without data.
#'   If `NULL`, missing regions keep the default missing-value appearance.
#' @param breaks Optional numeric boundaries for value intervals. They must be
#'   strictly increasing and cover all non-missing values.
#' @param break_labels Optional character labels for the value intervals. Its
#'   length must equal the number of intervals in `breaks`, or `bins` when
#'   `breaks` is `NULL`.
#' @param bins Number of automatically generated value intervals when `breaks`
#'   is `NULL`.
#' @param bg_level Extent of the bundled background map: `"china"`,
#'   `"province"`, or `"city"`.
#' @param bg_bound Boundary type of the background map.
#' @param bg_cols Colors for background map (fill, border).
#' @param h_cols Highlight colors (low, high) for filled areas.
#' @param label Whether and what to display as map labels. Use `FALSE` for no
#'   labels, `TRUE` or `"name"` for region names, `"value"` for values, a
#'   variable in `data`, or a column position such as `3` for custom labels.
#' @param label_size Font size for region labels.
#' @param legend Logical, whether to show a legend. Statistical maps show it by
#'   default.
#' @param legend_title Title of the legend.
#' @param legend_position Position of legend. Passed to `ggplot2::theme()`.
#' @param legend_size Optional font size of legend title and text. When `NULL`,
#'   the coordinated theme sizes are used.
#' @param legend_spacing Numeric. Spacing between adjacent legend items. This
#'   does not change the distance between a key and its text or key dimensions.
#' @param legend_direction Optional legend direction: `"horizontal"` or
#'   `"vertical"`.
#' @param legend_nrow,legend_ncol Optional number of rows or columns in the
#'   legend, passed to [ggplot2::guide_legend()].
#' @param legend_title_size,legend_text_size Optional font size for legend
#'   title and text. If `NULL`, `legend_size` is used for backward
#'   compatibility.
#' @param legend_key_width,legend_key_height Optional legend key width and
#'   height. Use a `grid::unit()` object, or provide a single numeric value
#'   interpreted as lines.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param font_family Font family used by plot text and map labels.
#' @param base_size Base font size in points. Defaults to 12 for coordinated
#'   output across ggcanreg charts.
#' @param bg_color Overall plot and panel background color. Use
#'   `"transparent"` for a transparent background.
#' @param footnote Optional note displayed below the map and legend.
#' @param fill Fill variable when `data` is an `sf` object. This is the
#'   recommended mapping argument for new code. For compatibility, `value` or
#'   the second positional argument can also supply the fill variable.
#' @param palette Palette name passed to [scale_fill_canreg()] for an `sf`
#'   object.
#' @param discrete Whether to use a discrete fill scale. The default `NULL`
#'   divides numeric values into intervals and treats non-numeric values as
#'   categories. Set to `FALSE` to request a continuous numeric color scale.
#' @param na_value Fill color for missing `sf` values.
#' @param na_label Legend label for regions without data.
#' @param border_color,border_width Boundary color and line width for an `sf`
#'   object.
#' @param label_color Label color for an `sf` object.
#' @param island_color Fill and border color for the South China Sea islands in
#'   the inset. The default is dark grey.
#' @param inset_size Relative size of the South China Sea inset. The default
#'   `1` keeps the standard size; values below or above `1` make the inset
#'   smaller or larger, respectively.
#' @param auxiliary_color Fill color for auxiliary national boundary features,
#'   including the South China Sea dashed boundary. The default is black so the
#'   boundary remains visible in the inset.
#' @param auxiliary_width Border width for auxiliary national boundary features.
#'
#' @return A ggplot object showing the map.
#' @export
#'
#' @examples
#' geometry <- sf::st_sfc(
#'   sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
#'   sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0))))
#' )
#' map_data <- sf::st_sf(
#'   registry = c("Registry A", "Registry B"),
#'   incidence = c(42.1, 38.7),
#'   geometry = geometry
#' )
#'
#' gg_map(map_data, fill = incidence, legend = TRUE, label = registry)
#'
#' gg_map()
#' gg_map(areacode = c("410102", "410103"))
gg_map <- function(
  data = NULL,
  areacode = NULL,
  value = NULL,
  fill_missing = NULL,
  breaks = NULL,
  break_labels = NULL,
  bins = 5,
  bg_level = "province",
  bg_bound = "county",
  bg_cols = c("lightgray", "white"),
  h_cols = c("white", "darkred"),
  label = FALSE,
  label_size = 3,
  legend = TRUE,
  legend_title = "Incidence",
  legend_position = "bottom",
  legend_size = NULL,
  legend_spacing = 0.2,
  legend_direction = NULL,
  legend_nrow = NULL,
  legend_ncol = NULL,
  legend_title_size = NULL,
  legend_text_size = NULL,
  legend_key_width = NULL,
  legend_key_height = NULL,
  title = NULL,
  subtitle = NULL,
  font_family = "sans",
  base_size = 12,
  bg_color = "white",
  footnote = NULL,
  map_level = "province",
  facet = NULL,
  facet_nrow = NULL,
  facet_ncol = NULL,
  fill = NULL,
  palette = "map_blue",
  discrete = NULL,
  na_value = "grey90",
  na_label = "No data",
  border_color = "white",
  border_width = 0.2,
  label_color = "grey20",
  island_color = "grey35",
  inset_size = 1,
  auxiliary_color = "black",
  auxiliary_width = 0.4
) {
  bg_level_missing <- missing(bg_level)
  map_level_missing <- missing(map_level)
  area_var <- rlang::enquo(areacode)
  value_var <- rlang::enquo(value)
  fill_var <- rlang::enquo(fill)
  label_var <- rlang::enquo(label)
  has_value <- !rlang::quo_is_missing(value_var) &&
    !rlang::quo_is_null(value_var)
  if (
    !is.null(fill_missing) &&
      (!is.numeric(fill_missing) ||
        length(fill_missing) != 1 ||
        is.na(fill_missing))
  ) {
    stop(
      "`fill_missing` must be NULL or a single non-missing numeric value.",
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
    legend_key_height = legend_key_height,
    legend_size = legend_size,
    legend_spacing = legend_spacing
  )

  map_level <- .match_map_level(map_level, "map_level")
  facet <- .match_map_facet(facet)
  facet_nrow <- .check_facet_layout(facet_nrow, "facet_nrow")
  facet_ncol <- .check_facet_layout(facet_ncol, "facet_ncol")
  if (identical(facet, "city") && map_level_missing) {
    map_level <- "city"
  }
  if (identical(facet, "city") && bg_level_missing) {
    bg_level <- "city"
  }
  if (identical(facet, "city") && identical(bg_bound, "province")) {
    stop(
      '`facet = "city"` requires `bg_bound = "city"` or `"county"`.',
      call. = FALSE
    )
  }
  .check_single_character(auxiliary_color, "auxiliary_color")
  .check_single_character(island_color, "island_color")
  if (
    !is.numeric(inset_size) ||
      length(inset_size) != 1L ||
      is.na(inset_size) ||
      !is.finite(inset_size) ||
      inset_size <= 0
  ) {
    stop(
      "`inset_size` must be a single positive finite number.",
      call. = FALSE
    )
  }
  .check_single_number(auxiliary_width, "auxiliary_width", min = 0)
  .check_map_bins(bins)
  .check_single_character(na_label, "na_label")
  .check_map_break_labels(break_labels)
  if (!is.null(footnote)) {
    .check_single_character(footnote, "footnote")
  }
  .check_single_character(font_family, "font_family")
  .check_single_number(base_size, "base_size", min = 0)
  .check_bg_color(bg_color)

  data_is_bundled <- is.null(data)
  if (data_is_bundled) {
    data <- .load_china_map()

    if (!rlang::quo_is_missing(area_var) && !rlang::quo_is_null(area_var)) {
      requested_codes <- .normalize_china_areacodes(
        as.character(rlang::eval_tidy(area_var))
      )
      data <- .match_bundled_areacodes(data, requested_codes)
      area_var <- rlang::as_quosure(NULL)
    } else {
      data <- dplyr::filter(
        data,
        .data$area_level %in% c(map_level, "auxiliary")
      )
    }
  }

  if (inherits(data, "sf")) {
    area_var <- .resolve_map_column(area_var, data, "areacode")
    value_var <- .resolve_map_column(value_var, data, "value")
    fill_var <- .resolve_map_column(fill_var, data, "fill")

    if (rlang::quo_is_null(fill_var) || rlang::quo_is_missing(fill_var)) {
      fill_var <- if (has_value) value_var else area_var
    }

    return(.gg_map_sf(
      data = data,
      fill = fill_var,
      fill_missing = fill_missing,
      breaks = breaks,
      break_labels = break_labels,
      bins = bins,
      facet = facet,
      facet_nrow = facet_nrow,
      facet_ncol = facet_ncol,
      label = label_var,
      label_size = label_size,
      label_color = label_color,
      legend = legend,
      legend_title = legend_title,
      legend_options = legend_options,
      palette = palette,
      discrete = discrete,
      na_value = na_value,
      na_label = na_label,
      border_color = border_color,
      border_width = border_width,
      island_color = island_color,
      inset_size = inset_size,
      auxiliary_color = auxiliary_color,
      auxiliary_width = auxiliary_width,
      title = title,
      subtitle = subtitle,
      font_family = font_family,
      base_size = base_size,
      bg_color = bg_color,
      footnote = footnote
    ))
  }

  area_var <- .resolve_map_column(area_var, data, "areacode")
  value_var <- .resolve_map_column(value_var, data, "value")
  custom_facet <- !is.null(facet) && !.is_geographic_map_facet(facet)
  if (custom_facet) {
    data <- .add_data_facet(data, facet)
  }

  # Normalize label option while preserving backward compatibility.
  label_expr <- rlang::get_expr(label_var)
  label_mode <- NULL
  label_name <- NULL
  custom_label <- FALSE
  if (isTRUE(label_expr)) {
    label_mode <- "name"
  } else if (isFALSE(label_expr) || is.null(label_expr)) {
    label_mode <- NULL
  } else if (is.character(label_expr) && length(label_expr) == 1) {
    if (label_expr %in% c("name", "value")) {
      label_mode <- label_expr
    } else {
      label_mode <- "custom"
      label_name <- label_expr
      label_var <- rlang::sym(label_name)
      custom_label <- TRUE
    }
  } else if (is.numeric(label_expr) && length(label_expr) == 1) {
    if (is.na(label_expr) || label_expr < 1 || label_expr > ncol(data)) {
      stop(
        "`label` column position must be between 1 and `ncol(data)`.",
        call. = FALSE
      )
    }
    label_mode <- "custom"
    label_name <- names(data)[label_expr]
    label_var <- rlang::sym(label_name)
    custom_label <- TRUE
  } else if (rlang::is_symbol(label_expr)) {
    label_mode <- "custom"
    label_name <- rlang::as_name(label_expr)
    custom_label <- TRUE
  } else {
    stop(
      "`label` must be FALSE, TRUE, \"name\", \"value\", a variable in `data`, or a column position.",
      call. = FALSE
    )
  }

  if (identical(label_mode, "value") && !has_value) {
    stop('`label = "value"` requires `value` to be supplied.', call. = FALSE)
  }

  if (rlang::quo_is_missing(area_var) || rlang::quo_is_null(area_var)) {
    stop("`areacode` must be supplied.", call. = FALSE)
  }

  # Ensure area code uses the same type as map data.
  data <- dplyr::mutate(
    data,
    .adcode_join = .normalize_china_areacodes(as.character(!!area_var))
  )
  areacodes <- dplyr::pull(data, ".adcode_join")

  # Select and transform bundled boundaries.
  map <- .select_china_map(
    areacodes,
    level = bg_level,
    bound = bg_bound
  ) |>
    dplyr::rename(adcode = "area_code", name = "area_name") |>
    sf::st_transform(crs = 3857)
  map <- dplyr::mutate(map, adcode = as.character(.data$adcode))
  auxiliary_map <- dplyr::filter(map, .data$area_level == "auxiliary")
  map <- dplyr::filter(map, .data$area_level != "auxiliary")
  if (!is.null(facet)) {
    if (.is_geographic_map_facet(facet)) {
      map <- .add_map_facet(map, facet)
    } else {
      map <- .expand_map_facets(map, data$.ggcanreg_facet)
    }
    auxiliary_map <- .expand_map_auxiliary(auxiliary_map, map, facet)
  }

  # Validate area codes
  map_codes <- dplyr::pull(map, "adcode")
  area_names <- dplyr::pull(map, "name")
  missing_codes <- areacodes[!areacodes %in% map_codes]
  if (length(missing_codes) > 0) {
    message(
      "Areacode(s) not found in map data: ",
      paste(unique(missing_codes), collapse = ", ")
    )
    message(
      "Available areacodes include:\n",
      paste0(map_codes, " (", area_names, ")", collapse = ", ")
    )
  }

  # Merge data with map
  map2 <- dplyr::left_join(
    map,
    data,
    by = if (custom_facet) {
      c("adcode" = ".adcode_join", ".ggcanreg_facet" = ".ggcanreg_facet")
    } else {
      c("adcode" = ".adcode_join")
    }
  )
  if (custom_label && !label_name %in% names(map2)) {
    stop(
      "The custom `label` variable `",
      label_name,
      "` was not found in `data`.",
      call. = FALSE
    )
  }
  if (has_value && !is.null(fill_missing)) {
    map2 <- dplyr::mutate(
      map2,
      !!value_var := dplyr::coalesce(!!value_var, fill_missing)
    )
  }

  # Ensure color palettes
  bg_cols <- rep(bg_cols, length.out = 2)
  h_cols <- rep(h_cols, length.out = 2)

  fill_spec <- NULL
  if (has_value) {
    fill_spec <- .prepare_map_fill(
      dplyr::pull(map2, !!value_var),
      breaks = breaks,
      break_labels = break_labels,
      bins = bins,
      discrete = discrete,
      na_label = na_label
    )
    map2$.ggcanreg_fill <- fill_spec$values
  }

  # Create base map
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = map, fill = bg_cols[1], color = bg_cols[2])

  if (nrow(auxiliary_map) > 0L) {
    p <- p +
      ggplot2::geom_sf(
        data = auxiliary_map,
        fill = auxiliary_color,
        color = auxiliary_color,
        linewidth = auxiliary_width,
        show.legend = FALSE
      )
  }

  if (has_value) {
    p <- p +
      ggplot2::geom_sf(
        data = map2,
        ggplot2::aes(fill = .data$.ggcanreg_fill),
        color = "white",
        show.legend = legend
      )

    reverse_legend <- .map_legend_reverse(fill_spec, breaks, legend_options)
    if (isTRUE(fill_spec$discrete)) {
      p <- p +
        .map_discrete_fill_scale(
          fill_spec,
          palette = palette,
          na_value = na_value,
          na_label = na_label,
          reverse = reverse_legend
        )
    } else {
      p <- p +
        ggplot2::scale_fill_gradient(
          low = h_cols[1],
          high = h_cols[2],
          breaks = breaks,
          na.value = na_value
        )
    }
    label_data <- map2
  } else {
    reverse_legend <- FALSE
    highlight <- dplyr::filter(map, adcode %in% areacodes)
    p <- p +
      ggplot2::geom_sf(data = highlight, fill = h_cols[2], color = "white")
    if (custom_label) {
      label_data <- dplyr::filter(map2, adcode %in% areacodes)
    } else {
      label_data <- highlight
    }
  }

  # Add theme
  p <- p +
    theme_canreg_map(
      base_size = base_size,
      base_family = font_family,
      legend_position = legend_options$position
    ) +
    .map_presentation_theme(
      faceted = !is.null(facet),
      has_subtitle = .has_map_subtitle(subtitle),
      font_family = font_family,
      base_size = base_size,
      bg_color = bg_color
    ) +
    ggplot2::labs(
      x = "",
      y = "",
      title = title,
      subtitle = subtitle,
      caption = footnote
    )

  # Legend
  if (legend) {
    p <- p +
      ggplot2::labs(fill = legend_title) +
      .legend_theme(
        legend = legend,
        legend_title = legend_title,
        options = legend_options
      )

    guide_type <- if (has_value && isTRUE(fill_spec$discrete)) {
      "legend"
    } else {
      "colorbar"
    }
    legend_guides <- .legend_guides(
      "fill",
      guide_type,
      legend_options,
      reverse = reverse_legend,
      title_position = .map_legend_title_position(legend_options)
    )
    if (!is.null(legend_guides)) {
      p <- p + legend_guides
    }
  } else {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  # Labels
  if (!is.null(label_mode)) {
    if (identical(label_mode, "value")) {
      p <- p +
        ggplot2::geom_sf_text(
          data = label_data,
          ggplot2::aes(label = !!value_var),
          size = label_size,
          family = font_family,
          color = "white"
        )
    } else if (identical(label_mode, "custom")) {
      p <- p +
        ggplot2::geom_sf_text(
          data = label_data,
          ggplot2::aes(label = !!label_var),
          size = label_size,
          family = font_family,
          color = "white"
        )
    } else {
      p <- p +
        ggplot2::geom_sf_text(
          data = label_data,
          ggplot2::aes(label = name),
          size = label_size,
          family = font_family,
          color = "white"
        )
    }
  }

  if (!is.null(facet)) {
    p <- .facet_map_plot(
      p,
      facet_nrow = facet_nrow,
      facet_ncol = facet_ncol
    )
  }

  return(p)
}


.gg_map_sf <- function(
  data,
  fill,
  fill_missing = NULL,
  breaks = NULL,
  break_labels = NULL,
  bins = 5,
  facet = NULL,
  facet_nrow = NULL,
  facet_ncol = NULL,
  label,
  label_size = 3,
  label_color = "grey20",
  legend = TRUE,
  legend_title = NULL,
  legend_options,
  palette = "map_blue",
  discrete = NULL,
  na_value = "grey90",
  na_label = "No data",
  border_color = "white",
  border_width = 0.2,
  island_color = "grey35",
  inset_size = 1,
  auxiliary_color = "black",
  auxiliary_width = 0.4,
  title = NULL,
  subtitle = NULL,
  font_family = "sans",
  base_size = 12,
  bg_color = "white",
  footnote = NULL
) {
  .check_single_logical(legend, "legend")
  .check_single_number(label_size, "label_size", min = 0)
  if (
    !is.numeric(border_width) ||
      length(border_width) != 1 ||
      is.na(border_width) ||
      border_width < 0
  ) {
    stop("`border_width` must be a single non-negative number.", call. = FALSE)
  }
  .check_single_character(label_color, "label_color")
  .check_single_character(border_color, "border_color")
  .check_single_character(na_value, "na_value")

  if (!is.null(discrete)) {
    .check_single_logical(discrete, "discrete")
  }

  has_fill <- !rlang::quo_is_null(fill) && !rlang::quo_is_missing(fill)
  auxiliary_data <- data[0, ]
  map_data <- data
  if ("area_level" %in% names(data)) {
    auxiliary_data <- dplyr::filter(data, .data$area_level == "auxiliary")
    map_data <- dplyr::filter(data, .data$area_level != "auxiliary")
  }
  if (!is.null(facet)) {
    if (.is_geographic_map_facet(facet)) {
      map_data <- .add_map_facet(map_data, facet)
    } else {
      map_data <- .add_data_facet(map_data, facet)
    }
    auxiliary_data <- .expand_map_auxiliary(auxiliary_data, map_data, facet)
  }
  fill_var <- NULL
  fill_spec <- NULL

  if (has_fill) {
    map_data <- dplyr::mutate(map_data, .ggcanreg_fill = !!fill)
    if (!is.null(fill_missing)) {
      map_data$.ggcanreg_fill <- dplyr::coalesce(
        map_data$.ggcanreg_fill,
        fill_missing
      )
    }
    fill_spec <- .prepare_map_fill(
      map_data$.ggcanreg_fill,
      breaks = breaks,
      break_labels = break_labels,
      bins = bins,
      discrete = discrete,
      na_label = na_label
    )
    map_data$.ggcanreg_fill <- fill_spec$values
    fill_var <- rlang::sym(".ggcanreg_fill")
    discrete <- fill_spec$discrete
  }

  inset_data <- map_data[0, ]
  inset_island_data <- map_data[0, ]
  inset_auxiliary_data <- auxiliary_data[0, ]
  inset_frame <- map_data[0, ]
  if (
    is.null(facet) &&
      .has_south_china_sea_boundary(auxiliary_data)
  ) {
    inset_layout <- .layout_south_china_sea_inset(
      map_data,
      auxiliary_data,
      inset_size = inset_size
    )
    map_data <- inset_layout$map
    inset_data <- inset_layout$inset
    inset_island_data <- inset_layout$islands
    inset_auxiliary_data <- inset_layout$auxiliary
    inset_frame <- inset_layout$frame
    auxiliary_data <- auxiliary_data[0, ]
  }

  p <- ggplot2::ggplot(map_data)
  reverse_legend <- FALSE

  if (has_fill) {
    p <- p +
      ggplot2::geom_sf(
        ggplot2::aes(fill = !!fill_var),
        color = border_color,
        linewidth = border_width,
        show.legend = legend
      )

    reverse_legend <- .map_legend_reverse(fill_spec, breaks, legend_options)
    if (isTRUE(discrete)) {
      p <- p +
        .map_discrete_fill_scale(
          fill_spec,
          palette = palette,
          na_value = na_value,
          na_label = na_label,
          reverse = reverse_legend
        )
    } else {
      p <- p +
        scale_fill_canreg(
          palette = palette,
          discrete = FALSE,
          na.value = na_value,
          breaks = breaks
        )
    }
  } else {
    p <- p +
      ggplot2::geom_sf(
        fill = na_value,
        color = border_color,
        linewidth = border_width
      )
  }

  if (nrow(auxiliary_data) > 0L) {
    p <- p +
      ggplot2::geom_sf(
        data = auxiliary_data,
        fill = auxiliary_color,
        color = auxiliary_color,
        linewidth = auxiliary_width,
        show.legend = FALSE
      )
  }

  if (nrow(inset_frame) > 0L) {
    p <- p +
      ggplot2::geom_sf(
        data = inset_frame,
        fill = bg_color,
        color = "grey60",
        linewidth = 0.4,
        show.legend = FALSE
      )
  }

  if (nrow(inset_data) > 0L) {
    if (has_fill) {
      p <- p +
        ggplot2::geom_sf(
          data = inset_data,
          ggplot2::aes(fill = !!fill_var),
          color = border_color,
          linewidth = border_width,
          show.legend = FALSE
        )
    } else {
      p <- p +
        ggplot2::geom_sf(
          data = inset_data,
          fill = na_value,
          color = border_color,
          linewidth = border_width,
          show.legend = FALSE
        )
    }
  }

  if (nrow(inset_island_data) > 0L) {
    p <- p +
      ggplot2::geom_sf(
        data = inset_island_data,
        fill = island_color,
        color = island_color,
        linewidth = auxiliary_width,
        show.legend = FALSE
      )
  }

  if (nrow(inset_auxiliary_data) > 0L) {
    p <- p +
      ggplot2::geom_sf(
        data = inset_auxiliary_data,
        fill = auxiliary_color,
        color = auxiliary_color,
        linewidth = auxiliary_width,
        show.legend = FALSE
      )
  }

  label_expr <- rlang::quo_get_expr(label)
  label_var <- NULL
  if (isTRUE(label_expr) || identical(label_expr, "name")) {
    label_column <- intersect(c("name", "area_name"), names(map_data))[1]
    if (is.na(label_column)) {
      stop(
        '`label = TRUE` requires a `name` or `area_name` column in the `sf` object.',
        call. = FALSE
      )
    }
    label_var <- rlang::sym(label_column)
  } else if (identical(label_expr, "value")) {
    if (!has_fill) {
      stop('`label = "value"` requires `fill` to be supplied.', call. = FALSE)
    }
    label_var <- fill_var
  } else if (rlang::is_symbol(label_expr)) {
    label_name <- rlang::as_name(label_expr)
    if (!label_name %in% names(map_data)) {
      stop(
        sprintf(
          "The custom `label` variable `%s` was not found in `data`.",
          label_name
        ),
        call. = FALSE
      )
    }
    label_var <- rlang::sym(label_name)
  } else if (is.character(label_expr) && length(label_expr) == 1) {
    if (!label_expr %in% names(map_data)) {
      stop(
        sprintf(
          "The custom `label` variable `%s` was not found in `data`.",
          label_expr
        ),
        call. = FALSE
      )
    }
    label_var <- rlang::sym(label_expr)
  } else if (!isFALSE(label_expr) && !is.null(label_expr)) {
    stop(
      "`label` must be FALSE, TRUE, \"name\", \"value\", or a variable in `data`.",
      call. = FALSE
    )
  }

  if (!is.null(label_var)) {
    p <- p +
      ggplot2::geom_sf_text(
        ggplot2::aes(label = !!label_var),
        size = label_size,
        family = font_family,
        color = label_color,
        show.legend = FALSE
      )
  }

  p <- p +
    theme_canreg_map(
      base_size = base_size,
      base_family = font_family,
      legend_position = legend_options$position
    ) +
    .map_presentation_theme(
      faceted = !is.null(facet),
      has_subtitle = .has_map_subtitle(subtitle),
      font_family = font_family,
      base_size = base_size,
      bg_color = bg_color
    ) +
    .legend_theme(
      legend = legend,
      legend_title = legend_title,
      options = legend_options,
      show = has_fill
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = footnote
    )

  if (has_fill) {
    p <- p + ggplot2::labs(fill = legend_title)
  }

  legend_guides <- .legend_guides(
    "fill",
    if (isTRUE(discrete)) "legend" else "colorbar",
    legend_options,
    reverse = reverse_legend,
    title_position = .map_legend_title_position(legend_options)
  )
  if (!is.null(legend_guides) && has_fill) {
    p <- p + legend_guides
  }

  if (!is.null(facet)) {
    p <- .facet_map_plot(
      p,
      facet_nrow = facet_nrow,
      facet_ncol = facet_ncol
    )
  }

  p
}


.has_south_china_sea_boundary <- function(data) {
  nrow(data) > 0L &&
    "area_code" %in% names(data) &&
    any(data$area_code == "100000_JD", na.rm = TRUE)
}


.layout_south_china_sea_inset <- function(
  map_data,
  auxiliary_data,
  inset_size = 1
) {
  split_data <- .split_map_geometry_at_latitude(map_data, 18)
  inset_region <- sf::st_as_sfc(
    sf::st_bbox(
      c(xmin = 105, ymin = 3, xmax = 125, ymax = 26),
      crs = sf::st_crs(4326)
    )
  )
  inset_region <- sf::st_transform(inset_region, sf::st_crs(map_data))
  inset_bbox <- sf::st_bbox(inset_region)
  inset_data <- suppressWarnings(sf::st_crop(map_data, inset_bbox))
  inset_auxiliary_data <- suppressWarnings(
    sf::st_crop(auxiliary_data, inset_bbox)
  )
  main_bbox <- sf::st_bbox(split_data$north)
  inset_bbox <- sf::st_bbox(
    c(
      sf::st_geometry(inset_data),
      sf::st_geometry(inset_auxiliary_data)
    )
  )

  main_width <- unname(main_bbox[["xmax"]] - main_bbox[["xmin"]])
  main_height <- unname(main_bbox[["ymax"]] - main_bbox[["ymin"]])
  inset_width <- unname(inset_bbox[["xmax"]] - inset_bbox[["xmin"]])
  inset_height <- unname(inset_bbox[["ymax"]] - inset_bbox[["ymin"]])
  base_scale <- min(
    main_width * 0.2 / inset_width,
    main_height * 0.45 / inset_height
  )
  base_padding <- main_height * 0.018
  scale <- base_scale * inset_size
  padding <- base_padding * inset_size
  gap <- -main_width * 0.15
  inner_width <- inset_width * scale
  inner_height <- inset_height * scale
  frame_right <- unname(main_bbox[["xmax"]]) +
    gap +
    inset_width * base_scale +
    2 * base_padding
  target_origin <- c(
    frame_right - inner_width - padding,
    unname(main_bbox[["ymin"]]) + padding
  )
  source_origin <- c(
    unname(inset_bbox[["xmin"]]),
    unname(inset_bbox[["ymin"]])
  )

  inset_data <- .transform_map_inset(
    inset_data,
    source_origin,
    target_origin,
    scale
  )
  inset_auxiliary_data <- .transform_map_inset(
    inset_auxiliary_data,
    source_origin,
    target_origin,
    scale
  )
  inset_island_data <- .transform_map_inset(
    split_data$south,
    source_origin,
    target_origin,
    scale
  )
  frame_bbox <- c(
    xmin = target_origin[[1]] - padding,
    ymin = target_origin[[2]] - padding,
    xmax = target_origin[[1]] + inner_width + padding,
    ymax = target_origin[[2]] + inner_height + padding
  )

  list(
    map = split_data$north,
    inset = inset_data,
    islands = inset_island_data,
    auxiliary = inset_auxiliary_data,
    frame = .map_inset_frame(frame_bbox, sf::st_crs(map_data))
  )
}


.split_map_geometry_at_latitude <- function(data, latitude_cutoff) {
  original_geometry <- sf::st_geometry(data)
  geographic_geometry <- sf::st_transform(original_geometry, 4326)
  candidate <- vapply(
    geographic_geometry,
    \(geometry) sf::st_bbox(geometry)[["ymin"]] < latitude_cutoff,
    logical(1)
  )
  north_geometry <- lapply(original_geometry, identity)
  south_geometry <- rep(
    list(sf::st_geometrycollection()),
    length(original_geometry)
  )

  for (row in which(candidate)) {
    parts <- suppressWarnings(
      sf::st_cast(
        sf::st_sfc(original_geometry[[row]], crs = sf::st_crs(data)),
        "POLYGON"
      )
    )
    geographic_parts <- sf::st_transform(parts, 4326)
    keep <- vapply(
      geographic_parts,
      \(geometry) sf::st_bbox(geometry)[["ymax"]] >= latitude_cutoff,
      logical(1)
    )
    north_geometry[[row]] <- .union_map_geometry(parts[keep])
    south_geometry[[row]] <- .union_map_geometry(parts[!keep])
  }

  north <- data
  south <- data
  sf::st_geometry(north) <- sf::st_sfc(
    north_geometry,
    crs = sf::st_crs(data)
  )
  sf::st_geometry(south) <- sf::st_sfc(
    south_geometry,
    crs = sf::st_crs(data)
  )
  list(north = north, south = south)
}


.union_map_geometry <- function(geometry) {
  if (length(geometry) == 0L) {
    return(sf::st_geometrycollection())
  }
  sf::st_union(geometry)[[1]]
}


.transform_map_inset <- function(
  data,
  source_origin,
  target_origin,
  scale
) {
  crs <- sf::st_crs(data)
  geometry <- sf::st_geometry(data)
  geometry <- (geometry - source_origin) * scale + target_origin
  sf::st_crs(geometry) <- crs
  sf::st_geometry(data) <- geometry
  data
}


.map_inset_frame <- function(bbox, crs) {
  coordinates <- matrix(
    c(
      bbox[["xmin"]],
      bbox[["ymin"]],
      bbox[["xmax"]],
      bbox[["ymin"]],
      bbox[["xmax"]],
      bbox[["ymax"]],
      bbox[["xmin"]],
      bbox[["ymax"]],
      bbox[["xmin"]],
      bbox[["ymin"]]
    ),
    ncol = 2,
    byrow = TRUE
  )
  sf::st_sf(
    .ggcanreg_inset_frame = TRUE,
    geometry = sf::st_sfc(sf::st_polygon(list(coordinates)), crs = crs)
  )
}


.check_map_bins <- function(bins) {
  if (
    !is.numeric(bins) ||
      length(bins) != 1L ||
      is.na(bins) ||
      !is.finite(bins) ||
      bins < 2 ||
      bins != as.integer(bins)
  ) {
    stop("`bins` must be a single integer of at least 2.", call. = FALSE)
  }
  invisible(bins)
}


.check_map_break_labels <- function(break_labels, interval_count = NULL) {
  if (is.null(break_labels)) {
    return(invisible(TRUE))
  }

  if (
    !is.character(break_labels) ||
      length(break_labels) == 0L ||
      anyNA(break_labels)
  ) {
    stop(
      "`break_labels` must be NULL or a non-missing character vector.",
      call. = FALSE
    )
  }

  if (!is.null(interval_count) && length(break_labels) != interval_count) {
    stop(
      "`break_labels` must have one label for each value interval.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


.prepare_map_fill <- function(
  values,
  breaks,
  break_labels,
  bins,
  discrete,
  na_label
) {
  if (is.numeric(values)) {
    non_finite <- !is.finite(values) & !is.na(values)
    if (any(non_finite)) {
      warning(
        "Non-finite map fill values were treated as missing.",
        call. = FALSE
      )
      values[non_finite] <- NA_real_
    }

    if (!identical(discrete, FALSE)) {
      binned <- .bin_map_values(
        values,
        breaks = breaks,
        break_labels = break_labels,
        bins = bins,
        na_label = na_label
      )
      return(list(
        values = binned$values,
        labels = binned$labels,
        discrete = TRUE,
        binned = TRUE
      ))
    }

    if (!is.null(break_labels)) {
      stop(
        "`break_labels` requires the default discrete map scale.",
        call. = FALSE
      )
    }

    return(list(values = values, discrete = FALSE, binned = FALSE))
  }

  if (identical(discrete, FALSE)) {
    stop("`discrete = FALSE` requires numeric map fill values.", call. = FALSE)
  }

  if (!is.null(break_labels)) {
    stop(
      "`break_labels` can only be used with numeric map fill values.",
      call. = FALSE
    )
  }

  values <- .add_map_missing_level(values, na_label)
  list(
    values = values,
    labels = levels(values),
    discrete = TRUE,
    binned = FALSE
  )
}


.bin_map_values <- function(values, breaks, break_labels, bins, na_label) {
  interval_count <- if (is.null(breaks)) bins else length(breaks) - 1L
  .check_map_break_labels(break_labels, interval_count)

  observed <- values[!is.na(values)]
  if (length(observed) == 0L) {
    return(list(
      values = factor(rep(na_label, length(values)), levels = na_label),
      labels = na_label
    ))
  }

  value_range <- range(observed)
  if (value_range[1] == value_range[2]) {
    label <- format(signif(value_range[1], 6), trim = TRUE, scientific = FALSE)
    output <- rep(label, length(values))
    output[is.na(values)] <- na_label
    output_levels <- c(label, if (anyNA(values)) na_label)
    output_labels <- c(
      if (is.null(break_labels)) label else break_labels,
      if (anyNA(values)) na_label
    )
    return(list(
      values = factor(output, levels = output_levels, ordered = TRUE),
      labels = output_labels
    ))
  }

  if (is.null(breaks)) {
    breaks <- seq(value_range[1], value_range[2], length.out = bins + 1L)
  } else {
    if (
      !is.numeric(breaks) ||
        length(breaks) < 2L ||
        anyNA(breaks) ||
        any(!is.finite(breaks)) ||
        is.unsorted(breaks, strictly = TRUE)
    ) {
      stop(
        "`breaks` must contain at least two finite, strictly increasing values.",
        call. = FALSE
      )
    }
    if (value_range[1] < breaks[1] || value_range[2] > breaks[length(breaks)]) {
      stop(
        "`breaks` must cover the range of non-missing map values.",
        call. = FALSE
      )
    }
  }

  output <- cut(
    values,
    breaks = breaks,
    include.lowest = TRUE,
    right = TRUE,
    ordered_result = TRUE,
    dig.lab = 10
  )
  interval_labels <- levels(output)
  if (!is.null(break_labels)) {
    output_labels <- break_labels
  } else {
    output_labels <- .format_map_interval_labels(breaks)
  }
  output <- as.character(output)
  output[is.na(values)] <- na_label
  output_levels <- c(interval_labels, if (anyNA(values)) na_label)
  output_labels <- c(output_labels, if (anyNA(values)) na_label)
  list(
    values = factor(output, levels = output_levels, ordered = TRUE),
    labels = output_labels
  )
}


.format_map_interval_labels <- function(breaks) {
  integer_breaks <- all(abs(breaks - round(breaks)) < sqrt(.Machine$double.eps))
  digits <- if (integer_breaks) 0L else 2L
  formatted <- formatC(breaks, format = "f", digits = digits)

  while (anyDuplicated(formatted) && digits < 8L) {
    digits <- digits + 1L
    formatted <- formatC(breaks, format = "f", digits = digits)
  }

  paste(
    formatted[-length(formatted)],
    formatted[-1L],
    sep = " \u2013 "
  )
}


.add_map_missing_level <- function(values, na_label) {
  output <- as.character(values)
  missing <- is.na(output)
  output[missing] <- na_label

  value_levels <- if (is.factor(values)) {
    levels(values)
  } else {
    unique(output[!missing])
  }
  factor(output, levels = c(value_levels, if (any(missing)) na_label))
}


.map_discrete_fill_scale <- function(
  fill_spec,
  palette,
  na_value,
  na_label,
  reverse = FALSE
) {
  fill_levels <- levels(fill_spec$values)
  fill_labels <- fill_spec$labels %||% fill_levels
  data_levels <- setdiff(fill_levels, na_label)
  scale_breaks <- if (isTRUE(reverse) && na_label %in% fill_levels) {
    c(na_label, data_levels)
  } else {
    fill_levels
  }
  scale_labels <- fill_labels[match(scale_breaks, fill_levels)]
  palette_values <- canreg_palette(palette)

  if (
    length(data_levels) > 0L &&
      !is.null(names(palette_values)) &&
      all(data_levels %in% names(palette_values))
  ) {
    colors <- palette_values[data_levels]
  } else if (length(data_levels) > 0L) {
    colors <- stats::setNames(
      unname(canreg_palette(palette, n = length(data_levels))),
      data_levels
    )
  } else {
    colors <- character()
  }

  if (na_label %in% fill_levels) {
    colors <- c(colors, stats::setNames(na_value, na_label))
  }

  ggplot2::scale_fill_manual(
    values = colors,
    breaks = scale_breaks,
    labels = scale_labels,
    drop = FALSE
  )
}


.map_legend_reverse <- function(fill_spec, breaks, legend_options) {
  if (is.null(fill_spec) || !isTRUE(fill_spec$binned) || is.null(breaks)) {
    return(FALSE)
  }

  isTRUE(legend_options$direction == "vertical") ||
    !is.null(legend_options$ncol) ||
    (!is.null(legend_options$nrow) && legend_options$nrow > 1)
}


.map_legend_title_position <- function(legend_options) {
  is_horizontal <- identical(legend_options$direction, "horizontal") ||
    (is.null(legend_options$direction) &&
      is.character(legend_options$position) &&
      legend_options$position %in% c("top", "bottom"))

  if (is_horizontal) "left" else "top"
}


.map_presentation_theme <- function(
  faceted = FALSE,
  has_subtitle = FALSE,
  font_family = "sans",
  base_size = 12,
  bg_color = "white"
) {
  ggplot2::theme(
    text = ggplot2::element_text(
      family = font_family,
      size = base_size,
      colour = "#0B2D63"
    ),
    plot.title.position = "plot",
    plot.title = ggplot2::element_text(
      size = ggplot2::rel(2),
      face = "bold",
      colour = "#0B2D63",
      lineheight = 0.95,
      margin = ggplot2::margin(
        b = if (has_subtitle) {
          4
        } else if (faceted) {
          20
        } else {
          14
        }
      )
    ),
    plot.subtitle = ggplot2::element_text(
      size = ggplot2::rel(1.15),
      colour = "#40577F",
      lineheight = 1.05,
      margin = ggplot2::margin(b = if (faceted) 20 else 14)
    ),
    plot.caption.position = "plot",
    plot.caption = ggplot2::element_text(
      size = ggplot2::rel(0.8),
      colour = "#40577F",
      hjust = 0,
      margin = ggplot2::margin(t = 6)
    ),
    plot.background = ggplot2::element_rect(
      fill = bg_color,
      colour = NA
    ),
    panel.background = ggplot2::element_rect(
      fill = bg_color,
      colour = NA
    ),
    plot.margin = ggplot2::margin(t = 14, r = 18, b = 12, l = 18),
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
    legend.box = "horizontal",
    legend.justification = "center"
  )
}


.has_map_subtitle <- function(subtitle) {
  is.character(subtitle) &&
    length(subtitle) == 1L &&
    !is.na(subtitle) &&
    nzchar(subtitle)
}


.load_china_map <- function() {
  data_environment <- new.env(parent = emptyenv())
  utils::data("china_map", package = "ggcanreg", envir = data_environment)
  data_environment$china_map
}


.normalize_china_areacodes <- function(codes) {
  aliases <- c(
    "410123" = "410181",
    "410224" = "410212",
    "410307" = "410311",
    "410322" = "410306",
    "410524" = "410526",
    "410780" = "410783",
    "411023" = "411003",
    "411222" = "411203",
    "411626" = "411603",
    "411680" = "411628"
  )
  alias_index <- match(codes, names(aliases))
  codes[!is.na(alias_index)] <- unname(aliases[alias_index[
    !is.na(alias_index)
  ]])
  codes
}


.resolve_map_column <- function(variable, data, argument) {
  if (rlang::quo_is_missing(variable) || rlang::quo_is_null(variable)) {
    return(variable)
  }

  expression <- rlang::quo_get_expr(variable)
  if (!is.character(expression) || length(expression) != 1L) {
    return(variable)
  }

  if (!expression %in% names(data)) {
    stop(
      sprintf(
        "`%s = \"%s\"` does not name a column in `data`.",
        argument,
        expression
      ),
      call. = FALSE
    )
  }

  rlang::new_quosure(rlang::sym(expression), rlang::quo_get_env(variable))
}


.match_bundled_areacodes <- function(map, requested_codes) {
  if (
    length(requested_codes) == 0L ||
      anyNA(requested_codes) ||
      any(requested_codes == "")
  ) {
    stop(
      "`areacode` must contain at least one non-missing area code.",
      call. = FALSE
    )
  }

  requested_codes <- unique(requested_codes)
  missing_codes <- setdiff(requested_codes, map$area_code)
  matched_map <- dplyr::filter(map, .data$area_code %in% requested_codes)

  if (nrow(matched_map) == 0L) {
    stop(
      paste0(
        "No `china_map$area_code` values matched `areacode`: ",
        paste(requested_codes, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (length(missing_codes) > 0L) {
    message(
      "Areacode(s) not found in `china_map$area_code`: ",
      paste(missing_codes, collapse = ", "),
      "."
    )
  }

  matched_map
}


.match_map_level <- function(
  x,
  argument,
  choices = c("province", "city", "county")
) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% choices) {
    stop(
      sprintf(
        "`%s` must be one of %s.",
        argument,
        paste(sprintf('"%s"', choices), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  x
}


.match_map_facet <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (
    !is.character(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !nzchar(x)
  ) {
    stop(
      "`facet` must be NULL or a single non-empty character value.",
      call. = FALSE
    )
  }
  x
}


.is_geographic_map_facet <- function(facet) {
  facet %in% c("province", "city")
}


.check_facet_layout <- function(x, argument) {
  if (is.null(x)) {
    return(NULL)
  }
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x < 1 ||
      x != as.integer(x)
  ) {
    stop(
      sprintf("`%s` must be NULL or a single positive integer.", argument),
      call. = FALSE
    )
  }
  as.integer(x)
}


.add_map_facet <- function(data, facet) {
  source_map <- .load_china_map()
  area_level <- if ("area_level" %in% names(data)) {
    as.character(data$area_level)
  } else {
    rep(NA_character_, nrow(data))
  }
  area_code <- if ("area_code" %in% names(data)) {
    as.character(data$area_code)
  } else if ("adcode" %in% names(data)) {
    as.character(data$adcode)
  } else {
    rep(NA_character_, nrow(data))
  }
  parent_code <- if ("parent_code" %in% names(data)) {
    as.character(data$parent_code)
  } else {
    rep(NA_character_, nrow(data))
  }

  if (facet == "province") {
    facet_code <- if ("province_code" %in% names(data)) {
      as.character(data$province_code)
    } else if ("province" %in% names(data)) {
      as.character(data$province)
    } else if ("province_name" %in% names(data)) {
      as.character(data$province_name)
    } else {
      rep(NA_character_, nrow(data))
    }
    facet_label <- if ("province_name" %in% names(data)) {
      as.character(data$province_name)
    } else {
      as.character(source_map$province_name[match(
        facet_code,
        source_map$province_code
      )])
    }
  } else {
    facet_code <- if ("city_code" %in% names(data)) {
      as.character(data$city_code)
    } else if ("city" %in% names(data)) {
      as.character(data$city)
    } else if ("city_name" %in% names(data)) {
      as.character(data$city_name)
    } else {
      ifelse(
        area_level == "city",
        area_code,
        parent_code
      )
    }
    facet_label <- if ("city_name" %in% names(data)) {
      as.character(data$city_name)
    } else {
      city_map <- dplyr::filter(source_map, .data$area_level == "city")
      as.character(city_map$area_name[match(facet_code, city_map$area_code)])
    }
  }

  non_missing <- !is.na(facet_code) & facet_code != ""
  if (any(!non_missing)) {
    stop(
      sprintf(
        "Unable to derive `%s` facet values from the supplied map data.",
        facet
      ),
      call. = FALSE
    )
  }
  facet_label[is.na(facet_label) | facet_label == ""] <- facet_code[
    is.na(facet_label) | facet_label == ""
  ]

  facet_codes <- unique(facet_code)
  facet_labels <- vapply(
    facet_codes,
    function(code) {
      facet_label[match(code, facet_code)]
    },
    character(1)
  )
  data$.ggcanreg_facet <- factor(
    facet_code,
    levels = facet_codes,
    labels = facet_labels
  )
  data
}


.add_data_facet <- function(data, facet) {
  if (!facet %in% names(data)) {
    stop(
      sprintf("`facet = \"%s\"` does not name a column in `data`.", facet),
      call. = FALSE
    )
  }

  facet_values <- as.character(data[[facet]])
  if (anyNA(facet_values) || any(!nzchar(facet_values))) {
    stop(
      sprintf(
        "`facet = \"%s\"` must not contain missing or empty values.",
        facet
      ),
      call. = FALSE
    )
  }

  data$.ggcanreg_facet <- factor(
    facet_values,
    levels = unique(facet_values)
  )
  data
}


.expand_map_facets <- function(data, facets) {
  facet_levels <- levels(facets)
  expanded <- data[
    rep(seq_len(nrow(data)), each = length(facet_levels)),
  ]
  expanded$.ggcanreg_facet <- factor(
    rep(facet_levels, times = nrow(data)),
    levels = facet_levels
  )
  expanded
}


.expand_map_auxiliary <- function(auxiliary_data, map_data, facet) {
  if (nrow(auxiliary_data) == 0L || is.null(facet)) {
    return(auxiliary_data)
  }

  facet_levels <- levels(map_data$.ggcanreg_facet)
  expanded <- auxiliary_data[
    rep(
      seq_len(nrow(auxiliary_data)),
      each = length(facet_levels)
    ),
  ]
  expanded$.ggcanreg_facet <- factor(
    rep(facet_levels, times = nrow(auxiliary_data)),
    levels = facet_levels
  )
  expanded
}


.facet_map_plot <- function(
  plot,
  facet_nrow = NULL,
  facet_ncol = NULL
) {
  reference_data <- NULL
  if (
    inherits(plot$data, "sf") &&
      ".ggcanreg_facet" %in% names(plot$data)
  ) {
    reference_data <- plot$data
  }
  if (is.null(reference_data)) {
    for (layer in plot$layers) {
      if (
        inherits(layer$data, "sf") &&
          ".ggcanreg_facet" %in% names(layer$data)
      ) {
        reference_data <- layer$data
        break
      }
    }
  }
  if (is.null(reference_data) || nrow(reference_data) == 0L) {
    return(plot)
  }

  facet_transforms <- .map_facet_transforms(reference_data)
  facet_plot <- plot
  if (
    inherits(facet_plot$data, "sf") &&
      ".ggcanreg_facet" %in% names(facet_plot$data)
  ) {
    facet_plot$data <- .normalize_map_facets(
      facet_plot$data,
      facet_transforms
    )
  }
  facet_plot$layers <- lapply(plot$layers, function(layer) {
    if (
      inherits(layer$data, "sf") &&
        ".ggcanreg_facet" %in% names(layer$data)
    ) {
      layer_copy <- ggplot2::ggproto(NULL, layer)
      layer_copy$data <- .normalize_map_facets(
        layer_copy$data,
        facet_transforms
      )
      layer_copy
    } else {
      layer
    }
  })

  if (is.null(facet_nrow) && is.null(facet_ncol)) {
    facet_ncol <- as.integer(
      ceiling(sqrt(length(facet_transforms) * 1.5))
    )
  }
  facet_plot +
    ggplot2::facet_wrap(
      ggplot2::vars(!!rlang::sym(".ggcanreg_facet")),
      nrow = facet_nrow,
      ncol = facet_ncol
    ) +
    ggplot2::coord_sf(
      xlim = c(-0.55, 0.55),
      ylim = c(-0.55, 0.55),
      expand = FALSE,
      datum = NA,
      default = TRUE
    ) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        size = ggplot2::rel(0.85),
        face = "bold",
        colour = "#0B2D63",
        hjust = 0.5,
        margin = ggplot2::margin(b = 3)
      ),
      panel.spacing.x = grid::unit(0.35, "lines"),
      panel.spacing.y = grid::unit(0.55, "lines")
    )
}


.map_facet_transforms <- function(data) {
  facet_values <- levels(data$.ggcanreg_facet)
  if (is.null(facet_values)) {
    facet_values <- unique(as.character(data$.ggcanreg_facet))
  }

  transforms <- lapply(facet_values, function(facet_value) {
    facet_data <- data[
      as.character(data$.ggcanreg_facet) == facet_value,
    ]
    bbox <- sf::st_bbox(facet_data)
    width <- unname(bbox[["xmax"]] - bbox[["xmin"]])
    height <- unname(bbox[["ymax"]] - bbox[["ymin"]])
    scale <- max(width, height)
    if (!is.finite(scale) || scale <= 0) {
      scale <- 1
    }
    list(
      center = c(
        unname((bbox[["xmin"]] + bbox[["xmax"]]) / 2),
        unname((bbox[["ymin"]] + bbox[["ymax"]]) / 2)
      ),
      scale = scale
    )
  })
  stats::setNames(transforms, facet_values)
}


.normalize_map_facets <- function(data, transforms) {
  facet_values <- as.character(data$.ggcanreg_facet)
  geometry <- sf::st_set_crs(sf::st_geometry(data), NA)
  for (facet_value in intersect(unique(facet_values), names(transforms))) {
    rows <- which(facet_values == facet_value)
    transform <- transforms[[facet_value]]
    geometry[rows] <-
      (geometry[rows] - transform$center) / transform$scale
  }
  sf::st_geometry(data) <- geometry
  data
}


.select_china_map <- function(areacodes, level, bound) {
  level <- .match_map_level(
    level,
    "bg_level",
    choices = c("china", "province", "city")
  )
  bound <- .match_map_level(bound, "bg_bound")
  source_map <- .load_china_map()
  map <- source_map

  if (level == "province") {
    province_codes <- unique(paste0(substr(areacodes, 1L, 2L), "0000"))
    map <- dplyr::filter(
      map,
      .data$area_level == bound & .data$province_code %in% province_codes
    )
  } else if (level == "city") {
    city_codes <- unique(c(
      areacodes[areacodes %in% map$area_code[map$area_level == "city"]],
      map$parent_code[map$area_code %in% areacodes]
    ))
    city_codes <- city_codes[!is.na(city_codes)]

    if (bound == "county") {
      map <- dplyr::filter(
        map,
        .data$area_level == "county" & .data$parent_code %in% city_codes
      )
    } else if (bound == "city") {
      map <- dplyr::filter(
        map,
        .data$area_level == "city" & .data$area_code %in% city_codes
      )
    } else {
      province_codes <- unique(paste0(substr(areacodes, 1L, 2L), "0000"))
      map <- dplyr::filter(
        map,
        .data$area_level == "province" & .data$area_code %in% province_codes
      )
    }
  } else {
    map <- dplyr::filter(
      map,
      .data$area_level %in% c(bound, "auxiliary")
    )
  }

  requested_boundaries <- dplyr::filter(
    source_map,
    .data$area_code %in% areacodes
  )
  map <- dplyr::bind_rows(map, requested_boundaries)
  map <- map[!duplicated(map$area_code), ]

  if (nrow(map) == 0L) {
    stop(
      "No bundled boundaries matched `areacode`, `bg_level`, and `bg_bound`.",
      call. = FALSE
    )
  }

  map
}


#' Get map of china from dataV.
#'
#' @param x Area codes in China, such as "410102".
#' @param level Top level of the returned map.
#' @param bound Boundary of the returned map.
#' @param cache_dir Cache directory of the returned map.
#' @param cache_refresh Logical value, TRUE or FALSE rerfresh the stored cached
#'                      map or not.
#'
#' @return Map data of sf format.
#' @export
#'
#' @examples
#' \dontrun{
#' henan <- read_map("410102", level = "province", bound = "county")
#' class(henan)
#' }
read_map <- function(
  x,
  level = "province",
  bound = "county",
  cache_dir = "~/.cache_map",
  cache_refresh = FALSE
) {
  adcode <- "adcode"
  childrenNum <- "childrenNum"
  x_exist <- substr(x, 1, 2) %in%
    as.character(c(11:15, 21:23, 31:37, 41:46, 50:54, 61:65, 71, 81, 82, 10))

  if (all(!x_exist)) {
    stop(paste("stops because of illegal inputs x: ", paste(x, collapse = ",")))
  } else if (any(!x_exist)) {
    x_wrong <- x[!x_exist]
    x <- x[x_exist]
    cat(paste(
      "x: ",
      paste(x_wrong, collapse = ","),
      "excluded due to wrong province code."
    ))
  } else {
    #cat("all input x passed.")
  }
  # map address
  maplink <- "https://geo.datav.aliyun.com/areas_v3/bound/"
  # full address of json style map data.
  json_addr <- function(x) {
    ifelse(
      substr(x, 3, 4) == "90",
      paste0(maplink, x, ".json"),
      paste0(maplink, x, "_full.json")
    )
  }

  # function read JSON map data.
  read_json <- memoise::memoise(
    function(x) {
      pb$tick()
      geo_data <- sf::st_read(json_addr(x), as_tibble = TRUE, quiet = TRUE)
      if (!("subFeatureIndex" %in% names(geo_data))) {
        geo_data$subFeatureIndex <- 0
      }
      return(geo_data)
    },
    cache = cachem::cache_disk(dir = cache_dir)
  )

  if (cache_refresh) {
    memoise::forget(read_json)
  }

  if (tolower(level) == "china") {
    if (tolower(bound) == "province") {
      pb <- progress::progress_bar$new(
        total = 1,
        format = "Map loading [:bar] :percent time left: :eta"
      )
      geo_data <- read_json("100000")
    } else if (tolower(bound) == "city") {
      geo_data1 <- sf::st_read(
        json_addr("100000"),
        as_tibble = TRUE,
        quiet = TRUE
      )
      province_code <- geo_data1$adcode
      province_code <- province_code[
        nchar(province_code) == 6 & !province_code == "710000"
      ]
      pb <- progress::progress_bar$new(
        total = length(province_code),
        format = "Map loading [:bar] :percent time left: :eta"
      )
      geo_data <- dplyr::bind_rows(lapply(province_code, read_json)) |>
        dplyr::mutate(adcode = as.character(adcode))
      geo_data <- dplyr::bind_rows(geo_data, geo_data1) |>
        dplyr::arrange(adcode)
    } else if (tolower(bound) == "county") {
      pb <- progress::progress_bar$new(
        total = 366,
        format = "Map loading [:bar] :percent time left: :eta"
      )
      geo_data1 <- sf::st_read(
        json_addr("100000"),
        as_tibble = TRUE,
        quiet = TRUE
      )
      province_code <- geo_data1$adcode
      province_code <- province_code[
        nchar(province_code) == 6 & !province_code == "710000"
      ]
      geo_data <- dplyr::bind_rows(lapply(province_code, read_json)) |>
        dplyr::mutate(adcode = as.character(adcode))
      a1 <- geo_data |> dplyr::filter(childrenNum == 0)
      a2 <- geo_data |> dplyr::filter(childrenNum > 0)
      geo_data2 <- dplyr::bind_rows(lapply(a2$adcode, read_json)) |>
        dplyr::mutate(adcode = as.character(adcode))
      geo_data <- dplyr::bind_rows(geo_data1, geo_data, geo_data2, a1) |>
        dplyr::arrange(adcode)
    } else {
      stop(paste(
        "bound value: ",
        bound,
        " was not supported under level:",
        level
      ))
    }
  } else if (tolower(level) == "province") {
    province <- paste0(unique(substr(x, 1, 2)), "0000")
    pb <- progress::progress_bar$new(
      total = length(province),
      format = "Map loading [:bar] :percent time left: :eta"
    )
    prov_bound <- dplyr::bind_rows(lapply(province, read_json))
    citys <- prov_bound$adcode[!prov_bound$childrenNum == 0]
    if (tolower(bound) == "city") {
      geo_data <- prov_bound
    } else if (tolower(bound) == "county") {
      pb <- progress::progress_bar$new(
        total = length(citys),
        format = "Map loading [:bar] :percent time left: :eta"
      )
      geo_data <- dplyr::bind_rows(prov_bound, lapply(citys, read_json)) |>
        dplyr::arrange(adcode)
    } else {
      stop(paste(
        "bound value: ",
        bound,
        " was not supported under level:",
        level
      ))
    }
  } else if (tolower(level) == "city") {
    if (tolower(bound) == "county") {
      citys <- paste0(unique(substr(x, 1, 4)), "00")
      pb <- progress::progress_bar$new(
        total = length(citys),
        format = "Map loading [:bar] :percent time left: :eta"
      )
      geo_data <- dplyr::bind_rows(lapply(citys, read_json))
    } else {
      stop(paste(
        "bound value: ",
        bound,
        " was not supported under level:",
        level
      ))
    }
  } else {
    stop(paste("level value: ", level, " was not supported."))
  }
  pb$terminate()
  return(geo_data)
}
