#' Draw bar chart
#'
#' @param data Data set to be used for bar chart.
#' @param x \code{character} or \code{factor} column of \code{data}.
#' @param y Optional \code{numeric} column of \code{data} representing the bar
#'        length. If omitted, observations are counted by \code{x}.
#'        For cancer registry summaries, supplying an explicit count or rate
#'        variable such as `cases` or `incidence` is recommended to avoid
#'        accidentally plotting row counts.
#' @param facet facet \code{character} or \code{factor} column of \code{data}
#'        defining the faceting groups.
#' @param group Optional grouping column for dodged bars.
#' @param rev_group \code{logical}. Should group levels be reversed?
#' @param sort_by_group Group value used as reference when sorting grouped bars.
#'        Use \code{"first"}, \code{"last"}, or an existing group value.
#'        This affects both ordering and grouped `top_n` selection.
#' @param y_lim Numeric vector of length 2 giving the limits of the value axis.
#' @param highlight \code{character}. One or more value(s) of \code{x} that
#'        should be highlighted in the plot. Highlighting is only applied when
#'        \code{group} is not supplied.
#' @param highlight_color \code{character}. Color of the highlight bars.
#' @param bar_color \code{character}. The color of the bars
#' @param palette Palette name passed to [scale_fill_canreg()] when grouped bars
#'        are drawn.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param x_lab,y_lab Preferred axis labels.
#' @param legend_title Optional legend title for grouped bars.
#' @param sort \code{logical} or \code{character}. Should the data be sorted
#'        before plotting? Use \code{TRUE} for the default ascending order,
#'        \code{FALSE} to keep the original order, or \code{"asc"}/
#'        \code{"desc"} to explicitly sort in ascending/descending order.
#' @param top_n \code{numeric}. If a value for \code{top_n} is provided only the
#'        top \code{top_n} records by `y` will be displayed. With facets, top
#'        categories are selected within each facet; with groups, selection uses
#'        `sort_by_group` as the reference group.
#' @param add_label \code{logical}. Should value labels be shown on bars?
#' @param label_digits Number of decimal places used for value labels.
#' @param label_size \code{numeric}. Size of value labels.
#' @param label_angle \code{numeric}. Rotation angle of value labels in degrees.
#' @param label_hjust Optional horizontal adjustment for value labels. If
#'        \code{NULL}, a default value is chosen based on \code{reverse}.
#' @param label_vjust Optional vertical adjustment for value labels. If
#'        \code{NULL}, a default value is chosen based on plot orientation and
#'        \code{reverse}.
#' @param label_color \code{character}. Color of value labels. When
#'        \code{highlight} is used without \code{group}, highlighted labels use
#'        \code{highlight_color}.
#' @param horizontal \code{logical}. Should the plot be oriented horizontally?
#' @param reverse \code{logical}. Should the value axis be reversed?
#' @param x_label_position \code{character} 'left' or 'right'.
#' @param legend \code{logical}. Whether to show the group legend.
#' @param legend_position Position of legend. Passed to \code{ggplot2::theme()}.
#' @param legend_direction Optional legend direction: \code{"horizontal"} or
#'        \code{"vertical"}.
#' @param legend_nrow,legend_ncol Optional number of rows or columns in the
#'        legend, passed to \code{ggplot2::guide_legend()}.
#' @param legend_title_size,legend_text_size Optional font size for legend
#'        title and text.
#' @param legend_key_width,legend_key_height Optional legend key width and
#'        height. Use a \code{grid::unit()} object, or provide a single numeric
#'        value interpreted as lines.
#' @param ncol Number of columns used by \code{facet_wrap()}.
#' @param font_family Font family used by plot text and value labels. The
#'        default \code{"sans"} is portable; use an installed family required
#'        by the target journal when necessary.
#' @param base_size Base font size in points. Defaults to 12 for coordinated
#'        output across ggcanreg charts.
#' @param bg_color Background color for the entire figure, including the plot,
#'        panels, and facet strips. Use \code{"transparent"} for slide overlays
#'        or transparent PNG export.
#'
#' @return An object of class \code{ggplot}
#' @export
#'
gg_barchart <- function(
  data,
  x,
  y,
  group = NULL,
  facet = NULL,
  rev_group = TRUE,
  sort_by_group = "first",
  y_lim = NULL,
  highlight = NULL,
  highlight_color = "#F18222",
  bar_color = "#01847C",
  palette = "default",
  title = NULL,
  subtitle = NULL,
  x_lab = NULL,
  y_lab = NULL,
  legend_title = NULL,
  sort = TRUE,
  top_n = NULL,
  add_label = FALSE,
  label_digits = 1,
  label_size = 3.5,
  label_angle = 0,
  label_hjust = NULL,
  label_vjust = NULL,
  label_color = "black",
  horizontal = TRUE,
  reverse = FALSE,
  x_label_position = "left",
  legend = TRUE,
  legend_position = "top",
  legend_direction = NULL,
  legend_nrow = NULL,
  legend_ncol = NULL,
  legend_title_size = NULL,
  legend_text_size = NULL,
  legend_key_width = NULL,
  legend_key_height = NULL,
  ncol = 1,
  font_family = "sans",
  base_size = 12,
  bg_color = "white"
) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  group_var <- rlang::enquo(group)
  facet_var <- rlang::enquo(facet)
  has_facet <- !rlang::quo_is_null(facet_var)
  has_fill <- !rlang::quo_is_null(group_var)

  validate_barchart_args(
    data = data,
    x_label_position = x_label_position,
    y_lim = y_lim,
    sort = sort,
    top_n = top_n,
    add_label = add_label,
    label_size = label_size,
    label_angle = label_angle,
    label_hjust = label_hjust,
    label_vjust = label_vjust,
    label_color = label_color,
    legend = legend,
    legend_position = legend_position,
    legend_direction = legend_direction,
    legend_nrow = legend_nrow,
    legend_ncol = legend_ncol,
    legend_title_size = legend_title_size,
    legend_text_size = legend_text_size,
    legend_key_width = legend_key_width,
    legend_key_height = legend_key_height,
    ncol = ncol
  )

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

  if (!is.null(highlight) && has_fill) {
    warning(
      "`highlight` is ignored when `group` is supplied; grouped bars are colored by `group`.",
      call. = FALSE
    )
  }

  data <- pre_process_data(
    data = data,
    x = !!x_var,
    y = !!y_var,
    group = !!group_var,
    rev_group = rev_group,
    group_ref = sort_by_group,
    facet = !!facet_var,
    highlight = highlight,
    highlight_color = highlight_color,
    bar_color = bar_color,
    sort = sort,
    top_n = top_n
  )

  if (rlang::quo_is_missing(y_var)) {
    y_var <- rlang::sym("n")
  }

  if (is.null(y_lim)) {
    y_lim <- value_axis_limits(data, !!y_var)
  }

  if (add_label) {
    if (!is.null(highlight) && !has_fill) {
      data <- dplyr::mutate(
        data,
        .label_color = ifelse(
          !!x_var %in% highlight,
          highlight_color,
          label_color
        )
      )
    } else {
      data$.label_color <- label_color
    }
  }

  bar_width <- 0.68
  args <- list(width = bar_width)
  if (has_fill) {
    args$position <- "dodge"
  } else if (!is.null(highlight)) {
    args$mapping <- aes(fill = .data$.color)
  } else {
    args$fill <- bar_color
  }

  if (horizontal) {
    if (has_fill) {
      p <- ggplot(data, aes(!!y_var, !!x_var, fill = !!group_var)) +
        do.call(geom_col, args)
    } else {
      p <- ggplot(data, aes(!!y_var, !!x_var)) +
        do.call(geom_col, args)
    }

    if (add_label) {
      label_args <- list(
        mapping = aes(
          label = scales::number(!!y_var, accuracy = 10^-label_digits),
          color = .data$.label_color
        ),
        size = label_size,
        family = font_family,
        angle = label_angle,
        hjust = if (is.null(label_hjust)) {
          if (reverse) 1.15 else -0.15
        } else {
          label_hjust
        },
        vjust = if (is.null(label_vjust)) {
          0.5
        } else {
          label_vjust
        },
        show.legend = FALSE
      )
      if (has_fill) {
        label_args$position <- position_dodge(width = bar_width)
      }
      p <- p + do.call(geom_text, label_args)
    }

    if (reverse) {
      p <- p +
        scale_x_reverse(
          expand = expansion(mult = 0, 0.1),
          position = "top",
          limits = rev(y_lim),
          breaks = pretty(y_lim, n = 6)
        )
    } else {
      p <- p +
        scale_x_continuous(
          position = "top",
          limits = y_lim,
          breaks = pretty(y_lim, n = 6),
          expand = expansion(mult = 0, 0.1)
        )
    }

    if (!has_facet) {
      p <- p + scale_y_discrete(position = x_label_position)
    }
  } else {
    if (has_fill) {
      p <- ggplot(data, aes(!!x_var, !!y_var, fill = !!group_var))
    } else {
      p <- ggplot(data, aes(!!x_var, !!y_var))
    }
    p <- p + do.call(geom_col, args)

    if (add_label) {
      label_args <- list(
        mapping = aes(
          label = scales::number(!!y_var, accuracy = 10^-label_digits),
          color = .data$.label_color
        ),
        size = label_size,
        family = font_family,
        angle = label_angle,
        hjust = if (is.null(label_hjust)) {
          0.5
        } else {
          label_hjust
        },
        vjust = if (is.null(label_vjust)) {
          if (reverse) 1.3 else -0.3
        } else {
          label_vjust
        },
        show.legend = FALSE
      )
      if (has_fill) {
        label_args$position <- position_dodge(width = bar_width)
      }
      p <- p + do.call(geom_text, label_args)
    }

    if (reverse) {
      p <- p +
        scale_y_reverse(
          limits = rev(y_lim),
          breaks = pretty(y_lim, n = 6),
          expand = expansion(mult = 0, 0.1)
        )
    } else {
      p <- p +
        scale_y_continuous(
          limits = y_lim,
          breaks = pretty(y_lim, n = 6),
          expand = expansion(mult = 0, 0.1)
        )
    }
  }

  if (has_fill) {
    p <- p + scale_fill_canreg(palette = palette)
  }

  if (!is.null(highlight) && !has_fill) {
    p <- p + scale_fill_identity()
  }

  if (add_label) {
    p <- p + scale_color_identity()
  }

  p <- p +
    .journal_chart_theme(
      base_size = base_size,
      font_family = font_family,
      legend_position = legend_position,
      grid = if (horizontal) "x" else "y",
      subtitle = subtitle,
      bg_color = bg_color
    ) +
    .legend_theme(
      legend = legend,
      legend_title = legend_title,
      options = legend_options,
      show = has_fill
    )

  legend_guides <- .legend_guides("fill", "legend", legend_options)
  if (!is.null(legend_guides)) {
    p <- p + legend_guides
  }

  if (has_facet) {
    p <- p +
      facet_wrap(
        vars(!!facet_var),
        scales = "free",
        ncol = ncol,
        strip.position = "top"
      ) +
      scale_axis_text(horizontal, x_label_position) +
      theme(strip.placement = "outside")
  }

  p <- p +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_lab,
      y = y_lab,
      fill = legend_title
    )

  return(p)
}


scale_axis_text <- function(horizontal = TRUE, position) {
  if (horizontal) {
    scale_y_discrete(label = replace_axis_text, position = position)
  } else {
    scale_x_discrete(label = replace_axis_text)
  }
}

replace_axis_text <- function(x) {
  sapply(strsplit(x, "___"), function(x) x[1])
}


validate_barchart_args <- function(
  data,
  x_label_position = "left",
  y_lim = NULL,
  sort = TRUE,
  top_n = NULL,
  add_label = FALSE,
  label_size = 3.5,
  label_angle = 0,
  label_hjust = NULL,
  label_vjust = NULL,
  label_color = "black",
  legend = TRUE,
  legend_position = "top",
  legend_direction = NULL,
  legend_nrow = NULL,
  legend_ncol = NULL,
  legend_title_size = NULL,
  legend_text_size = NULL,
  legend_key_width = NULL,
  legend_key_height = NULL,
  ncol = 1
) {
  .check_data_frame(data)

  if (!x_label_position %in% c("left", "right")) {
    stop("`x_label_position` must be either 'left' or 'right'.", call. = FALSE)
  }

  if (!is.null(y_lim)) {
    if (!is.numeric(y_lim) || length(y_lim) != 2 || anyNA(y_lim)) {
      stop(
        "`y_lim` must be a numeric vector of length 2 without missing values.",
        call. = FALSE
      )
    }
    if (y_lim[1] == y_lim[2]) {
      stop("`y_lim` must contain two different values.", call. = FALSE)
    }
  }

  valid_sort <-
    (is.logical(sort) && length(sort) == 1 && !is.na(sort)) ||
    (is.character(sort) &&
      length(sort) == 1 &&
      !is.na(sort) &&
      tolower(sort) %in% c("asc", "ascending", "desc", "descending"))
  if (!valid_sort) {
    stop("`sort` must be TRUE, FALSE, 'asc', or 'desc'.", call. = FALSE)
  }

  if (!is.null(top_n)) {
    if (!is.numeric(top_n) || length(top_n) != 1 || is.na(top_n) || top_n < 1) {
      stop("`top_n` must be a single positive number.", call. = FALSE)
    }
  }

  .check_single_logical(add_label, "add_label")

  if (
    !is.numeric(label_size) ||
      length(label_size) != 1 ||
      is.na(label_size) ||
      label_size <= 0
  ) {
    stop("`label_size` must be a single positive number.", call. = FALSE)
  }

  if (
    !is.numeric(label_angle) || length(label_angle) != 1 || is.na(label_angle)
  ) {
    stop("`label_angle` must be a single numeric value.", call. = FALSE)
  }

  if (
    !is.null(label_hjust) &&
      (!is.numeric(label_hjust) ||
        length(label_hjust) != 1 ||
        is.na(label_hjust))
  ) {
    stop("`label_hjust` must be NULL or a single numeric value.", call. = FALSE)
  }

  if (
    !is.null(label_vjust) &&
      (!is.numeric(label_vjust) ||
        length(label_vjust) != 1 ||
        is.na(label_vjust))
  ) {
    stop("`label_vjust` must be NULL or a single numeric value.", call. = FALSE)
  }

  .check_single_character(label_color, "label_color")

  .check_single_logical(legend, "legend")

  .legend_options(
    legend_position = legend_position,
    legend_direction = legend_direction,
    legend_nrow = legend_nrow,
    legend_ncol = legend_ncol,
    legend_title_size = legend_title_size,
    legend_text_size = legend_text_size,
    legend_key_width = legend_key_width,
    legend_key_height = legend_key_height
  )

  if (!is.numeric(ncol) || length(ncol) != 1 || is.na(ncol) || ncol < 1) {
    stop("`ncol` must be a single positive number.", call. = FALSE)
  }

  invisible(TRUE)
}


value_axis_limits <- function(data, y) {
  y_var <- rlang::enquo(y)
  values <- dplyr::pull(data, !!y_var)
  values <- values[is.finite(values)]

  if (length(values) == 0) {
    return(c(0, 1))
  }

  range_values <- range(values, na.rm = TRUE)
  if (range_values[1] > 0) {
    range_values[1] <- 0
  }
  if (range_values[1] == range_values[2]) {
    if (range_values[1] == 0) {
      range_values[2] <- 1
    } else {
      range_values <- range_values + c(0, abs(range_values[1]) * 0.1)
    }
  }

  range_values
}
