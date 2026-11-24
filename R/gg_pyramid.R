#' Draw a pyramid chart
#'
#' @param data A data frame containing the variables used for the plot.
#' @param x \code{numeric} representing the bar length.
#' @param y \code{character} or \code{factor} column of \code{data}.
#' @param group \code{character} or \code{factor} column of \code{data}
#' @param facet \code{character} or \code{factor} column of \code{data} used to
#'        create facets of the pyramids. Each facet value is shown at the upper
#'        left of its pyramid.
#' @param legend_labels \code{character} vector of length 2, label for the groups.
#' @param bar_color \code{character} vector of length 2, colors for the two
#'        groups, default is c("darkgreen", "darkred").
#' @param palette Palette name passed to [canreg_palette()] when
#'        `bar_color = NULL`.
#' @param group_label Deprecated. Use `legend_labels` instead.
#' @param group_color Deprecated. Use `bar_color` instead.
#' @param bg_color \code{character} Background color for the entire chart.
#'        Use `"transparent"` for slide overlays. Defaults to `"white"`.
#' @param sort \code{logical} or \code{character}. Should the data be sorted
#'        before plotting? Use \code{TRUE} for the default ascending order,
#'        \code{FALSE} to keep the original order, or \code{"asc"}/
#'        \code{"desc"} to explicitly sort in ascending/descending order.
#' @param title \code{character} Main title for the plot.
#' @param subtitle \code{character} Subtitle for the plot.
#' @param x_lab \code{character} A label for the X-axis (optional).
#' @param gap Space between bars.
#' @param space Deprecated. Use `gap` instead.
#' @param font_family Font family used by plot text.
#' @param base_size Base font size in points. Defaults to 12 for coordinated
#'        output across ggcanreg charts.
#' @param ... Params.
#'
#' @return A patchwork object combining two ggplot bar charts for each pyramid.
#' @export
#'
gg_pyramid <- function(
  data,
  x,
  y,
  group,
  facet = NULL,
  legend_labels = c("Male", "Female"),
  bar_color = NULL,
  palette = "sex",
  bg_color = "white",
  sort = FALSE,
  title = NULL,
  subtitle = NULL,
  x_lab = NULL,
  gap = 6,
  group_label = NULL,
  group_color = NULL,
  space = NULL,
  font_family = "sans",
  base_size = 12,
  ...
) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  group <- rlang::enquo(group)
  facet <- rlang::enquo(facet)

  .check_data_frame(data)
  .check_pyramid_variable(data, x, "x")
  .check_pyramid_variable(data, y, "y")
  .check_pyramid_variable(data, group, "group")
  .check_pyramid_variable(data, facet, "facet", optional = TRUE)
  .check_single_character(font_family, "font_family")
  .check_single_number(base_size, "base_size", min = 0)
  .check_bg_color(bg_color)

  x_values <- dplyr::pull(data, !!x)
  if (!is.numeric(x_values)) {
    stop("`x` must be a numeric column.", call. = FALSE)
  }
  if (anyNA(x_values) || any(!is.finite(x_values))) {
    stop("`x` must contain only finite, non-missing values.", call. = FALSE)
  }
  if (any(x_values < 0)) {
    stop("`x` must contain non-negative values.", call. = FALSE)
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

  if (!is.null(group_label)) {
    warning(
      "`group_label` is deprecated; use `legend_labels` instead.",
      call. = FALSE
    )
    legend_labels <- group_label
  }
  if (!is.null(group_color)) {
    warning(
      "`group_color` is deprecated; use `bar_color` instead.",
      call. = FALSE
    )
    bar_color <- group_color
  }
  if (!is.null(space)) {
    warning(
      "`space` is deprecated; use `gap` instead.",
      call. = FALSE
    )
    gap <- space
  }

  if (is.null(bar_color)) {
    bar_color <- canreg_palette(palette, n = 2)
  }
  if (!is.character(bar_color) || length(bar_color) < 2 || anyNA(bar_color)) {
    stop(
      "`bar_color` must be NULL or a character vector of length at least 2.",
      call. = FALSE
    )
  }
  bar_color <- bar_color[seq_len(2)]
  if (
    !is.null(legend_labels) &&
      (!is.character(legend_labels) ||
        length(legend_labels) != 2 ||
        anyNA(legend_labels))
  ) {
    stop(
      "`legend_labels` must be NULL or a character vector of length 2.",
      call. = FALSE
    )
  }
  .check_single_number(gap, "gap")

  data <- data |> arrange(!!facet, !!group)
  pyramid <- function(data) {
    # calculate the maxium value of the axis
    limit <- data |> pull(!!x) |> max()
    group_values <- data |> pull(!!group) |> unique()
    facet_label <- if (!rlang::quo_is_null(facet)) {
      as.character(dplyr::first(dplyr::pull(data, !!facet)))
    } else {
      NULL
    }

    # check the group variable if contains two groups
    if (length(group_values) != 2) {
      stop(
        "Group variable must have exactly two unique values for pyramid chart."
      )
    }

    # sort the order of bars
    sort_order <- if (is.logical(sort) && !is.na(sort) && sort) {
      "asc"
    } else if (
      is.character(sort) &&
        length(sort) == 1 &&
        !is.na(sort) &&
        tolower(sort) %in% c("asc", "ascending")
    ) {
      "asc"
    } else if (
      is.character(sort) &&
        length(sort) == 1 &&
        !is.na(sort) &&
        tolower(sort) %in% c("desc", "descending")
    ) {
      "desc"
    } else {
      NULL
    }
    if (!is.null(sort_order)) {
      data <- dplyr::group_by(data, desc(!!group)) |>
        dplyr::mutate(
          !!y := if (sort_order == "asc") {
            stats::reorder(!!y, !!x)
          } else {
            stats::reorder(!!y, -!!x)
          }
        )
    }

    # judge if group label was specified
    if (is.null(legend_labels)) {
      legend_labels <- group_values
    }

    # plot each bar chart of the two groups
    group_data <- data |> group_by(!!group) |> group_split()
    plots <- lapply(
      group_data,
      pbar,
      !!x,
      !!y,
      !!group,
      legend_labels,
      group_values,
      bar_color,
      bg_color,
      limit,
      gap,
      font_family,
      base_size,
      facet_label
    )
    plots[[1]] + plots[[2]]
  }

  if (!rlang::quo_is_null(facet)) {
    data <- dplyr::group_by(data, !!facet) |> dplyr::group_split()
  } else {
    data <- list(data)
  }

  # combine the two bar charts
  annotation_theme <- .journal_chart_theme(
    base_size = base_size,
    font_family = font_family,
    legend_position = "none",
    grid = "none",
    subtitle = subtitle,
    bg_color = bg_color
  ) +
    theme(
      plot.caption = element_text(
        hjust = 0.5,
        size = rel(0.95),
        margin = margin(t = 7)
      )
    )

  final_plot <- patchwork::wrap_plots(lapply(data, pyramid), ...) +
    plot_annotation(
      title = title,
      subtitle = subtitle,
      caption = x_lab,
      theme = annotation_theme
    )
  return(final_plot)
}


.check_pyramid_variable <- function(data, var, arg, optional = FALSE) {
  if (optional && (rlang::quo_is_null(var) || rlang::quo_is_missing(var))) {
    return(invisible(TRUE))
  }

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


# define bar chart function
pbar <- function(
  data,
  x,
  y,
  group,
  legend_labels,
  group_values,
  bar_color,
  bg_color,
  limit = 45000,
  gap,
  font_family,
  base_size,
  facet_label = NULL
) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  group <- rlang::enquo(group)
  group_value <- data |> pull(!!group) |> unique()

  # define the fill color of each bar chart
  if (group_value == group_values[1]) {
    fill_color <- bar_color[1]
  } else if (group_value == group_values[2]) {
    fill_color <- bar_color[2]
  }

  # plot the bar chart
  bar <- ggplot(data) +
    geom_bar(
      aes(x = !!x, y = !!y, fill = factor(!!group)),
      stat = "identity",
      color = bg_color,
      width = 0.9
    ) +
    scale_fill_manual(values = unname(fill_color))

  # set the x axis
  if (group_value == group_values[1]) {
    bar <- bar +
      scale_x_reverse(
        limits = c(limit, 0),
        expand = expansion(mult = c(.05, 0))
      )
  } else if (group_value == group_values[2]) {
    bar <- bar +
      scale_x_continuous(
        limits = c(0, limit),
        expand = expansion(mult = c(0, .05))
      )
  }

  # theme common
  bar <- bar +
    .journal_chart_theme(
      base_size = base_size,
      font_family = font_family,
      legend_position = "none",
      grid = "x",
      bg_color = bg_color
    ) +
    theme(
      axis.title = element_blank(),
      axis.title.x.bottom = element_blank(),
      axis.title.x.top = element_blank(),
      axis.title.y.left = element_blank(),
      axis.title.y.right = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.text.x = element_text(face = "bold"),
      axis.text.y = element_text(hjust = 0.5),
      axis.title.x = element_blank()
    )

  # theme unique for each bar chart
  if (group_value == group_values[1]) {
    bar <- bar +
      theme(
        axis.text.y = element_blank(),
        axis.text.y.left = element_blank(),
        axis.text.y.right = element_blank(),
        plot.margin = margin(t = 4, r = gap, b = 8, l = 12),
        legend.position = "none",
        plot.tag.position = "topleft",
        plot.tag = element_text(
          size = rel(0.95),
          face = "bold",
          hjust = 0,
          margin = margin(b = 3)
        ),
        plot.title.position = "panel",
        plot.title = element_text(
          hjust = 1,
          size = rel(0.95),
          margin = margin(b = 3, t = 3)
        )
      ) +
      labs(title = legend_labels[1], tag = facet_label)
  } else if (group_value == group_values[2]) {
    bar <- bar +
      theme(
        axis.text.y.left = element_text(
          angle = 0,
          hjust = 0.5,
          margin = margin(r = 2 * gap)
        ),
        plot.margin = margin(t = 4, r = 12, b = 8, l = gap),
        legend.position = "none",
        plot.title.position = "panel",
        plot.title = element_text(
          hjust = 0,
          size = rel(0.95),
          margin = margin(b = 3, t = 3)
        )
      ) +
      ggtitle(legend_labels[2])
  }

  return(bar)
}
