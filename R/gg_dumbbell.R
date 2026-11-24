#' Plot Dumbbell Chart
#'
#' Easily create a dumbbell chart
#'
#' @param data Dataset to use for the dumbbell chart
#' @param x Character or factor column of data (usually a category)
#' @param y Numeric column of data representing dumbbell endpoint values.
#' @param group Column of data identifying the two endpoint groups.
#' @param line_size Numeric. Line width.
#' @param line_color Character. Line color.
#' @param point_size Numeric. Size of the points.
#' @param point_colors Character vector of length 2. Colors of the two endpoint groups.
#' @param palette Palette name passed to [canreg_palette()] when
#'   `point_colors = NULL`.
#' @param point_label_size Numeric. Font size of endpoint labels in points.
#' @param point_label_color Character. Color of endpoint labels.
#' @param point_label_space Numeric. Extra space used to position endpoint labels.
#' @param label_digits Number of decimal places used for endpoint labels.
#' @param diff_label Logical. Whether to show difference labels.
#' @param diff_digits Number of decimal places used for difference labels.
#' @param diff_direction Direction used to calculate differences. Use
#'   `"first_minus_last"` or `"last_minus_first"`.
#' @param group_order Optional character vector of length 2 specifying the order
#'   of the two endpoint groups.
#' @param diff_label_x Numeric. Fixed x position for difference labels. If NULL,
#'   the position is calculated automatically from the data range.
#' @param diff_label_width Integer. Fixed character width for difference labels.
#'   If NULL, the width is calculated from the longest difference label.
#' @param diff_label_size Numeric. Font size of difference labels in points.
#' @param sort Logical or character. If `TRUE`, sort by difference in
#'   descending order. If `FALSE`, keep the original category order. Use
#'   `"asc"` or `"desc"` to sort by difference explicitly.
#' @param sort_by Character. Sort by `"diff"` for the absolute endpoint
#'   difference, `"first"` for the first group value, or `"last"` for the
#'   second group value.
#' @param horizontal Logical. Should the plot be horizontal?
#' @param top_n Integer. Display only the top `top_n` x-axis categories by
#'   `sort_by`. If `NULL`, all categories are shown. The selected categories
#'   are always the largest categories by `sort_by`; `sort` only controls their
#'   display order.
#' @param x_lim Optional numeric vector of length 2 for x-axis limits. Use
#'   `NA` to keep one side automatic, e.g. `c(0, NA)`.
#' @param legend Logical. Show legend?
#' @param legend_labels Character vector. Custom labels for legend. If `NULL`,
#'   labels are taken from the levels or values of `group`.
#' @param legend_title Optional legend title.
#' @param legend_position Position of legend. Passed to `ggplot2::theme()`.
#' @param legend_size Optional font size of legend text in points. When `NULL`,
#'   the coordinated theme size is used.
#' @param legend_direction Optional legend direction: `"horizontal"` or
#'   `"vertical"`.
#' @param legend_nrow,legend_ncol Optional number of rows or columns in the
#'   legend, passed to `ggplot2::guide_legend()`.
#' @param legend_title_size,legend_text_size Optional font size for legend
#'   title and text. If `legend_text_size` is `NULL`, `legend_size` is used for
#'   backward compatibility.
#' @param legend_key_width,legend_key_height Optional legend key width and
#'   height. Use a `grid::unit()` object, or provide a single numeric value
#'   interpreted as lines.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param x_lab Numeric-axis title.
#' @param font_family Font family used by plot text and data labels.
#' @param base_size Base font size in points. Defaults to 12 for coordinated
#'   output across ggcanreg charts.
#' @param bg_color Background color for the entire figure, including the plot
#'   panel. Use `"transparent"` for slide overlays.
#'
#' @return A ggplot object
#' @importFrom rlang .data
#' @export
gg_dumbbell <- function(
  data,
  x,
  y,
  group,
  line_size = 1.2,
  line_color = "lightgray",
  point_size = 4,
  point_colors = NULL,
  palette = "sex",
  point_label_size = 8,
  point_label_color = "black",
  point_label_space = 5,
  label_digits = 1,
  diff_label = TRUE,
  diff_digits = 1,
  diff_direction = c("first_minus_last", "last_minus_first"),
  group_order = NULL,
  diff_label_x = NULL,
  diff_label_width = NULL,
  diff_label_size = 8,
  sort = TRUE,
  sort_by = "last",
  horizontal = TRUE,
  top_n = NULL,
  x_lim = NULL,
  legend = TRUE,
  legend_labels = NULL,
  legend_title = NULL,
  legend_position = "top",
  legend_size = NULL,
  legend_direction = NULL,
  legend_nrow = NULL,
  legend_ncol = NULL,
  legend_title_size = NULL,
  legend_text_size = NULL,
  legend_key_width = NULL,
  legend_key_height = NULL,
  title = NULL,
  subtitle = NULL,
  x_lab = NULL,
  font_family = "sans",
  base_size = 12,
  bg_color = "white"
) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  group_var <- rlang::enquo(group)
  diff_direction <- match.arg(diff_direction)

  .check_data_frame(data)
  .check_dumbbell_variable(data, x_var, "x")
  .check_dumbbell_variable(data, y_var, "y")
  .check_dumbbell_variable(data, group_var, "group")

  y_values <- dplyr::pull(data, !!y_var)
  if (!is.numeric(y_values)) {
    stop("`y` must be a numeric column.", call. = FALSE)
  }
  if (anyNA(y_values) || any(!is.finite(y_values))) {
    stop("`y` must contain only finite, non-missing values.", call. = FALSE)
  }

  .check_single_logical(legend, "legend")
  .check_single_logical(horizontal, "horizontal")
  .check_single_logical(diff_label, "diff_label")
  .check_single_number(line_size, "line_size", min = 0)
  .check_single_number(point_size, "point_size", min = 0)
  .check_single_number(point_label_size, "point_label_size", min = 0)
  .check_single_number(point_label_space, "point_label_space")
  .check_single_number(diff_label_size, "diff_label_size", min = 0)
  legend_options <- .legend_options(
    legend_position = legend_position,
    legend_direction = legend_direction,
    legend_nrow = legend_nrow,
    legend_ncol = legend_ncol,
    legend_title_size = legend_title_size,
    legend_text_size = legend_text_size,
    legend_key_width = legend_key_width,
    legend_key_height = legend_key_height,
    legend_size = legend_size
  )

  group_values_all <- unique(as.character(dplyr::pull(data, !!group_var)))
  if (length(group_values_all) != 2) {
    stop("`group` must contain exactly two unique values.", call. = FALSE)
  }
  if (!is.null(group_order)) {
    if (
      !is.character(group_order) ||
        length(group_order) != 2 ||
        anyNA(group_order)
    ) {
      stop(
        "`group_order` must be NULL or a character vector of length 2.",
        call. = FALSE
      )
    }
    if (!setequal(group_order, group_values_all)) {
      stop(
        "`group_order` must contain the same values as `group`.",
        call. = FALSE
      )
    }
    data <- data |>
      dplyr::mutate(!!group_var := factor(!!group_var, levels = group_order))
  }

  if (is.null(point_colors)) {
    point_colors <- canreg_palette(palette, n = 2)
  }
  if (!is.character(point_colors) || length(point_colors) < 2) {
    stop(
      "`point_colors` must be NULL or a character vector of length at least 2.",
      call. = FALSE
    )
  }
  point_colors <- unname(point_colors[seq_len(2)])

  .check_digits(label_digits, "label_digits")
  .check_digits(diff_digits, "diff_digits")

  if (
    !is.null(top_n) &&
      (!is.numeric(top_n) ||
        length(top_n) != 1 ||
        is.na(top_n) ||
        top_n <= 0 ||
        top_n != floor(top_n))
  ) {
    stop("`top_n` must be `NULL` or a single positive integer.", call. = FALSE)
  }

  data <- data |>
    group_by(!!x_var) |>
    arrange(!!x_var, !!group_var) |>
    mutate(
      first_value = dplyr::first(!!y_var),
      last_value = dplyr::last(!!y_var),
      change = if (diff_direction == "first_minus_last") {
        round(.data$first_value - .data$last_value, diff_digits)
      } else {
        round(.data$last_value - .data$first_value, diff_digits)
      },
      way = ifelse(.data$change > 0, 1, 2),
      y_min = min(!!y_var, na.rm = TRUE),
      y_max = max(!!y_var, na.rm = TRUE),
      label_space = max(
        point_label_space,
        (.data$y_max[1] - .data$y_min[1]) * 0.03,
        na.rm = TRUE
      ),
      is_left_endpoint = !!y_var == .data$y_min,
      y_label = ifelse(
        .data$is_left_endpoint,
        !!y_var - .data$label_space,
        !!y_var + .data$label_space
      ),
      label_hjust = ifelse(.data$is_left_endpoint, 1, 0),
      label = scales::number(!!y_var, accuracy = 10^-label_digits)
    ) |>
    ungroup()

  valid_sort <- identical(sort, TRUE) ||
    identical(sort, FALSE) ||
    (is.character(sort) && length(sort) == 1 && sort %in% c("desc", "asc"))

  if (!valid_sort) {
    stop("`sort` must be `TRUE`, `FALSE`, \"desc\", or \"asc\".", call. = FALSE)
  }

  if (!is.character(sort_by) || length(sort_by) != 1) {
    stop("`sort_by` must be a single character value.", call. = FALSE)
  }
  if (!sort_by %in% c("diff", "first", "last")) {
    stop(
      "`sort_by` must be one of \"diff\", \"first\", or \"last\".",
      call. = FALSE
    )
  }

  original_x_value <- data |>
    pull(!!x_var) |>
    unique()

  sort_data <- data |>
    dplyr::distinct(!!x_var, .data$change, .data$first_value, .data$last_value)

  sort_data <- sort_data |>
    mutate(
      sort_value = if (sort_by == "diff") {
        abs(.data$change)
      } else if (sort_by == "first") {
        .data$first_value
      } else {
        .data$last_value
      }
    )

  if (!is.null(top_n)) {
    top_x_value <- sort_data |>
      arrange(dplyr::desc(.data$sort_value)) |>
      pull(!!x_var) |>
      utils::head(top_n)

    data <- data |>
      dplyr::filter(!!x_var %in% top_x_value)

    sort_data <- sort_data |>
      dplyr::filter(!!x_var %in% top_x_value)

    original_x_value <- original_x_value[original_x_value %in% top_x_value]
  }

  if (identical(sort, FALSE)) {
    x_value <- original_x_value
  } else {
    sort_direction <- if (identical(sort, TRUE)) {
      "desc"
    } else {
      sort
    }

    x_value <- sort_data |>
      arrange(
        if (sort_direction == "desc") {
          dplyr::desc(.data$sort_value)
        } else {
          .data$sort_value
        }
      ) |>
      pull(!!x_var)
  }

  display_x_value <- if (horizontal) rev(x_value) else x_value
  data <- data |>
    mutate(!!x_var := factor(!!x_var, levels = display_x_value))

  group_values <- data |>
    pull(!!group_var)

  legend_breaks <- if (is.factor(group_values)) {
    group_levels <- levels(group_values)
    group_levels[group_levels %in% as.character(unique(group_values))]
  } else {
    as.character(unique(group_values))
  }

  if (is.null(legend_labels)) {
    legend_labels <- legend_breaks
  }

  #data <- pre_process_data(data, !!x_var, !!y_var, sort = sort, top_n = top_n)
  y_min <- data |>
    pull(!!y_var) |>
    min(na.rm = TRUE)

  y_max <- data |>
    pull(!!y_var) |>
    max(na.rm = TRUE)

  y_range <- y_max - y_min
  if (is.null(diff_label_x)) {
    diff_label_x <- y_max +
      max(point_label_space * 2.5, y_range * 0.18, na.rm = TRUE)
  }

  if (
    !is.null(x_lim) &&
      (!is.numeric(x_lim) || length(x_lim) != 2 || all(is.na(x_lim)))
  ) {
    stop(
      "`x_lim` must be `NULL` or a numeric vector of length 2 with at least one non-NA value.",
      call. = FALSE
    )
  }

  diff_data <- data |>
    dplyr::distinct(!!x_var, .data$change, .keep_all = TRUE)

  change_label <- scales::number(diff_data$change, accuracy = 10^-diff_digits)
  if (is.null(diff_label_width)) {
    diff_label_width <- max(nchar(change_label), na.rm = TRUE)
  }

  formatted_diff_label <- sprintf(
    paste0("%", diff_label_width, "s"),
    change_label
  )
  label_widths <- vapply(
    formatted_diff_label,
    function(label) {
      grid::convertWidth(
        grid::grobWidth(
          grid::textGrob(
            label,
            gp = grid::gpar(
              fontfamily = font_family,
              fontsize = diff_label_size
            )
          )
        ),
        "mm",
        valueOnly = TRUE
      )
    },
    numeric(1)
  )
  widest_diff_label <- formatted_diff_label[[which.max(label_widths)]]

  diff_data <- diff_data |>
    mutate(
      diff_label = formatted_diff_label,
      diff_label_background = widest_diff_label
    )

  pt_to_mm <- function(x) {
    x / (72.27 / 25.4)
  }

  plot <- data |>
    ggplot(aes(x = !!y_var, y = !!x_var, fill = factor(!!group_var))) +
    geom_line(
      aes(group = !!x_var),
      colour = line_color,
      linewidth = line_size
    ) +
    geom_point(shape = 21, size = point_size, colour = "white") +
    geom_text(
      aes(x = .data$y_label, label = .data$label, hjust = .data$label_hjust),
      size = pt_to_mm(point_label_size),
      color = point_label_color,
      family = font_family
    )

  if (isTRUE(diff_label)) {
    plot <- plot +
      geom_label(
        data = diff_data,
        aes(
          x = diff_label_x,
          y = !!x_var,
          label = .data$diff_label_background
        ),
        inherit.aes = FALSE,
        size = pt_to_mm(diff_label_size),
        fill = point_colors[1],
        color = point_colors[1],
        family = font_family,
        linewidth = 0
      ) +
      geom_text(
        data = diff_data,
        aes(x = diff_label_x, y = !!x_var, label = .data$diff_label),
        inherit.aes = FALSE,
        size = pt_to_mm(diff_label_size),
        color = "white",
        family = font_family
      )
  }

  # Legend (manual via dummy fill scale)
  plot <- plot +
    scale_fill_manual(
      name = legend_title,
      values = c(point_colors[1], point_colors[2]),
      breaks = legend_breaks,
      labels = legend_labels,
      guide = if (legend) {
        guide_legend(override.aes = list(shape = 21))
      } else {
        "none"
      }
    )

  plot <- plot +
    .journal_chart_theme(
      base_size = base_size,
      font_family = font_family,
      legend_position = legend_position,
      grid = "x",
      subtitle = subtitle,
      bg_color = bg_color
    ) +
    .legend_theme(
      legend = legend,
      legend_title = legend_title,
      options = legend_options
    )

  legend_guides <- .legend_guides(
    "fill",
    "legend",
    legend_options,
    override.aes = list(shape = 21)
  )
  if (!is.null(legend_guides)) {
    plot <- plot + legend_guides
  }

  if (!is.null(x_lim)) {
    plot <- plot + scale_x_continuous(limits = x_lim)
  } else {
    plot <- plot +
      scale_x_continuous(expand = expansion(mult = c(0.06, 0.12)))
  }

  if (!horizontal) {
    plot <- plot + coord_flip()
  }

  plot +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_lab,
      y = NULL,
      fill = legend_title
    )
}


.check_dumbbell_variable <- function(data, var, arg) {
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
