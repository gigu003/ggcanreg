#' Create a Pie Chart
#'
#' This function generates a pie chart using ggplot2, with an option to color the slices by a categorical variable.
#'
#' @param data A data frame to be ploted for the pie chart.
#' @param x \code{numeric} column of \code{data} representing the values of
#'        the pie chart slices (e.g., percentages or counts). Slices are drawn
#'        by relative size; labels are not automatically converted to
#'        percentages.
#' @param label Optional \code{character} or \code{factor} column of \code{data}
#'        representing the label and color the slices of the pie chart. If
#'        \code{NULL}, slices are colored by row.
#' @param facet Optional column of \code{data} used to split the plot into
#'        multiple pie charts. Slice positions and percentages are calculated
#'        independently within each facet. When supplied, the default facet
#'        strip labels are hidden because facet values are shown as center
#'        labels by default.
#' @param ncol Optional number of columns for faceted pie charts. Passed to
#'        \code{ggplot2::facet_wrap()} when \code{facet} is supplied.
#' @param palette Color palette. Defaults to \code{"default"}, a discrete
#'        ggcanreg palette. Names supported by \code{canreg_palette()} are used
#'        as discrete palettes; other names are passed to
#'        \code{colorspace::sequential_hcl()} for backward compatibility.
#' @param colors Colors.
#' @param slice_gap_color Color of the gaps or borders between pie slices.
#' @param slice_gap_width Width of the gaps or borders between pie slices.
#' @param legend Logical. If \code{TRUE}, show the legend. The legend is always
#'        hidden when \code{label = NULL} because the fill variable is internal.
#' @param legend_title Optional legend title.
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
#' @param label_position Label position. \code{"none"} shows no text labels on
#'        the pie; \code{"inside"} places rotated labels on slices;
#'        \code{"outside"} places labels outside the pie and connects them to
#'        slices with guide lines.
#' @param label_color Label text color.
#' @param label_size Label text size.
#' @param label_value Logical. If \code{TRUE}, append the slice value on a
#'        new line below the label.
#' @param label_value_type Type or types of values appended to labels.
#'        \code{"percent"} shows the slice proportion calculated from
#'        \code{x}; \code{"value"} shows the original \code{x} value. Use
#'        \code{c("value", "percent")} to show both on the same line.
#' @param label_value_digits Number of digits used when formatting percent
#'        values appended to labels. Original values are not rounded by this
#'        argument.
#' @param label_value_suffix Suffix appended to formatted label values.
#'        Can be a single string or a named vector with names \code{"percent"}
#'        and \code{"value"}. Defaults to adding the percent sign only to
#'        percentages.
#' @param label_value_sep Separator used when multiple label value types are
#'        displayed on the same line.
#' @param label_radius Radius position for labels when
#'        \code{label_position = "inside"}. Smaller values move labels toward
#'        the center; larger values move labels toward the outer edge.
#' @param label_hjust Horizontal alignment for labels when
#'        \code{label_position = "inside"}. If \code{NULL}, labels are
#'        automatically aligned by slice side so the outer end of each label is
#'        anchored near the slice outer edge. If specified, \code{1} anchors
#'        the left side of labels at \code{label_radius}, \code{0.5} centers
#'        labels, and \code{0} anchors the right side.
#' @param inner_radius Inner radius of the pie. Larger values create a larger
#'        center hole, producing a donut chart. Must be greater than or equal to
#'        0 and smaller than 1.5.
#' @param line_color Guide line color when \code{label_position = "outside"}.
#' @param outside_label_repel Logical. If \code{TRUE}, slightly adjust outside
#'        label positions to reduce overlap when labels are close together.
#' @param outside_label_min_gap Minimum spacing between outside labels, expressed
#'        as a proportion of the total pie value.
#' @param outside_line_length Length of guide lines for outside labels.
#' @param outside_label_gap Gap between the guide line end and outside label.
#' @param outside_line_width Width of guide lines for outside labels.
#' @param outside_line_type Line type of guide lines for outside labels.
#'        Common values include \code{"solid"}, \code{"dashed"},
#'        \code{"dotted"}, \code{"dotdash"}, \code{"longdash"}, and
#'        \code{"twodash"}. Numeric linetype values supported by ggplot2,
#'        such as \code{1}, \code{2}, and \code{3}, can also be used.
#' @param pie_direction Direction in which slices are displayed.
#'        \code{"desc"} displays slices clockwise;
#'        \code{"asc"} displays slices counterclockwise.
#' @param sort Slice sorting order. \code{"none"} keeps the input data order;
#'        \code{"desc"} sorts slices from large to small by \code{x};
#'        \code{"asc"} sorts slices from small to large by \code{x}.
#' @param top_n Optional number of largest slices to keep. When supplied,
#'        slices are selected by \code{x} from large to small, independently of
#'        \code{sort}; all remaining slices are merged into \code{other_label}.
#'        Only available when \code{label} is supplied. When \code{facet} is
#'        supplied, \code{top_n} is applied independently within each facet.
#' @param other_label Label used for slices merged by \code{top_n}. Defaults to
#'        \code{"\\u5176\\u4ed6"}.
#' @param other_group Optional label value or values to be treated as
#'        "Other". When supplied, matching slices are moved to the end after
#'        applying \code{sort}. Only available when \code{label} is supplied.
#' @param highlight Optional value or values used to highlight one or more
#'        slices. When \code{label} is supplied, values are matched against the
#'        label column. When \code{label = NULL}, numeric values are matched
#'        against row numbers.
#' @param highlight_color Optional color or colors for highlighted slices. If a
#'        single color is supplied, all highlighted slices use that color. If
#'        multiple colors are supplied, their length must match the number of
#'        highlighted slice groups.
#' @param highlight_radius Outer radius for highlighted slices. Values larger
#'        than \code{1.5} make highlighted slices extend beyond normal slices.
#' @param center_label Optional text label displayed at the center of the pie or
#'        donut chart. If \code{NULL} and \code{facet} is supplied, facet values
#'        are used as center labels. When \code{facet} is supplied,
#'        \code{center_label} can be length 1 or have the same length as the
#'        number of facet values.
#' @param center_label_size Center label text size.
#' @param center_label_color Center label text color.
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param font_family Font family used by plot text and pie labels.
#' @param base_size Base font size in points. Defaults to 12 for coordinated
#'        output across ggcanreg charts.
#' @param bg_color Background color for the entire figure, including the plot
#'        panel. Use `"transparent"` for slide overlays.
#'
#' @return A ggplot object representing the pie chart.
#' @export
#'
gg_pie <- function(
  data,
  x,
  label = NULL,
  facet = NULL,
  ncol = NULL,
  palette = "default",
  colors = NULL,
  slice_gap_color = "white",
  slice_gap_width = 0.2,
  legend = FALSE,
  legend_title = NULL,
  legend_position = "right",
  legend_direction = "vertical",
  legend_nrow = NULL,
  legend_ncol = NULL,
  legend_title_size = NULL,
  legend_text_size = NULL,
  legend_key_width = NULL,
  legend_key_height = NULL,
  label_position = c("none", "inside", "outside"),
  label_color = "black",
  label_size = 3,
  label_value = FALSE,
  label_value_type = "percent",
  label_value_digits = 1,
  label_value_suffix = c(percent = "%", value = ""),
  label_value_sep = ", ",
  label_radius = 1.45,
  label_hjust = NULL,
  inner_radius = 0.5,
  line_color = "grey50",
  outside_label_repel = TRUE,
  outside_label_min_gap = 0.04,
  outside_line_length = 0.25,
  outside_label_gap = 0.08,
  outside_line_width = 0.3,
  outside_line_type = "solid",
  pie_direction = c("desc", "asc"),
  sort = c("none", "desc", "asc"),
  top_n = NULL,
  other_label = "\u5176\u4ed6",
  other_group = NULL,
  highlight = NULL,
  highlight_color = NULL,
  highlight_radius = 1.7,
  center_label = NULL,
  center_label_size = 5,
  center_label_color = "grey20",
  title = NULL,
  subtitle = NULL,
  font_family = "sans",
  base_size = 12,
  bg_color = "white"
) {
  x <- rlang::enquo(x)
  label <- rlang::enquo(label)
  facet <- rlang::enquo(facet)
  label_position <- match.arg(label_position)
  pie_direction <- match.arg(pie_direction)
  sort <- match.arg(sort)
  coord_direction <- ifelse(pie_direction == "desc", 1, -1)
  has_label <- !rlang::quo_is_null(label)
  has_facet <- !rlang::quo_is_null(facet)

  .check_data_frame(data)

  if (!is.null(ncol)) {
    if (
      !is.numeric(ncol) ||
        length(ncol) != 1 ||
        is.na(ncol) ||
        !is.finite(ncol) ||
        ncol < 1 ||
        ncol != floor(ncol)
    ) {
      stop("`ncol` must be NULL or a single positive integer.", call. = FALSE)
    }
    ncol <- as.integer(ncol)
  }

  .check_single_logical(legend, "legend")
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

  if (
    length(label_value_type) < 1 ||
      !all(label_value_type %in% c("percent", "value"))
  ) {
    stop(
      "`label_value_type` must contain only \"percent\" and/or \"value\".",
      call. = FALSE
    )
  }
  label_value_type <- unique(label_value_type)

  if (
    !is.numeric(inner_radius) ||
      length(inner_radius) != 1 ||
      is.na(inner_radius) ||
      inner_radius < 0 ||
      inner_radius >= 1.5
  ) {
    stop(
      "`inner_radius` must be a single number greater than or equal to 0 and smaller than 1.5.",
      call. = FALSE
    )
  }

  .check_single_logical(outside_label_repel, "outside_label_repel")
  .check_single_logical(label_value, "label_value")
  .check_digits(label_value_digits, "label_value_digits")
  if (
    !is.character(label_value_suffix) ||
      length(label_value_suffix) < 1 ||
      anyNA(label_value_suffix)
  ) {
    stop(
      "`label_value_suffix` must be a character vector without missing values.",
      call. = FALSE
    )
  }
  if (length(label_value_suffix) > 1 && is.null(names(label_value_suffix))) {
    stop(
      "`label_value_suffix` must be length 1 or a named character vector.",
      call. = FALSE
    )
  }
  if (
    !is.character(label_value_sep) ||
      length(label_value_sep) != 1 ||
      is.na(label_value_sep)
  ) {
    stop("`label_value_sep` must be a single string.", call. = FALSE)
  }
  if (
    !is.numeric(outside_label_min_gap) ||
      length(outside_label_min_gap) != 1 ||
      is.na(outside_label_min_gap) ||
      outside_label_min_gap < 0
  ) {
    stop(
      "`outside_label_min_gap` must be a single non-negative number.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(outside_line_length) ||
      length(outside_line_length) != 1 ||
      is.na(outside_line_length) ||
      outside_line_length < 0
  ) {
    stop(
      "`outside_line_length` must be a single non-negative number.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(outside_label_gap) ||
      length(outside_label_gap) != 1 ||
      is.na(outside_label_gap) ||
      outside_label_gap < 0
  ) {
    stop(
      "`outside_label_gap` must be a single non-negative number.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(outside_line_width) ||
      length(outside_line_width) != 1 ||
      is.na(outside_line_width) ||
      outside_line_width < 0
  ) {
    stop(
      "`outside_line_width` must be a single non-negative number.",
      call. = FALSE
    )
  }

  if (rlang::quo_is_missing(x)) {
    stop("`x` must be supplied.", call. = FALSE)
  }

  x_values <- dplyr::pull(data, !!x)
  if (!is.numeric(x_values)) {
    stop("`x` must be a numeric column.", call. = FALSE)
  }
  if (anyNA(x_values)) {
    stop("`x` must not contain missing values.", call. = FALSE)
  }
  if (any(!is.finite(x_values))) {
    stop("`x` must contain only finite values.", call. = FALSE)
  }
  if (any(x_values < 0)) {
    stop("`x` must contain non-negative values.", call. = FALSE)
  }
  if (sum(x_values) <= 0) {
    stop("The sum of `x` must be greater than 0.", call. = FALSE)
  }

  other_group_values <- other_group

  if (!is.null(top_n)) {
    if (!has_label) {
      stop("`top_n` can only be used when `label` is supplied.", call. = FALSE)
    }
    if (
      !is.numeric(top_n) ||
        length(top_n) != 1 ||
        is.na(top_n) ||
        !is.finite(top_n) ||
        top_n < 1 ||
        top_n != floor(top_n)
    ) {
      stop("`top_n` must be a single positive integer.", call. = FALSE)
    }
    if (
      !is.character(other_label) ||
        length(other_label) != 1 ||
        is.na(other_label) ||
        !nzchar(other_label)
    ) {
      stop("`other_label` must be a single non-empty string.", call. = FALSE)
    }

    top_n <- as.integer(top_n)
    data <- data |>
      dplyr::mutate(!!label := as.character(!!label))

    x_name <- rlang::as_name(x)
    label_name <- rlang::as_name(label)

    apply_top_n <- function(.data) {
      label_values_top <- .data[[label_name]]
      x_values_top <- .data[[x_name]]
      is_existing_other <- if (is.null(other_group)) {
        rep(FALSE, length(label_values_top))
      } else {
        label_values_top %in% as.character(other_group)
      }

      candidate_index <- which(!is_existing_other)
      top_index_desc <- candidate_index[order(
        x_values_top[candidate_index],
        decreasing = TRUE
      )]
      top_index <- top_index_desc[seq_len(min(top_n, length(top_index_desc)))]
      merge_index <- setdiff(seq_len(nrow(.data)), top_index)

      if (length(merge_index) == 0L) {
        return(.data)
      }

      selected_data <- .data[sort(top_index), , drop = FALSE]
      other_row <- .data[merge_index[1L], , drop = FALSE]
      other_row[[x_name]] <- sum(x_values_top[merge_index])
      other_row[[label_name]] <- other_label

      dplyr::bind_rows(selected_data, other_row)
    }

    if (has_facet) {
      data <- data |>
        dplyr::group_by(!!facet) |>
        dplyr::group_modify(~ apply_top_n(.x)) |>
        dplyr::ungroup()
    } else {
      data <- apply_top_n(data)
    }

    if (nrow(data) != length(x_values)) {
      x_values <- dplyr::pull(data, !!x)
      other_group_values <- unique(c(as.character(other_group), other_label))
    }
  }

  if (has_label) {
    label_values_raw <- dplyr::pull(data, !!label)
    label_levels_original <- if (is.factor(label_values_raw)) {
      levels(droplevels(label_values_raw))
    } else {
      unique(as.character(label_values_raw))
    }
  }

  if (sort == "desc") {
    data <- data |>
      dplyr::arrange(dplyr::desc(!!x))
  } else if (sort == "asc") {
    data <- data |>
      dplyr::arrange(!!x)
  }

  if (!is.null(other_group_values)) {
    if (!has_label) {
      stop(
        "`other_group` can only be used when `label` is supplied.",
        call. = FALSE
      )
    }

    data <- data |>
      dplyr::mutate(
        .ggcanreg_pie_is_other = as.character(!!label) %in%
          as.character(other_group_values)
      )

    if (sort == "asc") {
      data <- data |>
        dplyr::arrange(!.data$.ggcanreg_pie_is_other) |>
        dplyr::select(-".ggcanreg_pie_is_other")
    } else {
      data <- data |>
        dplyr::arrange(.data$.ggcanreg_pie_is_other) |>
        dplyr::select(-".ggcanreg_pie_is_other")
    }
  }

  # Convert 'label' to factor to ensure discrete categories. When no label is
  # supplied, use an internal row-level label so calls like gg_pie(data, x)
  # still draw each slice with a separate color.
  if (has_label) {
    label_values_raw <- dplyr::pull(data, !!label)
    label_levels <- label_levels_original

    if (!is.null(other_group_values)) {
      other_levels <- label_levels[
        label_levels %in%
          as.character(other_group_values)
      ]
      label_levels <- c(setdiff(label_levels, other_levels), other_levels)
    }

    data <- data |>
      dplyr::mutate(!!label := factor(!!label, levels = label_levels))
    fill_var <- label
    fill_levels <- label_levels
    num_labels <- length(fill_levels)
    label_values <- as.character(dplyr::pull(data, !!label))
  } else {
    fill_var <- rlang::sym(".ggcanreg_pie_label")
    data <- data |>
      dplyr::mutate(.ggcanreg_pie_label = factor(seq_len(dplyr::n())))
    fill_levels <- levels(dplyr::pull(data, !!fill_var))
    num_labels <- nrow(data)
    label_values <- as.character(dplyr::pull(data, !!x))
  }

  if (is.null(highlight)) {
    is_highlight <- rep(FALSE, nrow(data))
  } else if (has_label) {
    is_highlight <- label_values %in% as.character(highlight)
  } else {
    is_highlight <- seq_len(nrow(data)) %in% highlight
  }

  if (has_facet) {
    data <- data |>
      dplyr::group_by(!!facet) |>
      dplyr::mutate(
        .ggcanreg_pie_plot_value = !!x / sum(!!x),
        .ggcanreg_pie_label_percent = .data$.ggcanreg_pie_plot_value * 100
      ) |>
      dplyr::ungroup()
  } else {
    data <- data |>
      dplyr::mutate(
        .ggcanreg_pie_plot_value = !!x,
        .ggcanreg_pie_label_percent = !!x / sum(!!x) * 100
      )
  }

  highlight_offset <- highlight_radius - 1.5
  label_text <- if (isTRUE(label_value) && has_label) {
    label_value_raw <- dplyr::pull(data, !!x)
    label_percent_raw <- dplyr::pull(data, ".ggcanreg_pie_label_percent")
    label_value_parts <- lapply(label_value_type, function(value_type) {
      suffix <- if (length(label_value_suffix) == 1) {
        label_value_suffix
      } else if (value_type %in% names(label_value_suffix)) {
        label_value_suffix[[value_type]]
      } else {
        ""
      }

      if (value_type == "percent") {
        scales::number(
          label_percent_raw,
          accuracy = 10^-label_value_digits,
          suffix = suffix
        )
      } else {
        paste0(as.character(label_value_raw), suffix)
      }
    })
    label_value_line <- do.call(
      paste,
      c(label_value_parts, sep = label_value_sep)
    )
    paste0(
      label_values,
      "\n",
      label_value_line
    )
  } else {
    label_values
  }

  if (has_facet) {
    data <- data |>
      dplyr::mutate(
        .ggcanreg_pie_label_text = label_text,
        .ggcanreg_pie_is_highlight = is_highlight
      )

    data <- data |>
      dplyr::group_by(!!facet) |>
      dplyr::mutate(
        .ggcanreg_pie_xmax = ifelse(
          .data$.ggcanreg_pie_is_highlight,
          highlight_radius,
          1.5
        ),
        .ggcanreg_pie_label_radius = ifelse(
          .data$.ggcanreg_pie_is_highlight,
          label_radius + highlight_offset,
          label_radius
        ),
        .ggcanreg_pie_line_xend = .data$.ggcanreg_pie_xmax +
          outside_line_length,
        .ggcanreg_pie_outside_label_x = .data$.ggcanreg_pie_line_xend +
          outside_label_gap,
        .ggcanreg_pie_ymax = cumsum(.data$.ggcanreg_pie_plot_value),
        .ggcanreg_pie_ymin = dplyr::lag(.data$.ggcanreg_pie_ymax, default = 0),
        .ggcanreg_pie_ymid = (.data$.ggcanreg_pie_ymin +
          .data$.ggcanreg_pie_ymax) /
          2,
        .ggcanreg_pie_total = sum(.data$.ggcanreg_pie_plot_value),
        .ggcanreg_pie_label_angle_raw = 90 -
          coord_direction *
            360 *
            .data$.ggcanreg_pie_ymid /
            .data$.ggcanreg_pie_total,
        .ggcanreg_pie_label_angle = .data$.ggcanreg_pie_label_angle_raw,
        .ggcanreg_pie_label_angle = ((.data$.ggcanreg_pie_label_angle + 180) %%
          360) -
          180,
        .ggcanreg_pie_label_flipped = .data$.ggcanreg_pie_label_angle > 90 |
          .data$.ggcanreg_pie_label_angle < -90,
        .ggcanreg_pie_outside_hjust = ifelse(
          .data$.ggcanreg_pie_label_flipped,
          1,
          0
        ),
        .ggcanreg_pie_label_angle = ifelse(
          .data$.ggcanreg_pie_label_flipped &
            .data$.ggcanreg_pie_label_angle > 90,
          .data$.ggcanreg_pie_label_angle - 180,
          ifelse(
            .data$.ggcanreg_pie_label_flipped &
              .data$.ggcanreg_pie_label_angle < -90,
            .data$.ggcanreg_pie_label_angle + 180,
            .data$.ggcanreg_pie_label_angle
          )
        ),
        .ggcanreg_pie_label_hjust = if (is.null(label_hjust)) {
          ifelse(.data$.ggcanreg_pie_label_flipped, 0, 1)
        } else {
          ifelse(
            .data$.ggcanreg_pie_label_flipped,
            label_hjust,
            1 - label_hjust
          )
        },
        .ggcanreg_pie_outside_label_y = .data$.ggcanreg_pie_ymid,
        .ggcanreg_pie_line_yend = .data$.ggcanreg_pie_ymid
      ) |>
      dplyr::ungroup()
  } else {
    data <- data |>
      dplyr::mutate(
        .ggcanreg_pie_label_text = label_text,
        .ggcanreg_pie_is_highlight = is_highlight,
        .ggcanreg_pie_xmax = ifelse(
          .data$.ggcanreg_pie_is_highlight,
          highlight_radius,
          1.5
        ),
        .ggcanreg_pie_label_radius = ifelse(
          .data$.ggcanreg_pie_is_highlight,
          label_radius + highlight_offset,
          label_radius
        ),
        .ggcanreg_pie_line_xend = .data$.ggcanreg_pie_xmax +
          outside_line_length,
        .ggcanreg_pie_outside_label_x = .data$.ggcanreg_pie_line_xend +
          outside_label_gap,
        .ggcanreg_pie_ymax = cumsum(.data$.ggcanreg_pie_plot_value),
        .ggcanreg_pie_ymin = dplyr::lag(.data$.ggcanreg_pie_ymax, default = 0),
        .ggcanreg_pie_ymid = (.data$.ggcanreg_pie_ymin +
          .data$.ggcanreg_pie_ymax) /
          2,
        .ggcanreg_pie_total = sum(.data$.ggcanreg_pie_plot_value),
        .ggcanreg_pie_label_angle_raw = 90 -
          coord_direction *
            360 *
            .data$.ggcanreg_pie_ymid /
            .data$.ggcanreg_pie_total,
        .ggcanreg_pie_label_angle = .data$.ggcanreg_pie_label_angle_raw,
        .ggcanreg_pie_label_angle = ((.data$.ggcanreg_pie_label_angle + 180) %%
          360) -
          180,
        .ggcanreg_pie_label_flipped = .data$.ggcanreg_pie_label_angle > 90 |
          .data$.ggcanreg_pie_label_angle < -90,
        .ggcanreg_pie_outside_hjust = ifelse(
          .data$.ggcanreg_pie_label_flipped,
          1,
          0
        ),
        .ggcanreg_pie_label_angle = ifelse(
          .data$.ggcanreg_pie_label_flipped &
            .data$.ggcanreg_pie_label_angle > 90,
          .data$.ggcanreg_pie_label_angle - 180,
          ifelse(
            .data$.ggcanreg_pie_label_flipped &
              .data$.ggcanreg_pie_label_angle < -90,
            .data$.ggcanreg_pie_label_angle + 180,
            .data$.ggcanreg_pie_label_angle
          )
        ),
        .ggcanreg_pie_label_hjust = if (is.null(label_hjust)) {
          ifelse(.data$.ggcanreg_pie_label_flipped, 0, 1)
        } else {
          ifelse(
            .data$.ggcanreg_pie_label_flipped,
            label_hjust,
            1 - label_hjust
          )
        },
        .ggcanreg_pie_outside_label_y = .data$.ggcanreg_pie_ymid,
        .ggcanreg_pie_line_yend = .data$.ggcanreg_pie_ymid
      )
  }

  if (label_position == "outside" && isTRUE(outside_label_repel)) {
    if (has_facet) {
      data <- data |>
        dplyr::group_by(!!facet) |>
        dplyr::mutate(
          .ggcanreg_pie_outside_label_y = .adjust_pie_label_positions(
            .data$.ggcanreg_pie_outside_label_y,
            total = dplyr::first(.data$.ggcanreg_pie_total),
            min_gap = outside_label_min_gap
          ),
          .ggcanreg_pie_line_yend = .data$.ggcanreg_pie_outside_label_y
        ) |>
        dplyr::ungroup()
    } else {
      adjusted_y <- .adjust_pie_label_positions(
        data$.ggcanreg_pie_outside_label_y,
        total = data$.ggcanreg_pie_total[[1]],
        min_gap = outside_label_min_gap
      )
      data <- data |>
        dplyr::mutate(
          .ggcanreg_pie_outside_label_y = adjusted_y,
          .ggcanreg_pie_line_yend = adjusted_y
        )
    }
  }

  # Ensure there are enough colors in the palette
  if (is.null(colors)) {
    colors <- tryCatch(
      canreg_palette(palette, n = num_labels),
      error = function(e) {
        colorspace::sequential_hcl(
          n = max(num_labels, 3L),
          palette = palette
        )[seq_len(num_labels)]
      }
    )
    names(colors) <- fill_levels
  } else {
    color_names <- names(colors)
    has_color_names <- !is.null(color_names) && any(nzchar(color_names))

    if (has_color_names) {
      missing_colors <- setdiff(fill_levels, color_names)
      if (length(missing_colors) > 0L) {
        stop(
          "Named `colors` must include one color for each pie slice/group.",
          call. = FALSE
        )
      }
      colors <- colors[fill_levels]
    } else if (length(colors) < num_labels) {
      stop(
        "`colors` must contain at least as many colors as pie slices/groups.",
        call. = FALSE
      )
    } else {
      colors <- colors[seq_len(num_labels)]
      names(colors) <- fill_levels
    }
  }

  if (!is.null(highlight_color) && any(is_highlight)) {
    fill_values <- dplyr::pull(data, !!fill_var)
    highlighted_levels <- unique(as.character(fill_values[is_highlight]))

    if (length(highlight_color) == 1L) {
      highlight_color <- rep(highlight_color, length(highlighted_levels))
    } else if (length(highlight_color) != length(highlighted_levels)) {
      stop(
        "`highlight_color` must be length 1 or have the same length as highlighted slice groups.",
        call. = FALSE
      )
    }

    if (!is.null(names(colors)) && all(highlighted_levels %in% names(colors))) {
      colors[highlighted_levels] <- highlight_color
    } else {
      colors[match(highlighted_levels, fill_levels)] <- highlight_color
    }
  }

  # Generate the pie chart. Use explicit rectangle boundaries instead of
  # ggplot2's default stacking so guide lines and slices share the same
  # computed ymin/ymax positions.
  p <- ggplot(data) +
    geom_rect(
      aes(
        xmin = inner_radius,
        xmax = .data$.ggcanreg_pie_xmax,
        ymin = .data$.ggcanreg_pie_ymin,
        ymax = .data$.ggcanreg_pie_ymax,
        fill = !!fill_var
      ),
      color = slice_gap_color,
      linewidth = slice_gap_width
    ) +
    scale_fill_manual(values = colors) +
    .journal_chart_theme(
      base_size = base_size,
      font_family = font_family,
      legend_position = legend_position,
      grid = "none",
      subtitle = subtitle,
      bg_color = bg_color
    ) +
    .legend_theme(
      legend = legend,
      legend_title = legend_title,
      options = legend_options,
      show = has_label
    ) +
    theme(
      axis.title = element_blank(),
      axis.title.x.bottom = element_blank(),
      axis.title.x.top = element_blank(),
      axis.title.y.left = element_blank(),
      axis.title.y.right = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.text.x = element_blank(),
      axis.text.x.bottom = element_blank(),
      axis.text.x.top = element_blank(),
      axis.text.y = element_blank(),
      axis.text.y.left = element_blank(),
      axis.text.y.right = element_blank(),
      axis.text.theta = element_blank(),
      axis.text.r = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      plot.title = element_text(hjust = 0),
      plot.subtitle = element_text(hjust = 0)
    )

  legend_guides <- .legend_guides("fill", "legend", legend_options)
  if (!is.null(legend_guides)) {
    p <- p + legend_guides
  }

  p <- p +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      fill = legend_title
    )

  pie_radius <- max(data$.ggcanreg_pie_xmax, na.rm = TRUE)
  x_upper_limit <- if (label_position == "outside") {
    max(2, pie_radius + 0.45)
  } else {
    pie_radius
  }

  p <- p +
    scale_x_continuous(
      limits = c(0, x_upper_limit),
      expand = c(0, 0)
    )

  if (has_facet) {
    facet_name <- rlang::as_name(facet)
    facet_values_raw <- dplyr::pull(data, !!facet)
    facet_values <- if (is.factor(facet_values_raw)) {
      levels(droplevels(facet_values_raw))
    } else {
      unique(facet_values_raw)
    }
    n_facets <- length(facet_values)

    center_label_values <- if (is.null(center_label)) {
      as.character(facet_values)
    } else if (length(center_label) == 1L) {
      rep(as.character(center_label), n_facets)
    } else if (length(center_label) == n_facets) {
      as.character(center_label)
    } else {
      stop(
        "When `facet` is supplied, `center_label` must be length 1 or have the same length as the number of facet values.",
        call. = FALSE
      )
    }

    center_label_data <- data.frame(
      .ggcanreg_pie_center_label = center_label_values,
      stringsAsFactors = FALSE
    )
    center_label_data[[facet_name]] <- if (is.factor(facet_values_raw)) {
      factor(facet_values, levels = levels(facet_values_raw))
    } else {
      facet_values
    }

    p <- p +
      geom_text(
        data = center_label_data,
        aes(x = 0, y = 0, label = .data$.ggcanreg_pie_center_label),
        inherit.aes = FALSE,
        size = center_label_size,
        color = center_label_color,
        family = font_family
      )
  } else if (!is.null(center_label)) {
    if (length(center_label) != 1L) {
      stop(
        "`center_label` must be length 1 when `facet` is not supplied.",
        call. = FALSE
      )
    }

    p <- p +
      annotate(
        "text",
        x = 0,
        y = 0,
        label = as.character(center_label),
        size = center_label_size,
        color = center_label_color,
        family = font_family
      )
  }

  if (label_position == "inside") {
    p <- p +
      geom_text(
        aes(
          x = .data$.ggcanreg_pie_label_radius,
          y = .data$.ggcanreg_pie_ymid,
          label = .data$.ggcanreg_pie_label_text,
          angle = .data$.ggcanreg_pie_label_angle,
          hjust = .data$.ggcanreg_pie_label_hjust
        ),
        inherit.aes = FALSE,
        color = label_color,
        size = label_size,
        family = font_family
      )
  } else if (label_position == "outside") {
    p <- p +
      geom_segment(
        aes(
          x = .data$.ggcanreg_pie_xmax,
          xend = .data$.ggcanreg_pie_line_xend,
          y = .data$.ggcanreg_pie_ymid,
          yend = .data$.ggcanreg_pie_line_yend
        ),
        inherit.aes = FALSE,
        color = line_color,
        linewidth = outside_line_width,
        linetype = outside_line_type
      ) +
      geom_text(
        aes(
          x = .data$.ggcanreg_pie_outside_label_x,
          y = .data$.ggcanreg_pie_outside_label_y,
          label = .data$.ggcanreg_pie_label_text,
          hjust = .data$.ggcanreg_pie_outside_hjust
        ),
        inherit.aes = FALSE,
        color = label_color,
        size = label_size,
        family = font_family
      ) +
      guides(fill = "none")
  }

  if (!legend || !has_label) {
    p <- p + guides(fill = "none")
  }

  if (has_facet) {
    facet_theme <- theme(
      strip.text = element_blank(),
      strip.background = element_blank()
    )
    if (label_position == "inside") {
      facet_theme <- facet_theme +
        theme(
          panel.spacing = grid::unit(2, "pt"),
          plot.margin = ggplot2::margin(t = 4, r = 4, b = 4, l = 4)
        )
    }
    p <- p + facet_wrap(vars(!!facet), ncol = ncol) + facet_theme
  }

  p <- p +
    coord_polar(
      "y",
      start = 0,
      direction = coord_direction,
      clip = "off"
    )

  p
}


.adjust_pie_label_positions <- function(y, total, min_gap) {
  if (length(y) <= 1L || min_gap <= 0) {
    return(y)
  }

  gap <- total * min_gap
  order_index <- order(y)
  y_sorted <- y[order_index]

  for (i in seq_along(y_sorted)[-1L]) {
    min_y <- y_sorted[i - 1L] + gap
    if (y_sorted[i] < min_y) {
      y_sorted[i] <- min_y
    }
  }

  overflow <- max(y_sorted) - total
  if (overflow > 0) {
    y_sorted <- y_sorted - overflow
  }

  for (i in rev(seq_along(y_sorted)[-length(y_sorted)])) {
    max_y <- y_sorted[i + 1L] - gap
    if (y_sorted[i] > max_y) {
      y_sorted[i] <- max_y
    }
  }

  y_adjusted <- y
  y_adjusted[order_index] <- y_sorted
  y_adjusted
}
