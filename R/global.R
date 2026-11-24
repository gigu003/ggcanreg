if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "adcode",
    "name",
    "value",
    "cancer",
    "change",
    "way",
    "y_label",
    "label",
    ".cumsum"
  ))
}


reorder_ <- function(x, by, group, sep = "___") {
  x_ <- paste(x, group, sep = sep)
  reorder(x_, by)
}

reorder_dir <- function(x, by, sort_direction = "asc") {
  if (identical(sort_direction, "desc")) {
    stats::reorder(x, -by)
  } else {
    stats::reorder(x, by)
  }
}

reorder_dir_group <- function(
  x,
  by,
  group,
  sort_direction = "asc",
  sep = "___"
) {
  x_ <- paste(x, group, sep = sep)
  reorder_dir(x_, by, sort_direction = sort_direction)
}

normalize_sort_direction <- function(sort = TRUE) {
  if (is.logical(sort) && length(sort) == 1 && !is.na(sort)) {
    if (sort) {
      return("asc")
    }
    return(FALSE)
  }

  if (is.character(sort) && length(sort) == 1 && !is.na(sort)) {
    sort <- tolower(sort)
    if (sort %in% c("asc", "ascending")) {
      return("asc")
    }
    if (sort %in% c("desc", "descending")) return("desc")
  }

  stop("`sort` must be TRUE, FALSE, 'asc', or 'desc'.", call. = FALSE)
}

pre_process_data <- function(
  data,
  x,
  y,
  group = NULL,
  group_ref = NULL,
  rev_group = TRUE,
  facet = NULL,
  highlight = NULL,
  highlight_color = "#F18222",
  bar_color = "#01847C",
  sort = TRUE,
  top_n = NULL
) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  sort_direction <- normalize_sort_direction(sort)
  should_sort <- !identical(sort_direction, FALSE)
  group_var <- rlang::enquo(group)
  has_group <- !rlang::quo_is_null(group_var)
  facet_var <- rlang::enquo(facet)
  has_facet <- !rlang::quo_is_null(facet_var)
  # y 缺失时计算计数
  if (rlang::quo_is_missing(y_var)) {
    count_vars <- list(x_var)
    if (has_group) {
      count_vars <- c(count_vars, list(group_var))
    }
    if (has_facet) {
      count_vars <- c(count_vars, list(facet_var))
    }
    data <- dplyr::count(data, !!!count_vars)
    y_var <- rlang::sym("n")
  }

  # 高亮
  if (!is.null(highlight)) {
    data <- data |>
      dplyr::mutate(
        .color = ifelse(!!x_var %in% highlight, highlight_color, bar_color)
      )
  } else {
    data$.color <- bar_color
  }

  if (has_group && should_sort) {
    ref_levels <- get_levels(
      data,
      !!x_var,
      !!y_var,
      !!group_var,
      group_ref = group_ref,
      sort_direction = sort_direction
    )
    ref_values <- dplyr::pull(data, !!group_var) |> unique()
    if (rev_group) {
      ref_values <- rev(ref_values)
    }
    data <- dplyr::mutate(
      data,
      !!x_var := factor(!!x_var, levels = ref_levels),
      !!group_var := factor(!!group_var, levels = ref_values)
    )
  } else if (has_group) {
    ref_levels <- unique(dplyr::pull(data, !!x_var))
    ref_values <- dplyr::pull(data, !!group_var) |> unique()
    if (rev_group) {
      ref_values <- rev(ref_values)
    }
    data <- dplyr::mutate(
      data,
      !!x_var := factor(!!x_var, levels = ref_levels),
      !!group_var := factor(!!group_var, levels = ref_values)
    )
  } else if (should_sort) {
    data <- dplyr::mutate(
      data,
      !!x_var := reorder_dir(!!x_var, !!y_var, sort_direction = sort_direction)
    )
  }

  if (!is.null(top_n)) {
    if (has_facet) {
      if (has_group) {
        selected_group <- resolve_group_ref(data, !!group_var, group_ref)
        top_x <- data |>
          dplyr::filter(!!group_var == selected_group) |>
          dplyr::group_by(!!facet_var) |>
          dplyr::slice_max(order_by = !!y_var, n = top_n, with_ties = FALSE) |>
          dplyr::ungroup() |>
          dplyr::select(!!facet_var, !!x_var)
        join_vars <- rlang::set_names(
          c(rlang::as_name(facet_var), rlang::as_name(x_var))
        )
        data <- dplyr::semi_join(data, top_x, by = join_vars)
      } else {
        data <- dplyr::group_by(data, !!facet_var) |>
          dplyr::slice_max(order_by = !!y_var, n = top_n, with_ties = FALSE) |>
          dplyr::ungroup()
      }
    } else {
      if (has_group) {
        keep_levels <- if (identical(sort_direction, "asc")) {
          utils::tail(ref_levels, top_n)
        } else {
          utils::head(ref_levels, top_n)
        }
        data <- dplyr::filter(data, !!x_var %in% keep_levels)
      } else {
        data <- dplyr::slice_max(
          data,
          order_by = !!y_var,
          n = top_n,
          with_ties = FALSE
        )
      }
    }
  }

  if (has_facet) {
    if (has_group) {
      data <- dplyr::mutate(
        data,
        !!x_var := reorder_dir_group(
          !!x_var,
          !!y_var,
          !!facet_var,
          sort_direction = if (should_sort) sort_direction else "asc"
        )
      )
      if (should_sort && identical(sort_direction, "desc")) {
        data <- dplyr::arrange(data, !!facet_var, dplyr::desc(!!y_var))
      } else {
        data <- dplyr::arrange(data, !!facet_var, !!y_var)
      }
    } else {
      data <- dplyr::mutate(
        data,
        !!x_var := reorder_dir_group(
          !!x_var,
          !!y_var,
          !!facet_var,
          sort_direction = if (should_sort) sort_direction else "asc"
        )
      )
      if (should_sort && identical(sort_direction, "desc")) {
        data <- dplyr::arrange(data, !!facet_var, dplyr::desc(!!y_var))
      } else {
        data <- dplyr::arrange(data, !!facet_var, !!y_var)
      }
    }
  }

  return(data)
}


get_levels <- function(
  data,
  x,
  y,
  group = NULL,
  group_ref = "first",
  sort_direction = "asc"
) {
  x_var <- rlang::enquo(x)
  y_var <- rlang::enquo(y)
  group_var <- rlang::enquo(group)
  group_ref <- resolve_group_ref(data, !!group_var, group_ref)

  if (identical(sort_direction, "desc")) {
    data |>
      dplyr::filter(!!group_var == group_ref) |>
      dplyr::arrange(dplyr::desc(!!y_var)) |>
      dplyr::pull(!!x_var) |>
      unique()
  } else {
    data |>
      dplyr::filter(!!group_var == group_ref) |>
      dplyr::arrange(!!y_var) |>
      dplyr::pull(!!x_var) |>
      unique()
  }
}


resolve_group_ref <- function(data, group, group_ref = "first") {
  group_var <- rlang::enquo(group)
  ref_values <- dplyr::pull(data, !!group_var)
  ref_values <- unique(as.character(ref_values))

  if (is.null(group_ref)) {
    group_ref <- "first"
  }
  if (!group_ref %in% c(ref_values, "first", "last")) {
    group_ref <- "first"
  }
  if (group_ref == "first") {
    group_ref <- ref_values[1]
  } else if (group_ref == "last") {
    group_ref <- ref_values[length(ref_values)]
  }

  group_ref
}
