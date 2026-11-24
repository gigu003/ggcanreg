# Internal legend helpers -------------------------------------------------

.validate_legend_position <- function(legend_position) {
  valid_legend_position <-
    (is.character(legend_position) &&
      length(legend_position) == 1 &&
      !is.na(legend_position)) ||
    (is.numeric(legend_position) &&
      length(legend_position) == 2 &&
      !anyNA(legend_position))

  if (!valid_legend_position) {
    stop(
      "`legend_position` must be a single character value or a numeric vector of length 2.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.validate_positive_integer <- function(x, arg) {
  if (is.null(x)) {
    return(invisible(TRUE))
  }

  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x <= 0 || x != floor(x)) {
    stop(
      sprintf("`%s` must be NULL or a single positive integer.", arg),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.validate_positive_number <- function(x, arg) {
  if (is.null(x)) {
    return(invisible(TRUE))
  }

  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x <= 0) {
    stop(
      sprintf("`%s` must be NULL or a single positive number.", arg),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.as_legend_unit <- function(x, arg) {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "unit")) {
    return(x)
  }
  if (is.numeric(x) && length(x) == 1 && !is.na(x) && x > 0) {
    return(grid::unit(x, "lines"))
  }

  stop(
    sprintf(
      "`%s` must be NULL, a grid::unit() object, or a single positive number.",
      arg
    ),
    call. = FALSE
  )
}

.legend_options <- function(
  legend_position = "top",
  legend_direction = NULL,
  legend_nrow = NULL,
  legend_ncol = NULL,
  legend_title_size = NULL,
  legend_text_size = NULL,
  legend_key_width = NULL,
  legend_key_height = NULL,
  legend_size = NULL,
  legend_spacing = NULL
) {
  .validate_legend_position(legend_position)

  if (!is.null(legend_direction)) {
    legend_direction <- match.arg(legend_direction, c("horizontal", "vertical"))
  }

  .validate_positive_integer(legend_nrow, "legend_nrow")
  .validate_positive_integer(legend_ncol, "legend_ncol")
  .validate_positive_number(legend_title_size, "legend_title_size")
  .validate_positive_number(legend_text_size, "legend_text_size")
  .validate_positive_number(legend_size, "legend_size")

  if (
    !is.null(legend_spacing) &&
      (!is.numeric(legend_spacing) ||
        length(legend_spacing) != 1 ||
        is.na(legend_spacing) ||
        legend_spacing < 0)
  ) {
    stop(
      "`legend_spacing` must be a single non-negative number.",
      call. = FALSE
    )
  }

  if (is.null(legend_title_size) && !is.null(legend_size)) {
    legend_title_size <- legend_size
  }
  if (is.null(legend_text_size) && !is.null(legend_size)) {
    legend_text_size <- legend_size
  }
  list(
    position = legend_position,
    direction = legend_direction,
    nrow = legend_nrow,
    ncol = legend_ncol,
    title_size = legend_title_size,
    text_size = legend_text_size,
    key_width = .as_legend_unit(legend_key_width, "legend_key_width"),
    key_height = .as_legend_unit(legend_key_height, "legend_key_height"),
    spacing = legend_spacing
  )
}

.legend_theme <- function(
  legend = TRUE,
  legend_title = NULL,
  options,
  show = TRUE
) {
  ggplot2::theme(
    legend.position = if (legend && show) options$position else "none",
    legend.direction = options$direction,
    legend.title = if (is.null(legend_title)) {
      ggplot2::element_blank()
    } else {
      ggplot2::element_text(size = options$title_size)
    },
    legend.text = ggplot2::element_text(size = options$text_size),
    legend.key.width = options$key_width,
    legend.key.height = options$key_height,
    legend.key.spacing.x = if (is.null(options$spacing)) {
      NULL
    } else {
      grid::unit(options$spacing, "cm")
    },
    legend.key.spacing.y = if (is.null(options$spacing)) {
      NULL
    } else {
      grid::unit(options$spacing, "cm")
    },
    legend.background = ggplot2::element_rect(
      fill = "transparent",
      colour = NA
    ),
    legend.box.background = ggplot2::element_rect(
      fill = "transparent",
      colour = NA
    ),
    legend.key = ggplot2::element_rect(fill = "transparent", colour = NA)
  )
}

.legend_guides <- function(
  aesthetic = "fill",
  type = c("legend", "colorbar"),
  options,
  override.aes = NULL,
  reverse = FALSE,
  title_position = NULL
) {
  type <- match.arg(type)
  guide_args <- list()

  if (!is.null(options$direction)) {
    guide_args$direction <- options$direction
  }
  if (!is.null(title_position)) {
    guide_args$title.position <- title_position
  }
  if (type == "legend") {
    if (!is.null(options$nrow)) {
      guide_args$nrow <- options$nrow
    }
    if (!is.null(options$ncol)) {
      guide_args$ncol <- options$ncol
    }
    if (isTRUE(reverse)) {
      guide_args$reverse <- TRUE
    }
    if (!is.null(override.aes)) {
      guide_args$override.aes <- override.aes
    }
  }

  if (length(guide_args) == 0) {
    return(NULL)
  }

  guide <- if (type == "legend") {
    do.call(ggplot2::guide_legend, guide_args)
  } else {
    do.call(ggplot2::guide_colorbar, guide_args)
  }

  do.call(ggplot2::guides, stats::setNames(list(guide), aesthetic))
}
