# Internal argument validation helpers -------------------------------------

.check_data_frame <- function(x, arg = "data") {
  if (!inherits(x, "data.frame")) {
    stop(sprintf("`%s` must be a data frame.", arg), call. = FALSE)
  }

  invisible(x)
}

.check_single_logical <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop(sprintf("`%s` must be TRUE or FALSE.", arg), call. = FALSE)
  }

  invisible(x)
}

.check_single_character <- function(x, arg) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    stop(sprintf("`%s` must be a single character value.", arg), call. = FALSE)
  }

  invisible(x)
}

.check_single_number <- function(x, arg, min = NULL) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x)) {
    stop(sprintf("`%s` must be a single number.", arg), call. = FALSE)
  }
  if (!is.null(min) && x <= min) {
    stop(sprintf("`%s` must be greater than %s.", arg, min), call. = FALSE)
  }

  invisible(x)
}

.check_digits <- function(digits, arg = "digits") {
  if (
    !is.numeric(digits) || length(digits) != 1 || is.na(digits) || digits < 0
  ) {
    stop(
      sprintf("`%s` must be a single non-negative number.", arg),
      call. = FALSE
    )
  }

  invisible(digits)
}

.check_data_variable <- function(data, var, arg, optional = FALSE) {
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
