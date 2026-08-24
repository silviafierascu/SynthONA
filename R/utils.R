# Small internal helpers ------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

#' Rescale a numeric vector to the unit interval
#'
#' Constant vectors map to 0.5 rather than to `NaN`, so downstream composite
#' scores stay defined when a metric has no variation (for example betweenness
#' on an empty layer).
#'
#' @param x Numeric vector.
#' @return Numeric vector on `[0, 1]`.
#' @keywords internal
scale01 <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) {
    return(rep(0.5, length(x)))
  }
  (x - rng[1]) / (rng[2] - rng[1])
}

soft_clip <- function(x, lo = 0, hi = 1) pmin(hi, pmax(lo, x))

normalise_prob <- function(x) {
  x <- ifelse(is.na(x) | x < 0, 0, x)
  s <- sum(x)
  if (s == 0) rep(1 / length(x), length(x)) else x / s
}

safe_sample <- function(x, size = 1L, replace = FALSE, prob = NULL) {
  if (length(x) == 0) {
    return(x[0])
  }
  size <- min(size, if (replace) size else length(x))
  if (size <= 0) {
    return(x[0])
  }
  x[sample.int(length(x), size = size, replace = replace, prob = prob)]
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

#' Resolve an output directory
#'
#' Relative paths are resolved against the session working directory. The
#' package never writes outside the path the caller supplies, and the default
#' used throughout the API is a session temporary directory.
#'
#' @param output_root Directory path.
#' @return Normalised absolute path.
#' @keywords internal
resolve_output_root <- function(output_root = default_output_dir()) {
  output_root <- output_root %||% default_output_dir()
  if (!grepl("^(?:[A-Za-z]:[/\\]|/|~)", output_root)) {
    output_root <- file.path(getwd(), output_root)
  }
  normalizePath(output_root, winslash = "/", mustWork = FALSE)
}

#' Default output directory
#'
#' A session temporary directory. Exports never touch the user's file space
#' unless an explicit `output_root` is supplied.
#'
#' @return Path to the default output directory.
#' @export
default_output_dir <- function() {
  file.path(tempdir(), "synthona")
}

write_json_pretty <- function(x, path) {
  jsonlite::write_json(
    x,
    path = path, auto_unbox = TRUE, pretty = TRUE,
    dataframe = "rows", null = "null", digits = 6
  )
}

write_table_csv <- function(x, path) {
  utils::write.csv(as.data.frame(x), file = path, row.names = FALSE, na = "")
  invisible(path)
}
