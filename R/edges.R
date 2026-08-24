# Edge table schema -----------------------------------------------------------

#' An empty edge table matching the protocol schema
#'
#' @return A zero-row tibble with the protocol edge columns.
#' @keywords internal
empty_edge_tbl <- function() {
  tibble::tibble(
    from = character(), to = character(), weight = numeric(),
    layer = character(), directed = logical(), same_dept = logical(),
    same_location = logical(), same_legacy = logical(), snapshot = character()
  )
}

#' Coerce a data frame to the protocol edge schema
#'
#' Fills in any missing protocol column, fixes column types and orders the
#' protocol columns first. Extra columns are preserved after them.
#'
#' @param df A data frame of edges, or `NULL`.
#' @return A tibble matching the protocol edge schema.
#' @keywords internal
coerce_edge_schema <- function(df) {
  if (is.null(df) || !inherits(df, "data.frame")) {
    return(empty_edge_tbl())
  }
  out <- tibble::as_tibble(df)
  n <- nrow(out)

  if (!"from" %in% names(out))          out$from <- character(n)
  if (!"to" %in% names(out))            out$to <- character(n)
  if (!"weight" %in% names(out))        out$weight <- rep(NA_real_, n)
  if (!"layer" %in% names(out))         out$layer <- rep(NA_character_, n)
  if (!"directed" %in% names(out))      out$directed <- rep(FALSE, n)
  if (!"same_dept" %in% names(out))     out$same_dept <- rep(NA, n)
  if (!"same_location" %in% names(out)) out$same_location <- rep(NA, n)
  if (!"same_legacy" %in% names(out))   out$same_legacy <- rep(NA, n)
  if (!"snapshot" %in% names(out))      out$snapshot <- rep(NA_character_, n)

  out$from          <- as.character(out$from)
  out$to            <- as.character(out$to)
  out$weight        <- suppressWarnings(as.numeric(out$weight))
  out$layer         <- as.character(out$layer)
  out$directed      <- as.logical(out$directed)
  out$same_dept     <- as.logical(out$same_dept)
  out$same_location <- as.logical(out$same_location)
  out$same_legacy   <- as.logical(out$same_legacy)
  out$snapshot      <- as.character(out$snapshot)

  extra <- setdiff(names(out), EDGE_SCHEMA_COLS)
  out[, c(EDGE_SCHEMA_COLS, extra), drop = FALSE]
}

#' Row-bind edge tables under the protocol schema
#'
#' @param ... Edge tables.
#' @param .tables A list of edge tables, as an alternative to `...`.
#' @return A single tibble matching the protocol edge schema.
#' @keywords internal
bind_edge_tables <- function(..., .tables = NULL) {
  tables <- c(list(...), .tables)
  tables <- tables[!vapply(tables, is.null, logical(1))]
  if (length(tables) == 0) {
    return(empty_edge_tbl())
  }
  tables <- lapply(tables, coerce_edge_schema)
  tables <- tables[vapply(tables, nrow, integer(1)) > 0]
  if (length(tables) == 0) {
    return(empty_edge_tbl())
  }
  coerce_edge_schema(dplyr::bind_rows(tables))
}

#' Normalise undirected endpoints so that `from <= to`
#'
#' Both endpoints are computed from the *original* pair. Assigning `from` and
#' then deriving `to` from the updated value collapses every reversed pair
#' onto a self-loop, because `dplyr::mutate()` evaluates sequentially.
#'
#' @param ed An edge table.
#' @return `ed` with endpoints ordered.
#' @keywords internal
order_endpoints <- function(ed) {
  lo <- pmin(ed$from, ed$to)
  hi <- pmax(ed$from, ed$to)
  ed$from <- lo
  ed$to <- hi
  ed
}

#' Drop duplicate ties within a layer
#'
#' Directed layers are de-duplicated on the ordered pair; undirected layers on
#' the unordered pair. Self-loops are removed.
#'
#' @param df An edge table.
#' @return `df` without duplicate or self-referential ties.
#' @keywords internal
compact_edges <- function(df) {
  df <- coerce_edge_schema(df)
  if (nrow(df) == 0) {
    return(df)
  }
  df <- df[df$from != df$to, , drop = FALSE]
  if (nrow(df) == 0) {
    return(df)
  }
  df$directed <- df$layer %in% DIRECTED_LAYERS

  lo <- pmin(df$from, df$to)
  hi <- pmax(df$from, df$to)
  key <- ifelse(
    df$directed,
    paste(df$from, df$to, df$layer, df$snapshot, sep = "\r"),
    paste(lo, hi, df$layer, df$snapshot, sep = "\r")
  )
  df[!duplicated(key), , drop = FALSE]
}

#' Collapse a multiplex edge table onto a simple weighted graph projection
#'
#' Ties between the same pair across layers are summed into a single weight.
#'
#' @param edges An edge table.
#' @param directed Whether to preserve tie direction.
#' @return A tibble with `from`, `to` and `weight`.
#' @keywords internal
collapse_edge_projection <- function(edges, directed = FALSE) {
  ed <- coerce_edge_schema(edges)
  if (nrow(ed) == 0) {
    return(tibble::tibble(from = character(), to = character(), weight = numeric()))
  }
  ed$weight[is.na(ed$weight)] <- 1
  if (!directed) {
    ed <- order_endpoints(ed)
  }
  ed <- ed[ed$from != ed$to, , drop = FALSE]
  if (nrow(ed) == 0) {
    return(tibble::tibble(from = character(), to = character(), weight = numeric()))
  }
  dplyr::summarise(
    dplyr::group_by(ed, .data$from, .data$to),
    weight = sum(.data$weight, na.rm = TRUE),
    .groups = "drop"
  )
}
