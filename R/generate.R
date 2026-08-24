# Top-level generation --------------------------------------------------------

#' Generate a synthetic organisational network dataset
#'
#' Runs the full protocol for one parameter specification: draws a workforce,
#' builds the base structure, derives the relational layers, evolves any
#' requested snapshots, and records the ground truth.
#'
#' The returned object is the complete dataset. Applying survey measurement
#' error to it is a separate step, [synthona_observe()], so that the same true
#' network can be observed under many different survey designs.
#'
#' @param params A [synthona_params()] object, or a scenario identifier
#'   accepted by [synthona_scenario()].
#' @param non_human_actors Number of non-human actors (AI agents, bots, shared
#'   accounts) to add to the workforce.
#'
#' @return An object of class `synthona_dataset`, a list with `nodes`, `edges`,
#'   `truth` and `params`.
#' @export
#' @examples
#' d <- synthona_generate(synthona_params(n = 150, topology = "sbm"))
#' d
#' head(d$nodes[, c("person_id", "department", "role_bucket")])
#'
#' # The same specification always reproduces the same dataset
#' identical(d$edges, synthona_generate(d$params)$edges)
synthona_generate <- function(params, non_human_actors = 0L) {
  if (is.character(params)) {
    params <- synthona_scenario(params)
  }
  stopifnot(inherits(params, "synthona_params"))

  nodes <- generate_profiles(params)
  n_agents <- non_human_actors
  if (n_agents < 1) {
    n_agents <- params$extras$non_human_actors %||% 0L
  }
  if (n_agents > 0) {
    nodes <- add_non_human_actors(nodes, count = n_agents, params = params)
  }

  base <- generate_base_structure(nodes, params)
  edges <- generate_layers(base$graph, base$nodes, params)
  edges <- generate_snapshots(base$nodes, edges, params)

  # Ground truth is recorded on the first snapshot, which is the network as
  # generated before any evolution or measurement error.
  first_snapshot <- params$snapshots[1]
  baseline_edges <- edges[edges$snapshot == first_snapshot, , drop = FALSE]
  truth <- record_truth(base$nodes, baseline_edges, base$truth, params)

  structure(
    list(
      nodes = base$nodes,
      edges = edges,
      truth = truth,
      params = params,
      is_observed = FALSE,
      observation = NULL,
      generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    ),
    class = "synthona_dataset"
  )
}

#' @export
print.synthona_dataset <- function(x, ...) {
  cat("<synthona_dataset>", x$params$label, "\n")
  cat(sprintf("  actors    : %d\n", nrow(x$nodes)))
  cat(sprintf("  ties      : %d across %d layer(s)\n",
              nrow(x$edges), length(unique(x$edges$layer))))
  cat(sprintf("  layers    : %s\n", paste(sort(unique(x$edges$layer)), collapse = ", ")))
  snaps <- unique(x$edges$snapshot)
  cat(sprintf("  snapshots : %s\n", paste(snaps, collapse = ", ")))
  cat(sprintf("  truth     : %d communities, %d brokers\n",
              x$truth$n_communities, length(x$truth$brokers)))
  if (isTRUE(x$is_observed)) {
    cat(sprintf("  observed  : response rate %.0f%%, name limit %s\n",
                100 * x$observation$response_rate,
                if (is.finite(x$observation$name_generator_limit)) {
                  x$observation$name_generator_limit
                } else {
                  "none"
                }))
  } else {
    cat("  observed  : no (complete network)\n")
  }
  invisible(x)
}

#' Extract an igraph graph from a dataset
#'
#' @param dataset A `synthona_dataset`.
#' @param layer Optional layer to restrict to.
#' @param snapshot Optional snapshot to restrict to. Defaults to the first.
#' @param directed Whether to build a directed graph.
#'
#' @return An igraph graph.
#' @export
#' @examples
#' d <- synthona_generate(synthona_params(n = 100))
#' synthona_graph(d, layer = "trust")
synthona_graph <- function(dataset, layer = NULL, snapshot = NULL, directed = NULL) {
  stopifnot(inherits(dataset, "synthona_dataset"))
  snapshot <- snapshot %||% dataset$params$snapshots[1]
  graph_from_edges(
    dataset$nodes, dataset$edges,
    layer = layer, snapshot = snapshot, directed = directed
  )
}

#' Summarise a dataset
#'
#' @param object A `synthona_dataset`.
#' @param n_random Number of random reference graphs for the small-world
#'   coefficient. 0 skips it.
#' @param ... Unused.
#'
#' @return A one-row tibble of network metrics per snapshot.
#' @export
summary.synthona_dataset <- function(object, n_random = 0L, ...) {
  purrr::map_dfr(unique(object$edges$snapshot), function(s) {
    compute_network_metrics(object$nodes, object$edges, snapshot = s, n_random = n_random)
  })
}
