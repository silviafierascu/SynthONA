# Longitudinal generation -----------------------------------------------------
#
# Panels are generated as a process rather than as a set of independent
# perturbations of one baseline. Under `snapshot_mode = "cumulative"` each wave
# evolves from the wave before it, so a tie that dissolves stays dissolved and
# a tie that forms persists until it dissolves. Perturbing the same baseline
# repeatedly instead produces waves in which ties reappear after being dropped,
# which is not a trajectory any longitudinal method should be scored against.

#' Default tie turnover settings
#'
#' @return A list of evolution settings.
#' @keywords internal
default_evolution <- function() {
  list(
    churn = 0.06,       # share of ties dissolving per wave
    renewal = TRUE,     # replace dissolved ties to hold degree roughly steady
    weight_drift = 0.04 # standard deviation of tie-strength drift per wave
  )
}

#' Apply one shock to an edge table
#'
#' Shocks are declared as data in `params$extras$shocks`, so scenarios express
#' what happens to them without the generator needing to know their names. Each
#' shock is a list with an `at` field naming the snapshot it fires on, and one
#' of the following effects:
#'
#' \describe{
#'   \item{`drop`}{Share of ties in `layer` to dissolve.}
#'   \item{`add`}{Ties to create, as a multiple of headcount.}
#'   \item{`weight_delta`}{Additive shift applied to tie strengths in `layer`.}
#'   \item{`remove_actor`}{Either `"top_broker"` or a `person_id`, whose ties
#'     are all removed. Used for key-person and succession analysis.}
#' }
#'
#' @param edges An edge table.
#' @param nodes The node table.
#' @param shock A shock specification.
#' @param params A [synthona_params()] object.
#' @param seed Integer seed.
#'
#' @return The modified edge table.
#' @keywords internal
apply_shock <- function(edges, nodes, shock, params, seed) {
  layer <- shock$layer %||% NULL
  in_layer <- if (is.null(layer)) rep(TRUE, nrow(edges)) else edges$layer == layer

  if (!is.null(shock$drop) && any(in_layer)) {
    idx <- which(in_layer)
    drop <- with_local_seed(
      derive_seed(seed, "shock:drop"),
      idx[stats::runif(length(idx)) < shock$drop]
    )
    if (length(drop) > 0) edges <- edges[-drop, , drop = FALSE]
  }

  if (!is.null(shock$weight_delta)) {
    in_layer <- if (is.null(layer)) rep(TRUE, nrow(edges)) else edges$layer == layer
    edges$weight[in_layer] <- soft_clip(edges$weight[in_layer] + shock$weight_delta, 0, 1)
  }

  if (!is.null(shock$remove_actor)) {
    target <- if (identical(shock$remove_actor, "top_broker")) {
      top_broker_id(nodes, edges)
    } else {
      shock$remove_actor
    }
    if (!is.na(target)) {
      edges <- edges[edges$from != target & edges$to != target, , drop = FALSE]
    }
  }

  if (!is.null(shock$add)) {
    n_new <- as.integer(round(shock$add * nrow(nodes)))
    new_edges <- form_new_ties(
      nodes, n_new, layer %||% params$layers[1], params,
      tag = paste0(":shock:", shock$at %||% "")
    )
    edges <- bind_edge_tables(edges, new_edges)
  }

  edges
}

top_broker_id <- function(nodes, edges) {
  g <- graph_from_edges(nodes, edges, directed = FALSE)
  if (igraph::ecount(g) == 0) {
    return(NA_character_)
  }
  b <- igraph::betweenness(g, directed = FALSE, normalized = TRUE)
  igraph::V(g)$name[which.max(b)]
}

#' Create new ties between people who are not currently connected
#'
#' New ties respect the same within-department preference as the base
#' structure, so turnover does not gradually erase the planted community
#' structure.
#'
#' @param nodes The node table.
#' @param n_edges Number of ties to create.
#' @param layer Layer to assign.
#' @param params A [synthona_params()] object.
#' @param tag Seed tag distinguishing this draw from others.
#'
#' @return An edge table.
#' @keywords internal
form_new_ties <- function(nodes, n_edges, layer, params, tag = "") {
  if (is.na(n_edges) || n_edges < 1) {
    return(empty_edge_tbl())
  }
  within <- sample_within_department_ties(
    nodes, n_edges * params$within_share, params,
    tag = tag
  )
  across <- sample_cross_department_ties(
    nodes, n_edges * (1 - params$within_share), params,
    tag = tag
  )
  pairs <- dplyr::bind_rows(within, across)
  if (nrow(pairs) == 0) {
    return(empty_edge_tbl())
  }

  from_id <- nodes$person_id[pairs$from]
  to_id <- nodes$person_id[pairs$to]
  same_dept <- nodes$department[pairs$from] == nodes$department[pairs$to]
  same_loc <- nodes$location[pairs$from] == nodes$location[pairs$to]
  same_legacy <- if ("legacy_company" %in% names(nodes)) {
    nodes$legacy_company[pairs$from] == nodes$legacy_company[pairs$to]
  } else {
    rep(NA, nrow(pairs))
  }

  seed <- derive_seed(params$seed_attributes, paste0("form:", layer, tag))
  edf <- coerce_edge_schema(tibble::tibble(
    from = from_id, to = to_id,
    weight = draw_edge_weights(layer, same_dept, same_loc, same_legacy, seed),
    layer = layer,
    directed = layer %in% DIRECTED_LAYERS,
    same_dept = same_dept, same_location = same_loc, same_legacy = same_legacy
  ))
  orient_edges(edf, nodes, layer, seed = derive_seed(seed, "orient"))
}

#' Advance an edge table by one wave
#'
#' @param edges Edge table for the current wave.
#' @param nodes The node table.
#' @param params A [synthona_params()] object.
#' @param step Index of the wave being produced, starting at 1.
#' @param snapshot_id Identifier of the wave being produced.
#'
#' @return The edge table for the next wave.
#' @keywords internal
evolve_edges <- function(edges, nodes, params, step, snapshot_id) {
  ev <- utils::modifyList(default_evolution(), params$extras$evolution %||% list())
  seed <- derive_seed(params$seed_topology, paste0("wave:", snapshot_id))
  n_before <- nrow(edges)

  if (ev$churn > 0 && n_before > 0) {
    survive <- with_local_seed(
      derive_seed(seed, "churn"),
      stats::runif(n_before) >= ev$churn
    )
    edges <- edges[survive, , drop = FALSE]
  }

  if (ev$weight_drift > 0 && nrow(edges) > 0) {
    drift <- with_local_seed(
      derive_seed(seed, "drift"),
      stats::rnorm(nrow(edges), 0, ev$weight_drift)
    )
    edges$weight <- soft_clip(edges$weight + drift, 0.01, 1)
  }

  if (isTRUE(ev$renewal)) {
    deficit <- n_before - nrow(edges)
    if (deficit > 0) {
      layer_mix <- table(edges$layer)
      layer_for_new <- if (length(layer_mix) > 0) {
        names(layer_mix)[which.max(layer_mix)]
      } else {
        params$layers[1]
      }
      edges <- bind_edge_tables(
        edges,
        form_new_ties(nodes, deficit, layer_for_new, params,
          tag = paste0(":wave:", snapshot_id)
        )
      )
    }
  }

  shocks <- Filter(
    function(s) identical(s$at, snapshot_id),
    params$extras$shocks %||% list()
  )
  for (s in shocks) {
    edges <- apply_shock(edges, nodes, s, params, derive_seed(seed, "shock"))
  }

  compact_edges(edges)
}

#' Generate every snapshot of a dataset
#'
#' Under `"cumulative"` mode each snapshot evolves from the previous one.
#' Under `"alternative"` mode every snapshot is an independent variant of the
#' same baseline, which is what what-if comparisons such as competing
#' reorganisation options require.
#'
#' @param nodes The node table.
#' @param edges The baseline edge table.
#' @param params A [synthona_params()] object.
#'
#' @return An edge table with a populated `snapshot` column.
#' @export
generate_snapshots <- function(nodes, edges, params) {
  snaps <- params$snapshots
  if (length(snaps) == 1L) {
    edges$snapshot <- snaps
    return(compact_edges(edges))
  }

  out <- vector("list", length(snaps))
  current <- edges

  for (i in seq_along(snaps)) {
    snap <- snaps[i]
    wave <- if (i == 1L) {
      current
    } else if (identical(params$snapshot_mode, "cumulative")) {
      evolve_edges(current, nodes, params, step = i - 1L, snapshot_id = snap)
    } else {
      evolve_edges(edges, nodes, params, step = i - 1L, snapshot_id = snap)
    }
    if (identical(params$snapshot_mode, "cumulative")) {
      current <- wave
    }
    wave$snapshot <- snap
    out[[i]] <- wave
  }

  compact_edges(bind_edge_tables(.tables = out))
}
