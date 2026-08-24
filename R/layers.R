# Relational layer generation -------------------------------------------------

#' Share of the base structure a layer retains
#'
#' Single-bracket indexing is used deliberately: `[[` raises a subscript error
#' on an unknown name, which would make the default unreachable.
#'
#' @param layer Layer identifier.
#' @return A probability.
#' @keywords internal
layer_keep_prob <- function(layer) {
  unname(LAYER_KEEP_PROB[layer]) %|na|% 0.50
}

`%|na|%` <- function(x, y) if (length(x) == 0 || is.na(x)) y else x

#' Draw tie strengths for a set of edges
#'
#' Weights are drawn from beta distributions whose shape depends on the layer
#' and on whether the pair shares a department, a location or a legacy
#' organisation. Draws are vectorised over edges rather than taken one at a
#' time, which keeps generation linear in the number of ties.
#'
#' @param layer Layer identifier.
#' @param same_dept,same_location,same_legacy Logical vectors over edges.
#' @param seed Integer seed.
#'
#' @return A numeric vector of tie strengths on `(0, 1)`.
#' @keywords internal
draw_edge_weights <- function(layer, same_dept, same_location, same_legacy, seed) {
  n <- length(same_dept)
  if (n == 0) {
    return(numeric(0))
  }
  if (identical(layer, "reporting")) {
    return(rep(1, n))
  }

  proximate <- (!is.na(same_dept) & same_dept) | (!is.na(same_location) & same_location)
  cross_legacy <- !is.na(same_legacy) & !same_legacy

  shape_a <- rep(2.0, n)
  shape_b <- rep(4.0, n)

  shape_a[proximate] <- 3.0
  shape_b[proximate] <- 2.0

  if (identical(layer, "trust")) {
    shape_a <- ifelse(proximate, 3.5, 2.5)
    shape_b <- ifelse(proximate, 1.5, 2.0)
  } else if (identical(layer, "decision_influence")) {
    shape_a[] <- 3.2
    shape_b[] <- 1.8
  }

  # Ties that bridge two legacy organisations are weaker regardless of layer.
  shape_a[cross_legacy] <- 1.8
  shape_b[cross_legacy] <- 3.8

  with_local_seed(seed, round(stats::rbeta(n, shape_a, shape_b), 3))
}

#' Convert a graph to an edge table with dyad context
#'
#' @param g An igraph graph.
#' @param nodes The node table used to build `g`.
#' @param layer Layer identifier to assign.
#' @param seed Integer seed for tie strengths.
#'
#' @return An edge table matching the protocol schema.
#' @keywords internal
graph_to_edge_df <- function(g, nodes, layer = "communication", seed = SEED_ATTRIBUTE_BASE) {
  edf <- igraph::as_data_frame(g, what = "edges")
  if (nrow(edf) == 0) {
    return(empty_edge_tbl())
  }

  i <- match(edf$from, nodes$person_id)
  j <- match(edf$to, nodes$person_id)

  same_dept <- nodes$department[i] == nodes$department[j]
  same_loc <- nodes$location[i] == nodes$location[j]
  same_legacy <- if ("legacy_company" %in% names(nodes)) {
    nodes$legacy_company[i] == nodes$legacy_company[j]
  } else {
    rep(NA, nrow(edf))
  }

  coerce_edge_schema(tibble::tibble(
    from = as.character(edf$from),
    to = as.character(edf$to),
    weight = draw_edge_weights(layer, same_dept, same_loc, same_legacy, seed),
    layer = layer,
    directed = layer %in% DIRECTED_LAYERS,
    same_dept = same_dept,
    same_location = same_loc,
    same_legacy = same_legacy
  ))
}

#' Orient the ties of a directed layer
#'
#' Advice flows from the person seeking it to the person giving it, so it runs
#' up the hierarchy; reporting, mentorship and decision influence run down it.
#' Ties between peers at the same level are oriented at random.
#'
#' @param edf An edge table.
#' @param nodes The node table.
#' @param layer Layer identifier.
#' @param seed Integer seed.
#'
#' @return `edf` with endpoints oriented.
#' @keywords internal
orient_edges <- function(edf, nodes, layer, seed) {
  if (nrow(edf) == 0) {
    return(edf)
  }
  if (!(layer %in% DIRECTED_LAYERS)) {
    edf$directed <- FALSE
    return(edf)
  }

  from_level <- nodes$level[match(edf$from, nodes$person_id)]
  to_level <- nodes$level[match(edf$to, nodes$person_id)]
  from_level[is.na(from_level)] <- 0L
  to_level[is.na(to_level)] <- 0L

  swap <- if (layer %in% c("reporting", "mentorship", "decision_influence")) {
    from_level < to_level
  } else if (layer == "advice") {
    from_level > to_level
  } else {
    rep(FALSE, nrow(edf))
  }

  # Peers are oriented at random rather than left in generation order.
  peers <- from_level == to_level
  if (any(peers)) {
    coin <- with_local_seed(seed, stats::runif(sum(peers)) < 0.5)
    swap[peers] <- coin
  }

  old_from <- edf$from
  edf$from[swap] <- edf$to[swap]
  edf$to[swap] <- old_from[swap]
  edf$directed <- TRUE
  edf
}

#' Derive one relational layer from the base structure
#'
#' Each layer is a thinned, oriented view of the same underlying structure.
#' Layer seeds are derived from the layer *name*, so adding a layer does not
#' change the ties generated for any existing one.
#'
#' @param base_graph The base structure graph.
#' @param nodes The node table used to build it.
#' @param layer Layer identifier.
#' @param params A [synthona_params()] object.
#'
#' @return An edge table for the layer.
#' @keywords internal
derive_layer <- function(base_graph, nodes, layer, params) {
  seed <- derive_seed(params$seed_attributes, paste0("layer:", layer))
  edf <- graph_to_edge_df(base_graph, nodes, layer = layer, seed = seed)
  if (nrow(edf) == 0) {
    return(edf)
  }

  keep_prob <- params$extras$layer_keep_prob[[layer]] %||% layer_keep_prob(layer)
  if (keep_prob < 1) {
    keep <- with_local_seed(
      derive_seed(seed, "thin"),
      stats::runif(nrow(edf)) < keep_prob
    )
    edf <- edf[keep, , drop = FALSE]
  }
  if (nrow(edf) == 0) {
    return(edf)
  }
  orient_edges(edf, nodes, layer, seed = derive_seed(seed, "orient"))
}

#' Generate every requested relational layer
#'
#' @param base_graph The base structure graph.
#' @param nodes The node table used to build it.
#' @param params A [synthona_params()] object.
#'
#' @return An edge table spanning all requested layers.
#' @export
#' @examples
#' p <- synthona_params(n = 100, layers = c("communication", "trust"))
#' base <- generate_base_structure(generate_profiles(p), p)
#' edges <- generate_layers(base$graph, base$nodes, p)
#' table(edges$layer)
generate_layers <- function(base_graph, nodes, params) {
  tables <- lapply(params$layers, function(l) derive_layer(base_graph, nodes, l, params))
  edges <- bind_edge_tables(.tables = tables)
  compact_edges(edges)
}
