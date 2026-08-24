# Network metrics -------------------------------------------------------------

#' Build an igraph graph from a node and edge table
#'
#' @param nodes The node table.
#' @param edges The edge table.
#' @param layer Optional layer to restrict to.
#' @param snapshot Optional snapshot to restrict to.
#' @param directed Whether to build a directed graph. Defaults to the direction
#'   implied by the layer.
#' @param collapse_multiplex Whether to sum tie strengths across layers onto a
#'   single projection.
#'
#' @return An igraph graph with vertex attributes and edge weights.
#' @export
#' @examples
#' d <- synthona_generate(synthona_params(n = 80))
#' g <- graph_from_edges(d$nodes, d$edges)
#' igraph::ecount(g)
graph_from_edges <- function(nodes, edges, layer = NULL, snapshot = NULL,
                             directed = NULL, collapse_multiplex = TRUE) {
  ed <- coerce_edge_schema(edges)
  if (!is.null(layer)) ed <- ed[ed$layer %in% layer, , drop = FALSE]
  if (!is.null(snapshot)) ed <- ed[!is.na(ed$snapshot) & ed$snapshot %in% snapshot, , drop = FALSE]

  if (is.null(directed)) {
    directed <- !is.null(layer) && nrow(ed) > 0 && all(ed$layer %in% DIRECTED_LAYERS)
  }

  if (nrow(ed) == 0) {
    g <- igraph::make_empty_graph(n = nrow(nodes), directed = directed)
    return(apply_vertex_attributes(g, nodes))
  }

  ed_graph <- if (collapse_multiplex) {
    collapse_edge_projection(ed, directed = directed)
  } else {
    ed[, c("from", "to", "weight"), drop = FALSE]
  }

  g <- igraph::graph_from_data_frame(
    as.data.frame(ed_graph[, c("from", "to")]),
    directed = directed, vertices = as.data.frame(nodes)
  )
  igraph::E(g)$weight <- ed_graph$weight
  g
}

#' Convert tie strength to a distance for shortest-path metrics
#'
#' `igraph` treats edge weights as *distances*: a larger weight means a longer
#' path. Tie strength means the opposite, so passing strength directly to
#' `betweenness()` or `closeness()` inverts the result, ranking the weakest
#' ties as the most efficient routes. Strength is therefore reciprocated
#' before any shortest-path computation.
#'
#' @param g An igraph graph carrying a `weight` edge attribute.
#' @return A numeric vector of distances, or `NULL` if the graph is unweighted.
#' @keywords internal
tie_distance <- function(g) {
  if (!"weight" %in% igraph::edge_attr_names(g) || igraph::ecount(g) == 0) {
    return(NULL)
  }
  w <- igraph::E(g)$weight
  w[is.na(w) | w <= 0] <- stats::median(w[w > 0], na.rm = TRUE) %|na|% 1
  1 / w
}

#' Actor-level network metrics
#'
#' @param nodes The node table.
#' @param edges The edge table.
#' @param snapshot Optional snapshot to restrict to.
#' @param weighted Whether to use tie strength in shortest-path metrics.
#'
#' @return A tibble of per-actor metrics.
#' @export
compute_node_metrics <- function(nodes, edges, snapshot = NULL, weighted = TRUE) {
  g <- graph_from_edges(nodes, edges, snapshot = snapshot, directed = FALSE)
  dist <- if (weighted) tie_distance(g) else NULL
  comp <- igraph::components(g)

  articulation <- if (igraph::ecount(g) > 0) {
    igraph::V(g)$name[as.integer(igraph::articulation_points(g))]
  } else {
    character()
  }

  out <- tibble::tibble(
    person_id = nodes$person_id,
    degree = igraph::degree(g),
    strength = if (igraph::ecount(g) > 0) igraph::strength(g) else rep(0, nrow(nodes)),
    betweenness = igraph::betweenness(g, directed = FALSE, weights = dist, normalized = TRUE),
    closeness = suppressWarnings(igraph::closeness(g, weights = dist, normalized = TRUE)),
    page_rank = igraph::page_rank(g, directed = FALSE, weights = igraph::E(g)$weight)$vector,
    constraint = tryCatch(
      igraph::constraint(g),
      error = function(e) rep(NA_real_, igraph::vcount(g))
    ),
    is_articulation = nodes$person_id %in% articulation,
    component_id = as.integer(comp$membership)
  )
  if (!is.null(snapshot)) out$snapshot <- snapshot
  out
}

#' Group-level cohesion metrics
#'
#' Reports the Krackhardt E-I index per group: the balance of ties reaching
#' outside the group against ties staying inside it. It runs from -1, a group
#' that talks only to itself, to +1, a group that talks only to others.
#'
#' @param nodes The node table.
#' @param edges The edge table.
#' @param group_var Node column defining the grouping.
#' @param snapshot Optional snapshot to restrict to.
#'
#' @return A tibble with one row per group.
#' @export
compute_group_metrics <- function(nodes, edges, group_var = "department", snapshot = NULL) {
  if (!group_var %in% names(nodes)) {
    stop("Unknown group_var: ", group_var, call. = FALSE)
  }
  g <- graph_from_edges(nodes, edges, snapshot = snapshot, directed = FALSE)
  membership <- nodes[[group_var]]
  groups <- sort(unique(membership))

  el <- igraph::as_edgelist(g, names = TRUE)
  if (nrow(el) == 0) {
    out <- tibble::tibble(
      group = groups, internal = 0L, external = 0L,
      ei_index = NA_real_, size = as.integer(table(membership)[groups])
    )
    if (!is.null(snapshot)) out$snapshot <- snapshot
    return(out)
  }

  from_group <- membership[match(el[, 1], nodes$person_id)]
  to_group <- membership[match(el[, 2], nodes$person_id)]

  out <- purrr::map_dfr(groups, function(grp) {
    internal <- sum(from_group == grp & to_group == grp, na.rm = TRUE)
    external <- sum(
      (from_group == grp & to_group != grp) | (to_group == grp & from_group != grp),
      na.rm = TRUE
    )
    total <- internal + external
    tibble::tibble(
      group = grp,
      size = sum(membership == grp, na.rm = TRUE),
      internal = internal,
      external = external,
      ei_index = if (total == 0) NA_real_ else round((external - internal) / total, 3)
    )
  })
  if (!is.null(snapshot)) out$snapshot <- snapshot
  out
}

#' Whole-network metrics
#'
#' @param nodes The node table.
#' @param edges The edge table.
#' @param snapshot Optional snapshot to restrict to.
#' @param n_random Number of random graphs used for the small-world reference.
#'   Set to 0 to skip it, which is much faster.
#' @param seed Integer seed for the random reference graphs.
#'
#' @return A one-row tibble of network-level metrics.
#' @export
compute_network_metrics <- function(nodes, edges, snapshot = NULL,
                                    n_random = 20L, seed = SEED_TOPOLOGY_BASE) {
  g <- graph_from_edges(nodes, edges, snapshot = snapshot, directed = FALSE)
  comp <- igraph::components(g)
  giant <- igraph::induced_subgraph(g, which(comp$membership == which.max(comp$csize)))
  deg <- igraph::degree(g)
  btw <- igraph::betweenness(g, directed = FALSE, weights = tie_distance(g), normalized = TRUE)

  out <- tibble::tibble(
    node_count = igraph::vcount(g),
    edge_count = igraph::ecount(g),
    density = round(igraph::edge_density(g), 5),
    mean_degree = round(mean(deg), 3),
    max_degree = max(deg),
    degree_gini = round(gini(deg), 3),
    clustering = round(igraph::transitivity(g, type = "global"), 4),
    mean_path_length = if (igraph::vcount(giant) > 1) {
      round(suppressWarnings(igraph::mean_distance(
        giant, directed = FALSE, weights = tie_distance(giant)
      )), 3)
    } else {
      NA_real_
    },
    assortativity_degree = round(igraph::assortativity_degree(g, directed = FALSE), 3),
    modularity_department = round(department_modularity(g), 3),
    component_count = comp$no,
    giant_component_share = round(max(comp$csize) / max(1L, igraph::vcount(g)), 3),
    broker_share = round(mean(btw > stats::quantile(btw, 0.9, na.rm = TRUE), na.rm = TRUE), 4),
    small_world_sigma = small_world_sigma(g, n_random = n_random, seed = seed)
  )
  if (!is.null(snapshot)) out$snapshot <- snapshot
  out
}

gini <- function(x) {
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n == 0 || sum(x) == 0) {
    return(NA_real_)
  }
  as.numeric((2 * sum(seq_len(n) * x)) / (n * sum(x)) - (n + 1) / n)
}

department_modularity <- function(g) {
  if (igraph::ecount(g) == 0 || !"department" %in% igraph::vertex_attr_names(g)) {
    return(NA_real_)
  }
  igraph::modularity(g, as.integer(factor(igraph::V(g)$department)))
}

#' Small-world coefficient
#'
#' The ratio of observed clustering to random clustering, divided by the ratio
#' of observed path length to random path length. Values above 1 indicate
#' small-world structure.
#'
#' @param g An igraph graph.
#' @param n_random Number of random reference graphs. 0 skips the computation.
#' @param seed Integer seed.
#'
#' @return A numeric value, or `NA_real_`.
#' @export
small_world_sigma <- function(g, n_random = 20L, seed = SEED_TOPOLOGY_BASE) {
  if (n_random < 1 || igraph::ecount(g) == 0) {
    return(NA_real_)
  }
  dens <- igraph::edge_density(g)
  c_obs <- igraph::transitivity(g, type = "global")
  comp <- igraph::components(g)
  giant <- igraph::induced_subgraph(g, which(comp$membership == which.max(comp$csize)))
  # Sigma compares an observed graph against random graphs of the same order
  # and density, and those references are unweighted. The observed path length
  # has to be measured the same way or the ratio is not a comparison at all:
  # weighted against unweighted inflates sigma several-fold.
  l_obs <- suppressWarnings(igraph::mean_distance(giant, directed = FALSE, weights = NA))

  ref <- with_local_seed(seed, {
    vapply(seq_len(n_random), function(i) {
      gr <- igraph::sample_gnp(igraph::vcount(g), dens, directed = FALSE)
      cr <- igraph::components(gr)
      gir <- igraph::induced_subgraph(gr, which(cr$membership == which.max(cr$csize)))
      c(
        igraph::transitivity(gr, type = "global"),
        suppressWarnings(igraph::mean_distance(gir, directed = FALSE, weights = NA))
      )
    }, numeric(2))
  })

  c_rand <- mean(ref[1, ], na.rm = TRUE)
  l_rand <- mean(ref[2, ], na.rm = TRUE)
  round((c_obs / max(c_rand, 1e-8)) / (l_obs / max(l_rand, 1e-8)), 3)
}

#' Synthetic outcome variables
#'
#' Engagement, innovation and burnout risk, generated as functions of network
#' position and attributes. These give a known data-generating process for
#' testing whether an analysis recovers the relationship between position and
#' outcome. All noise is drawn from a named seed stream, so repeated calls
#' return identical values.
#'
#' @param nodes The node table.
#' @param edges The edge table.
#' @param params A [synthona_params()] object.
#' @param snapshot Optional snapshot to restrict to.
#'
#' @return A tibble of per-actor outcomes.
#' @export
compute_outcomes <- function(nodes, edges, params, snapshot = NULL) {
  node_m <- compute_node_metrics(nodes, edges, snapshot = snapshot)
  trust_deg <- igraph::degree(graph_from_edges(nodes, edges, layer = "trust", snapshot = snapshot, directed = FALSE))
  comm_deg <- igraph::degree(graph_from_edges(nodes, edges, layer = "communication", snapshot = snapshot, directed = FALSE))

  n <- nrow(nodes)
  noise <- function(tag) {
    with_local_seed(
      derive_seed(params$seed_attributes, paste0("outcome:", tag, ":", snapshot %||% "")),
      stats::rnorm(n, 0, 0.03)
    )
  }

  constraint <- node_m$constraint
  constraint[is.na(constraint)] <- stats::median(constraint, na.rm = TRUE) %|na|% 0.5

  out <- tibble::tibble(
    person_id = nodes$person_id,
    snapshot = snapshot %||% NA_character_,
    engagement_score = round(soft_clip(
      0.35 + 0.25 * scale01(trust_deg) + 0.20 * scale01(node_m$closeness) -
        0.15 * scale01(constraint) + noise("engagement"), 0, 1
    ), 3),
    innovation_score = round(soft_clip(
      0.25 + 0.25 * scale01(comm_deg) + 0.25 * scale01(node_m$betweenness) +
        0.15 * scale01(nodes$ai_adoption_score) + noise("innovation"), 0, 1
    ), 3),
    burnout_risk = round(soft_clip(
      0.15 + 0.30 * scale01(comm_deg) + 0.20 * scale01(node_m$betweenness) -
        0.15 * scale01(trust_deg) + noise("burnout"), 0, 1
    ), 3)
  )
  out
}
