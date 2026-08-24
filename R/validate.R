# Validation ------------------------------------------------------------------
#
# Acceptance checks exist to catch a generator that has silently stopped doing
# what it claims. A check that cannot fail is worse than no check, because it
# reports success either way: counting how many actors exceed their own 90th
# percentile of betweenness, for instance, always returns about a tenth of the
# organisation regardless of whether any real brokerage structure exists.
#
# Every check below compares a realised quantity against the parameter that
# was supposed to produce it, or against a value the generator could plausibly
# miss.

#' Validate a generated dataset against its own specification
#'
#' @param dataset A `synthona_dataset` from [synthona_generate()].
#' @param degree_tolerance Relative tolerance allowed between realised and
#'   target mean degree.
#'
#' @return A tibble of checks with a `pass` column.
#' @export
#' @examples
#' d <- synthona_generate(synthona_params(n = 300, topology = "sbm"))
#' validate_dataset(d)
validate_dataset <- function(dataset, degree_tolerance = 0.20) {
  stopifnot(inherits(dataset, "synthona_dataset"))
  params <- dataset$params
  nodes <- dataset$nodes
  snapshot <- params$snapshots[1]
  edges <- dataset$edges[dataset$edges$snapshot == snapshot, , drop = FALSE]

  net <- compute_network_metrics(nodes, edges, n_random = 0L)
  grp <- compute_group_metrics(nodes, edges)
  node_m <- compute_node_metrics(nodes, edges)

  checks <- list()

  # 1. Realised degree must match the requested degree. This is the check that
  #    fails first if calibration breaks.
  realised <- net$mean_degree
  target <- params$mean_degree
  checks[[length(checks) + 1L]] <- validation_row(
    "mean_degree",
    sprintf("Realised mean degree within %.0f%% of target", 100 * degree_tolerance),
    sprintf("target %.1f", target),
    round(realised, 2),
    abs(realised - target) / target <= degree_tolerance
  )

  # 2 and 3. Community structure, but only for the topologies that model it.
  #    Erdos-Renyi, small-world and preferential-attachment structures are
  #    defined without reference to departments, so `within_share` has nothing
  #    to act on and departmental modularity is expected to be near zero.
  #    Asserting otherwise would fail a generator that is working correctly.
  k <- length(unique(nodes$department))
  if (models_departments(params$topology)) {
    checks[[length(checks) + 1L]] <- validation_row(
      "planted_communities",
      "Departmental modularity is positive when within_share exceeds chance",
      "> 0.05",
      round(net$modularity_department, 3),
      params$within_share <= (1 / k) + 0.05 ||
        (!is.na(net$modularity_department) && net$modularity_department > 0.05)
    )

    within_realised <- mean(edges$same_dept, na.rm = TRUE)
    checks[[length(checks) + 1L]] <- validation_row(
      "within_share",
      "Realised within-department tie share within 0.15 of target",
      sprintf("target %.2f", params$within_share),
      round(within_realised, 3),
      !is.na(within_realised) && abs(within_realised - params$within_share) <= 0.15
    )
  }

  # 4. The organisation must hang together. A benchmark that fragments into
  #    dozens of components is not measuring what most ONA methods assume.
  checks[[length(checks) + 1L]] <- validation_row(
    "connectivity",
    "Giant component holds at least 90% of actors",
    ">= 0.90",
    net$giant_component_share,
    net$giant_component_share >= 0.90
  )

  # 5. Brokerage must be more concentrated than chance. An absolute threshold
  #    would not travel across sizes: betweenness is genuinely spread out in a
  #    small dense network, where everyone is two steps from everyone, and
  #    concentrated in a large sparse one. The comparison is therefore against
  #    an Erdos-Renyi graph of the same order and density, which is the least
  #    structured organisation the same tie budget could have produced.
  gini_btw <- gini(node_m$betweenness)
  null_gini <- null_brokerage_gini(net$node_count, net$density, params$seed_topology)
  checks[[length(checks) + 1L]] <- validation_row(
    "brokerage_concentration",
    "Betweenness is more concentrated than in a random graph of equal density",
    sprintf("> %.3f (random null)", null_gini),
    round(gini_btw, 3),
    !is.na(gini_btw) && !is.na(null_gini) && gini_btw > null_gini
  )

  # 6. Groups must differ from one another, otherwise department is noise.
  ei_sd <- stats::sd(grp$ei_index, na.rm = TRUE)
  checks[[length(checks) + 1L]] <- validation_row(
    "group_variation",
    "Departments vary in openness (sd of E-I index above 0.05)",
    "> 0.05",
    round(ei_sd, 3),
    !is.na(ei_sd) && ei_sd > 0.05
  )

  # 7. Every requested layer must be present.
  missing_layers <- setdiff(params$layers, unique(edges$layer))
  checks[[length(checks) + 1L]] <- validation_row(
    "layers_present",
    "Every requested layer produced ties",
    paste(params$layers, collapse = ", "),
    if (length(missing_layers) == 0) "all present" else paste(missing_layers, collapse = ", "),
    length(missing_layers) == 0
  )

  # 8. Ground truth must be non-degenerate.
  checks[[length(checks) + 1L]] <- validation_row(
    "truth_recorded",
    "Ground truth records communities and brokers",
    ">= 2 communities, >= 1 broker",
    sprintf("%d / %d", dataset$truth$n_communities, length(dataset$truth$brokers)),
    dataset$truth$n_communities >= 2 && length(dataset$truth$brokers) >= 1
  )

  extra <- scenario_validation_checks(dataset)
  if (!is.null(extra)) checks[[length(checks) + 1L]] <- extra

  out <- dplyr::bind_rows(checks)
  out$scenario_id <- params$scenario_id %||% NA_character_
  out
}

validation_row <- function(check_id, description, threshold, measured, pass) {
  tibble::tibble(
    check_id = check_id,
    description = description,
    threshold = as.character(threshold),
    measured = as.character(measured),
    pass = isTRUE(pass)
  )
}

#' Does a topology model departmental structure?
#'
#' The block and hierarchy models place ties with reference to which department
#' a person belongs to. The classical reference topologies do not: they are
#' included as null models precisely because their structure is independent of
#' the attribute data.
#'
#' @param topology A topology identifier.
#' @return `TRUE` if the topology is department-aware.
#' @keywords internal
models_departments <- function(topology) {
  topology %in% c("hierarchy", "sbm", "sbm_dual_legacy")
}

#' Expected brokerage concentration under a random graph
#'
#' The Gini coefficient of betweenness in an Erdos-Renyi graph of the given
#' order and density, averaged over several draws. Used as the null against
#' which a generated organisation must show more concentrated brokerage.
#'
#' @param n Number of actors.
#' @param density Edge density.
#' @param seed Integer seed.
#' @param n_draws Number of random graphs to average over.
#'
#' @return A numeric Gini coefficient, or `NA_real_`.
#' @keywords internal
null_brokerage_gini <- function(n, density, seed = SEED_TOPOLOGY_BASE, n_draws = 5L) {
  if (is.na(n) || is.na(density) || n < 3 || density <= 0) {
    return(NA_real_)
  }
  vals <- with_local_seed(derive_seed(seed, "null:brokerage"), {
    vapply(seq_len(n_draws), function(i) {
      g <- igraph::sample_gnp(n, density, directed = FALSE)
      gini(igraph::betweenness(g, directed = FALSE, normalized = TRUE))
    }, numeric(1))
  })
  mean(vals, na.rm = TRUE)
}
