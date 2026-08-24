# Scenario-specific extras -----------------------------------------------------
#
# Most of the protocol is scenario-agnostic: a scenario is a parameter
# specification, and the generator never branches on which one it is. Three
# things resist that treatment, because they are the substantive content of a
# particular benchmark rather than a parameter of the general model:
#
#   * derived attributes the scenario is *about* -- who counts as an adoption
#     champion, which roles are exposed in a restructuring;
#   * the ties those attributes imply -- an adoption champion reaches for the
#     tool, a culture programme's seed group carries extra energy;
#   * the metrics and acceptance checks the benchmark is scored on.
#
# They are collected here so the rest of the package stays free of scenario
# branching. Everything is keyed on `params$scenario_id`, so a hand-built
# specification with no scenario identifier passes through untouched.

safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) NA_real_ else mean(x)
}

safe_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2) NA_real_ else stats::sd(x)
}

safe_round <- function(x, digits = 3L) {
  ifelse(is.finite(x), round(x, digits), NA_real_)
}

#' Sample actor pairs with structural bias
#'
#' Draws distinct unordered pairs, preferring same-department pairs in
#' proportion to `same_dept_bias` and nudging towards same-location pairs. A
#' negative `same_legacy_bias` prefers pairs that cross the legacy divide.
#'
#' @param nodes A node table.
#' @param n_edges Number of pairs to draw.
#' @param seed Integer seed.
#' @param same_dept_bias Weight given to same-department pairs, in `[0, 1]`.
#' @param same_legacy_bias Optional weight for the legacy split.
#'
#' @return A tibble of `from`, `to` and the three context flags.
#' @keywords internal
sample_structured_pairs <- function(nodes, n_edges, seed, same_dept_bias = 0.7,
                                    same_legacy_bias = NULL) {
  nodes <- tibble::as_tibble(nodes)
  empty <- tibble::tibble(
    from = character(), to = character(),
    same_dept = logical(), same_location = logical(), same_legacy = logical()
  )
  if (nrow(nodes) < 2 || n_edges <= 0) return(empty)

  target <- min(as.integer(n_edges), nrow(nodes) * (nrow(nodes) - 1L) / 2L)
  if (target <= 0) return(empty)

  has_legacy <- "legacy_company" %in% names(nodes)

  with_local_seed(seed, {
    seen <- new.env(parent = emptyenv())
    out <- vector("list", target)
    n_out <- 0L
    attempts <- 0L
    max_attempts <- max(250L, target * 30L)

    while (n_out < target && attempts < max_attempts) {
      attempts <- attempts + 1L
      i <- sample.int(nrow(nodes), 1)
      cand <- setdiff(seq_len(nrow(nodes)), i)

      same_dept <- nodes$department[cand] == nodes$department[i]
      same_location <- nodes$location[cand] == nodes$location[i]
      same_legacy <- if (has_legacy) {
        nodes$legacy_company[cand] == nodes$legacy_company[i]
      } else {
        rep(NA, length(cand))
      }

      prob <- ifelse(same_dept, same_dept_bias, 1 - same_dept_bias)
      prob <- prob + ifelse(same_location, 0.10, 0)
      if (!is.null(same_legacy_bias) && !all(is.na(same_legacy))) {
        prob <- prob + ifelse(same_legacy, same_legacy_bias, -same_legacy_bias)
      }
      prob <- normalise_prob(pmin(0.99, pmax(0.01, prob)))

      j <- if (length(cand) == 1L) cand else sample(cand, 1, prob = prob)
      a <- nodes$person_id[i]
      b <- nodes$person_id[j]
      lo <- min(a, b)
      hi <- max(a, b)
      key <- paste(lo, hi, sep = "|")
      if (exists(key, envir = seen, inherits = FALSE)) next

      assign(key, TRUE, envir = seen)
      n_out <- n_out + 1L
      out[[n_out]] <- tibble::tibble(
        from = lo,
        to = hi,
        same_dept = nodes$department[i] == nodes$department[j],
        same_location = nodes$location[i] == nodes$location[j],
        same_legacy = if (has_legacy) {
          nodes$legacy_company[i] == nodes$legacy_company[j]
        } else {
          NA
        }
      )
    }

    if (n_out == 0L) empty else dplyr::bind_rows(out[seq_len(n_out)])
  })
}

#' Draw a set of additional ties in one layer
#'
#' @inheritParams sample_structured_pairs
#' @param nodes Actors eligible to be joined.
#' @param layer The layer the ties belong to.
#'
#' @return An edge tibble in the standard schema.
#' @keywords internal
add_unique_edges <- function(nodes, n_edges, layer, seed, same_dept_bias = 0.7,
                             same_legacy_bias = NULL) {
  if (nrow(nodes) < 2 || n_edges <= 0) return(empty_edge_tbl())

  out <- sample_structured_pairs(
    nodes, n_edges, seed,
    same_dept_bias = same_dept_bias, same_legacy_bias = same_legacy_bias
  )
  if (nrow(out) == 0) return(empty_edge_tbl())

  out$weight <- draw_edge_weights(
    layer, out$same_dept, out$same_location, out$same_legacy,
    seed = derive_seed(seed, "extras:weight")
  )
  out$layer <- layer
  out$directed <- FALSE
  orient_edges(out, nodes, layer, seed = derive_seed(seed, "extras:orient"))
}

#' Attach the derived attributes a scenario is about
#'
#' Adds the actor-level columns that give a scenario its subject matter: who
#' the adoption champions are, which roles a restructuring exposes, who seeds
#' a culture programme. Scenarios with no such columns, and specifications
#' with no scenario identifier, are returned unchanged.
#'
#' All noise is drawn under a derived seed, so the columns are reproducible
#' and the calling session's RNG state is left alone.
#'
#' @param nodes A node table.
#' @param params A [synthona_params()] object.
#'
#' @return The node table, with any scenario columns added.
#' @keywords internal
apply_scenario_node_extras <- function(nodes, params) {
  sid <- params$scenario_id %||% NA_character_
  if (is.na(sid)) return(nodes)
  seed <- derive_seed(params$seed_attributes, paste0("extras:nodes:", sid))

  if (identical(sid, "AI_M")) {
    human <- !nodes$is_non_human
    cut <- stats::quantile(nodes$ai_adoption_score[human], 0.9, na.rm = TRUE)
    nodes$adoption_champion <- human & nodes$ai_adoption_score >= cut
    noise <- with_local_seed(seed, stats::rnorm(nrow(nodes), 0, 0.05))
    nodes$manager_enablement_score <- round(soft_clip(
      0.45 +
        0.25 * scale01(nodes$change_readiness) +
        0.20 * as.numeric(nodes$role_bucket %in% c("manager", "senior_manager", "executive")) +
        noise,
      0, 1
    ), 3)
  }

  if (identical(sid, "MA_M")) {
    # legacy_company is assigned by the dual-legacy topology, not here.
    draw <- with_local_seed(seed, stats::rbeta(nrow(nodes), 2.5, 2.5))
    nodes$integration_readiness <- round(soft_clip(
      draw + ifelse(nodes$role_bucket == "executive", 0.12, 0), 0.01, 0.99
    ), 3)
  }

  if (identical(sid, "RESIZE_M")) {
    nodes$at_risk_role <- nodes$department %in% c("Operations", "Admin", "Finance") &
      nodes$level <= 2
  }

  if (identical(sid, "SUCCESSION_M")) {
    nodes$key_person_candidate <-
      nodes$level >= stats::quantile(nodes$level, 0.8, na.rm = TRUE) |
      nodes$ai_adoption_score >= 0.82
  }

  if (identical(sid, "CULTURE_M")) {
    nodes$community_seed <-
      nodes$department %in% c("Strategy", "Product", "Design", "Engineering", "Client Services") |
      nodes$change_readiness > 0.7
  }

  nodes
}

#' Add the ties a scenario's derived attributes imply
#'
#' Some scenarios are defined by ties that no general layer rule would
#' produce: the humans who reach for an AI tool, the seed group carrying a
#' culture programme, the cross-legacy innovation ties a merger is judged on.
#'
#' @param nodes A node table, after [apply_scenario_node_extras()].
#' @param edges An edge table.
#' @param params A [synthona_params()] object.
#'
#' @return The edge table, with any scenario ties added.
#' @keywords internal
apply_scenario_edge_extras <- function(nodes, edges, params) {
  sid <- params$scenario_id %||% NA_character_
  if (is.na(sid)) return(edges)
  seed <- derive_seed(params$seed_attributes, paste0("extras:edges:", sid))

  if (identical(sid, "AI_M") && "tool_interaction" %in% params$layers) {
    human <- !nodes$is_non_human
    cut <- stats::quantile(nodes$ai_adoption_score[human], 0.70, na.rm = TRUE)
    users <- nodes$person_id[human & nodes$ai_adoption_score >= cut]
    agents <- nodes$person_id[nodes$is_non_human]
    if (length(users) > 0 && length(agents) > 0) {
      tool <- with_local_seed(seed, {
        tibble::tibble(
          from = users,
          to = sample(agents, length(users), replace = TRUE),
          weight = round(stats::rbeta(length(users), 4, 2), 3),
          layer = "tool_interaction",
          directed = TRUE,
          same_dept = FALSE,
          same_location = FALSE,
          same_legacy = NA
        )
      })
      edges <- bind_edge_tables(edges, tool)
    }
  }

  if (identical(sid, "CULTURE_M") && "energy" %in% params$layers) {
    seed_group <- nodes[nodes$community_seed %||% FALSE, , drop = FALSE]
    edges <- bind_edge_tables(edges, add_unique_edges(
      seed_group,
      n_edges = max(10L, round(nrow(nodes) * 0.03)),
      layer = "energy",
      seed = seed,
      same_dept_bias = 0.45
    ))
  }

  if (identical(sid, "MA_M") && "innovation" %in% params$layers) {
    edges <- bind_edge_tables(edges, add_unique_edges(
      nodes,
      n_edges = max(12L, round(nrow(nodes) * 0.02)),
      layer = "innovation",
      seed = seed,
      same_dept_bias = 0.40,
      same_legacy_bias = -0.20
    ))
  }

  compact_edges(coerce_edge_schema(edges))
}

#' Scenario-specific metrics
#'
#' The measures a particular benchmark is judged on, beyond the network,
#' group and actor metrics every dataset carries: adoption by team for the AI
#' rollout, integration progress for the merger, span of control for the
#' restructuring, the cost of losing the top broker for the succession case.
#'
#' @param dataset A `synthona_dataset`.
#' @param snapshot Snapshot to measure. Defaults to the first.
#'
#' @return A named list, empty for scenarios that define no extra metrics.
#' @export
#' @examples
#' d <- synthona_generate(synthona_scenario("MA_M", n = 200))
#' names(scenario_metrics(d))
scenario_metrics <- function(dataset, snapshot = NULL) {
  stopifnot(inherits(dataset, "synthona_dataset"))
  params <- dataset$params
  sid <- params$scenario_id %||% NA_character_
  if (is.na(sid)) return(list())

  snapshot <- snapshot %||% params$snapshots[1]
  nodes <- dataset$nodes
  edges <- dataset$edges[dataset$edges$snapshot == snapshot, , drop = FALSE]
  out <- list()

  if (identical(sid, "AI_M")) {
    out$team_adoption <- nodes |>
      dplyr::group_by(.data$department) |>
      dplyr::summarise(
        adoption_velocity = safe_round(safe_mean(.data$ai_adoption_score), 3),
        champion_count = sum(.data$adoption_champion, na.rm = TRUE),
        manager_enablement = safe_round(safe_mean(.data$manager_enablement_score), 3),
        .groups = "drop"
      )
  }

  if (identical(sid, "MA_M")) {
    cross <- !is.na(edges$same_legacy) & !edges$same_legacy
    trust_cross <- safe_mean(edges$weight[edges$layer == "trust" & cross])
    comm_cross <- safe_mean(edges$weight[edges$layer == "communication" & cross])
    out$integration <- list(
      cross_legacy_tie_ratio = safe_round(safe_mean(as.numeric(cross)), 3),
      trust_lag_vs_communication = safe_round(comm_cross - trust_cross, 3)
    )
  }

  if (identical(sid, "RESIZE_M")) {
    reporting <- edges[edges$layer == "reporting", , drop = FALSE]
    spans <- table(reporting$from)
    out$manager_spans <- tibble::tibble(
      person_id = names(spans),
      manager_span = as.integer(spans)
    )
  }

  if (identical(sid, "SUCCESSION_M")) {
    g <- graph_from_edges(nodes, edges, snapshot = snapshot)
    b <- igraph::betweenness(g, directed = FALSE, normalized = TRUE)
    top <- igraph::V(g)$name[which.max(b)]
    g2 <- igraph::delete_vertices(g, top)
    comp <- igraph::components(g2)
    out$node_removal <- list(
      removed_person_id = top,
      components_after_removal = comp$no,
      giant_component_share_after_removal =
        round(max(comp$csize) / igraph::vcount(g2), 3)
    )
  }

  if (identical(sid, "CULTURE_M")) {
    g <- graph_from_edges(nodes, edges, snapshot = snapshot)
    out$culture <- list(
      small_world_sigma = small_world_sigma(g),
      cross_boundary_trust = safe_round(
        safe_mean(edges$weight[edges$layer == "trust" & !edges$same_dept]), 3
      )
    )
  }

  out
}

#' Scenario-specific acceptance checks
#'
#' The checks a scenario has to pass to be the benchmark it claims to be,
#' appended to the generic validation report by [validate_dataset()].
#'
#' @param dataset A `synthona_dataset`.
#' @param snapshot Snapshot to check.
#'
#' @return A validation tibble, with zero rows for scenarios that add none.
#' @keywords internal
scenario_validation_checks <- function(dataset, snapshot = NULL) {
  params <- dataset$params
  sid <- params$scenario_id %||% NA_character_
  if (is.na(sid)) return(NULL)

  snapshot <- snapshot %||% params$snapshots[1]
  nodes <- dataset$nodes
  edges <- dataset$edges[dataset$edges$snapshot == snapshot, , drop = FALSE]
  checks <- list()

  if (identical(sid, "AI_M")) {
    tool_ties <- sum(edges$layer == "tool_interaction")
    champions <- sum(nodes$adoption_champion %||% FALSE, na.rm = TRUE)
    checks <- c(checks, list(
      validation_row(
        "tool_layer", "Tool interaction layer produced ties", "> 0",
        tool_ties, tool_ties > 0
      ),
      validation_row(
        "adoption_champions", "Adoption champions identified", ">= 1",
        champions, champions > 0
      )
    ))
  }

  if (identical(sid, "MA_M")) {
    has_legacy <- "legacy_company" %in% names(nodes)
    cross_ratio <- safe_mean(as.numeric(!is.na(edges$same_legacy) & !edges$same_legacy))
    checks <- c(checks, list(
      validation_row(
        "legacy_attr", "Legacy origin recorded on every actor", "present",
        if (has_legacy) "present" else "absent", has_legacy
      ),
      validation_row(
        "cross_legacy_ratio", "The two legacy organisations are connected", "> 0.05",
        safe_round(cross_ratio, 3),
        isTRUE(is.finite(cross_ratio) && cross_ratio > 0.05)
      )
    ))
  }

  if (identical(sid, "CULTURE_M")) {
    seeded <- sum(nodes$community_seed %||% FALSE, na.rm = TRUE)
    checks <- c(checks, list(validation_row(
      "culture_seed_group", "A seed group carries the programme", ">= 1",
      seeded, seeded > 0
    )))
  }

  if (identical(sid, "RESIZE_M")) {
    at_risk <- sum(nodes$at_risk_role %||% FALSE, na.rm = TRUE)
    checks <- c(checks, list(validation_row(
      "at_risk_roles", "Exposed roles identified", ">= 1",
      at_risk, at_risk > 0
    )))
  }

  if (identical(sid, "SUCCESSION_M")) {
    candidates <- sum(nodes$key_person_candidate %||% FALSE, na.rm = TRUE)
    checks <- c(checks, list(validation_row(
      "key_person_candidates", "Succession candidates identified", ">= 1",
      candidates, candidates > 0
    )))
  }

  if (length(checks) == 0) return(NULL)
  dplyr::bind_rows(checks)
}
