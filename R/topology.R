# Base topology generation ----------------------------------------------------

#' Attach a node table to a graph as vertex attributes
#'
#' The node table must be in the same order as the graph vertices. Every
#' generator in this package returns the node table it actually used, precisely
#' so that callers never have to assume the two are aligned.
#'
#' @param g An igraph graph.
#' @param nodes A node table with one row per vertex.
#' @return `g` with vertex attributes set.
#' @keywords internal
apply_vertex_attributes <- function(g, nodes) {
  stopifnot(igraph::vcount(g) == nrow(nodes))
  for (col in names(nodes)) {
    igraph::vertex_attr(g, col) <- nodes[[col]]
  }
  igraph::V(g)$name <- nodes$person_id
  g
}

# Degree calibration ----------------------------------------------------------
#
# Every generator below is parameterised on target mean degree. The helpers
# here translate a target degree into whatever quantity the underlying model
# needs (a tie probability, a neighbourhood size, an attachment parameter).

#' Tie probability giving a target mean degree
#'
#' @param n Number of nodes.
#' @param mean_degree Target mean degree.
#' @return A probability on `[0, 1]`.
#' @keywords internal
degree_to_probability <- function(n, mean_degree) {
  if (n < 2) {
    return(0)
  }
  soft_clip(mean_degree / (n - 1), 0, 1)
}

#' Stochastic block matrix giving a target mean degree and within-block share
#'
#' Solves for the within- and between-block tie probabilities that yield the
#' requested mean degree, splitting ties so that `within_share` of them fall
#' inside the block. Within-block probabilities are set per block, so that
#' large and small departments produce comparable per-person degree instead of
#' large departments dominating.
#'
#' @param block_sizes Integer vector of block sizes.
#' @param mean_degree Target mean degree.
#' @param within_share Share of ties expected within block.
#' @return A symmetric preference matrix.
#' @keywords internal
sbm_pref_matrix <- function(block_sizes, mean_degree, within_share, cohesion = NULL) {
  k <- length(block_sizes)
  n <- sum(block_sizes)
  stopifnot(k >= 1, n >= 2)

  cohesion <- cohesion %||% rep(1, k)
  # Normalise so the *organisation-wide* within-tie share still equals the
  # requested value; cohesion only redistributes it between departments.
  cohesion <- cohesion / stats::weighted.mean(cohesion, block_sizes)
  share <- soft_clip(within_share * cohesion, 0.02, 0.98)

  # Within: p_w = s_i * degree / (n_i - 1) gives each department its own
  # within-tie share while holding per-person degree comparable.
  p_within <- ifelse(block_sizes > 1, share * mean_degree / pmax(block_sizes - 1, 1), 0)

  # Between: a degree-corrected form, p_b[i,j] = base * o_i * o_j, where
  # o_i is how outward-facing department i is. This stays symmetric while
  # still letting departments differ in how much they reach outside.
  o <- 1 - share
  target_between <- sum(o * mean_degree * block_sizes) / 2
  on <- o * block_sizes
  denom <- (sum(on)^2 - sum(on^2)) / 2
  base <- if (denom > 0) target_between / denom else 0

  m <- outer(o, o) * base
  diag(m) <- p_within
  matrix(soft_clip(m, 0, 1), nrow = k, ncol = k)
}

#' Per-department cohesion multipliers
#'
#' Real organisations are not uniformly siloed: some units are inward-looking
#' and others act as connectors. Generating every department with identical
#' parameters makes the synthetic data easier than the real thing, because any
#' method that assumes homogeneous groups is never penalised for it.
#'
#' @param depts Character vector of department names, in sorted order.
#' @param params A [synthona_params()] object.
#' @return A named numeric vector of multipliers centred on 1.
#' @keywords internal
department_cohesion <- function(depts, params) {
  k <- length(depts)
  h <- params$extras$department_heterogeneity %||% 0.35
  if (h <= 0 || k < 2) {
    return(setNames(rep(1, k), depts))
  }
  setNames(
    with_local_seed(
      derive_seed(params$seed_attributes, "attr:department_cohesion"),
      stats::runif(k, 1 - h, 1 + h)
    ),
    depts
  )
}

# Generators ------------------------------------------------------------------

#' Generate the base organisational structure
#'
#' Dispatches on `params$topology` and returns both the graph and the node
#' table in the vertex order actually used, together with the ground truth the
#' structure was built from.
#'
#' @param nodes A node table from [generate_profiles()].
#' @param params A [synthona_params()] object.
#'
#' @return A list with `graph`, `nodes` and `truth`.
#' @export
#' @examples
#' p <- synthona_params(n = 120, topology = "sbm")
#' base <- generate_base_structure(generate_profiles(p), p)
#' igraph::vcount(base$graph)
#' base$truth$model
generate_base_structure <- function(nodes, params) {
  stopifnot(inherits(params, "synthona_params"))
  switch(params$topology,
    hierarchy = generate_hierarchy(nodes, params),
    sbm = generate_sbm(nodes, params, dual_legacy = FALSE),
    sbm_dual_legacy = generate_sbm(nodes, params, dual_legacy = TRUE),
    er = generate_random_graph(nodes, params),
    ws = generate_small_world(nodes, params),
    ba = generate_scale_free(nodes, params),
    stop("Unsupported topology: ", params$topology, call. = FALSE)
  )
}

#' @rdname generate_base_structure
#' @export
generate_random_graph <- function(nodes, params) {
  p <- degree_to_probability(nrow(nodes), params$mean_degree)
  g <- with_local_seed(
    derive_seed(params$seed_topology, "topo:er"),
    igraph::sample_gnp(nrow(nodes), p = p, directed = FALSE)
  )
  list(
    graph = apply_vertex_attributes(g, nodes),
    nodes = nodes,
    truth = list(model = "er", communities = NULL, tie_probability = p)
  )
}

#' @rdname generate_base_structure
#' @export
generate_small_world <- function(nodes, params) {
  nei <- max(1L, as.integer(round(params$mean_degree / 2)))
  g <- with_local_seed(derive_seed(params$seed_topology, "topo:ws"), {
    gg <- igraph::sample_smallworld(
      dim = 1, size = nrow(nodes), nei = nei,
      p = params$extras$rewire_prob %||% 0.10
    )
    igraph::simplify(gg)
  })
  list(
    graph = apply_vertex_attributes(g, nodes),
    nodes = nodes,
    truth = list(model = "ws", communities = NULL, neighbourhood = nei)
  )
}

#' @rdname generate_base_structure
#' @export
generate_scale_free <- function(nodes, params) {
  m <- max(1L, as.integer(round(params$mean_degree / 2)))
  boost <- ROLE_ATTACHMENT_BOOST[nodes$role_bucket]
  boost[is.na(boost)] <- 1

  # Seniority-weighted preferential attachment: the attachment kernel is
  # multiplied by a role weight, so hubs form where authority sits rather than
  # purely by arrival order.
  g <- with_local_seed(derive_seed(params$seed_topology, "topo:ba"), {
    igraph::sample_pa(
      n = nrow(nodes), m = m, power = params$extras$pa_power %||% 1.2,
      directed = FALSE, zero.appeal = 0.5
    )
  })

  # Reassign a share of the ties of low-attachment nodes toward high-attachment
  # nodes, which aligns the degree hierarchy with the seniority hierarchy.
  g <- with_local_seed(derive_seed(params$seed_topology, "topo:ba_rewire"), {
    rewire_toward(g, boost, share = 0.10)
  })

  list(
    graph = apply_vertex_attributes(igraph::simplify(g), nodes),
    nodes = nodes,
    truth = list(model = "ba", communities = NULL, attachment_m = m)
  )
}

rewire_toward <- function(g, boost, share = 0.10) {
  n_rewire <- round(igraph::ecount(g) * share)
  if (n_rewire < 1) {
    return(g)
  }
  low <- which(boost < stats::quantile(boost, 0.6))
  high <- which(boost >= stats::quantile(boost, 0.8))
  if (length(low) == 0 || length(high) == 0) {
    return(g)
  }
  for (i in seq_len(n_rewire)) {
    deg <- igraph::degree(g)
    candidates <- low[deg[low] > 1]
    if (length(candidates) == 0) break
    victim <- safe_sample(candidates, 1L)
    nbrs <- as.integer(igraph::neighbors(g, victim))
    if (length(nbrs) == 0) next
    drop <- igraph::get.edge.ids(g, c(victim, safe_sample(nbrs, 1L)))
    if (drop == 0) next
    target <- safe_sample(high, 1L, prob = boost[high])
    if (target == victim || igraph::are_adjacent(g, victim, target)) next
    g <- igraph::add_edges(igraph::delete_edges(g, drop), c(victim, target))
  }
  g
}

#' @rdname generate_base_structure
#' @param dual_legacy Whether to plant two legacy organisations, as in a
#'   post-merger integration.
#' @export
generate_sbm <- function(nodes, params, dual_legacy = FALSE) {
  # Blocks must line up with departments. igraph assigns SBM vertices to
  # blocks contiguously, so the node table is reordered to match before the
  # attributes are attached; otherwise the planted community structure lands
  # on a random permutation of the departments and disappears entirely.
  dept_levels <- sort(unique(nodes$department))
  ord <- order(match(nodes$department, dept_levels), nodes$person_id)
  nodes <- nodes[ord, , drop = FALSE]

  if (dual_legacy && !"legacy_company" %in% names(nodes)) {
    nodes <- assign_legacy_companies(nodes, params)
  }

  block_sizes <- as.integer(table(factor(nodes$department, levels = dept_levels)))

  # Two later steps change the tie count, so the block model is solved for a
  # target that anticipates them. The co-location overlay adds degree; the
  # cross-legacy thinning removes it. Calibrating first and modifying
  # afterwards is what pushes realised degree off target.
  overlay_frac <- params$extras$location_overlay %||% 0.12
  target_degree <- params$mean_degree * (1 - overlay_frac)

  if (dual_legacy) {
    rate <- params$extras$cross_legacy_thinning %||% 0.35
    p_a <- mean(nodes$legacy_company == "Legacy_A")
    expected_cross <- 2 * p_a * (1 - p_a)
    target_degree <- target_degree / max(1e-6, 1 - rate * expected_cross)
  }

  block <- sbm_pref_matrix(
    block_sizes, target_degree, params$within_share,
    cohesion = department_cohesion(dept_levels, params)
  )

  g <- with_local_seed(
    derive_seed(params$seed_topology, "topo:sbm"),
    igraph::sample_sbm(
      sum(block_sizes),
      pref.matrix = block, block.sizes = block_sizes,
      directed = FALSE, loops = FALSE
    )
  )
  g <- apply_vertex_attributes(g, nodes)

  overlay <- sample_location_overlay(nodes, params)
  if (nrow(overlay) > 0) {
    g <- igraph::simplify(igraph::add_edges(g, as.vector(t(as.matrix(overlay)))))
  }

  if (dual_legacy) {
    g <- thin_cross_legacy_ties(g, nodes, params)
  }

  list(
    graph = g,
    nodes = nodes,
    truth = list(
      model = if (dual_legacy) "sbm_dual_legacy" else "sbm",
      communities = setNames(nodes$department, nodes$person_id),
      block_sizes = setNames(block_sizes, dept_levels),
      pref_matrix = block
    )
  )
}

assign_legacy_companies <- function(nodes, params) {
  # Legacy origin is correlated with department rather than assigned by row
  # position: real mergers combine organisations with different functional
  # mixes, and an origin split that is orthogonal to every other attribute
  # makes integration analysis trivially easy.
  depts <- sort(unique(nodes$department))
  tilt <- with_local_seed(
    derive_seed(params$seed_attributes, "attr:legacy_tilt"),
    stats::runif(length(depts), 0.25, 0.75)
  )
  names(tilt) <- depts
  nodes$legacy_company <- with_local_seed(
    derive_seed(params$seed_attributes, "attr:legacy_company"),
    ifelse(stats::runif(nrow(nodes)) < tilt[nodes$department], "Legacy_A", "Legacy_B")
  )
  nodes
}

sample_location_overlay <- function(nodes, params) {
  # Co-location homophily, sampled directly rather than by enumerating all
  # O(n^2) pairs. Expected extra degree is a fraction of the target degree.
  extra_degree <- params$mean_degree * (params$extras$location_overlay %||% 0.12)
  n <- nrow(nodes)
  n_edges <- as.integer(round(extra_degree * n / 2))

  by_loc <- split(seq_len(n), nodes$location)
  by_loc <- by_loc[vapply(by_loc, length, integer(1)) >= 2]
  if (is.na(n_edges) || n_edges < 1 || length(by_loc) == 0) {
    return(no_pairs())
  }

  sizes <- vapply(by_loc, length, integer(1))
  out <- with_local_seed(derive_seed(params$seed_topology, "topo:location_overlay"), {
    which_loc <- sample(seq_along(by_loc), n_edges, replace = TRUE, prob = sizes)
    do.call(rbind, lapply(which_loc, function(i) sample(by_loc[[i]], 2L)))
  })
  tibble::tibble(from = out[, 1], to = out[, 2])
}

thin_cross_legacy_ties <- function(g, nodes, params) {
  with_local_seed(derive_seed(params$seed_topology, "topo:legacy_thinning"), {
    edf <- igraph::as_data_frame(g, what = "edges")
    if (nrow(edf) == 0) {
      return(g)
    }
    same <- nodes$legacy_company[match(edf$from, nodes$person_id)] ==
      nodes$legacy_company[match(edf$to, nodes$person_id)]
    cross <- which(!same)
    if (length(cross) == 0) {
      return(g)
    }
    rate <- params$extras$cross_legacy_thinning %||% 0.35
    drop <- cross[stats::runif(length(cross)) < rate]
    if (length(drop) == 0) {
      return(g)
    }
    igraph::delete_edges(g, igraph::E(g)[drop])
  })
}

#' @rdname generate_base_structure
#' @export
generate_hierarchy <- function(nodes, params) {
  tpl <- get_template(params$template)
  tree <- build_reporting_tree(nodes, params, tpl)
  nodes$manager_id <- unname(tree$manager[nodes$person_id])

  reporting <- tibble::tibble(
    from = unname(tree$manager[!is.na(tree$manager)]),
    to = names(tree$manager)[!is.na(tree$manager)]
  )
  g <- igraph::graph_from_data_frame(reporting, directed = FALSE, vertices = nodes)

  # The reporting tree supplies roughly one tie per person. The remaining tie
  # budget is split between within-department and cross-department informal
  # ties according to within_share.
  target_edges <- params$mean_degree * nrow(nodes) / 2
  remaining <- max(0, target_edges - igraph::ecount(g))

  cohesion <- department_cohesion(sort(unique(nodes$department)), params)
  within <- sample_within_department_ties(
    nodes, remaining * params$within_share, params,
    cohesion = cohesion
  )
  if (nrow(within) > 0) {
    g <- igraph::add_edges(g, as.vector(t(as.matrix(within))))
  }
  across <- sample_cross_department_ties(nodes, remaining * (1 - params$within_share), params)
  if (nrow(across) > 0) {
    g <- igraph::add_edges(g, as.vector(t(as.matrix(across))))
  }

  g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

  list(
    graph = apply_vertex_attributes(g, nodes),
    nodes = nodes,
    truth = list(
      model = "hierarchy",
      communities = setNames(nodes$department, nodes$person_id),
      manager = tree$manager,
      depth = tree$depth
    )
  )
}

build_reporting_tree <- function(nodes, params, tpl) {
  span <- params$extras$span_of_control %||% c(4L, 8L)

  with_local_seed(derive_seed(params$seed_topology, "topo:reporting"), {
    manager <- setNames(rep(NA_character_, nrow(nodes)), nodes$person_id)
    depth <- setNames(rep(NA_integer_, nrow(nodes)), nodes$person_id)

    for (dept in sort(unique(nodes$department))) {
      members <- nodes[nodes$department == dept, , drop = FALSE]
      members <- members[order(-members$level, -members$tenure_years, members$person_id), , drop = FALSE]
      root <- members$person_id[1]
      depth[root] <- 0L

      remaining <- setdiff(members$person_id, root)
      current <- root
      level_idx <- 1L

      while (length(remaining) > 0 && length(current) > 0) {
        next_level <- character()
        for (mgr in current) {
          if (length(remaining) == 0) break
          k <- min(length(remaining), sample(seq(span[1], span[2]), 1L))
          reports <- safe_sample(remaining, k)
          manager[reports] <- mgr
          depth[reports] <- level_idx
          remaining <- setdiff(remaining, reports)
          # Only staff at or above the template manager threshold take reports.
          can_manage <- members$level[match(reports, members$person_id)] >= tpl$manager_level_min
          next_level <- c(next_level, reports[!is.na(can_manage) & can_manage])
        }
        if (length(next_level) == 0 && length(remaining) > 0) {
          # No eligible managers left: attach the remainder to existing ones.
          eligible <- members$person_id[members$level >= tpl$manager_level_min]
          eligible <- unique(c(root, setdiff(eligible, remaining)))
          manager[remaining] <- safe_sample(eligible, length(remaining), replace = TRUE)
          depth[remaining] <- level_idx
          remaining <- character()
        }
        current <- next_level
        level_idx <- level_idx + 1L
      }
    }

    list(manager = manager, depth = depth)
  })
}

no_pairs <- function() tibble::tibble(from = integer(), to = integer())

#' Number of draws needed to obtain a target count of distinct pairs
#'
#' Sampling `k` pairs with replacement from `m` possibilities yields on average
#' `m * (1 - (1 - 1/m)^k)` distinct pairs. This inverts that relation, so that
#' de-duplicating afterwards leaves the requested number of ties.
#'
#' @param target Desired number of distinct pairs.
#' @param m Number of available pairs.
#' @return Number of draws to take.
#' @keywords internal
draws_for_distinct <- function(target, m) {
  target <- pmin(target, m)
  out <- ifelse(
    m <= 1 | target <= 0,
    target,
    log1p(-target / m) / log1p(-1 / m)
  )
  ifelse(is.finite(out), ceiling(out), target)
}

sample_within_department_ties <- function(nodes, n_edges, params, tag = "", cohesion = NULL) {
  idx_by_dept <- split(seq_len(nrow(nodes)), nodes$department)
  idx_by_dept <- idx_by_dept[vapply(idx_by_dept, length, integer(1)) >= 2]
  if (n_edges < 1 || length(idx_by_dept) == 0) {
    return(no_pairs())
  }

  sizes <- vapply(idx_by_dept, length, integer(1))
  # Allocate the budget in proportion to headcount, tilted by how cohesive
  # each department is, so per-person within-department degree is comparable
  # across sizes while still varying between units.
  tilt <- if (is.null(cohesion)) rep(1, length(sizes)) else cohesion[names(sizes)]
  tilt[is.na(tilt)] <- 1
  alloc <- round(normalise_prob(sizes * tilt) * n_edges)
  max_pairs <- sizes * (sizes - 1) / 2
  alloc <- pmin(alloc, max_pairs)
  # Pairs are drawn with replacement and de-duplicated later, so drawing
  # exactly `alloc` pairs yields fewer than `alloc` distinct ties. Invert the
  # collision curve to recover the target on average.
  alloc <- draws_for_distinct(alloc, max_pairs)

  pairs <- with_local_seed(derive_seed(params$seed_topology, paste0("topo:within_dept", tag)), {
    drawn <- purrr::imap(idx_by_dept, function(ids, nm) {
      k <- as.integer(alloc[[nm]])
      if (is.na(k) || k < 1) {
        NULL
      } else {
        t(vapply(seq_len(k), function(i) sample(ids, 2L), integer(2)))
      }
    })
    do.call(rbind, drawn[!vapply(drawn, is.null, logical(1))])
  })

  if (is.null(pairs) || nrow(pairs) == 0) {
    return(no_pairs())
  }
  tibble::tibble(from = pairs[, 1], to = pairs[, 2])
}

sample_cross_department_ties <- function(nodes, n_edges, params, tag = "") {
  n_edges <- as.integer(round(n_edges))
  n <- nrow(nodes)
  if (is.na(n_edges) || n_edges < 1 || length(unique(nodes$department)) < 2) {
    return(no_pairs())
  }

  # Cross-unit ties are held disproportionately by senior staff, so endpoints
  # are drawn with a seniority-weighted kernel.
  w <- ROLE_ATTACHMENT_BOOST[nodes$role_bucket]
  w[is.na(w)] <- 1
  w <- normalise_prob(w)

  drawn <- with_local_seed(derive_seed(params$seed_topology, paste0("topo:cross_dept", tag)), {
    keep_from <- integer(0)
    keep_to <- integer(0)
    attempts <- 0L
    while (length(keep_from) < n_edges && attempts < 20L) {
      need <- max(8L, (n_edges - length(keep_from)) * 2L)
      a <- sample.int(n, need, replace = TRUE, prob = w)
      b <- sample.int(n, need, replace = TRUE, prob = w)
      ok <- nodes$department[a] != nodes$department[b]
      keep_from <- c(keep_from, a[ok])
      keep_to <- c(keep_to, b[ok])
      attempts <- attempts + 1L
    }
    list(from = keep_from, to = keep_to)
  })

  if (length(drawn$from) == 0) {
    return(no_pairs())
  }
  take <- seq_len(min(n_edges, length(drawn$from)))
  tibble::tibble(from = drawn$from[take], to = drawn$to[take])
}
