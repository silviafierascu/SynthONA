# Regression tests for defects found in the prototype implementation.
# Each test names the failure mode it guards against, so that a future change
# that reintroduces it fails loudly rather than silently degrading the data.

test_that("SBM blocks line up with departments", {
  # The prototype attached attributes positionally to a graph whose blocks are
  # assigned contiguously, which scattered each planted community across the
  # organisation and drove department assortativity to zero.
  p <- synthona_params(n = 400, topology = "sbm", mean_degree = 12, within_share = 0.85)
  base <- generate_base_structure(generate_profiles(p), p)
  g <- base$graph

  assortativity <- igraph::assortativity_nominal(
    g, as.integer(factor(igraph::V(g)$department)), directed = FALSE
  )
  expect_gt(assortativity, 0.4)

  el <- igraph::as_edgelist(g, names = FALSE)
  dept <- igraph::V(g)$department
  expect_gt(mean(dept[el[, 1]] == dept[el[, 2]]), 0.6)
})

test_that("undirected projection does not turn reversed pairs into self-loops", {
  # Assigning `from` and then deriving `to` from the updated value collapses
  # every pair with from > to onto a self-loop, losing roughly half the ties.
  ed <- tibble::tibble(
    from = c("p_005", "p_002", "p_009", "p_001"),
    to = c("p_002", "p_007", "p_003", "p_004"),
    weight = 1, layer = "communication"
  )
  out <- collapse_edge_projection(ed, directed = FALSE)

  expect_equal(nrow(out), 4L)
  expect_false(any(out$from == out$to))
  expect_true(all(out$from < out$to))
})

test_that("layer_keep_prob falls back instead of erroring on an unknown layer", {
  # `[[` raises a subscript error on a missing name, which made the documented
  # default unreachable.
  expect_equal(layer_keep_prob("communication"), 0.85)
  expect_equal(layer_keep_prob("not_a_layer"), 0.50)
})

test_that("generation never disturbs the RNG state of the caller", {
  set.seed(12345)
  before <- .Random.seed
  invisible(synthona_generate(synthona_params(n = 60)))
  expect_identical(before, .Random.seed)
})

test_that("outcomes are stable across repeated calls", {
  # The prototype drew outcome noise without a seed, so the same dataset
  # produced different engagement scores on every call.
  d <- synthona_generate(synthona_params(n = 80))
  expect_identical(
    compute_outcomes(d$nodes, d$edges, d$params),
    compute_outcomes(d$nodes, d$edges, d$params)
  )
})

test_that("cumulative snapshots evolve rather than re-perturbing the baseline", {
  p <- synthona_params(
    n = 200, snapshots = c("t1", "t2", "t3"), snapshot_mode = "cumulative",
    extras = list(evolution = list(churn = 0.15, renewal = FALSE))
  )
  d <- synthona_generate(p)
  counts <- vapply(
    c("t1", "t2", "t3"),
    function(s) sum(d$edges$snapshot == s), integer(1)
  )
  # Without renewal, tie count must fall monotonically: a wave that re-derived
  # from the baseline each time would fluctuate instead.
  expect_true(all(diff(counts) < 0))
})

test_that("tie strength is inverted before being used as a path distance", {
  # igraph reads weights as distances, so passing strength directly ranks the
  # weakest ties as the most efficient routes.
  g <- igraph::make_ring(4)
  igraph::E(g)$weight <- c(1, 0.5, 0.25, 0.1)
  expect_equal(tie_distance(g), 1 / c(1, 0.5, 0.25, 0.1))
})

test_that("path length treats tie strength as strength, not distance", {
  # igraph::mean_distance() picks up the `weight` edge attribute and reads it
  # as distance unless told otherwise. Tie strength means the opposite, so a
  # bare call produced mean path lengths below 1 -- impossible in a simple
  # graph, and the signature of this bug in the published datasets.
  d <- synthona_generate(synthona_params(
    n = 120, topology = "er", mean_degree = 6, layers = "communication",
    seed_topology = 42
  ))
  m <- compute_network_metrics(d$nodes, d$edges, n_random = 0L)
  g <- synthona_graph(d)
  unweighted <- igraph::mean_distance(g, directed = FALSE, weights = NA)

  expect_gt(m$mean_path_length, 1)
  # Weights are reciprocated, and every tie strength is below 1, so every hop
  # costs more than one unit of distance.
  expect_gt(m$mean_path_length, unweighted)
})

test_that("the small-world coefficient compares like with like", {
  # Sigma divides observed path length by that of unweighted random reference
  # graphs. Measuring the observed side weighted made every topology look
  # small-world, including Erdos-Renyi, whose sigma is 1 by construction.
  er <- synthona_generate(synthona_params(
    n = 150, topology = "er", mean_degree = 6, layers = "communication",
    seed_topology = 42
  ))
  ws <- synthona_generate(synthona_params(
    n = 150, topology = "ws", mean_degree = 6, layers = "communication",
    seed_topology = 42
  ))

  sigma_er <- compute_network_metrics(er$nodes, er$edges, n_random = 20L)$small_world_sigma
  sigma_ws <- compute_network_metrics(ws$nodes, ws$edges, n_random = 20L)$small_world_sigma

  expect_lt(sigma_er, 1.5)
  expect_gt(sigma_ws, sigma_er)
})
