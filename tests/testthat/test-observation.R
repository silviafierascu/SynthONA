test_that("observation designs are validated", {
  expect_error(observation_design(response_rate = 1.4), "between 0 and 1")
  expect_error(observation_design(name_generator_limit = 0), "at least 1")
  expect_error(observation_design(recall_probability = -0.1), "between 0 and 1")
})

test_that("a census observation leaves the network intact", {
  d <- synthona_generate(synthona_params(n = 150))
  obs <- synthona_observe(d, observation_design())
  expect_equal(nrow(obs$edges), nrow(d$edges))
})

test_that("lower response rates record fewer ties", {
  d <- synthona_generate(synthona_params(n = 300))
  counts <- vapply(c(1, 0.7, 0.4), function(r) {
    nrow(synthona_observe(d, observation_design(response_rate = r))$edges)
  }, integer(1))
  expect_true(all(diff(counts) < 0))
})

test_that("the name generator caps how many alters a respondent lists", {
  d <- synthona_generate(synthona_params(n = 200, layers = "communication"))
  obs <- synthona_observe(d, observation_design(name_generator_limit = 4))
  per_ego <- table(obs$edges$from)
  expect_lte(max(per_ego), 4L)
})

test_that("the name generator keeps the strongest ties", {
  d <- synthona_generate(synthona_params(n = 200, layers = "communication"))
  obs <- synthona_observe(d, observation_design(name_generator_limit = 3))
  expect_gt(mean(obs$edges$weight), mean(d$edges$weight))
})

test_that("observation preserves ground truth unchanged", {
  # The point of separating observation from generation: the answer key must
  # not degrade along with the data.
  d <- synthona_generate(synthona_params(n = 200))
  obs <- synthona_observe(d, observation_design(response_rate = 0.4))
  expect_identical(obs$truth$communities, d$truth$communities)
  expect_identical(obs$truth$brokers, d$truth$brokers)
  expect_true(obs$is_observed)
  expect_false(d$is_observed)
})

test_that("measurement error degrades community recovery", {
  skip_on_cran()
  d <- synthona_generate(
    synthona_params(n = 400, topology = "sbm", mean_degree = 12, within_share = 0.85)
  )
  score <- function(design) {
    obs <- synthona_observe(d, design)
    g <- synthona_graph(obs)
    score_communities(d$truth, igraph::cluster_louvain(g))$ari
  }
  full <- score(observation_design())
  sparse <- score(observation_design(
    response_rate = 0.3, name_generator_limit = 3, recall_probability = 0.6
  ))
  expect_gt(full, sparse)
})

test_that("scoring helpers behave at the extremes", {
  d <- synthona_generate(synthona_params(n = 150, topology = "sbm"))

  perfect <- score_communities(d$truth, d$truth$communities)
  expect_equal(perfect$ari, 1)
  expect_equal(perfect$nmi, 1)

  exact <- score_brokers(d$truth, d$truth$brokers)
  expect_equal(exact$precision, 1)
  expect_equal(exact$recall, 1)

  none <- score_brokers(d$truth, character())
  expect_equal(none$true_positives, 0L)
})
