test_that("parameters are validated", {
  expect_error(synthona_params(n = 5), "at least 20")
  expect_error(synthona_params(topology = "nope"), "must be one of")
  expect_error(synthona_params(mean_degree = -1), "positive")
  expect_error(synthona_params(n = 50, mean_degree = 60), "smaller than")
  expect_error(synthona_params(within_share = 1.5), "between 0 and 1")
  expect_error(synthona_params(layers = "gossip"), "Unknown layer")
  expect_error(synthona_params(snapshots = c("a", "a")), "duplicates")
  expect_error(synthona_params(template = "nope"), "Unknown template")
})

test_that("mean degree stays flat as the organisation grows", {
  # The central design claim: tie volume is calibrated per person, so datasets
  # of different sizes stay comparable instead of density being held constant
  # and degree growing with headcount.
  degrees <- vapply(c(200L, 800L, 2000L), function(n) {
    p <- synthona_params(n = n, topology = "er", mean_degree = 12)
    g <- generate_base_structure(generate_profiles(p), p)$graph
    mean(igraph::degree(g))
  }, numeric(1))

  expect_true(all(abs(degrees - 12) / 12 < 0.10))
  # Density must fall roughly as 1/n over the same range.
  expect_lt(max(degrees) / min(degrees), 1.25)
})

test_that("within_share controls how siloed the organisation is", {
  shares <- c(0.40, 0.90)
  realised <- vapply(shares, function(s) {
    p <- synthona_params(n = 400, topology = "sbm", mean_degree = 12, within_share = s)
    d <- synthona_generate(p)
    mean(d$edges$same_dept, na.rm = TRUE)
  }, numeric(1))

  expect_lt(realised[1], realised[2])
  expect_true(all(abs(realised - shares) < 0.2))
})

test_that("the same specification always reproduces the same dataset", {
  p <- synthona_params(n = 150, topology = "sbm")
  a <- synthona_generate(p)
  b <- synthona_generate(p)
  expect_identical(a$edges, b$edges)
  expect_identical(a$nodes, b$nodes)
  expect_identical(a$truth$brokers, b$truth$brokers)
})

test_that("attribute and topology seeds vary independently", {
  base <- synthona_params(n = 200, topology = "sbm")
  same_people <- synthona_generate(update_params(base, seed_topology = 999L))
  original <- synthona_generate(base)

  # Same workforce, different structure.
  expect_identical(original$nodes$department, same_people$nodes$department)
  expect_false(identical(original$edges, same_people$edges))
})

test_that("adding a layer does not change the ties of existing layers", {
  # Layer seeds are derived from the layer name rather than its position, so
  # extending a specification leaves earlier layers untouched.
  two <- synthona_generate(synthona_params(n = 200, layers = c("communication", "trust")))
  three <- synthona_generate(
    synthona_params(n = 200, layers = c("communication", "trust", "innovation"))
  )
  pick <- function(d, l) {
    e <- d$edges[d$edges$layer == l, c("from", "to", "weight")]
    e[order(e$from, e$to), ]
  }
  expect_equal(pick(two, "communication"), pick(three, "communication"))
  expect_equal(pick(two, "trust"), pick(three, "trust"))
})

test_that("every registered scenario generates and validates", {
  skip_on_cran()
  for (id in names(synthona_registry())) {
    d <- synthona_generate(synthona_scenario(id, n = 150))
    v <- validate_dataset(d)
    expect_true(all(v$pass), info = paste(id, ":", paste(v$check_id[!v$pass], collapse = ", ")))
  }
})

test_that("directed layers are oriented and undirected ones are not", {
  d <- synthona_generate(
    synthona_params(n = 200, layers = c("communication", "advice", "mentorship"))
  )
  expect_true(all(d$edges$directed[d$edges$layer == "advice"]))
  expect_true(all(d$edges$directed[d$edges$layer == "mentorship"]))
  expect_false(any(d$edges$directed[d$edges$layer == "communication"]))
})

test_that("edge tables always satisfy the schema contract", {
  d <- synthona_generate(synthona_params(n = 100))
  expect_true(all(EDGE_SCHEMA_COLS %in% names(d$edges)))
  expect_type(d$edges$from, "character")
  expect_type(d$edges$weight, "double")
  expect_false(any(d$edges$from == d$edges$to))
  expect_false(anyNA(d$edges$weight))
})
