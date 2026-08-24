# The protocol document offers two ways to run: calling the package, or
# sourcing the standalone script. If those two ever diverge, a reader running
# the document would get different results from a reader using the package,
# and neither would know. The standalone is generated from R/, and this test
# holds the shipped copy to that guarantee.

standalone_path <- function() {
  p <- system.file("standalone", "synthona-standalone.R", package = "SynthONA")
  if (!nzchar(p)) {
    p <- testthat::test_path("..", "..", "inst", "standalone", "synthona-standalone.R")
  }
  p
}

test_that("the standalone script is shipped and loads cleanly", {
  path <- standalone_path()
  skip_if_not(file.exists(path), "standalone script not built")

  env <- new.env(parent = globalenv())
  expect_silent(sys.source(path, envir = env))
  expect_true(is.function(env$synthona_generate))
  expect_true(is.function(env$synthona_params))
  expect_true(is.function(env$synthona_observe))
})

test_that("the standalone reproduces the package exactly", {
  skip_on_cran()
  path <- standalone_path()
  skip_if_not(file.exists(path), "standalone script not built")

  env <- new.env(parent = globalenv())
  sys.source(path, envir = env)

  for (topology in c("sbm", "hierarchy", "ba")) {
    args <- list(
      n = 200, topology = topology, mean_degree = 12, within_share = 0.75,
      layers = c("communication", "trust")
    )
    from_pkg <- synthona_generate(do.call(synthona_params, args))
    from_alone <- env$synthona_generate(do.call(env$synthona_params, args))

    expect_identical(from_pkg$nodes, from_alone$nodes, info = topology)
    expect_identical(from_pkg$edges, from_alone$edges, info = topology)
    expect_identical(from_pkg$truth$brokers, from_alone$truth$brokers, info = topology)
    expect_identical(
      from_pkg$truth$communities, from_alone$truth$communities,
      info = topology
    )
  }
})

test_that("the standalone exposes every exported function", {
  path <- standalone_path()
  skip_if_not(file.exists(path), "standalone script not built")

  env <- new.env(parent = globalenv())
  sys.source(path, envir = env)

  exported <- getNamespaceExports("SynthONA")
  # S3 methods are dispatched, not called directly.
  exported <- exported[!grepl("^(print|summary)\\.", exported)]
  missing <- exported[!vapply(exported, exists, logical(1), envir = env, inherits = FALSE)]
  expect_equal(missing, character(0))
})

test_that("the standalone carries the same scenario registry", {
  path <- standalone_path()
  skip_if_not(file.exists(path), "standalone script not built")

  env <- new.env(parent = globalenv())
  sys.source(path, envir = env)

  # The equivalence test above passes explicit parameters, so it cannot see a
  # stale registry baked into the shipped script. Compare the registries too:
  # a scenario that silently loses a layer produces a different dataset under
  # the same identifier.
  expect_identical(env$synthona_registry(), synthona_registry())
})
