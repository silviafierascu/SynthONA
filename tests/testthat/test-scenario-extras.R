# The scenario extras were lost once already: the packaged registry dropped
# layers, and the derived attributes and the ties they imply were not ported
# at all, which left AI_M's agents barely connected and CULTURE_M's seed group
# doing nothing. These tests pin the parts a general layer rule cannot produce.

expected_extras <- list(
  AI_M         = c("adoption_champion", "manager_enablement_score"),
  CULTURE_M    = "community_seed",
  MA_M         = c("legacy_company", "integration_readiness"),
  RESIZE_M     = "at_risk_role",
  SUCCESSION_M = "key_person_candidate"
)

test_that("each scenario carries the attributes its catalog entry promises", {
  for (id in names(expected_extras)) {
    d <- synthona_generate(synthona_scenario(id, n = 200))
    expect_true(
      all(expected_extras[[id]] %in% names(d$nodes)),
      info = paste(id, "is missing:",
                   paste(setdiff(expected_extras[[id]], names(d$nodes)), collapse = ", "))
    )
  }
})

test_that("a specification with no scenario identifier gains no extra columns", {
  plain <- synthona_generate(synthona_params(n = 120))
  expect_false(any(unlist(expected_extras) %in% names(plain$nodes)))
})

test_that("AI_M wires its keen adopters to the agents", {
  d <- synthona_generate(synthona_scenario("AI_M", n = 400))
  agents <- d$nodes$person_id[d$nodes$is_non_human]
  first <- d$edges[d$edges$snapshot == d$params$snapshots[1], ]
  tool <- first[first$layer == "tool_interaction", ]
  touching <- sum(tool$from %in% agents | tool$to %in% agents)

  expect_gt(length(agents), 0)
  # Roughly the top 30% of humans reach for the tool. Without the scenario
  # edge extras this collapsed to whatever the base topology happened to give,
  # which was an order of magnitude fewer.
  expect_gt(touching, 0.15 * sum(!d$nodes$is_non_human))
})

test_that("CULTURE_M's seed group carries the energy layer", {
  d <- synthona_generate(synthona_scenario("CULTURE_M", n = 300))
  first <- d$edges[d$edges$snapshot == d$params$snapshots[1], ]
  energy <- first[first$layer == "energy", ]
  seeds <- d$nodes$person_id[d$nodes$community_seed]

  expect_gt(nrow(energy), 0)
  seed_share <- mean(energy$from %in% seeds | energy$to %in% seeds)
  expect_gt(seed_share, mean(d$nodes$community_seed))
})

test_that("scenario metrics are produced for the scenarios that define them", {
  expect_named(scenario_metrics(synthona_generate(synthona_scenario("AI_M", n = 200))),
               "team_adoption")
  expect_named(scenario_metrics(synthona_generate(synthona_scenario("MA_M", n = 200))),
               "integration")
  expect_identical(
    scenario_metrics(synthona_generate(synthona_params(n = 100))),
    list()
  )
})

test_that("scenario validation checks reach the report", {
  d <- synthona_generate(synthona_scenario("AI_M", n = 200))
  ids <- validate_dataset(d)$check_id
  expect_true(all(c("tool_layer", "adoption_champions") %in% ids))
})

test_that("the extras are reproducible and leave the session RNG alone", {
  set.seed(1234)
  before <- .Random.seed
  a <- synthona_generate(synthona_scenario("AI_M", n = 200))
  expect_identical(before, .Random.seed)

  b <- synthona_generate(synthona_scenario("AI_M", n = 200))
  expect_identical(a$nodes$manager_enablement_score, b$nodes$manager_enablement_score)
  expect_identical(a$nodes$adoption_champion, b$nodes$adoption_champion)
  expect_identical(a$edges, b$edges)
})
