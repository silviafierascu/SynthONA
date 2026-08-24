# Attribute generation --------------------------------------------------------

# Distribution settings shared by every template. These are deliberately
# separate from the template definitions: templates describe *who works here*
# (departments, locations, ladder), while these describe *how attributes are
# distributed* within any workforce.
attribute_config <- function() {
  list(
    tenure_meanlog = 1.1,
    tenure_sdlog = 0.7,
    performance_mean = 3.2,
    performance_sd = 0.6,
    newcomer_threshold = 1.0,
    cohort_span = 8L,
    female_share = 0.46
  )
}

#' Generate a synthetic workforce
#'
#' Draws employee attributes for one organisation from the industry template
#' named in `params`. All randomness is drawn from named sub-streams of
#' `params$seed_attributes`, so the workforce is reproducible independently of
#' the network structure built on top of it.
#'
#' @param params A [synthona_params()] object.
#'
#' @return A tibble with one row per employee.
#' @export
#' @examples
#' nodes <- generate_profiles(synthona_params(n = 50))
#' nrow(nodes)
#' table(nodes$role_bucket)
generate_profiles <- function(params) {
  stopifnot(inherits(params, "synthona_params"))
  tpl <- get_template(params$template)
  cfg <- attribute_config()
  n <- params$n
  base <- params$seed_attributes

  n_levels <- length(tpl$seniority_levels)
  mgr_min <- tpl$manager_level_min

  department <- with_local_seed(
    derive_seed(base, "attr:department"),
    sample(tpl$departments, n, replace = TRUE, prob = tpl$dept_weights)
  )
  location <- with_local_seed(
    derive_seed(base, "attr:location"),
    sample(tpl$locations, n, replace = TRUE, prob = tpl$location_weights)
  )
  seniority <- with_local_seed(
    derive_seed(base, "attr:seniority"),
    sample(tpl$seniority_levels, n, replace = TRUE, prob = tpl$seniority_weights)
  )
  tenure_years <- with_local_seed(
    derive_seed(base, "attr:tenure"),
    round(pmax(0.1, stats::rlnorm(n, cfg$tenure_meanlog, cfg$tenure_sdlog)), 1)
  )
  performance <- with_local_seed(
    derive_seed(base, "attr:performance"),
    round(soft_clip(stats::rnorm(n, cfg$performance_mean, cfg$performance_sd), 1, 5), 2)
  )
  gender <- with_local_seed(
    derive_seed(base, "attr:gender"),
    sample(c("female", "male"), n,
      replace = TRUE,
      prob = c(cfg$female_share, 1 - cfg$female_share)
    )
  )

  level <- as.integer(setNames(seq_len(n_levels), tpl$seniority_levels)[seniority])
  level <- pmax(1L, pmin(n_levels, level))

  # Hire cohort follows tenure rather than being drawn independently, so the
  # two attributes stay mutually consistent.
  this_year <- 2026L
  hire_cohort <- as.integer(pmax(this_year - cfg$cohort_span, this_year - ceiling(tenure_years)))

  role_bucket <- dplyr::case_when(
    level >= n_levels ~ "executive",
    level >= n_levels - 1L ~ "senior_manager",
    level >= mgr_min ~ "manager",
    .default = "individual"
  )

  ai_adoption <- with_local_seed(derive_seed(base, "attr:ai_adoption"), {
    boost <- ifelse(department %in% c("Engineering", "IT", "Product", "Strategy"), 0.12, 0) +
      ifelse(role_bucket %in% c("manager", "senior_manager", "executive"), 0.08, 0)
    round(soft_clip(stats::rbeta(n, 2.5, 3.5) + boost, 0.01, 0.99), 3)
  })
  change_readiness <- with_local_seed(
    derive_seed(base, "attr:change_readiness"),
    round(soft_clip(
      stats::rbeta(n, 3, 2) + ifelse(tenure_years < 2, 0.08, -0.03), 0.01, 0.99
    ), 3)
  )
  attrition_risk <- with_local_seed(
    derive_seed(base, "attr:attrition_risk"),
    round(soft_clip(
      stats::rbeta(n, 2, 5) +
        ifelse(tenure_years < 1.5, 0.10, 0) +
        ifelse(performance > 4.4, 0.05, 0),
      0.01, 0.99
    ), 3)
  )

  nodes <- tibble::tibble(
    person_id = sprintf("p_%05d", seq_len(n)),
    display_name = paste("Person", seq_len(n)),
    department = department,
    seniority_level = seniority,
    level = level,
    role_bucket = role_bucket,
    location = location,
    tenure_years = tenure_years,
    performance_score = performance,
    is_newcomer = tenure_years < cfg$newcomer_threshold,
    hire_cohort = hire_cohort,
    gender = gender,
    ai_adoption_score = ai_adoption,
    change_readiness = change_readiness,
    attrition_risk = attrition_risk,
    is_non_human = FALSE,
    template_id = tpl$id,
    scenario_id = params$scenario_id %||% NA_character_
  )

  stopifnot(
    "row count mismatch" = nrow(nodes) == n,
    "performance out of range" = all(nodes$performance_score >= 1 & nodes$performance_score <= 5),
    "non-positive tenure" = all(nodes$tenure_years > 0),
    "duplicate person_id" = !anyDuplicated(nodes$person_id)
  )

  nodes
}

#' Append non-human actors to a workforce
#'
#' Adds AI agents, bots or shared service accounts as network participants.
#' They are flagged with `is_non_human` and given a department and location
#' drawn from the template in use, so that group-level metrics do not acquire
#' a spurious singleton category.
#'
#' @param nodes A node table from [generate_profiles()].
#' @param count Number of non-human actors to add.
#' @param params The [synthona_params()] used to generate `nodes`.
#' @param prefix Identifier prefix for the added actors.
#'
#' @return `nodes` with the additional actors appended.
#' @export
add_non_human_actors <- function(nodes, count = 4L, params = NULL, prefix = "agent") {
  if (count <= 0) {
    return(nodes)
  }
  tpl <- if (!is.null(params)) get_template(params$template) else get_template(nodes$template_id[1])
  seed <- if (!is.null(params)) params$seed_attributes else SEED_ATTRIBUTE_BASE

  # Agents are deployed where the work is: departments are drawn in proportion
  # to headcount rather than placed in a category of their own.
  placement <- with_local_seed(derive_seed(seed, "attr:agent_placement"), {
    list(
      department = sample(tpl$departments, count, replace = TRUE, prob = tpl$dept_weights),
      location = sample(tpl$locations, count, replace = TRUE, prob = tpl$location_weights)
    )
  })

  extra <- tibble::tibble(
    person_id = sprintf("%s_%03d", prefix, seq_len(count)),
    display_name = paste("Agent", seq_len(count)),
    department = placement$department,
    seniority_level = NA_character_,
    level = NA_integer_,
    role_bucket = "system",
    location = placement$location,
    tenure_years = 0.1,
    performance_score = NA_real_,
    is_newcomer = FALSE,
    hire_cohort = max(nodes$hire_cohort, na.rm = TRUE),
    gender = NA_character_,
    ai_adoption_score = 1,
    change_readiness = 1,
    attrition_risk = 0,
    is_non_human = TRUE,
    template_id = nodes$template_id[1],
    scenario_id = nodes$scenario_id[1]
  )

  dplyr::bind_rows(nodes, extra[, names(nodes), drop = FALSE])
}
