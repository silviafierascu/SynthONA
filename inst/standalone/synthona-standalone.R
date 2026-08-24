# SynthONA - standalone implementation
#
# A Parameterised Protocol for Generating Synthetic Organisational
# Network Benchmark Datasets.
#
# Generated from the package sources, version 0.1.0.
# DO NOT EDIT BY HAND - edit R/ and re-run data-raw/make-standalone.R.
#
# This file provides the complete protocol without requiring the SynthONA
# package to be installed. It still needs these CRAN packages:
#
#   dplyr, igraph, jsonlite, purrr, tibble
#
# Usage:
#   source("synthona-standalone.R")
#   d <- synthona_generate(synthona_params(n = 300, topology = "sbm"))

.synthona_required <- c("dplyr", "igraph", "jsonlite", "purrr", "tibble")
.synthona_missing <- .synthona_required[
  !vapply(.synthona_required, requireNamespace, logical(1), quietly = TRUE)
]
if (length(.synthona_missing) > 0) {
  stop(
    "The standalone protocol needs these packages: ",
    paste(.synthona_missing, collapse = ", "),
    "\nInstall them with install.packages(c(\"",
    paste(.synthona_missing, collapse = "\", \""), "\"))",
    call. = FALSE
  )
}
rm(.synthona_required, .synthona_missing)


# ============================================================================
# constants.R
# ============================================================================

# Protocol constants ----------------------------------------------------------

SYNTHONA_LAYERS <- c(
  "communication", "collaboration", "advice", "trust", "innovation",
  "mentorship", "decision_influence", "reporting", "tool_interaction", "energy"
)

DIRECTED_LAYERS <- c(
  "advice", "decision_influence", "mentorship", "reporting", "tool_interaction"
)

# Share of the base structure each layer retains. Communication is close to the
# full observable structure; mentorship and tool interaction are sparse.
LAYER_KEEP_PROB <- c(
  communication      = 0.85,
  collaboration      = 0.70,
  advice             = 0.45,
  trust              = 0.54,
  innovation         = 0.35,
  mentorship         = 0.25,
  decision_influence = 0.30,
  reporting          = 1.00,
  tool_interaction   = 0.20,
  energy             = 0.30
)

BASELINE_MODELS <- c(
  "ER", "WS", "BA", "SBM_HEALTHY", "SBM_SILOS",
  "SBM_FRAGMENTED", "SBM_INNOVATION", "HIER_CORP"
)

SEED_TOPOLOGY_BASE  <- 47L
SEED_ATTRIBUTE_BASE <- 1047L

# Edge table column contract. Every edge table produced by the protocol
# carries exactly these columns, in this order, before any extras.
EDGE_SCHEMA_COLS <- c(
  "from", "to", "weight", "layer", "directed",
  "same_dept", "same_location", "same_legacy", "snapshot"
)

# ============================================================================
# utils.R
# ============================================================================

# Small internal helpers ------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

scale01 <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) {
    return(rep(0.5, length(x)))
  }
  (x - rng[1]) / (rng[2] - rng[1])
}

soft_clip <- function(x, lo = 0, hi = 1) pmin(hi, pmax(lo, x))

normalise_prob <- function(x) {
  x <- ifelse(is.na(x) | x < 0, 0, x)
  s <- sum(x)
  if (s == 0) rep(1 / length(x), length(x)) else x / s
}

safe_sample <- function(x, size = 1L, replace = FALSE, prob = NULL) {
  if (length(x) == 0) {
    return(x[0])
  }
  size <- min(size, if (replace) size else length(x))
  if (size <= 0) {
    return(x[0])
  }
  x[sample.int(length(x), size = size, replace = replace, prob = prob)]
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

resolve_output_root <- function(output_root = default_output_dir()) {
  output_root <- output_root %||% default_output_dir()
  if (!grepl("^(?:[A-Za-z]:[/\\]|/|~)", output_root)) {
    output_root <- file.path(getwd(), output_root)
  }
  normalizePath(output_root, winslash = "/", mustWork = FALSE)
}

default_output_dir <- function() {
  file.path(tempdir(), "synthona")
}

write_json_pretty <- function(x, path) {
  jsonlite::write_json(
    x,
    path = path, auto_unbox = TRUE, pretty = TRUE,
    dataframe = "rows", null = "null", digits = 6
  )
}

write_table_csv <- function(x, path) {
  utils::write.csv(as.data.frame(x), file = path, row.names = FALSE, na = "")
  invisible(path)
}

# ============================================================================
# rng.R
# ============================================================================

# Reproducibility -------------------------------------------------------------
#
# Every stochastic step in the protocol draws from an explicitly named seed
# stream. Two rules make results reproducible:
#
#   1. No function in this package leaves the caller's global RNG state
#      modified. `with_local_seed()` saves `.Random.seed`, seeds locally, and
#      restores the previous state on exit.
#   2. Sub-seeds are derived from a base seed and a *character tag* rather
#      than from a positional offset. Adding a new layer or snapshot therefore
#      does not shift the seeds of any existing one, so datasets generated by
#      different releases of the protocol stay comparable.

with_local_seed <- function(seed, expr) {
  if (is.null(seed)) {
    return(force(expr))
  }
  has_old <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (has_old) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  on.exit(
    {
      if (is.null(old)) {
        if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
          rm(".Random.seed", envir = globalenv())
        }
      } else {
        assign(".Random.seed", old, envir = globalenv())
      }
    },
    add = TRUE
  )
  set.seed(as.integer(seed))
  force(expr)
}

derive_seed <- function(base, tag) {
  stopifnot(length(tag) == 1L, is.character(tag))
  bytes <- as.integer(charToRaw(tag))
  offset <- sum(bytes * seq_along(bytes) * 31L) %% 1000003L
  as.integer((as.numeric(base) + offset) %% 2147483647)
}

# ============================================================================
# edges.R
# ============================================================================

# Edge table schema -----------------------------------------------------------

empty_edge_tbl <- function() {
  tibble::tibble(
    from = character(), to = character(), weight = numeric(),
    layer = character(), directed = logical(), same_dept = logical(),
    same_location = logical(), same_legacy = logical(), snapshot = character()
  )
}

coerce_edge_schema <- function(df) {
  if (is.null(df) || !inherits(df, "data.frame")) {
    return(empty_edge_tbl())
  }
  out <- tibble::as_tibble(df)
  n <- nrow(out)

  if (!"from" %in% names(out))          out$from <- character(n)
  if (!"to" %in% names(out))            out$to <- character(n)
  if (!"weight" %in% names(out))        out$weight <- rep(NA_real_, n)
  if (!"layer" %in% names(out))         out$layer <- rep(NA_character_, n)
  if (!"directed" %in% names(out))      out$directed <- rep(FALSE, n)
  if (!"same_dept" %in% names(out))     out$same_dept <- rep(NA, n)
  if (!"same_location" %in% names(out)) out$same_location <- rep(NA, n)
  if (!"same_legacy" %in% names(out))   out$same_legacy <- rep(NA, n)
  if (!"snapshot" %in% names(out))      out$snapshot <- rep(NA_character_, n)

  out$from          <- as.character(out$from)
  out$to            <- as.character(out$to)
  out$weight        <- suppressWarnings(as.numeric(out$weight))
  out$layer         <- as.character(out$layer)
  out$directed      <- as.logical(out$directed)
  out$same_dept     <- as.logical(out$same_dept)
  out$same_location <- as.logical(out$same_location)
  out$same_legacy   <- as.logical(out$same_legacy)
  out$snapshot      <- as.character(out$snapshot)

  extra <- setdiff(names(out), EDGE_SCHEMA_COLS)
  out[, c(EDGE_SCHEMA_COLS, extra), drop = FALSE]
}

bind_edge_tables <- function(..., .tables = NULL) {
  tables <- c(list(...), .tables)
  tables <- tables[!vapply(tables, is.null, logical(1))]
  if (length(tables) == 0) {
    return(empty_edge_tbl())
  }
  tables <- lapply(tables, coerce_edge_schema)
  tables <- tables[vapply(tables, nrow, integer(1)) > 0]
  if (length(tables) == 0) {
    return(empty_edge_tbl())
  }
  coerce_edge_schema(dplyr::bind_rows(tables))
}

order_endpoints <- function(ed) {
  lo <- pmin(ed$from, ed$to)
  hi <- pmax(ed$from, ed$to)
  ed$from <- lo
  ed$to <- hi
  ed
}

compact_edges <- function(df) {
  df <- coerce_edge_schema(df)
  if (nrow(df) == 0) {
    return(df)
  }
  df <- df[df$from != df$to, , drop = FALSE]
  if (nrow(df) == 0) {
    return(df)
  }
  df$directed <- df$layer %in% DIRECTED_LAYERS

  lo <- pmin(df$from, df$to)
  hi <- pmax(df$from, df$to)
  key <- ifelse(
    df$directed,
    paste(df$from, df$to, df$layer, df$snapshot, sep = "\r"),
    paste(lo, hi, df$layer, df$snapshot, sep = "\r")
  )
  df[!duplicated(key), , drop = FALSE]
}

collapse_edge_projection <- function(edges, directed = FALSE) {
  ed <- coerce_edge_schema(edges)
  if (nrow(ed) == 0) {
    return(tibble::tibble(from = character(), to = character(), weight = numeric()))
  }
  ed$weight[is.na(ed$weight)] <- 1
  if (!directed) {
    ed <- order_endpoints(ed)
  }
  ed <- ed[ed$from != ed$to, , drop = FALSE]
  if (nrow(ed) == 0) {
    return(tibble::tibble(from = character(), to = character(), weight = numeric()))
  }
  dplyr::summarise(
    dplyr::group_by(ed, .data$from, .data$to),
    weight = sum(.data$weight, na.rm = TRUE),
    .groups = "drop"
  )
}

# ============================================================================
# templates.R
# ============================================================================

# Industry templates ----------------------------------------------------------

synthona_templates <- function() {
  list(
    tech_product = list(
      id = "tech_product",
      label = "Technology product organisation",
      departments = c("Engineering", "Product", "Sales", "Marketing", "Operations", "HR", "Finance"),
      dept_weights = c(0.30, 0.16, 0.14, 0.10, 0.12, 0.08, 0.10),
      locations = c("HQ", "Remote", "Regional"),
      location_weights = c(0.45, 0.35, 0.20),
      seniority_levels = c("IC-1", "IC-2", "IC-3", "Senior", "Director", "Exec"),
      seniority_weights = c(0.26, 0.24, 0.19, 0.17, 0.10, 0.04),
      manager_level_min = 4L,
      culture = "async_hybrid"
    ),
    professional_services = list(
      id = "professional_services",
      label = "Professional services firm",
      departments = c("Client Services", "Delivery", "Strategy", "Sales", "Operations", "People"),
      dept_weights = c(0.28, 0.22, 0.12, 0.14, 0.14, 0.10),
      locations = c("HQ", "Client Site", "Remote", "Regional"),
      location_weights = c(0.30, 0.25, 0.25, 0.20),
      seniority_levels = c("Analyst", "Consultant", "Manager", "Senior Manager", "Partner", "Exec"),
      seniority_weights = c(0.22, 0.24, 0.24, 0.16, 0.10, 0.04),
      manager_level_min = 3L,
      culture = "client_facing"
    ),
    manufacturing = list(
      id = "manufacturing",
      label = "Manufacturing organisation",
      departments = c("Plant Ops", "Engineering", "Supply Chain", "Quality", "Sales", "HR", "Finance"),
      dept_weights = c(0.24, 0.16, 0.18, 0.10, 0.10, 0.10, 0.12),
      locations = c("Plant", "HQ", "Regional", "Remote"),
      location_weights = c(0.42, 0.28, 0.20, 0.10),
      seniority_levels = c("Operator", "Specialist", "Supervisor", "Manager", "Director", "Exec"),
      seniority_weights = c(0.28, 0.24, 0.18, 0.16, 0.10, 0.04),
      manager_level_min = 3L,
      culture = "site_centric"
    ),
    healthcare_ops = list(
      id = "healthcare_ops",
      label = "Healthcare operations organisation",
      departments = c("Clinical Ops", "Admin", "IT", "People", "Finance", "Quality", "External Relations"),
      dept_weights = c(0.32, 0.14, 0.12, 0.10, 0.10, 0.12, 0.10),
      locations = c("Campus", "Regional", "Remote"),
      location_weights = c(0.55, 0.25, 0.20),
      seniority_levels = c("Staff", "Senior Staff", "Lead", "Manager", "Director", "Exec"),
      seniority_weights = c(0.30, 0.24, 0.18, 0.14, 0.10, 0.04),
      manager_level_min = 3L,
      culture = "high_reliability"
    )
  )
}

get_template <- function(template_id) {
  templates <- synthona_templates()
  tpl <- templates[[template_id]]
  if (is.null(tpl)) {
    stop(
      "Unknown template: ", template_id,
      ". Available: ", paste(names(templates), collapse = ", "),
      call. = FALSE
    )
  }
  tpl
}

# Preferential-attachment multiplier by role bucket. Applied when generating
# scale-free structure so that senior roles accumulate ties faster.
ROLE_ATTACHMENT_BOOST <- c(
  individual = 1.00, manager = 1.20, senior_manager = 1.45,
  executive = 1.80, system = 2.00
)

# ============================================================================
# params.R
# ============================================================================

# Parameter specification -----------------------------------------------------

SYNTHONA_TOPOLOGIES <- c("hierarchy", "sbm", "sbm_dual_legacy", "er", "ws", "ba")

synthona_params <- function(n = 500L,
                            template = "tech_product",
                            topology = "hierarchy",
                            mean_degree = 12,
                            within_share = 0.65,
                            layers = c("communication", "advice", "trust"),
                            snapshots = "snapshot",
                            snapshot_mode = c("cumulative", "alternative"),
                            seed_topology = SEED_TOPOLOGY_BASE,
                            seed_attributes = SEED_ATTRIBUTE_BASE,
                            scenario_id = NULL,
                            label = NULL,
                            extras = list()) {
  snapshot_mode <- match.arg(snapshot_mode)

  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 20) {
    stop("`n` must be a single number of at least 20.", call. = FALSE)
  }
  n <- as.integer(n)

  tpl <- get_template(template)

  if (!is.character(topology) || length(topology) != 1L ||
    !topology %in% SYNTHONA_TOPOLOGIES) {
    stop(
      "`topology` must be one of: ",
      paste(SYNTHONA_TOPOLOGIES, collapse = ", "), ".",
      call. = FALSE
    )
  }

  if (!is.numeric(mean_degree) || length(mean_degree) != 1L ||
    is.na(mean_degree) || mean_degree <= 0) {
    stop("`mean_degree` must be a single positive number.", call. = FALSE)
  }
  if (mean_degree >= n) {
    stop(
      "`mean_degree` (", mean_degree, ") must be smaller than `n` (", n, ").",
      call. = FALSE
    )
  }

  if (!is.numeric(within_share) || length(within_share) != 1L ||
    is.na(within_share) || within_share < 0 || within_share > 1) {
    stop("`within_share` must be a single number between 0 and 1.", call. = FALSE)
  }

  unknown_layers <- setdiff(layers, SYNTHONA_LAYERS)
  if (length(unknown_layers) > 0) {
    stop(
      "Unknown layer(s): ", paste(unknown_layers, collapse = ", "),
      ". Available: ", paste(SYNTHONA_LAYERS, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (length(layers) == 0) {
    stop("At least one layer must be requested.", call. = FALSE)
  }

  if (!is.character(snapshots) || length(snapshots) == 0 || anyNA(snapshots)) {
    stop("`snapshots` must be a non-empty character vector.", call. = FALSE)
  }
  if (anyDuplicated(snapshots) > 0) {
    stop("`snapshots` must not contain duplicates.", call. = FALSE)
  }

  structure(
    list(
      n = n,
      template = tpl$id,
      topology = topology,
      mean_degree = as.numeric(mean_degree),
      within_share = as.numeric(within_share),
      layers = unique(as.character(layers)),
      snapshots = as.character(snapshots),
      snapshot_mode = snapshot_mode,
      seed_topology = as.integer(seed_topology),
      seed_attributes = as.integer(seed_attributes),
      scenario_id = scenario_id,
      label = label %||% scenario_id %||% sprintf("%s_n%d", topology, n),
      extras = extras,
      protocol_version = protocol_version()
    ),
    class = "synthona_params"
  )
}

protocol_version <- function() {
  v <- tryCatch(utils::packageVersion("SynthONA"), error = function(e) NULL)
  if (is.null(v)) "0.1.0" else as.character(v)
}

print.synthona_params <- function(x, ...) {
  cat("<synthona_params>", x$label, "\n")
  cat(sprintf("  employees      : %d\n", x$n))
  cat(sprintf("  template       : %s\n", x$template))
  cat(sprintf("  topology       : %s\n", x$topology))
  cat(sprintf(
    "  mean degree    : %.1f  (within-department share %.2f)\n",
    x$mean_degree, x$within_share
  ))
  cat(sprintf("  layers         : %s\n", paste(x$layers, collapse = ", ")))
  cat(sprintf(
    "  snapshots      : %s (%s)\n",
    paste(x$snapshots, collapse = ", "), x$snapshot_mode
  ))
  cat(sprintf(
    "  seeds          : topology %d / attributes %d\n",
    x$seed_topology, x$seed_attributes
  ))
  if (length(x$extras) > 0) {
    cat(sprintf("  extras         : %s\n", paste(names(x$extras), collapse = ", ")))
  }
  invisible(x)
}

update_params <- function(params, ...) {
  stopifnot(inherits(params, "synthona_params"))
  changes <- list(...)
  unknown <- setdiff(names(changes), names(params))
  if (length(unknown) > 0) {
    stop("Unknown parameter(s): ", paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  spec <- utils::modifyList(unclass(params), changes)
  synthona_params(
    n = spec$n, template = spec$template, topology = spec$topology,
    mean_degree = spec$mean_degree, within_share = spec$within_share,
    layers = spec$layers, snapshots = spec$snapshots,
    snapshot_mode = spec$snapshot_mode,
    seed_topology = spec$seed_topology, seed_attributes = spec$seed_attributes,
    scenario_id = spec$scenario_id, label = spec$label, extras = spec$extras
  )
}

# ============================================================================
# attributes.R
# ============================================================================

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

# ============================================================================
# topology.R
# ============================================================================

# Base topology generation ----------------------------------------------------

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

degree_to_probability <- function(n, mean_degree) {
  if (n < 2) {
    return(0)
  }
  soft_clip(mean_degree / (n - 1), 0, 1)
}

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

# ============================================================================
# layers.R
# ============================================================================

# Relational layer generation -------------------------------------------------

layer_keep_prob <- function(layer) {
  unname(LAYER_KEEP_PROB[layer]) %|na|% 0.50
}

`%|na|%` <- function(x, y) if (length(x) == 0 || is.na(x)) y else x

draw_edge_weights <- function(layer, same_dept, same_location, same_legacy, seed) {
  n <- length(same_dept)
  if (n == 0) {
    return(numeric(0))
  }
  if (identical(layer, "reporting")) {
    return(rep(1, n))
  }

  proximate <- (!is.na(same_dept) & same_dept) | (!is.na(same_location) & same_location)
  cross_legacy <- !is.na(same_legacy) & !same_legacy

  shape_a <- rep(2.0, n)
  shape_b <- rep(4.0, n)

  shape_a[proximate] <- 3.0
  shape_b[proximate] <- 2.0

  if (identical(layer, "trust")) {
    shape_a <- ifelse(proximate, 3.5, 2.5)
    shape_b <- ifelse(proximate, 1.5, 2.0)
  } else if (identical(layer, "decision_influence")) {
    shape_a[] <- 3.2
    shape_b[] <- 1.8
  }

  # Ties that bridge two legacy organisations are weaker regardless of layer.
  shape_a[cross_legacy] <- 1.8
  shape_b[cross_legacy] <- 3.8

  with_local_seed(seed, round(stats::rbeta(n, shape_a, shape_b), 3))
}

graph_to_edge_df <- function(g, nodes, layer = "communication", seed = SEED_ATTRIBUTE_BASE) {
  edf <- igraph::as_data_frame(g, what = "edges")
  if (nrow(edf) == 0) {
    return(empty_edge_tbl())
  }

  i <- match(edf$from, nodes$person_id)
  j <- match(edf$to, nodes$person_id)

  same_dept <- nodes$department[i] == nodes$department[j]
  same_loc <- nodes$location[i] == nodes$location[j]
  same_legacy <- if ("legacy_company" %in% names(nodes)) {
    nodes$legacy_company[i] == nodes$legacy_company[j]
  } else {
    rep(NA, nrow(edf))
  }

  coerce_edge_schema(tibble::tibble(
    from = as.character(edf$from),
    to = as.character(edf$to),
    weight = draw_edge_weights(layer, same_dept, same_loc, same_legacy, seed),
    layer = layer,
    directed = layer %in% DIRECTED_LAYERS,
    same_dept = same_dept,
    same_location = same_loc,
    same_legacy = same_legacy
  ))
}

orient_edges <- function(edf, nodes, layer, seed) {
  if (nrow(edf) == 0) {
    return(edf)
  }
  if (!(layer %in% DIRECTED_LAYERS)) {
    edf$directed <- FALSE
    return(edf)
  }

  from_level <- nodes$level[match(edf$from, nodes$person_id)]
  to_level <- nodes$level[match(edf$to, nodes$person_id)]
  from_level[is.na(from_level)] <- 0L
  to_level[is.na(to_level)] <- 0L

  swap <- if (layer %in% c("reporting", "mentorship", "decision_influence")) {
    from_level < to_level
  } else if (layer == "advice") {
    from_level > to_level
  } else {
    rep(FALSE, nrow(edf))
  }

  # Peers are oriented at random rather than left in generation order.
  peers <- from_level == to_level
  if (any(peers)) {
    coin <- with_local_seed(seed, stats::runif(sum(peers)) < 0.5)
    swap[peers] <- coin
  }

  old_from <- edf$from
  edf$from[swap] <- edf$to[swap]
  edf$to[swap] <- old_from[swap]
  edf$directed <- TRUE
  edf
}

derive_layer <- function(base_graph, nodes, layer, params) {
  seed <- derive_seed(params$seed_attributes, paste0("layer:", layer))
  edf <- graph_to_edge_df(base_graph, nodes, layer = layer, seed = seed)
  if (nrow(edf) == 0) {
    return(edf)
  }

  keep_prob <- params$extras$layer_keep_prob[[layer]] %||% layer_keep_prob(layer)
  if (keep_prob < 1) {
    keep <- with_local_seed(
      derive_seed(seed, "thin"),
      stats::runif(nrow(edf)) < keep_prob
    )
    edf <- edf[keep, , drop = FALSE]
  }
  if (nrow(edf) == 0) {
    return(edf)
  }
  orient_edges(edf, nodes, layer, seed = derive_seed(seed, "orient"))
}

generate_layers <- function(base_graph, nodes, params) {
  tables <- lapply(params$layers, function(l) derive_layer(base_graph, nodes, l, params))
  edges <- bind_edge_tables(.tables = tables)
  compact_edges(edges)
}

# ============================================================================
# temporal.R
# ============================================================================

# Longitudinal generation -----------------------------------------------------
#
# Panels are generated as a process rather than as a set of independent
# perturbations of one baseline. Under `snapshot_mode = "cumulative"` each wave
# evolves from the wave before it, so a tie that dissolves stays dissolved and
# a tie that forms persists until it dissolves. Perturbing the same baseline
# repeatedly instead produces waves in which ties reappear after being dropped,
# which is not a trajectory any longitudinal method should be scored against.

default_evolution <- function() {
  list(
    churn = 0.06,       # share of ties dissolving per wave
    renewal = TRUE,     # replace dissolved ties to hold degree roughly steady
    weight_drift = 0.04 # standard deviation of tie-strength drift per wave
  )
}

apply_shock <- function(edges, nodes, shock, params, seed) {
  layer <- shock$layer %||% NULL
  in_layer <- if (is.null(layer)) rep(TRUE, nrow(edges)) else edges$layer == layer

  if (!is.null(shock$drop) && any(in_layer)) {
    idx <- which(in_layer)
    drop <- with_local_seed(
      derive_seed(seed, "shock:drop"),
      idx[stats::runif(length(idx)) < shock$drop]
    )
    if (length(drop) > 0) edges <- edges[-drop, , drop = FALSE]
  }

  if (!is.null(shock$weight_delta)) {
    in_layer <- if (is.null(layer)) rep(TRUE, nrow(edges)) else edges$layer == layer
    edges$weight[in_layer] <- soft_clip(edges$weight[in_layer] + shock$weight_delta, 0, 1)
  }

  if (!is.null(shock$remove_actor)) {
    target <- if (identical(shock$remove_actor, "top_broker")) {
      top_broker_id(nodes, edges)
    } else {
      shock$remove_actor
    }
    if (!is.na(target)) {
      edges <- edges[edges$from != target & edges$to != target, , drop = FALSE]
    }
  }

  if (!is.null(shock$add)) {
    n_new <- as.integer(round(shock$add * nrow(nodes)))
    new_edges <- form_new_ties(
      nodes, n_new, layer %||% params$layers[1], params,
      tag = paste0(":shock:", shock$at %||% "")
    )
    edges <- bind_edge_tables(edges, new_edges)
  }

  edges
}

top_broker_id <- function(nodes, edges) {
  g <- graph_from_edges(nodes, edges, directed = FALSE)
  if (igraph::ecount(g) == 0) {
    return(NA_character_)
  }
  b <- igraph::betweenness(g, directed = FALSE, normalized = TRUE)
  igraph::V(g)$name[which.max(b)]
}

form_new_ties <- function(nodes, n_edges, layer, params, tag = "") {
  if (is.na(n_edges) || n_edges < 1) {
    return(empty_edge_tbl())
  }
  within <- sample_within_department_ties(
    nodes, n_edges * params$within_share, params,
    tag = tag
  )
  across <- sample_cross_department_ties(
    nodes, n_edges * (1 - params$within_share), params,
    tag = tag
  )
  pairs <- dplyr::bind_rows(within, across)
  if (nrow(pairs) == 0) {
    return(empty_edge_tbl())
  }

  from_id <- nodes$person_id[pairs$from]
  to_id <- nodes$person_id[pairs$to]
  same_dept <- nodes$department[pairs$from] == nodes$department[pairs$to]
  same_loc <- nodes$location[pairs$from] == nodes$location[pairs$to]
  same_legacy <- if ("legacy_company" %in% names(nodes)) {
    nodes$legacy_company[pairs$from] == nodes$legacy_company[pairs$to]
  } else {
    rep(NA, nrow(pairs))
  }

  seed <- derive_seed(params$seed_attributes, paste0("form:", layer, tag))
  edf <- coerce_edge_schema(tibble::tibble(
    from = from_id, to = to_id,
    weight = draw_edge_weights(layer, same_dept, same_loc, same_legacy, seed),
    layer = layer,
    directed = layer %in% DIRECTED_LAYERS,
    same_dept = same_dept, same_location = same_loc, same_legacy = same_legacy
  ))
  orient_edges(edf, nodes, layer, seed = derive_seed(seed, "orient"))
}

evolve_edges <- function(edges, nodes, params, step, snapshot_id) {
  ev <- utils::modifyList(default_evolution(), params$extras$evolution %||% list())
  seed <- derive_seed(params$seed_topology, paste0("wave:", snapshot_id))
  n_before <- nrow(edges)

  if (ev$churn > 0 && n_before > 0) {
    survive <- with_local_seed(
      derive_seed(seed, "churn"),
      stats::runif(n_before) >= ev$churn
    )
    edges <- edges[survive, , drop = FALSE]
  }

  if (ev$weight_drift > 0 && nrow(edges) > 0) {
    drift <- with_local_seed(
      derive_seed(seed, "drift"),
      stats::rnorm(nrow(edges), 0, ev$weight_drift)
    )
    edges$weight <- soft_clip(edges$weight + drift, 0.01, 1)
  }

  if (isTRUE(ev$renewal)) {
    deficit <- n_before - nrow(edges)
    if (deficit > 0) {
      layer_mix <- table(edges$layer)
      layer_for_new <- if (length(layer_mix) > 0) {
        names(layer_mix)[which.max(layer_mix)]
      } else {
        params$layers[1]
      }
      edges <- bind_edge_tables(
        edges,
        form_new_ties(nodes, deficit, layer_for_new, params,
          tag = paste0(":wave:", snapshot_id)
        )
      )
    }
  }

  shocks <- Filter(
    function(s) identical(s$at, snapshot_id),
    params$extras$shocks %||% list()
  )
  for (s in shocks) {
    edges <- apply_shock(edges, nodes, s, params, derive_seed(seed, "shock"))
  }

  compact_edges(edges)
}

generate_snapshots <- function(nodes, edges, params) {
  snaps <- params$snapshots
  if (length(snaps) == 1L) {
    edges$snapshot <- snaps
    return(compact_edges(edges))
  }

  out <- vector("list", length(snaps))
  current <- edges

  for (i in seq_along(snaps)) {
    snap <- snaps[i]
    wave <- if (i == 1L) {
      current
    } else if (identical(params$snapshot_mode, "cumulative")) {
      evolve_edges(current, nodes, params, step = i - 1L, snapshot_id = snap)
    } else {
      evolve_edges(edges, nodes, params, step = i - 1L, snapshot_id = snap)
    }
    if (identical(params$snapshot_mode, "cumulative")) {
      current <- wave
    }
    wave$snapshot <- snap
    out[[i]] <- wave
  }

  compact_edges(bind_edge_tables(.tables = out))
}

# ============================================================================
# truth.R
# ============================================================================

# Ground truth ----------------------------------------------------------------
#
# A benchmark is only a benchmark if the answer ships with the question. The
# functions here record what the generator actually built, computed on the
# complete network *before* any measurement error is applied. Scoring a method
# means comparing what it recovers from the observed data against these labels.

record_truth <- function(nodes, edges, structure_truth, params, broker_quantile = 0.90) {
  g <- graph_from_edges(nodes, edges, directed = FALSE)

  betweenness <- igraph::betweenness(g, directed = FALSE, normalized = TRUE)
  names(betweenness) <- igraph::V(g)$name
  cut_off <- stats::quantile(betweenness, broker_quantile, na.rm = TRUE)
  brokers <- names(betweenness)[betweenness >= cut_off & betweenness > 0]

  articulation <- if (igraph::ecount(g) > 0) {
    igraph::V(g)$name[as.integer(igraph::articulation_points(g))]
  } else {
    character()
  }

  communities <- structure_truth$communities
  if (is.null(communities)) {
    communities <- setNames(nodes$department, nodes$person_id)
  }

  structure(
    list(
      model = structure_truth$model,
      communities = communities,
      n_communities = length(unique(communities)),
      brokers = brokers,
      broker_quantile = broker_quantile,
      betweenness = betweenness,
      articulation_points = articulation,
      manager = structure_truth$manager,
      hierarchy_depth = structure_truth$depth,
      block_sizes = structure_truth$block_sizes,
      pref_matrix = structure_truth$pref_matrix,
      legacy_company = if ("legacy_company" %in% names(nodes)) {
        setNames(nodes$legacy_company, nodes$person_id)
      } else {
        NULL
      },
      params = params
    ),
    class = "synthona_truth"
  )
}

print.synthona_truth <- function(x, ...) {
  cat("<synthona_truth>", x$model, "\n")
  cat(sprintf("  planted communities : %d\n", x$n_communities))
  cat(sprintf("  true brokers        : %d (top %.0f%% by betweenness)\n",
              length(x$brokers), 100 * (1 - x$broker_quantile)))
  cat(sprintf("  articulation points : %d\n", length(x$articulation_points)))
  if (!is.null(x$manager)) {
    cat(sprintf("  hierarchy depth     : %d levels\n",
                max(x$hierarchy_depth, na.rm = TRUE) + 1L))
  }
  if (!is.null(x$legacy_company)) {
    cat(sprintf("  legacy split        : %s\n",
                paste(names(table(x$legacy_company)),
                      as.integer(table(x$legacy_company)),
                      sep = "=", collapse = ", ")))
  }
  invisible(x)
}

score_communities <- function(truth, recovered) {
  stopifnot(inherits(truth, "synthona_truth"))

  if (inherits(recovered, "communities")) {
    recovered <- setNames(igraph::membership(recovered), recovered$names)
  }
  ids <- intersect(names(truth$communities), names(recovered))
  if (length(ids) < 2) {
    stop("No overlapping actors between truth and recovered partition.", call. = FALSE)
  }
  a <- as.integer(factor(truth$communities[ids]))
  b <- as.integer(factor(recovered[ids]))

  tibble::tibble(
    n = length(ids),
    n_true = length(unique(a)),
    n_recovered = length(unique(b)),
    ari = adjusted_rand_index(a, b),
    nmi = normalised_mutual_information(a, b)
  )
}

adjusted_rand_index <- function(a, b) {
  tab <- table(a, b)
  choose2 <- function(x) x * (x - 1) / 2
  sum_ij <- sum(choose2(tab))
  sum_a <- sum(choose2(rowSums(tab)))
  sum_b <- sum(choose2(colSums(tab)))
  n2 <- choose2(sum(tab))
  expected <- sum_a * sum_b / n2
  max_index <- (sum_a + sum_b) / 2
  if (isTRUE(all.equal(max_index, expected))) {
    return(0)
  }
  as.numeric((sum_ij - expected) / (max_index - expected))
}

normalised_mutual_information <- function(a, b) {
  tab <- table(a, b)
  n <- sum(tab)
  p_ij <- tab / n
  p_i <- rowSums(p_ij)
  p_j <- colSums(p_ij)

  nz <- p_ij > 0
  mi <- sum(p_ij[nz] * log(p_ij[nz] / outer(p_i, p_j)[nz]))
  h_a <- -sum(p_i[p_i > 0] * log(p_i[p_i > 0]))
  h_b <- -sum(p_j[p_j > 0] * log(p_j[p_j > 0]))
  if (h_a == 0 && h_b == 0) {
    return(1)
  }
  as.numeric(2 * mi / (h_a + h_b))
}

score_brokers <- function(truth, recovered) {
  stopifnot(inherits(truth, "synthona_truth"))
  recovered <- unique(as.character(recovered))
  actual <- truth$brokers

  tp <- length(intersect(recovered, actual))
  precision <- if (length(recovered) == 0) NA_real_ else tp / length(recovered)
  recall <- if (length(actual) == 0) NA_real_ else tp / length(actual)
  f1 <- if (is.na(precision) || is.na(recall) || (precision + recall) == 0) {
    NA_real_
  } else {
    2 * precision * recall / (precision + recall)
  }

  tibble::tibble(
    n_true = length(actual), n_recovered = length(recovered),
    true_positives = tp, precision = precision, recall = recall, f1 = f1
  )
}

# ============================================================================
# observe.R
# ============================================================================

# Measurement layer -----------------------------------------------------------
#
# Organisational network data is almost never a census of a true network. It
# comes from a survey that some people ignore, with a name generator that caps
# how many colleagues they may list, answered from memory that favours strong
# and recent ties. Those three mechanisms are what separate an ONA benchmark
# from a generic graph sample: they are the reason recovered brokers are not
# the true brokers, and the reason a method that scores well on a complete
# network can fail on a real one.
#
# The observation layer is deliberately separate from generation, so the same
# true network can be observed many times under different survey designs.

observation_design <- function(response_rate = 1,
                               name_generator_limit = Inf,
                               recall_probability = 1,
                               roster_coverage = 1,
                               response_bias = 0.5) {
  check_share <- function(x, nm) {
    if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0 || x > 1) {
      stop("`", nm, "` must be a single number between 0 and 1.", call. = FALSE)
    }
  }
  check_share(response_rate, "response_rate")
  check_share(recall_probability, "recall_probability")
  check_share(roster_coverage, "roster_coverage")
  check_share(response_bias, "response_bias")
  if (!is.numeric(name_generator_limit) || length(name_generator_limit) != 1L ||
    is.na(name_generator_limit) || name_generator_limit < 1) {
    stop("`name_generator_limit` must be a single number of at least 1.", call. = FALSE)
  }

  structure(
    list(
      response_rate = response_rate,
      name_generator_limit = name_generator_limit,
      recall_probability = recall_probability,
      roster_coverage = roster_coverage,
      response_bias = response_bias
    ),
    class = "synthona_observation"
  )
}

print.synthona_observation <- function(x, ...) {
  cat("<synthona_observation>\n")
  cat(sprintf("  response rate        : %.0f%% (seniority bias %.2f)\n",
              100 * x$response_rate, x$response_bias))
  cat(sprintf("  name generator limit : %s\n",
              if (is.finite(x$name_generator_limit)) x$name_generator_limit else "none"))
  cat(sprintf("  recall probability   : %.2f at median tie strength\n",
              x$recall_probability))
  cat(sprintf("  roster coverage      : %.0f%%\n", 100 * x$roster_coverage))
  invisible(x)
}

synthona_observe <- function(dataset, design = observation_design(), seed = NULL) {
  stopifnot(inherits(dataset, "synthona_dataset"))
  if (is.null(design)) {
    return(dataset)
  }
  stopifnot(inherits(design, "synthona_observation"))

  nodes <- dataset$nodes
  edges <- dataset$edges
  seed <- seed %||% derive_seed(dataset$params$seed_attributes, "observe")

  n <- nrow(nodes)

  # Roster coverage: who could be named at all.
  on_roster <- select_share(n, design$roster_coverage, NULL, derive_seed(seed, "roster"))

  # Response propensity, optionally tilted toward senior, longer-tenured staff.
  # A fixed number of respondents is drawn rather than an independent coin per
  # person, so that `response_rate = 1` is exactly a census and the realised
  # rate always matches the requested one.
  level_scaled <- scale01(as.numeric(nodes$level))
  level_scaled[is.na(level_scaled)] <- 0.5
  tenure_scaled <- scale01(nodes$tenure_years)
  tilt <- 0.6 * level_scaled + 0.4 * tenure_scaled
  weights <- 1 + design$response_bias * (tilt - mean(tilt, na.rm = TRUE)) * 2
  weights[is.na(weights)] <- 1

  responded <- select_share(
    n, design$response_rate, soft_clip(weights, 0.05, Inf),
    derive_seed(seed, "response")
  )

  observed_actor <- tibble::tibble(
    person_id = nodes$person_id,
    on_roster = on_roster,
    responded = responded & on_roster
  )

  keep <- rep(TRUE, nrow(edges))

  # A tie is recorded only if both ends are on the roster.
  roster_ok <- observed_actor$on_roster[match(edges$from, observed_actor$person_id)] &
    observed_actor$on_roster[match(edges$to, observed_actor$person_id)]
  keep <- keep & roster_ok

  # Non-respondents do not report their own ties. For an undirected layer the
  # tie survives if either end responded; for a directed layer only the sender
  # reports it.
  from_resp <- observed_actor$responded[match(edges$from, observed_actor$person_id)]
  to_resp <- observed_actor$responded[match(edges$to, observed_actor$person_id)]
  reported <- ifelse(edges$directed, from_resp, from_resp | to_resp)
  keep <- keep & reported

  # Recall: rises with tie strength, so weak ties drop out first.
  if (design$recall_probability < 1 && nrow(edges) > 0) {
    strength <- edges$weight
    strength[is.na(strength)] <- stats::median(strength, na.rm = TRUE)
    med <- stats::median(strength, na.rm = TRUE)
    if (!is.finite(med)) med <- 0.5
    # Logistic in tie strength, calibrated to hit `recall_probability` at the
    # median tie.
    odds <- design$recall_probability / max(1e-6, 1 - design$recall_probability)
    p_recall <- soft_clip(1 / (1 + exp(-(log(odds) + 4 * (strength - med)))), 0, 1)
    recalled <- with_local_seed(
      derive_seed(seed, "recall"),
      stats::runif(nrow(edges)) < p_recall
    )
    keep <- keep & recalled
  }

  edges <- edges[keep & !is.na(keep), , drop = FALSE]

  # Name generator: each respondent may list only their strongest alters,
  # within each layer and snapshot.
  if (is.finite(design$name_generator_limit) && nrow(edges) > 0) {
    edges <- truncate_name_generator(edges, design$name_generator_limit)
  }

  out <- dataset
  out$edges <- compact_edges(edges)
  out$observed_actors <- observed_actor
  out$observation <- design
  out$is_observed <- TRUE
  out
}

select_share <- function(n, share, weights = NULL, seed = NULL) {
  if (share >= 1) {
    return(rep(TRUE, n))
  }
  k <- as.integer(round(share * n))
  if (k <= 0) {
    return(rep(FALSE, n))
  }
  chosen <- with_local_seed(seed, sample.int(n, k, replace = FALSE, prob = weights))
  seq_len(n) %in% chosen
}

truncate_name_generator <- function(edges, limit) {
  limit <- as.integer(limit)
  ed <- edges
  ed$.row <- seq_len(nrow(ed))
  ed$.snapshot <- ed$snapshot %||% NA_character_

  ranked <- dplyr::mutate(
    dplyr::group_by(ed, .data$from, .data$layer, .data$.snapshot),
    .rank = rank(-.data$weight, ties.method = "first"),
    .groups = NULL
  )
  ranked <- dplyr::ungroup(ranked)
  keep_rows <- ranked$.row[ranked$.rank <= limit]
  edges[sort(keep_rows), , drop = FALSE]
}

# ============================================================================
# metrics.R
# ============================================================================

# Network metrics -------------------------------------------------------------

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

tie_distance <- function(g) {
  if (!"weight" %in% igraph::edge_attr_names(g) || igraph::ecount(g) == 0) {
    return(NULL)
  }
  w <- igraph::E(g)$weight
  w[is.na(w) | w <= 0] <- stats::median(w[w > 0], na.rm = TRUE) %|na|% 1
  1 / w
}

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
      round(suppressWarnings(igraph::mean_distance(giant, directed = FALSE)), 3)
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

small_world_sigma <- function(g, n_random = 20L, seed = SEED_TOPOLOGY_BASE) {
  if (n_random < 1 || igraph::ecount(g) == 0) {
    return(NA_real_)
  }
  dens <- igraph::edge_density(g)
  c_obs <- igraph::transitivity(g, type = "global")
  comp <- igraph::components(g)
  giant <- igraph::induced_subgraph(g, which(comp$membership == which.max(comp$csize)))
  l_obs <- suppressWarnings(igraph::mean_distance(giant, directed = FALSE))

  ref <- with_local_seed(seed, {
    vapply(seq_len(n_random), function(i) {
      gr <- igraph::sample_gnp(igraph::vcount(g), dens, directed = FALSE)
      cr <- igraph::components(gr)
      gir <- igraph::induced_subgraph(gr, which(cr$membership == which.max(cr$csize)))
      c(
        igraph::transitivity(gr, type = "global"),
        suppressWarnings(igraph::mean_distance(gir, directed = FALSE))
      )
    }, numeric(2))
  })

  c_rand <- mean(ref[1, ], na.rm = TRUE)
  l_rand <- mean(ref[2, ], na.rm = TRUE)
  round((c_obs / max(c_rand, 1e-8)) / (l_obs / max(l_rand, 1e-8)), 3)
}

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

# ============================================================================
# validate.R
# ============================================================================

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

models_departments <- function(topology) {
  topology %in% c("hierarchy", "sbm", "sbm_dual_legacy")
}

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

# ============================================================================
# generate.R
# ============================================================================

# Top-level generation --------------------------------------------------------

synthona_generate <- function(params, non_human_actors = 0L) {
  if (is.character(params)) {
    params <- synthona_scenario(params)
  }
  stopifnot(inherits(params, "synthona_params"))

  nodes <- generate_profiles(params)
  n_agents <- non_human_actors
  if (n_agents < 1) {
    n_agents <- params$extras$non_human_actors %||% 0L
  }
  if (n_agents > 0) {
    nodes <- add_non_human_actors(nodes, count = n_agents, params = params)
  }

  base <- generate_base_structure(nodes, params)
  edges <- generate_layers(base$graph, base$nodes, params)
  edges <- generate_snapshots(base$nodes, edges, params)

  # Ground truth is recorded on the first snapshot, which is the network as
  # generated before any evolution or measurement error.
  first_snapshot <- params$snapshots[1]
  baseline_edges <- edges[edges$snapshot == first_snapshot, , drop = FALSE]
  truth <- record_truth(base$nodes, baseline_edges, base$truth, params)

  structure(
    list(
      nodes = base$nodes,
      edges = edges,
      truth = truth,
      params = params,
      is_observed = FALSE,
      observation = NULL,
      generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    ),
    class = "synthona_dataset"
  )
}

print.synthona_dataset <- function(x, ...) {
  cat("<synthona_dataset>", x$params$label, "\n")
  cat(sprintf("  actors    : %d\n", nrow(x$nodes)))
  cat(sprintf("  ties      : %d across %d layer(s)\n",
              nrow(x$edges), length(unique(x$edges$layer))))
  cat(sprintf("  layers    : %s\n", paste(sort(unique(x$edges$layer)), collapse = ", ")))
  snaps <- unique(x$edges$snapshot)
  cat(sprintf("  snapshots : %s\n", paste(snaps, collapse = ", ")))
  cat(sprintf("  truth     : %d communities, %d brokers\n",
              x$truth$n_communities, length(x$truth$brokers)))
  if (isTRUE(x$is_observed)) {
    cat(sprintf("  observed  : response rate %.0f%%, name limit %s\n",
                100 * x$observation$response_rate,
                if (is.finite(x$observation$name_generator_limit)) {
                  x$observation$name_generator_limit
                } else {
                  "none"
                }))
  } else {
    cat("  observed  : no (complete network)\n")
  }
  invisible(x)
}

synthona_graph <- function(dataset, layer = NULL, snapshot = NULL, directed = NULL) {
  stopifnot(inherits(dataset, "synthona_dataset"))
  snapshot <- snapshot %||% dataset$params$snapshots[1]
  graph_from_edges(
    dataset$nodes, dataset$edges,
    layer = layer, snapshot = snapshot, directed = directed
  )
}

summary.synthona_dataset <- function(object, n_random = 0L, ...) {
  purrr::map_dfr(unique(object$edges$snapshot), function(s) {
    compute_network_metrics(object$nodes, object$edges, snapshot = s, n_random = n_random)
  })
}

# ============================================================================
# scenarios.R
# ============================================================================

# Scenario registry -----------------------------------------------------------
#
# Scenarios are named, citable parameter specifications covering recurring
# organisational situations. They are ordinary `synthona_params` objects: any
# scenario can be inspected, modified with `update_params()`, or ignored
# entirely in favour of a hand-built specification.

synthona_registry <- function() {
  list(
    DEMO_M = list(
      catalog_id = "ONA-BMK-DEMO-M",
      title = "First client meeting",
      question = "Where are the silos, and who holds the organisation together?",
      n = 500L, template = "tech_product", topology = "hierarchy",
      mean_degree = 12, within_share = 0.72,
      layers = c("communication", "advice", "trust"),
      snapshots = "snapshot",
      tags = c("departmental_silo", "broker_fragility", "executive_demo")
    ),
    CHRO_M = list(
      catalog_id = "ONA-BMK-CHRO-M",
      title = "Privacy-safe people analytics pilot",
      question = "Do remote and regional staff have equivalent access to the organisation?",
      n = 800L, template = "professional_services", topology = "sbm",
      mean_degree = 11, within_share = 0.62,
      layers = c("communication", "trust", "collaboration"),
      snapshots = "snapshot",
      extras = list(location_overlay = 0.30),
      tags = c("location_homophily", "cross_site_fragmentation", "privacy_safe")
    ),
    REORG_M = list(
      catalog_id = "ONA-BMK-REORG-M",
      title = "Reorganisation options lab",
      question = "Which of two reorganisation options costs less connectivity?",
      n = 1000L, template = "tech_product", topology = "hierarchy",
      mean_degree = 12, within_share = 0.70,
      layers = c("reporting", "communication", "collaboration", "trust"),
      snapshots = c("baseline", "option_a", "option_b"),
      snapshot_mode = "alternative",
      extras = list(
        evolution = list(churn = 0, weight_drift = 0),
        shocks = list(
          list(at = "option_a", layer = "reporting", drop = 0.25),
          list(at = "option_b", layer = "reporting", drop = 0.10),
          list(at = "option_b", layer = "collaboration", add = 0.8)
        )
      ),
      tags = c("reorg", "what_if_simulation", "org_design")
    ),
    RESEARCH_SM = list(
      catalog_id = "ONA-BMK-RESEARCH-SM",
      title = "Reproducible research corpus",
      question = "How do community detection methods compare across topologies?",
      n = 300L, template = "tech_product", topology = "sbm",
      mean_degree = 10, within_share = 0.75,
      layers = "communication",
      snapshots = "snapshot",
      tags = c("benchmark_corpus", "method_comparison", "reproducibility")
    ),
    DEV_S = list(
      catalog_id = "ONA-BMK-DEV-S",
      title = "Tool builder integration pack",
      question = "Does my application handle multiplex, directed and weighted data?",
      n = 120L, template = "tech_product", topology = "ws",
      mean_degree = 10, within_share = 0.55,
      layers = c("communication", "trust", "innovation", "mentorship"),
      snapshots = "snapshot",
      tags = c("developer_pack", "multiplex", "api_ready")
    ),
    AI_M = list(
      catalog_id = "ONA-BMK-AI-M",
      title = "AI adoption over four quarters",
      question = "How does an AI rollout reshape advice seeking?",
      n = 1200L, template = "tech_product", topology = "hierarchy",
      mean_degree = 13, within_share = 0.68,
      layers = c("communication", "advice", "tool_interaction"),
      snapshots = c("Q1", "Q2", "Q3", "Q4"),
      extras = list(
        non_human_actors = 6L,
        evolution = list(churn = 0.08),
        shocks = list(
          list(at = "Q2", layer = "tool_interaction", add = 0.15, weight_delta = 0.05),
          list(at = "Q3", layer = "tool_interaction", add = 0.25, weight_delta = 0.10),
          list(at = "Q4", layer = "tool_interaction", add = 0.35, weight_delta = 0.10)
        )
      ),
      tags = c("ai_adoption", "longitudinal", "non_human_actors")
    ),
    RESIZE_M = list(
      catalog_id = "ONA-BMK-RESIZE-M",
      title = "Restructuring and knowledge loss",
      question = "What connectivity disappears when a layer of the organisation goes?",
      n = 1000L, template = "manufacturing", topology = "hierarchy",
      mean_degree = 11, within_share = 0.70,
      layers = c("reporting", "communication", "advice"),
      snapshots = c("baseline", "post_change"),
      extras = list(
        evolution = list(churn = 0.05),
        shocks = list(
          list(at = "post_change", layer = "reporting", drop = 0.18),
          list(at = "post_change", layer = "advice", drop = 0.22)
        )
      ),
      tags = c("restructuring", "knowledge_loss", "before_after")
    ),
    MA_M = list(
      catalog_id = "ONA-BMK-MA-M",
      title = "Post-merger integration",
      question = "Are the two legacy organisations actually integrating?",
      n = 1500L, template = "professional_services", topology = "sbm_dual_legacy",
      mean_degree = 12, within_share = 0.68,
      layers = c("communication", "collaboration", "trust", "innovation"),
      snapshots = c("M0", "M3", "M6", "M12"),
      extras = list(
        cross_legacy_thinning = 0.45,
        evolution = list(churn = 0.07),
        shocks = list(
          list(at = "M3", layer = "collaboration", add = 0.20),
          list(at = "M6", layer = "collaboration", add = 0.30, weight_delta = 0.05),
          list(at = "M12", layer = "innovation", add = 0.35, weight_delta = 0.08)
        )
      ),
      tags = c("merger", "integration", "longitudinal")
    ),
    CULTURE_M = list(
      catalog_id = "ONA-BMK-CULTURE-M",
      title = "Culture change programme",
      question = "Is the change programme reaching beyond its early adopters?",
      n = 900L, template = "healthcare_ops", topology = "sbm",
      mean_degree = 11, within_share = 0.66,
      layers = c("communication", "trust", "energy"),
      snapshots = c("before", "mid", "after"),
      extras = list(
        evolution = list(churn = 0.06),
        shocks = list(
          list(at = "mid", layer = "energy", add = 0.25, weight_delta = 0.06),
          list(at = "after", layer = "energy", add = 0.40, weight_delta = 0.08),
          list(at = "after", layer = "trust", weight_delta = 0.05)
        )
      ),
      tags = c("culture_change", "diffusion", "longitudinal")
    ),
    SUCCESSION_M = list(
      catalog_id = "ONA-BMK-SUCCESSION-M",
      title = "Key person risk and succession",
      question = "What breaks if the single most central person leaves?",
      n = 800L, template = "tech_product", topology = "ba",
      mean_degree = 12, within_share = 0.60,
      layers = c("communication", "advice", "mentorship"),
      snapshots = c("baseline", "departure"),
      snapshot_mode = "alternative",
      extras = list(
        evolution = list(churn = 0, weight_drift = 0),
        shocks = list(list(at = "departure", remove_actor = "top_broker"))
      ),
      tags = c("key_person_risk", "succession", "resilience")
    )
  )
}

synthona_scenario <- function(scenario_id, ...) {
  registry <- synthona_registry()
  spec <- registry[[scenario_id]]
  if (is.null(spec)) {
    stop(
      "Unknown scenario: ", scenario_id,
      ". Available: ", paste(names(registry), collapse = ", "),
      call. = FALSE
    )
  }

  params <- synthona_params(
    n = spec$n,
    template = spec$template,
    topology = spec$topology,
    mean_degree = spec$mean_degree,
    within_share = spec$within_share,
    layers = spec$layers,
    snapshots = spec$snapshots,
    snapshot_mode = spec$snapshot_mode %||% "cumulative",
    seed_topology = spec$seed_topology %||% SEED_TOPOLOGY_BASE,
    seed_attributes = spec$seed_attributes %||% SEED_ATTRIBUTE_BASE,
    scenario_id = scenario_id,
    label = spec$title,
    extras = spec$extras %||% list()
  )

  overrides <- list(...)
  if (length(overrides) > 0) {
    params <- do.call(update_params, c(list(params), overrides))
  }
  params
}

scenario_table <- function() {
  registry <- synthona_registry()
  purrr::map_dfr(names(registry), function(id) {
    s <- registry[[id]]
    tibble::tibble(
      scenario_id = id,
      catalog_id = s$catalog_id,
      title = s$title,
      question = s$question,
      n = s$n,
      topology = s$topology,
      mean_degree = s$mean_degree,
      within_share = s$within_share,
      layers = paste(s$layers, collapse = ", "),
      snapshots = paste(s$snapshots, collapse = ", ")
    )
  })
}

# ============================================================================
# export.R
# ============================================================================

# Export ----------------------------------------------------------------------

synthona_write <- function(dataset, output_root = default_output_dir(),
                           graphml = TRUE, metrics = TRUE) {
  stopifnot(inherits(dataset, "synthona_dataset"))
  params <- dataset$params

  root <- resolve_output_root(output_root)
  name <- params$scenario_id %||% params$label
  out_dir <- file.path(root, gsub("[^A-Za-z0-9_.-]+", "_", name))
  ensure_dir(out_dir)
  ensure_dir(file.path(out_dir, "layers"))

  write_table_csv(dataset$nodes, file.path(out_dir, "nodes.csv"))
  write_table_csv(dataset$edges, file.path(out_dir, "edges.csv"))

  for (l in unique(dataset$edges$layer)) {
    write_table_csv(
      dataset$edges[dataset$edges$layer == l, , drop = FALSE],
      file.path(out_dir, "layers", paste0("edges_", l, ".csv"))
    )
  }

  snapshots <- unique(dataset$edges$snapshot)
  if (length(snapshots) > 1) {
    ensure_dir(file.path(out_dir, "snapshots"))
    for (s in snapshots) {
      snap_dir <- file.path(out_dir, "snapshots", gsub("[^A-Za-z0-9_.-]+", "_", s))
      ensure_dir(snap_dir)
      write_table_csv(
        dataset$edges[dataset$edges$snapshot == s, , drop = FALSE],
        file.path(snap_dir, "edges.csv")
      )
    }
  }

  if (isTRUE(metrics)) {
    first <- snapshots[1]
    write_table_csv(
      compute_node_metrics(dataset$nodes, dataset$edges, snapshot = first),
      file.path(out_dir, "metrics_node.csv")
    )
    write_table_csv(
      compute_group_metrics(dataset$nodes, dataset$edges, snapshot = first),
      file.path(out_dir, "metrics_group.csv")
    )
    write_table_csv(
      purrr::map_dfr(snapshots, function(s) {
        compute_network_metrics(dataset$nodes, dataset$edges, snapshot = s, n_random = 0L)
      }),
      file.path(out_dir, "metrics_network.csv")
    )
    write_table_csv(
      compute_outcomes(dataset$nodes, dataset$edges, params, snapshot = first),
      file.path(out_dir, "outcomes.csv")
    )
    write_table_csv(validate_dataset(dataset), file.path(out_dir, "validation.csv"))
  }

  write_json_pretty(truth_as_list(dataset$truth), file.path(out_dir, "ground_truth.json"))
  write_json_pretty(manifest_for(dataset), file.path(out_dir, "manifest.json"))

  if (isTRUE(graphml)) {
    g <- graph_from_edges(dataset$nodes, dataset$edges, snapshot = snapshots[1], directed = FALSE)
    try(
      igraph::write_graph(g, file.path(out_dir, "graph.graphml"), format = "graphml"),
      silent = TRUE
    )
  }

  invisible(out_dir)
}

truth_as_list <- function(truth) {
  list(
    model = truth$model,
    n_communities = truth$n_communities,
    communities = as.list(truth$communities),
    brokers = truth$brokers,
    broker_quantile = truth$broker_quantile,
    articulation_points = truth$articulation_points,
    manager = if (!is.null(truth$manager)) as.list(truth$manager) else NULL,
    legacy_company = if (!is.null(truth$legacy_company)) as.list(truth$legacy_company) else NULL,
    block_sizes = if (!is.null(truth$block_sizes)) as.list(truth$block_sizes) else NULL
  )
}

manifest_for <- function(dataset) {
  params <- dataset$params
  list(
    protocol = "SynthONA",
    protocol_version = params$protocol_version,
    scenario_id = params$scenario_id %||% NA_character_,
    label = params$label,
    generated_at = dataset$generated_at,
    parameters = list(
      n = params$n,
      template = params$template,
      topology = params$topology,
      mean_degree = params$mean_degree,
      within_share = params$within_share,
      layers = params$layers,
      snapshots = params$snapshots,
      snapshot_mode = params$snapshot_mode,
      seed_topology = params$seed_topology,
      seed_attributes = params$seed_attributes,
      extras = params$extras
    ),
    observation = if (isTRUE(dataset$is_observed)) unclass(dataset$observation) else NULL,
    counts = list(
      actors = nrow(dataset$nodes),
      ties = nrow(dataset$edges),
      layers = length(unique(dataset$edges$layer)),
      snapshots = length(unique(dataset$edges$snapshot))
    ),
    reproduce = paste0(
      "synthona_generate(synthona_params(n = ", params$n,
      ", template = \"", params$template,
      "\", topology = \"", params$topology,
      "\", mean_degree = ", params$mean_degree,
      ", within_share = ", params$within_share,
      ", seed_topology = ", params$seed_topology,
      ", seed_attributes = ", params$seed_attributes, "))"
    )
  )
}

read_manifest <- function(path) {
  if (dir.exists(path)) path <- file.path(path, "manifest.json")
  if (!file.exists(path)) {
    stop("No manifest found at: ", path, call. = FALSE)
  }
  jsonlite::read_json(path, simplifyVector = TRUE)
}

# ============================================================================
# corpus.R
# ============================================================================

# Benchmark corpora -----------------------------------------------------------

build_corpus <- function(topologies = c("er", "ws", "ba", "sbm", "hierarchy"),
                         sizes = c(200L, 800L),
                         within_shares = c(0.55, 0.75, 0.90),
                         mean_degree = 12,
                         template = "tech_product",
                         layers = "communication",
                         seed_topology = SEED_TOPOLOGY_BASE,
                         seed_attributes = SEED_ATTRIBUTE_BASE) {
  grid <- expand.grid(
    topology = topologies, n = sizes, within_share = within_shares,
    stringsAsFactors = FALSE
  )

  out <- purrr::pmap(grid, function(topology, n, within_share) {
    label <- sprintf("%s_n%d_w%02d", topology, n, round(within_share * 100))
    params <- synthona_params(
      n = n, template = template, topology = topology,
      mean_degree = mean_degree, within_share = within_share,
      layers = layers, snapshots = "snapshot",
      seed_topology = derive_seed(seed_topology, label),
      seed_attributes = seed_attributes,
      label = label
    )
    synthona_generate(params)
  })

  names(out) <- vapply(out, function(d) d$params$label, character(1))
  out
}

corpus_summary <- function(corpus, n_random = 0L) {
  purrr::map_dfr(corpus, function(d) {
    m <- compute_network_metrics(d$nodes, d$edges, n_random = n_random)
    dplyr::bind_cols(
      tibble::tibble(
        label = d$params$label,
        topology = d$params$topology,
        n = d$params$n,
        target_degree = d$params$mean_degree,
        within_share = d$params$within_share
      ),
      m
    )
  })
}

write_corpus <- function(corpus, output_root = default_output_dir(), ...) {
  root <- resolve_output_root(output_root)
  ensure_dir(root)
  for (d in corpus) {
    synthona_write(d, output_root = root, ...)
  }
  write_table_csv(corpus_summary(corpus), file.path(root, "corpus_summary.csv"))
  write_json_pretty(
    list(
      protocol = "SynthONA",
      protocol_version = protocol_version(),
      datasets = names(corpus),
      written_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    ),
    file.path(root, "corpus_manifest.json")
  )
  invisible(root)
}

build_all_scenarios <- function(scenario_ids = names(synthona_registry()), ...) {
  out <- lapply(scenario_ids, function(id) synthona_generate(synthona_scenario(id, ...)))
  names(out) <- scenario_ids
  out
}
