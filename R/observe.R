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

#' Specify how a network is observed
#'
#' @param response_rate Share of employees who respond to the survey. The
#'   outgoing ties of a non-respondent are not recorded, though ties others
#'   name *to* them still are, which reproduces the characteristic asymmetry of
#'   survey network data.
#' @param name_generator_limit Maximum number of alters a respondent may name.
#'   `Inf` imposes no limit. Respondents keep their strongest ties.
#' @param recall_probability Probability that a tie of median strength is
#'   recalled. Recall rises with tie strength, so weak ties are lost first.
#' @param roster_coverage Share of the organisation present on the survey
#'   roster at all. Actors off the roster cannot be named by anyone.
#' @param response_bias Degree to which response propensity tracks seniority
#'   and tenure. At 0 non-response is uniform; at 1 junior and newly hired
#'   staff are markedly less likely to respond.
#'
#' @return An object of class `synthona_observation`.
#' @export
#' @examples
#' observation_design(response_rate = 0.65, name_generator_limit = 8)
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

#' @export
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

#' Observe a generated network through a survey design
#'
#' Applies non-response, name-generator truncation and recall bias to a
#' complete network, returning what a survey would actually have captured. The
#' underlying dataset is left untouched, so the same true network can be
#' observed repeatedly under different designs.
#'
#' @param dataset A `synthona_dataset` from [synthona_generate()].
#' @param design An [observation_design()], or `NULL` for a census.
#' @param seed Integer seed for the measurement draws.
#'
#' @return A `synthona_dataset` whose `edges` are the observed ties, carrying
#'   an `observation` element recording what was applied. The `truth` element
#'   is preserved unchanged.
#' @export
#' @examples
#' d <- synthona_generate(synthona_params(n = 200))
#' obs <- synthona_observe(d, observation_design(response_rate = 0.6,
#'                                               name_generator_limit = 5))
#' nrow(d$edges)
#' nrow(obs$edges)
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

#' Select an exact share of actors, optionally weighted
#'
#' Drawing a fixed count rather than an independent coin per actor means the
#' realised share always matches the requested one, and a share of 1 selects
#' everybody regardless of how the weights are tilted.
#'
#' @param n Number of actors.
#' @param share Share to select, between 0 and 1.
#' @param weights Optional selection weights.
#' @param seed Integer seed.
#' @return A logical vector of length `n`.
#' @keywords internal
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

#' Keep only the strongest alters each respondent names
#'
#' @param edges An edge table.
#' @param limit Maximum alters per ego, per layer and snapshot.
#' @return The truncated edge table.
#' @keywords internal
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
