# Parameter specification -----------------------------------------------------

SYNTHONA_TOPOLOGIES <- c("hierarchy", "sbm", "sbm_dual_legacy", "er", "ws", "ba")

#' Specify the parameters of a synthetic organisational network
#'
#' Builds and validates the parameter object that drives every generator in
#' the protocol. A `synthona_params` object is a complete, inspectable
#' description of a dataset: given the same object, [synthona_generate()]
#' reproduces the same network exactly.
#'
#' @section Calibration on mean degree:
#' Tie volume is specified as `mean_degree`, the average number of contacts a
#' person has, rather than as a tie probability. Fixing a probability makes
#' density constant and therefore forces average degree to grow in proportion
#' to headcount, so a 1500-person organisation would come out roughly fifteen
#' times better connected per capita than a 100-person one. Real organisations
#' do not behave that way: individual contact volume is bounded by time and
#' attention, so degree stays broadly flat while density falls. Calibrating on
#' degree keeps datasets comparable across sizes, which is what makes a
#' benchmark corpus usable for method comparison.
#'
#' `mean_degree` is the target for the *base structure*. Each relational layer
#' is a thinned view of it, so a single layer has lower degree than the target
#' (communication retains about 85% of ties, mentorship about 25%), while the
#' union across several layers approaches it. Use [validate_dataset()] to see
#' the realised figure for any specification.
#'
#' @param n Number of employees. At least 20.
#' @param template Industry template identifier, see [synthona_templates()].
#' @param topology Base structure. One of `"hierarchy"`, `"sbm"`,
#'   `"sbm_dual_legacy"`, `"er"`, `"ws"` or `"ba"`.
#' @param mean_degree Target average degree of the base structure.
#' @param within_share Proportion of the ties of a person that fall inside
#'   their own department, between 0 and 1. Higher values produce more
#'   strongly siloed organisations.
#' @param layers Relational layers to generate, see [SYNTHONA_LAYERS].
#' @param snapshots Character vector naming the snapshots to produce. A single
#'   value generates a cross-section; several values generate a panel.
#' @param snapshot_mode Either `"cumulative"`, where each snapshot evolves from
#'   the previous one, or `"alternative"`, where each snapshot is an
#'   independent variant of the same baseline. Use `"alternative"` for
#'   what-if comparisons such as competing reorganisation options.
#' @param seed_topology Integer seed for structural randomness.
#' @param seed_attributes Integer seed for attribute randomness. Holding this
#'   fixed while varying `seed_topology` produces structural variants of the
#'   same workforce.
#' @param scenario_id Optional scenario identifier, set when the parameters
#'   come from [synthona_scenario()].
#' @param label Optional human-readable label.
#' @param extras Named list of scenario-specific settings.
#'
#' @return An object of class `synthona_params`.
#' @export
#' @examples
#' p <- synthona_params(n = 300, topology = "sbm", mean_degree = 14)
#' p
#'
#' # A more strongly siloed organisation of the same size and tie volume
#' silos <- synthona_params(n = 300, topology = "sbm", mean_degree = 14,
#'                          within_share = 0.9)
#' silos$within_share
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

#' @export
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

#' Modify an existing parameter specification
#'
#' Returns a new validated `synthona_params` with the supplied fields replaced.
#' Useful for sweeping one parameter while holding everything else fixed.
#'
#' @param params A `synthona_params` object.
#' @param ... Named parameters to override.
#'
#' @return A new `synthona_params` object.
#' @export
#' @examples
#' base <- synthona_params(n = 200)
#' sizes <- lapply(c(200, 400, 800), function(k) update_params(base, n = k))
#' vapply(sizes, function(p) p$n, integer(1))
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
