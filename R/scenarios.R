# Scenario registry -----------------------------------------------------------
#
# Scenarios are named, citable parameter specifications covering recurring
# organisational situations. They are ordinary `synthona_params` objects: any
# scenario can be inspected, modified with `update_params()`, or ignored
# entirely in favour of a hand-built specification.

#' The scenario registry
#'
#' @return A named list of scenario definitions.
#' @export
#' @examples
#' names(synthona_registry())
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
      # Three snapshots so an integrating tool has something to animate and
      # something to compare; they differ through ordinary evolution.
      snapshots = c("snapshot", "animate", "compare"),
      tags = c("developer_pack", "multiplex", "api_ready")
    ),
    AI_M = list(
      catalog_id = "ONA-BMK-AI-M",
      title = "AI adoption over four quarters",
      question = "How does an AI rollout reshape advice seeking?",
      n = 1200L, template = "tech_product", topology = "hierarchy",
      mean_degree = 13, within_share = 0.68,
      layers = c("communication", "advice", "trust", "innovation",
                 "decision_influence", "tool_interaction"),
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
      layers = c("reporting", "communication", "advice", "mentorship", "trust"),
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
      layers = c("communication", "trust", "advice", "decision_influence",
                 "innovation"),
      snapshots = c("M0", "M3", "M6", "M12"),
      extras = list(
        cross_legacy_thinning = 0.45,
        evolution = list(churn = 0.07),
        shocks = list(
          list(at = "M3", layer = "advice", add = 0.20),
          list(at = "M6", layer = "advice", add = 0.30, weight_delta = 0.05),
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
      layers = c("communication", "trust", "energy", "innovation", "mentorship"),
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
      layers = c("decision_influence", "advice", "trust", "communication",
                 "mentorship"),
      snapshots = c("baseline", "removal_sim"),
      snapshot_mode = "alternative",
      extras = list(
        evolution = list(churn = 0, weight_drift = 0),
        shocks = list(list(at = "removal_sim", remove_actor = "top_broker"))
      ),
      tags = c("key_person_risk", "succession", "resilience")
    )
  )
}

#' Build the parameters for a registered scenario
#'
#' @param scenario_id Scenario identifier, see [synthona_registry()].
#' @param ... Parameter overrides passed to [update_params()], for example
#'   `n = 2000` to generate a larger organisation of the same shape.
#'
#' @return A [synthona_params()] object.
#' @export
#' @examples
#' synthona_scenario("DEMO_M")
#'
#' # The same scenario at a different size
#' synthona_scenario("DEMO_M", n = 2000)
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

#' The scenario registry as a table
#'
#' @return A tibble with one row per scenario.
#' @export
#' @examples
#' scenario_table()
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
