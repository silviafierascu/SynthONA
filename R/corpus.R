# Benchmark corpora -----------------------------------------------------------

#' Build a benchmark corpus
#'
#' Generates a grid of datasets that vary systematically along the dimensions a
#' method comparison needs to hold constant or sweep: topology, size and the
#' strength of the planted community structure. Because tie volume is
#' calibrated on mean degree, datasets of different sizes remain comparable and
#' size can be treated as an experimental factor rather than a confound.
#'
#' @param topologies Base structures to include.
#' @param sizes Organisation sizes to include.
#' @param within_shares Within-department tie shares to include.
#' @param mean_degree Target mean degree, held constant across the corpus.
#' @param template Industry template.
#' @param layers Layers to generate.
#' @param seed_topology,seed_attributes Base seeds. Each cell of the grid gets
#'   its own derived seed.
#'
#' @return A list of `synthona_dataset` objects, named by cell.
#' @export
#' @examples
#' corpus <- build_corpus(
#'   topologies = c("sbm", "hierarchy"),
#'   sizes = c(150, 300),
#'   within_shares = 0.75
#' )
#' names(corpus)
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

#' Summarise a corpus
#'
#' @param corpus A list of `synthona_dataset` objects from [build_corpus()].
#' @param n_random Random reference graphs for the small-world coefficient.
#'
#' @return A tibble with one row per dataset.
#' @export
#' @examples
#' corpus <- build_corpus(topologies = "sbm", sizes = c(150, 300),
#'                        within_shares = 0.75)
#' corpus_summary(corpus)
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

#' Write a corpus to disk
#'
#' @param corpus A list of `synthona_dataset` objects.
#' @param output_root Directory to write into.
#' @param ... Passed to [synthona_write()].
#'
#' @return The corpus root path, invisibly.
#' @export
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

#' Generate every registered scenario
#'
#' @param scenario_ids Scenarios to build. Defaults to all of them.
#' @param ... Parameter overrides applied to every scenario.
#'
#' @return A named list of `synthona_dataset` objects.
#' @export
build_all_scenarios <- function(scenario_ids = names(synthona_registry()), ...) {
  out <- lapply(scenario_ids, function(id) synthona_generate(synthona_scenario(id, ...)))
  names(out) <- scenario_ids
  out
}
