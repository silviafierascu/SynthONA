# Regenerate the Synthetic-ONA benchmark data directory
# =====================================================
#
# The Synthetic-ONA platform (ONA Visual Lab, story-mode, the viz tracks)
# reads a file layout that predates this package and differs from what
# synthona_write() produces: metrics_network is JSON with different field
# names, metrics_node is the node table joined to its metrics, graphs ship as
# both .gml and .graphml.xml, and the manifest is split into
# generation_params.json and validation_report.json.
#
# This script is the adapter. It is not part of the package -- the platform's
# contract should not become CRAN surface -- and it is listed in
# .Rbuildignore.
#
# Curated files are preserved, never rewritten: catalog_entry.json carries
# hand-written titles, persona tags and acceptance criteria, and viz-*.json
# are VizSpecs authored in the Composer.
#
# Usage:
#   Rscript data-raw/make-benchmark-data.R <path-to-Synthetic-ONA-checkout>

suppressMessages(pkgload::load_all(".", quiet = TRUE))

PRESERVE <- c("catalog_entry.json", "talking_nodes.json")
PRESERVE_PATTERN <- "^viz-.*[.]json$"

# Field names the platform expects, mapped from the package's metric names.
# Random reference graphs behind small_world_sigma. The published files carry
# the coefficient, so it is computed here even though the package examples
# skip it for speed.
SIGMA_REFERENCES <- 20L

NETWORK_METRIC_RENAMES <- c(
  mean_degree          = "avg_degree",
  mean_path_length     = "avg_path_length",
  assortativity_degree = "assortativity",
  broker_share         = "broker_concentration"
)

platform_network_metrics <- function(df) {
  names(df) <- ifelse(
    names(df) %in% names(NETWORK_METRIC_RENAMES),
    NETWORK_METRIC_RENAMES[names(df)],
    names(df)
  )
  df
}

# The platform reads validation reports as check_name / measured_value.
platform_validation <- function(df) {
  tibble::tibble(
    check_id = df$check_id,
    check_name = df$description,
    threshold = df$threshold,
    measured_value = df$measured,
    pass = df$pass
  )
}

# The platform loads .graphml.xml everywhere. At the top level of a dataset a
# .gml copy is kept alongside it for tools that prefer that format; snapshot
# directories carry only the GraphML, as the published layout does.
write_graphml <- function(g, out_dir) {
  try(igraph::write_graph(g, file.path(out_dir, "graph.graphml.xml"),
                          format = "graphml"), silent = TRUE)
}

write_graph_pair <- function(g, out_dir) {
  write_graphml(g, out_dir)
  try(igraph::write_graph(g, file.path(out_dir, "graph.gml"),
                          format = "gml"), silent = TRUE)
}

clear_generated <- function(out_dir) {
  if (!dir.exists(out_dir)) return(invisible())
  keep <- c(PRESERVE, list.files(out_dir, pattern = PRESERVE_PATTERN))
  for (f in setdiff(list.files(out_dir), keep)) {
    unlink(file.path(out_dir, f), recursive = TRUE)
  }
}

write_benchmark_scenario <- function(scenario_id, data_root) {
  params <- synthona_scenario(scenario_id)
  benchmark_id <- paste0(synthona_registry()[[scenario_id]]$catalog_id, "-v1")
  out_dir <- file.path(data_root, benchmark_id)

  # topology_primary and topology_overlay are design notes about how the
  # benchmark was conceived, not generation parameters. Carry them forward
  # from the existing file rather than inventing them.
  old_params_path <- file.path(out_dir, "generation_params.json")
  old <- if (file.exists(old_params_path)) {
    jsonlite::read_json(old_params_path, simplifyVector = TRUE)
  } else {
    list()
  }

  dataset <- synthona_generate(params)
  snapshots <- unique(dataset$edges$snapshot)
  first <- snapshots[1]

  clear_generated(out_dir)
  ensure_dir(out_dir)
  ensure_dir(file.path(out_dir, "layers"))

  write_table_csv(dataset$nodes, file.path(out_dir, "nodes.csv"))

  for (l in unique(dataset$edges$layer)) {
    write_table_csv(
      dataset$edges[dataset$edges$layer == l, , drop = FALSE],
      file.path(out_dir, "layers", paste0("edges_", l, ".csv"))
    )
  }

  node_metrics <- compute_node_metrics(dataset$nodes, dataset$edges, snapshot = first)
  # The platform expects one wide table: every attribute, then every metric.
  write_table_csv(
    dplyr::left_join(
      dataset$nodes,
      dplyr::select(node_metrics, -dplyr::any_of("snapshot")),
      by = "person_id"
    ) |> dplyr::mutate(snapshot = first),
    file.path(out_dir, "metrics_node.csv")
  )
  write_table_csv(
    compute_group_metrics(dataset$nodes, dataset$edges, snapshot = first),
    file.path(out_dir, "metrics_group.csv")
  )
  write_table_csv(
    compute_outcomes(dataset$nodes, dataset$edges, params, snapshot = first),
    file.path(out_dir, "metrics_outcomes.csv")
  )

  network <- platform_network_metrics(
    purrr::map_dfr(snapshots, function(s) {
      compute_network_metrics(dataset$nodes, dataset$edges, snapshot = s, n_random = SIGMA_REFERENCES)
    })
  )
  write_json_pretty(network, file.path(out_dir, "metrics_network.json"))

  write_json_pretty(
    platform_validation(validate_dataset(dataset)),
    file.path(out_dir, "validation_report.json")
  )

  # Written even when a scenario defines none, so every dataset carries the
  # same file set; the published layout uses an empty array for those.
  write_json_pretty(scenario_metrics(dataset),
                    file.path(out_dir, "scenario_metrics.json"))

  write_graph_pair(synthona_graph(dataset, snapshot = first), out_dir)

  ensure_dir(file.path(out_dir, "snapshots"))
  for (s in snapshots) {
    snap_dir <- file.path(out_dir, "snapshots", s)
    ensure_dir(snap_dir)
    write_table_csv(
      dataset$edges[dataset$edges$snapshot == s, , drop = FALSE],
      file.path(snap_dir, "edges.csv")
    )
    write_json_pretty(
      platform_network_metrics(compute_network_metrics(
        dataset$nodes, dataset$edges, snapshot = s, n_random = SIGMA_REFERENCES
      )),
      file.path(snap_dir, "metrics_network.json")
    )
    write_graphml(synthona_graph(dataset, snapshot = s), snap_dir)
  }

  write_json_pretty(
    list(
      benchmark_id = benchmark_id,
      scenario_id = scenario_id,
      title = params$label,
      topology_primary = old$topology_primary %||% params$topology,
      topology_overlay = old$topology_overlay %||% list(),
      node_count = nrow(dataset$nodes),
      layers_generated = params$layers,
      snapshots = snapshots,
      seed_topology = params$seed_topology,
      seed_attributes = params$seed_attributes,
      directed_layers = DIRECTED_LAYERS,
      catalog_version = old$catalog_version %||% "v3",
      generated_with = paste("SynthONA", utils::packageVersion("SynthONA"))
    ),
    file.path(out_dir, "generation_params.json")
  )

  invisible(out_dir)
}

# Baseline reference networks.
#
# The prototype built these from fixed tie probabilities (ER p = 0.06,
# SBM within = 0.20 / between = 0.08 and so on), which is why the published
# medium baselines carry roughly four times the mean degree of the small ones
# and cannot be compared across sizes. This package calibrates on mean degree
# instead, so each model's identity is expressed through within_share --
# derived from the prototype's within/between ratios -- and FRAGMENTED keeps
# its defining sparsity through a lower mean degree rather than a lower
# density that calibration would immediately undo.
BASELINE_SPECS <- list(
  ER             = list(topology = "er",        mean_degree = 6),
  WS             = list(topology = "ws",        mean_degree = 6),
  BA             = list(topology = "ba",        mean_degree = 6),
  SBM_HEALTHY    = list(topology = "sbm",       mean_degree = 6, within_share = 0.37),
  SBM_SILOS      = list(topology = "sbm",       mean_degree = 6, within_share = 0.70),
  SBM_FRAGMENTED = list(topology = "sbm",       mean_degree = 3, within_share = 0.49),
  SBM_INNOVATION = list(topology = "sbm",       mean_degree = 6, within_share = 0.26),
  HIER_CORP      = list(topology = "hierarchy", mean_degree = 6)
)

BASELINE_SIZES <- c(small = 100L, medium = 500L)

write_baseline_suite <- function(data_root) {
  manifest <- list()

  for (size_name in names(BASELINE_SIZES)) {
    n <- BASELINE_SIZES[[size_name]]
    for (model in names(BASELINE_SPECS)) {
      spec <- BASELINE_SPECS[[model]]
      params <- do.call(synthona_params, c(
        list(
          n = n,
          template = "tech_product",
          layers = "communication",
          scenario_id = paste0("BASE_", toupper(size_name)),
          label = paste(model, size_name)
        ),
        spec
      ))
      dataset <- synthona_generate(params)

      name <- paste0(model, "_", size_name)
      out_dir <- file.path(data_root, name)
      clear_generated(out_dir)
      ensure_dir(out_dir)

      write_table_csv(dataset$nodes, file.path(out_dir, "nodes.csv"))
      write_table_csv(dataset$edges, file.path(out_dir, "edges_communication.csv"))

      net <- platform_network_metrics(
        compute_network_metrics(dataset$nodes, dataset$edges, n_random = SIGMA_REFERENCES)
      )
      write_json_pretty(net, file.path(out_dir, "metrics_network.json"))
      write_graph_pair(synthona_graph(dataset), out_dir)

      manifest[[name]] <- net
    }
  }

  write_json_pretty(manifest, file.path(data_root, "baseline_manifest.json"))
  invisible(manifest)
}

# -----------------------------------------------------------------------------

main <- function(repo_root) {
  data_root <- file.path(repo_root, "data")
  if (!dir.exists(data_root)) {
    stop("No data/ directory under: ", repo_root, call. = FALSE)
  }

  for (id in names(synthona_registry())) {
    catalog_id <- synthona_registry()[[id]]$catalog_id
    if (!dir.exists(file.path(data_root, paste0(catalog_id, "-v1")))) {
      message("skipping ", id, " -- no published dataset to replace")
      next
    }
    message("scenario  ", id)
    write_benchmark_scenario(id, data_root)
  }

  message("baselines")
  write_baseline_suite(data_root)
  message("done")
}

# Sourcing the script with no arguments defines the functions and does
# nothing else, which is what the checks below rely on.
local({
  args <- commandArgs(trailingOnly = TRUE)
  if (!interactive() && length(args) > 0) main(args[[1]])
})
