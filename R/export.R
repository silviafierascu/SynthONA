# Export ----------------------------------------------------------------------

#' Write a dataset to disk
#'
#' Writes the node table, the edge table (whole, per layer and per snapshot),
#' the computed metrics, the ground truth and a manifest. The manifest records
#' the full parameter specification and the protocol version, so any exported
#' dataset can be regenerated exactly from the files alone.
#'
#' Nothing is written outside `output_root`, which defaults to a session
#' temporary directory.
#'
#' @param dataset A `synthona_dataset`.
#' @param output_root Directory to write into.
#' @param graphml Whether to also write a GraphML file.
#' @param metrics Whether to compute and write metric tables.
#'
#' @return The path written to, invisibly.
#' @export
#' @examples
#' d <- synthona_generate(synthona_params(n = 60))
#' path <- synthona_write(d, file.path(tempdir(), "demo"))
#' list.files(path)
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

#' Read a dataset manifest
#'
#' @param path Path to a directory written by [synthona_write()], or to a
#'   `manifest.json` file.
#'
#' @return The manifest as a list.
#' @export
read_manifest <- function(path) {
  if (dir.exists(path)) path <- file.path(path, "manifest.json")
  if (!file.exists(path)) {
    stop("No manifest found at: ", path, call. = FALSE)
  }
  jsonlite::read_json(path, simplifyVector = TRUE)
}
