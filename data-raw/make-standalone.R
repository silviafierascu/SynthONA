# Build the standalone implementation ------------------------------------------
#
# The protocol document offers two ways to run: calling the installed package,
# or sourcing a single self-contained script that needs no package at all.
#
# The standalone script is GENERATED from R/ rather than maintained by hand.
# Two hand-maintained copies of the same protocol would drift, and the version
# a reader runs would stop matching the version that was tested. Everything
# here is derived; edit R/ and re-run this script.
#
#   Rscript data-raw/make-standalone.R

# Load order. Only matters for definitions evaluated at load time (the
# constants); functions resolve their dependencies when called.
FILE_ORDER <- c(
  "constants.R", "utils.R", "rng.R", "edges.R", "templates.R", "params.R",
  "attributes.R", "topology.R", "layers.R", "temporal.R", "truth.R",
  "observe.R", "metrics.R", "validate.R", "scenario_extras.R", "generate.R",
  "scenarios.R", "export.R", "corpus.R"
)

strip_roxygen <- function(lines) {
  keep <- !grepl("^\\s*#'", lines)
  lines <- lines[keep]
  # Collapse runs of blank lines left behind by the stripped blocks.
  blank <- !nzchar(trimws(lines))
  drop <- blank & c(FALSE, utils::head(blank, -1))
  lines[!drop]
}

build_standalone <- function(pkg_root = ".",
                             out = file.path(pkg_root, "inst", "standalone",
                                             "synthona-standalone.R")) {
  r_dir <- file.path(pkg_root, "R")
  present <- list.files(r_dir, pattern = "[.]R$")
  missing <- setdiff(present, c(FILE_ORDER, "SynthONA-package.R"))
  if (length(missing) > 0) {
    stop(
      "R/ contains files not listed in FILE_ORDER: ",
      paste(missing, collapse = ", "),
      ". Add them so the standalone stays complete.",
      call. = FALSE
    )
  }

  version <- read.dcf(file.path(pkg_root, "DESCRIPTION"), "Version")[[1]]

  header <- c(
    "# SynthONA - standalone implementation",
    "#",
    "# A Parameterised Protocol for Generating Synthetic Organisational",
    "# Network Benchmark Datasets.",
    "#",
    paste0("# Generated from the package sources, version ", version, "."),
    "# DO NOT EDIT BY HAND - edit R/ and re-run data-raw/make-standalone.R.",
    "#",
    "# This file provides the complete protocol without requiring the SynthONA",
    "# package to be installed. It still needs these CRAN packages:",
    "#",
    "#   dplyr, igraph, jsonlite, purrr, tibble",
    "#",
    "# Usage:",
    "#   source(\"synthona-standalone.R\")",
    "#   d <- synthona_generate(synthona_params(n = 300, topology = \"sbm\"))",
    "",
    ".synthona_required <- c(\"dplyr\", \"igraph\", \"jsonlite\", \"purrr\", \"tibble\")",
    ".synthona_missing <- .synthona_required[",
    "  !vapply(.synthona_required, requireNamespace, logical(1), quietly = TRUE)",
    "]",
    "if (length(.synthona_missing) > 0) {",
    "  stop(",
    "    \"The standalone protocol needs these packages: \",",
    "    paste(.synthona_missing, collapse = \", \"),",
    "    \"\\nInstall them with install.packages(c(\\\"\",",
    "    paste(.synthona_missing, collapse = \"\\\", \\\"\"), \"\\\"))\",",
    "    call. = FALSE",
    "  )",
    "}",
    "rm(.synthona_required, .synthona_missing)",
    ""
  )

  body <- unlist(lapply(FILE_ORDER, function(f) {
    path <- file.path(r_dir, f)
    c(
      "",
      paste0("# ", strrep("=", 76)),
      paste0("# ", f),
      paste0("# ", strrep("=", 76)),
      "",
      strip_roxygen(readLines(path, warn = FALSE))
    )
  }))

  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(header, body), out)

  message(sprintf(
    "Wrote %s (%d lines from %d modules)",
    out, length(header) + length(body), length(FILE_ORDER)
  ))
  invisible(out)
}

if (sys.nframe() == 0L) {
  build_standalone(".")
}
