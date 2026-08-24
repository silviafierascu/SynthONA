# Protocol constants ----------------------------------------------------------

#' Relational layers the protocol can generate
#'
#' @format A character vector of layer identifiers.
#' @export
SYNTHONA_LAYERS <- c(
  "communication", "collaboration", "advice", "trust", "innovation",
  "mentorship", "decision_influence", "reporting", "tool_interaction", "energy"
)

#' Layers generated as directed relations
#'
#' Advice, mentorship, reporting, decision influence and tool interaction are
#' inherently asymmetric; the remaining layers are generated undirected.
#'
#' @format A character vector of layer identifiers.
#' @export
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

#' Baseline reference topologies
#'
#' @format A character vector of baseline model identifiers.
#' @export
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
