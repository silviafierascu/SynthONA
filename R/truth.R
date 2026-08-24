# Ground truth ----------------------------------------------------------------
#
# A benchmark is only a benchmark if the answer ships with the question. The
# functions here record what the generator actually built, computed on the
# complete network *before* any measurement error is applied. Scoring a method
# means comparing what it recovers from the observed data against these labels.

#' Record the ground truth of a generated dataset
#'
#' Captures the structures the generator planted, together with the actor-level
#' roles implied by the complete network: the community each person belongs to,
#' who the genuine brokers are, and which actors are true articulation points.
#'
#' @param nodes The node table.
#' @param edges The complete (unobserved) edge table.
#' @param structure_truth The `truth` element returned by
#'   [generate_base_structure()].
#' @param params A [synthona_params()] object.
#' @param broker_quantile Betweenness quantile above which an actor counts as a
#'   true broker.
#'
#' @return An object of class `synthona_truth`.
#' @export
#' @examples
#' p <- synthona_params(n = 120, topology = "sbm")
#' d <- synthona_generate(p)
#' d$truth
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

#' @export
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

#' Score a recovered partition against the planted communities
#'
#' Reports the adjusted Rand index and normalised mutual information between a
#' community assignment a method recovered and the one the protocol planted.
#'
#' @param truth A `synthona_truth` object.
#' @param recovered A vector of community labels, named by `person_id`, or a
#'   `igraph` communities object.
#'
#' @return A one-row tibble with `ari` and `nmi`.
#' @export
#' @examples
#' d <- synthona_generate(synthona_params(n = 200, topology = "sbm"))
#' g <- synthona_graph(d)
#' found <- igraph::cluster_louvain(g)
#' score_communities(d$truth, found)
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

#' Score recovered brokers against the true brokers
#'
#' @param truth A `synthona_truth` object.
#' @param recovered Character vector of `person_id`s a method identified as
#'   brokers.
#'
#' @return A one-row tibble of precision, recall and F1.
#' @export
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
