# SynthONA

**A Parameterised Protocol for Generating Synthetic Organisational Network Benchmark Datasets**

SynthONA generates synthetic organisational network analysis (ONA) datasets from an
explicit parameter specification, and ships each dataset with the ground truth used
to build it. That makes it possible to ask a question you cannot ask of real
organisational data: *did this method actually recover what was there?*

## Why another network generator

Organisational network research has no shared benchmark. Real ONA data is
confidential, so methods are validated on whatever proprietary dataset the authors
had access to, and results cannot be compared across studies. Generic graph
generators do not help much either: they produce networks without departments,
hierarchies, seniority, multiple relation types, or the measurement error that
defines survey-collected network data.

SynthONA addresses three things specifically.

**Datasets stay comparable across organisation sizes.** Tie volume is calibrated on
*mean degree* rather than tie probability. Fixing a probability holds density
constant, which forces average degree to grow in proportion to headcount — a
1500-person organisation would come out roughly fifteen times better connected per
person than a 100-person one. Real contact volume is bounded by time and attention,
so degree stays broadly flat while density falls. Without this, size is a confound
in every cross-size comparison.

```r
library(SynthONA)

vapply(c(200, 800, 2000), function(n) {
  d <- synthona_generate(synthona_params(n = n, mean_degree = 12))
  round(mean(igraph::degree(synthona_graph(d))), 1)
}, numeric(1))
#> [1] 10.9 11.4 11.5
```

**Every dataset carries its answer key.** The planted communities, the true brokers,
the articulation points and the reporting tree are recorded on the complete network
before any noise is applied.

```r
d <- synthona_generate(synthona_params(n = 400, topology = "sbm", within_share = 0.85))
d$truth
#> <synthona_truth> sbm
#>   planted communities : 7
#>   true brokers        : 40 (top 10% by betweenness)
#>   articulation points : 0
```

**Measurement error is modelled separately from generation.** Survey data is not a
census. People ignore the survey, name generators cap how many colleagues can be
listed, and memory favours strong ties. Applying these to a known network is what
turns a generator into a benchmark.

```r
scores <- lapply(
  list(
    census = observation_design(),
    typical = observation_design(response_rate = 0.5, name_generator_limit = 5,
                                 recall_probability = 0.8),
    poor = observation_design(response_rate = 0.3, name_generator_limit = 3,
                              recall_probability = 0.6)
  ),
  function(design) {
    obs <- synthona_observe(d, design)
    score_communities(d$truth, igraph::cluster_louvain(synthona_graph(obs)))$ari
  }
)
#> census 0.669   typical 0.549   poor 0.375
```

The same true network can be observed many times under different survey designs, so
you can ask how much response rate a method needs before its conclusions stop
holding.

## Installation

```r
# install.packages("remotes")
remotes::install_github("sfierascu/SynthONA")
```

## Getting started

```r
library(SynthONA)

# A specification is an inspectable object
p <- synthona_params(
  n = 500,
  template = "tech_product",
  topology = "hierarchy",
  mean_degree = 12,
  within_share = 0.72,
  layers = c("communication", "advice", "trust")
)
p

d <- synthona_generate(p)
validate_dataset(d)
synthona_write(d, "output")
```

Ten pre-specified scenarios cover recurring organisational situations, each framed
around the question it is meant to answer.

```r
scenario_table()[, c("scenario_id", "question")]

d <- synthona_generate(synthona_scenario("MA_M"))       # post-merger integration
d <- synthona_generate(synthona_scenario("MA_M", n = 400))  # same shape, smaller
```

For method comparison, `build_corpus()` sweeps a grid at constant tie volume:

```r
corpus <- build_corpus(
  topologies = c("er", "sbm", "hierarchy"),
  sizes = c(200, 800),
  within_shares = c(0.55, 0.75, 0.90)
)
corpus_summary(corpus)
```

## Reproducibility

Generation draws from two independent seed streams, one for structure and one for
attributes. Holding the attribute seed fixed while varying the topology seed gives
structural variants of the same workforce. Sub-seeds are derived from *names* rather
than positions, so adding a layer or a snapshot does not shift the random draws of
existing ones.

No function alters the calling session's RNG state, and every export writes a
`manifest.json` recording the full specification and protocol version, so any dataset
can be regenerated from its files alone.

```r
identical(synthona_generate(p)$edges, synthona_generate(p)$edges)
#> TRUE
```

## Citation

If you use SynthONA in published work, please cite the protocol. Run
`citation("SynthONA")` for the current reference.

## Licence

MIT © Silvia I. Fierăscu
