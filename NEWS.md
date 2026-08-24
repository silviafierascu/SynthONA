# SynthONA 0.1.0

First release of the protocol as an R package.

## Design

* Tie volume is calibrated on **target mean degree** rather than tie
  probability, so datasets remain comparable across organisation sizes.
  Holding a probability constant fixes density and forces average degree to
  grow with headcount, which confounds any cross-size comparison.
* Every dataset carries a **ground truth** record: planted communities, true
  brokers, articulation points and the reporting tree, computed on the
  complete network before any noise is applied. `score_communities()` and
  `score_brokers()` score a method against it.
* A **measurement layer** (`synthona_observe()`) applies survey non-response,
  name-generator truncation and recall bias. It is separate from generation, so
  one true network can be observed under many survey designs.
* Parameters are a validated, inspectable object (`synthona_params()`), and
  scenarios are ordinary parameter objects that can be modified with
  `update_params()`.
* Departments are generated **heterogeneously**: some units are more inward
  looking than others, rather than every department sharing one cohesion
  parameter.
* Longitudinal data is generated as a **process**. Under `"cumulative"` mode
  each wave evolves from the one before it, so dissolved ties stay dissolved.
  `"alternative"` mode is available for what-if comparisons.
* Scenario-specific behaviour is expressed as **declarative shocks** in the
  parameter object rather than branching on scenario identifiers inside the
  generators, so users can define their own.

## Two ways to run

* The protocol document runs in either **package mode** (`library(SynthONA)`) or
  **standalone mode**, which sources a single self-contained script and needs no
  package installed. Both execute the same protocol and produce identical
  datasets.
* The standalone script is *generated* from `R/` by
  `data-raw/make-standalone.R` rather than maintained separately, and a test
  asserts that the two return identical nodes, edges and ground truth. Two
  hand-maintained copies would drift without either the reader or the author
  noticing.
* Demonstration chunks seed `igraph::cluster_louvain()`, which is stochastic and
  otherwise returns a different partition on every render. Generation itself is
  deterministic; the method under test need not be, and when benchmarking in
  earnest the method should be run many times and its distribution reported
  rather than a single draw.

## Correctness

Fixes to defects carried by the prototype implementation, each covered by a
regression test:

* **Stochastic block model blocks are aligned with departments.** `sample_sbm()`
  assigns vertices to blocks contiguously; attaching attributes positionally to
  an unsorted node table scattered every planted community across the
  organisation, driving department assortativity to approximately zero and
  inverting every E-I index. Affected all block-model and post-merger datasets.
* **The undirected projection no longer creates self-loops.** Assigning `from`
  and then deriving `to` from the updated value inside a single `mutate()`
  collapsed every pair with `from > to` onto a self-loop, losing roughly half
  of all ties from every metric.
* **Tie strength is inverted before use as a path distance.** `igraph` reads
  edge weights as distances, so passing strength directly ranked the weakest
  ties as the most efficient routes.
* **No function alters the caller's RNG state.** All randomness runs through
  `with_local_seed()`, which restores `.Random.seed` on exit.
* **Outcome variables are reproducible.** They previously drew unseeded noise
  and returned different values on each call.
* **`layer_keep_prob()` falls back instead of erroring** on an unknown layer;
  `[[` raised a subscript error, making the documented default unreachable.
* **Sub-seeds are derived from names, not positions**, so adding a layer or
  snapshot no longer shifts the draws of existing ones.
* **Validation checks can fail.** The previous brokerage check counted actors
  above their own 90th percentile of betweenness, which is approximately a
  tenth of any organisation regardless of structure. Brokerage concentration is
  now compared against a random graph of equal order and density.
