# Documentation plan

## The four layers, and who each is for

Documentation fails when these blur into one another. The first decision for
any piece of writing is which layer it belongs to.

| Layer | Reader | Question it answers |
|---|---|---|
| README | Someone deciding in 60 seconds | Is this for me? |
| Reference manual (`man/` to PDF and pkgdown) | Someone who knows the function name | What are the arguments and what comes back? |
| Articles and vignettes | Someone with a task | How do I do the thing I came for? |
| JOSS paper | Someone deciding whether to trust and cite | Why does this exist, and is it sound? |

Nothing should appear in two of them. Where it does, the copies drift.

## Gaps, ordered by consequence

1. **pkgdown index drift.** Exported topics missing from `_pkgdown.yml` fail
   the site build outright.
2. **No article on scoring against ground truth.** The package's central
   claim, currently one section inside a general vignette.
3. **No article on the measurement layer.** The strongest differentiator, with
   nothing explaining how to use it.
4. **Exported functions without examples.** The most visible gap to a CRAN or
   JOSS reviewer.
5. **No package overview page** worth landing on from `?SynthONA`.
6. **Nothing on extending the package**, despite the pluggable engine design
   being a stated selling point.

## Article set

| # | Article | Answers | Build as |
|---|---|---|---|
| 1 | Getting started | What does a dataset look like, end to end? | Vignette |
| 2 | Designing a specification | How do I express the organisation I have in mind? | Vignette |
| 3 | Scoring a method against ground truth | How do I benchmark my algorithm? | Vignette |
| 4 | Simulating survey measurement error | What did my survey design cost me? | Article |
| 5 | Working with scenarios | How do I use the registry, and adapt one? | Article |
| 6 | Corpora and parameter sweeps | How do I vary one thing and hold the rest? | Article |
| 7 | Extending SynthONA | How do I add a topology engine or layer? | Article |

Three built vignettes keeps `R CMD check` time down. The rest live in
`pkgdown/articles/` and build only for the site.

## Writing order

1. Fix the pkgdown index, which unblocks the site.
2. Article 3, then article 4: the two that carry the package's argument.
3. Examples for the exported functions that lack them.
4. Articles 2, 5 and 6.
5. Article 7 and the package overview page.
6. The paper, which by then can cite the articles rather than re-explain them.

## What belongs in the paper, not here

Motivation and related work, the validation evidence, comparison with existing
generators, and the statement of need. If those creep into the articles they
get written twice and drift apart.

## Conventions

* Every article opens with the question it answers, not with a definition.
* All code runs. Sizes stay small enough for CRAN time limits in built
  vignettes; `pkgdown`-only articles may be larger.
* Reference pages describe arguments and return values. Articles describe
  tasks. Neither explains the other's material.
