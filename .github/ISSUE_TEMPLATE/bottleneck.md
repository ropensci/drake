---
name: Bottleneck
about: drake is too slow or consumes too many resources.
title: ''
labels: 'topic: performance'
assignees: wlandau

---

## Prework

- [ ] Read and abide by `drake`'s [code of conduct](https://github.com/ropensci/drake/blob/master/CODE_OF_CONDUCT.md).
- [ ] Search for duplicates among the [existing issues](https://github.com/ropensci/drake/issues), both open and closed.
- [ ] Advanced users: verify that the bottleneck still persists in the current development version (i.e. `remotes::install_github("ropensci/drake")`) and mention the [SHA-1 hash](https://git-scm.com/book/en/v1/Getting-Started-Git-Basics#Git-Has-Integrity) of the [Git commit you install](https://github.com/ropensci/drake/commits/master).

## Description

Describe the bottleneck clearly and concisely. 

## Reproducible example

Provide a minimal example with code that reproduces the slowdown.

## Benchmarks

How poorly does `drake` perform? Please share benchmarks: runtimes, memory consumption, [flame graphs](https://github.com/ropensci/drake/issues/647#issuecomment-451760866), etc. Tools to consider:

-  [`Rprof()`](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/Rprof.html), [`jointprof`](https://github.com/r-prof/jointprof), [`profile`](https://github.com/r-prof/profile), [`pprof`](https://github.com/google/pprof). Example with `drake` [here](https://github.com/wlandau/drake-examples/tree/master/overhead).
- [`profvis`](https://github.com/rstudio/profvis), though beware https://github.com/rstudio/profvis/issues/104.
- [`microbenchmark`](https://github.com/joshuaulrich/microbenchmark) and [`bench`](https://github.com/r-lib/bench).
