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

Provide a minimal reproducible example with code and output that demonstrates the problem. The `reprex()` function from the [`reprex`](https://github.com/tidyverse/reprex) package is extremely helpful for this.

To help us read your code, please try to follow the [tidyverse style guide](https://style.tidyverse.org/). The `style_text()` and `style_file()` functions from the [`styler`](https://github.com/r-lib/styler) package make it easier.

## Benchmarks

How poorly does `drake` perform? To find out, we recommend the [`proffer`](https://github.com/wlandau/proffer) package and take screenshots of the results displayed in your browser.

```r
library(drake)
library(proffer)
px <- pprof({
  # All your drake code goes here.
})
```

Alternatively, if installing [`proffer`](https://github.com/wlandau/proffer) is too cumbersome, create a zip archive of profiling data (e.g. `samples.zip` below) and upload it to this issue thread.

```r
Rprof(filename = "samples.rprof")
# Slow code goes here.
Rprof(NULL)
zip(zipfile = "samples.zip", files = "samples.rprof")
```
