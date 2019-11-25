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

How poorly does `drake` perform? Here is the easiest way to share diagnostic information.

1. Use `Rprof()`: 

```r
Rprof(filename = "my_profiling.rprof")
# Slow code goes here.
Rprof(NULL)
```

2. Create a zip archive of the `my_profiling.rprof` file.
3. Upload the zip file by dragging and dropping it into this issue thread. If the file is too big, please let us know and we can figure out another way to transfer the file.

For more sophisticated profiling workflows, see <https://github.com/wlandau/drake-examples/tree/master/overhead>.
