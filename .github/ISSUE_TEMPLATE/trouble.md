---
name: Trouble
about: Something is not working, and you want help.
title: ''
labels: 'type: trouble'
---

## Prework

- [ ] Read and abide by `drake`'s [code of conduct](https://github.com/ropensci/drake/blob/master/CODE_OF_CONDUCT.md).
- [ ] Search for duplicates among the [existing issues](https://github.com/ropensci/drake/issues), both open and closed.
- [ ] Advanced users: verify that the bug still persists in the current development version (i.e. `remotes::install_github("ropensci/drake")`) and mention the [SHA-1 hash](https://git-scm.com/book/en/v1/Getting-Started-Git-Basics#Git-Has-Integrity) of the [Git commit you install](https://github.com/ropensci/drake/commits/master).

## Description

Describe the trouble clearly and concisely. 

## Reproducible example

Provide a minimal reproducible example with code and output that demonstrates the problem. The [`reprex`](https://github.com/tidyverse/reprex) package is extremely helpful for this.

## Desired result

What output or behavior do you want to see? Please be as specific as you can.

## Session info

End the reproducible example with a call to `sessionInfo()` in the same session (e.g. `reprex(si = TRUE)`) and include the output.
