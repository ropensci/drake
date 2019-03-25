---
name: Bug report
about: Please share if you find something wrong with drake.
title: ''
labels: 'type: bug'
assignees: wlandau

---

## Description

Describe the bug clearly and concisely. 

## Development version (for advanced users)

If you can, please verify that the bug still persists in the most recent development version of `drake`.

```r
install.packages("remotes")
remotes::install_github("ropensci/drake")
```

Also, if you can, please include the [Git SHA-1 hash](https://git-scm.com/book/en/v1/Getting-Started-Git-Basics#Git-Has-Integrity) of the [Git commit you just installed](https://github.com/ropensci/drake/commits/master).

## Reproducible example

Provide a minimal reproducible example with code and output that demonstrates the bug. The [`reprex`](https://github.com/tidyverse/reprex) package is extremely helpful for this.

## Session info

End the reproducible example with a call to `sessionInfo()` in the same session (e.g. `reprex(si = TRUE)`) and include the output.

## Expected output

What output would the correct behavior have produced?
