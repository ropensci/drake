---
title: "The drake R package: a pipeline toolkit for reproducibility and high-performance computing"
tags:
  - R
  - reproducibility
  - high-performance computing
  - pipeline
  - workflow
  - Make
authors:
  - name: William Michael Landau
    orcid: 0000-0003-1878-3253
    email: will.landau@lilly.com
    affiliation: 1
affiliations:
  - name: Eli Lilly and Company
    index: 1
date: 4 January 2018
bibliography: paper.bib
---

# Summary

The `drake` R package [@drake] is a workflow manager and computational engine for data science projects. Its primary objective is to keep results up to date with the underlying code and data. When it runs a project, `drake` detects any pre-existing output and refreshes the pieces that are outdated or missing. Not every runthrough starts from scratch, and the final answers are reproducible. With a user-friendly R-focused interface, [comprehensive documentation](https://ropensci.github.io/drake), and [extensive implicit parallel computing support](https://github.com/ropensci/drake/blob/master/vignettes/parallelism.Rmd), `drake` surpasses the analogous functionality in similar tools such as [Make](www.gnu.org/software/make/) [@Make], [remake](https://github.com/richfitz/remake) [@remake], [memoise](https://github.com/r-lib/memoise) [@memoise], and [knitr](https://github.com/yihui/knitr) [@knitr].

# References
