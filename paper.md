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
    email: will.landau@gmail.com
    affiliation: 1
affiliations:
  - name: Eli Lilly and Company
    index: 1
date: 4 January 2018
bibliography: paper.bib
---

# Summary

The [drake](https://github.com/ropensci/drake) R package [@drake] is a workflow manager and computational engine for data science projects. Its primary objective is to keep results up to date with the underlying code and data. When it runs a project, [drake](https://github.com/ropensci/drake) detects any pre-existing output and refreshes the pieces that are outdated or missing. Not every runthrough starts from scratch, and the final answers are reproducible. With a user-friendly R-focused interface, [comprehensive documentation](https://ropensci.github.io/drake), and [extensive implicit parallel computing support](https://github.com/ropensci/drake/blob/master/vignettes/parallelism.Rmd), [drake](https://github.com/ropensci/drake) surpasses the analogous functionality in similar tools such as [Make](www.gnu.org/software/make/) [@Make], [remake](https://github.com/richfitz/remake) [@remake], [memoise](https://github.com/r-lib/memoise) [@memoise], and [knitr](https://github.com/yihui/knitr) [@knitr].

In reproducible research, [drake](https://github.com/ropensci/drake)'s role is to provide tangible evidence that a project's results are re-creatable. [Drake](https://github.com/ropensci/drake) quickly detects when the code, data, and output are synchronized. In other words, [drake](https://github.com/ropensci/drake) helps determine if the starting materials would produce the expected output if the project were to start over and run from scratch. This approach decreases the time and effort it takes to evaluate research projects for reproducibility.

Regarding high-performance computing, [drake](https://github.com/ropensci/drake) interfaces with a [wide variety of technologies](https://github.com/ropensci/drake/blob/master/vignettes/parallelism.Rmd#parallel-backends) to deploy the steps of a data analysis project. Options range from local multicore computing to serious distributed computing on a cluster. In addition, the parallel computing is implicit. In other words, [drake](https://github.com/ropensci/drake) constructs the directed acyclic network of the workflow and determines which steps can run simultaneously and which need to wait for dependencies. This automation eases the cognitive and computational burdens on the user, enhancing the readability of code and thus reproducibility.

# References
