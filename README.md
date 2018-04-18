<center>
<img src="https://ropensci.github.io/drake/images/infographic.svg" alt="infographic" align="center" style = "border: none; float: center;">
</center>

| Release | Usage | Development |
|:--------|:------|:------------|
| [![JOSS](http://joss.theoj.org/papers/10.21105/joss.00550/status.svg)](https://doi.org/10.21105/joss.00550) | [![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html) | [![AppVeyor](https://ci.appveyor.com/api/projects/status/4ypc9xnmqt70j94e?svg=true&branch=master)](https://ci.appveyor.com/project/ropensci/drake) |
| [![rOpenSci](https://badges.ropensci.org/156_status.svg)](https://github.com/ropensci/onboarding/issues/156) | [![minimal R version](https://img.shields.io/badge/R%3E%3D-3.2.0-blue.svg)](https://cran.r-project.org/) | [![Travis](https://travis-ci.org/ropensci/drake.svg?branch=master)](https://travis-ci.org/ropensci/drake) |
| [![CRAN](http://www.r-pkg.org/badges/version/drake)](http://cran.r-project.org/package=drake) | [![downloads](http://cranlogs.r-pkg.org/badges/drake)](http://cran.rstudio.com/package=drake) | [![Codecov](https://codecov.io/github/ropensci/drake/coverage.svg?branch=master)](https://codecov.io/github/ropensci/drake?branch=master) |
|[![Zenodo](https://zenodo.org/badge/82609103.svg)](https://zenodo.org/badge/latestdoi/82609103) | |

# The drake R package <img src="https://ropensci.github.io/drake/images/logo.svg" align="right" alt="logo" width="120" height = "139" style = "border: none; float: right;">

The `drake` package is a general-purpose workflow manager for data-driven tasks in R. It rebuilds intermediate data objects when their dependencies change, and it skips work when the results are already up to date. Not every runthrough starts from scratch, and completed workflows have tangible evidence of reproducibility. `Drake` is more scalable than [`knitr`](https://github.com/yihui/knitr), more thorough than [memoization](https://github.com/r-lib/memoise), and more R-focused than [other pipeline toolkits](https://github.com/pditommaso/awesome-pipeline) such as [GNU Make](www.gnu.org/software/make/), [`remake`](https://github.com/richfitz/remake), and [snakemake](https://snakemake.readthedocs.io).

# What gets done stays done.

Too many data science projects follow a [Sisyphean loop](https://en.wikipedia.org/wiki/Sisyphus):

1. Launch the code.
2. Wait while it runs.
3. Discover an issue.
4. Restart from scratch.

Have you ever tried to manually salvage old results for a new runthrough?

<center>
<a href="https://twitter.com/fossilosophy/status/966408174470299648">
<img src="https://ropensci.github.io/drake/images/tweet.png" alt="tweet" align="center" style = "border: none; float: center;">
</a>
</center>

With `drake`, you can automatically

1. Launch the parts that changed since last time.
2. Skip the rest.

```r
library(drake)

# Drake comes with a basic example.
# Get the code with drake_example("basic").
load_basic_example(verbose = FALSE)

# Your workspace starts with a bunch of "imports":
# functions, pre-loaded data objects, and saved files
# available before the real work begins.

# Drake looks for data objects in your R session environment
ls()

## [1] "my_plan"     "random_rows" "reg1"        "reg2"        "simulate"

# and saved files in your file system.
list.files()

## [1] "report.Rmd"

# The real work is outlined step-by-step in the `my_plan` data frame.
# The steps are called "targets", and they depend on the imports.
# File targets have the names in `file_out()`, and the non-file
# targets have the names in the `target` column of the data frame.
# Drake's `make()` function runs the commands to build the targets
# in the correct order.
head(my_plan)

## # A tibble: 6 x 2
##   target            command                                                                      
##   <chr>             <chr>                                                                        
## 1 ""                "knit(knitr_in(\"report.Rmd\"), file_out(\"report.md\"), quiet = TRUE)"
## 2 small             simulate(48)                                                                 
## 3 large             simulate(64)                                                                 
## 4 regression1_small reg1(small)                                                                  
## 5 regression1_large reg1(large)                                                                  
## 6 regression2_small reg2(small)  

# First round: drake builds all 15 targets.
make(my_plan) 

## target large
## target small
## target regression1_large
## target regression1_small
## target regression2_large
## target regression2_small
## target coef_regression1_large
## target coef_regression1_small
## target coef_regression2_large
## target coef_regression2_small
## target summ_regression1_large
## target summ_regression1_small
## target summ_regression2_large
## target summ_regression2_small
## target "report.md"

# If you change the reg2() function,
# all the regression2 targets are out of date,
# which in turn affects 'report.md'.
reg2 <- function(d){    
  d$x4 <- d$x ^ 4
  lm(y ~ x4, data = d)
}

# Second round: drake only rebuilds the targets
# that depend on the things you changed.
make(my_plan)

## target regression2_large
## target regression2_small
## target coef_regression2_large
## target coef_regression2_small
## target summ_regression2_large
## target summ_regression2_small
## target "report.md"

# If nothing important changed, drake rebuilds nothing.
make(my_plan)

## All targets are already up to date.

# Drake cares about nested functions too:
# with the exception of trivial formatting edits,
# changes to `random_rows()` will propagate to `simulate()`
# and all the downstream targets.
# Try it!
```

# Reproducibility with confidence

The R community emphasizes reproducibility. Traditional themes include [scientific replicability](https://en.wikipedia.org/wiki/Replication_crisis), literate programming with [knitr](https://yihui.name/knitr/), and version control with [git](https://git-scm.com/book/en/v2/Getting-Started-About-Version-Control). But internal consistency is important too. Reproducibility carries the promise that your output matches the code and data you say you used.

## Evidence

Suppose you are reviewing someone else's data analysis project for reproducibility. You scrutinize it carefully, checking that the datasets are available and the documentation is thorough. But could you re-create the results without the help of the original author? With `drake`, it is quick and easy to find out.

```r
make(my_plan)

## All targets are already up to date.

config <- drake_config(my_plan)
outdated(config)

## character(0)
```

With everything already up to date, you have **tangible evidence** of reproducibility. Even though you did not re-create the results, you know the results are re-creatable. They **faithfully show** what the code is producing. Given the right [package environment](https://rstudio.github.io/packrat/) and [system configuration](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/sessionInfo.html), you have everything you need to reproduce all the output by yourself.

## Ease

When it comes time to actually rerun the entire project, you have much more confidence. Starting over from scratch is trivially easy.

```r
clean()       # Remove the original author's results.
make(my_plan) # Independently re-create the results from the code and input data.
```

## Independent replication

With even more evidence and confidence, you can invest the time to independently replicate the original code base if necessary. Up until this point, you relied on basic `drake` functions such as `make()`, so you may not have needed to peek at any substantive author-defined code in advance. In that case, you can stay usefully ignorant as you reimplement the original author's methodology. In other words, `drake` could potentially improve the integrity of independent replication.

## Readability and transparency

Ideally, independent observers should be able to read your code and understand it. `Drake` helps in several ways.

- The [workflow plan data frame](https://ropensci.github.io/drake/reference/drake_plan.html) explicitly outlines the steps of the analysis, and [`vis_drake_graph()`](https://ropensci.github.io/drake/reference/vis_drake_graph.html) visualizes how those steps depend on each other.
- `Drake` takes care of the parallel scheduling and high-performance computing (HPC) for you. That means the HPC code is no longer tangled up with the code that actually expresses your ideas.
- You can [generate large collections of targets](https://ropensci.github.io/drake/articles/best-practices.html#generating-workflow-plan-data-frames) without necessarily changing your code base of imported functions, another nice separation between the concepts and the execution of your workflow

# Aggressively scale up.

Not every project can complete in a single R session on your laptop. Some projects need more speed or computing power. Some require a few local processor cores, and some need large high-performance computing systems. But parallel computing is hard. Your tables and figures depend on your analysis results, and your analyses depend on your datasets, so some tasks must finish before others even begin. But `drake` knows what to do. Parallelism is implicit and automatic. See the [parallelism vignette](https://github.com/ropensci/drake/blob/master/vignettes/parallelism.Rmd) for all the details.

```r
# Use the spare cores on your local machine.
make(my_plan, jobs = 4)

# Scale up to a supercomputer.
drake_batchtools_tmpl_file("slurm") # https://slurm.schedmd.com/
library(future.batchtools)
future::plan(batchtools_slurm, template = "batchtools.slurm.tmpl", workers = 100)
make(my_plan, parallelism = "future_lapply")
```

The network graph allows `drake` to wait for dependencies.

```r
# Change some code.
reg2 <- function(d){    
  d$x3 <- d$x ^ 3
  lm(y ~ x3, data = d)
}

# Plot an interactive graph.
config <- drake_config(my_plan)
vis_drake_graph(config)
```

[![](./images/graph.png)](https://ropensci.github.io/drake/images/reg2.html)

Within each column above, the nodes are conditionally independent given their dependencies. Each `make()` walks through the columns from left to right and applies parallel processing within each column. If any nodes are already up to date, `drake` looks downstream to maximize the number of outdated targets in a parallelizable stage. To show the parallelizable stages of the next `make()` programmatically, use the `parallel_stages()` function.

# Installation

You can choose among different versions of `drake`.

```r
# Install the latest stable release from CRAN.
install.packages("drake")

# Alternatively, install the development version from GitHub.
install.packages("devtools")
library(devtools)
install_github("ropensci/drake")
```

- You must properly install `drake` using `install.packages()`, `devtools::install_github()`, or similar. It is not enough to use `devtools::load_all()`, particularly for the parallel computing functionality, in which multiple R sessions initialize and then try to `require(drake)`.
- For `make(..., parallelism = "Makefile")`, Windows users need to download and install [`Rtools`](https://cran.r-project.org/bin/windows/Rtools/).
- If you want to build [the vignettes](https://github.com/ropensci/drake/tree/master/vignettes) when you install the development version, you must
    1. Install all the packages in the `Suggests:` field of the [DESCRIPTION file](https://github.com/ropensci/drake/blob/master/DESCRIPTION), including [cranlogs](https://cran.r-project.org/package=cranlogs) and [Ecdat](https://cran.r-project.org/package=Ecdat). All these packages are available through the [Comprehensive R Archive Network (CRAN)](https://cran.r-project.org), and you can install them with `install.packages()`.
    2. Set the `build` argument to `TRUE` in `install_github()`.

# Documentation

The main resources to learn `drake` are

1. The [documentation website](https://ropensci.github.io/drake/)
2. [Kirill Müller](https://github.com/krlmlr)'s [`drake` workshop from March 5, 2018](https://github.com/krlmlr/drake-sib-zurich).

## Cheat sheet

Thanks to [Kirill](https://github.com/krlmlr) for preparing a [`drake` cheat sheet](https://github.com/krlmlr/drake-sib-zurich/blob/master/cheat-sheet.pdf) for the [workshop](https://github.com/krlmlr/drake-sib-zurich).

## Frequently asked questions

The [FAQ page](https://ropensci.github.io/drake/articles/faq.html) is an index of links to [appropriately-labeled issues on GitHub](https://github.com/ropensci/drake/issues?q=is%3Aissue+is%3Aopen+label%3A%22frequently+asked+question%22). To contribute, please [submit a new issue](https://github.com/ropensci/drake/issues/new) and ask that it be labeled as a frequently asked question.

## Function reference

The [reference section](https://ropensci.github.io/drake/reference/index.html) lists all the available functions. Here are the most important ones.

- `drake_plan()`: create a workflow data frame (like `my_plan`).
- `make()`: build your project.
- `loadd()`: load one or more built targets into your R session.
- `readd()`: read and return a built target.
- `drake_config()`: create a master configuration list for other user-side functions.
- `vis_drake_graph()`: show an interactive visual network representation of your workflow.
- `outdated()`: see which targets will be built in the next `make()`.
- `deps()`: check the dependencies of a command or function.
- `failed()`: list the targets that failed to build in the last `make()`.
- `diagnose()`: return the full context of a build, including errors, warnings, and messages.

## Tutorials

Thanks to [Kirill](https://github.com/krlmlr) for constructing two interactive [`learnr`](https://rstudio.github.io/learnr/) tutorials: [one supporting `drake` itself](https://krlmlr.shinyapps.io/cooking-drake-tutorial/), and a [prerequisite walkthrough](https://krlmlr.shinyapps.io/cooking-tutorial/) of the [`cooking` package](https://github.com/krlmlr/cooking).

The articles below are tutorials taken from the [package vignettes](https://github.com/ropensci/drake/tree/master/vignettes).

- [Get started](https://ropensci.github.io/drake/articles/drake.html)
- [Example: R package download trends](https://ropensci.github.io/drake/articles/example-packages.html)
- [Example: gross state products](https://ropensci.github.io/drake/articles/example-gsp.html)
- [Basic example](https://ropensci.github.io/drake/articles/example-basic.html)
- [General best practices](https://ropensci.github.io/drake/articles/best-practices.html)
- [Cautionary notes and edge cases](https://ropensci.github.io/drake/articles/caution.html)
- [Debugging and testing drake projects](https://ropensci.github.io/drake/articles/debug.html)
- [Graphs with drake](https://ropensci.github.io/drake/articles/graph.html)
- [Parallel computing](https://ropensci.github.io/drake/articles/parallelism.html)
- [Time logging](https://ropensci.github.io/drake/articles/timing.html)
- [Storage](https://ropensci.github.io/drake/articles/storage.html)

## Presentations

- https://krlmlr.github.io/drake-pitch
- https://krlmlr.github.io/slides/drake-sib-zurich
- https://krlmlr.github.io/slides/drake-sib-zurich/cooking.html

## Built-in examples

`Drake` has built-in example projects with code files [available here](https://github.com/ropensci/drake/tree/master/inst/examples). You can generate the files for a project with `drake_example()` (e.g. `drake_example("gsp")`), and you can list the available projects with `drake_examples()`. The beginner-oriented examples are listed below. They help you learn `drake`'s main features, and they show one way to organize the files of `drake` projects.

- `basic`: A tiny, minimal example with the `mtcars` dataset to demonstrate how to use `drake`. Use `load_basic_example()` to set up the project in your workspace. The [quickstart vignette](https://github.com/ropensci/drake/blob/master/vignettes/quickstart.Rmd) is a parallel walkthrough of the same example.
- `gsp`: A concrete example using real econometrics data. It explores the relationships between gross state product and other quantities, and it shows off `drake`'s ability to generate lots of reproducibly-tracked tasks with ease.
- `packages`: A concrete example using data on R package downloads. It demonstrates how `drake` can refresh a project based on new incoming data without restarting everything from scratch.

## Real projects

Here are some real-world projects that use `drake`. If you would like to add some of your own, please submit a [pull request](https://github.com/ropensci/drake/pulls) or an [issue](https://github.com/ropensci/drake/issues) to contribute.

- [efcaguab/demografia-del-voto](https://github.com/efcaguab/demografia-del-voto)
- [efcaguab/great-white-shark-nsw](https://github.com/efcaguab/great-white-shark-nsw)
- [IndianaCHE/Detailed-SSP-Reports](https://github.com/IndianaCHE/Detailed-SSP-Reports)
- [tiernanmartin/home-and-hope](https://github.com/tiernanmartin/home-and-hope)

## Context and history

For context and history, check out [this post on the rOpenSci blog](https://ropensci.org/blog/2018/02/06/drake/) and [episode 22 of the R Podcast](https://www.r-podcast.org/episode/022-diving-in-to-drake-with-will-landau/).

# Help and troubleshooting

The following resources document many known issues and challenges.

- [Frequently-asked questions](https://github.com/ropensci/drake/issues?q=is%3Aissue+is%3Aopen+label%3A%22Frequently+Asked+Question%22).
- [Cautionary notes and edge cases](https://ropensci.github.io/drake/articles/caution.html)
- [Debugging and testing drake projects](https://ropensci.github.io/drake/articles/debug.html)
- [Other known issues](https://github.com/ropensci/drake/issues) (please search both open and closed ones).

If you are still having trouble, please submit a [new issue](https://github.com/ropensci/drake/issues/new) with a bug report or feature request, along with a minimal reproducible example where appropriate.

# Contributing

Bug reports, suggestions, and especially code contributions are welcome. Please see [CONTRIBUTING.md](https://github.com/ropensci/drake/blob/master/CONTRIBUTING.md). Maintainers and contributors must follow this repository's [code of conduct](https://github.com/ropensci/drake/blob/master/CONDUCT.md).

# Similar work

## GNU Make

The original idea of a time-saving reproducible build system extends back at least as far as [GNU Make](https://www.gnu.org/software/make/), which still aids the work of [data scientists](http://blog.kaggle.com/2012/10/15/make-for-data-scientists/) as well as the original user base of complied language programmers. In fact, the name "drake" stands for "Data Frames in R for Make". [Make](http://kbroman.org/minimal_make/) is used widely in reproducible research. Below are some examples from [Karl Broman's website](http://kbroman.org/minimal_make/).

- Bostock, Mike (2013). "A map of flowlines from NHDPlus." https://github.com/mbostock/us-rivers. Powered by the Makefile at https://github.com/mbostock/us-rivers/blob/master/Makefile.
- Broman, Karl W (2012). "Halotype Probabilities in Advanced Intercross Populations." *G3* 2(2), 199-202.Powered by the `Makefile` at https://github.com/kbroman/ailProbPaper/blob/master/Makefile.
- Broman, Karl W (2012). "Genotype Probabilities at Intermediate Generations in the Construction of Recombinant Inbred Lines." *Genetics 190(2), 403-412. Powered by the Makefile at https://github.com/kbroman/preCCProbPaper/blob/master/Makefile.
- Broman, Karl W and Kim, Sungjin and Sen, Saunak and Ane, Cecile and Payseur, Bret A (2012). "Mapping Quantitative Trait Loci onto a Phylogenetic Tree." *Genetics* 192(2), 267-279. Powered by the `Makefile` at https://github.com/kbroman/phyloQTLpaper/blob/master/Makefile.

There are several reasons for R users to prefer `drake` instead.

- `Drake` already has a [Make](http://kbroman.org/minimal_make/)-powered parallel backend. Just run `make(..., parallelism = "Makefile", jobs = 2)` to enjoy most of the original benefits of [Make](http://kbroman.org/minimal_make/) itself.
- Improved scalability. With [Make](http://kbroman.org/minimal_make/), you must write a potentially large and cumbersome [Makefile](https://github.com/kbroman/preCCProbPaper/blob/master/Makefile) by hand. But with `drake`, you can use [wildcard templating](https://github.com/ropensci/drake/blob/master/vignettes/quickstart.Rmd#generate-the-workflow-plan) to automatically generate massive collections of targets with minimal code.
- Lower overhead for light-weight tasks. For each [Make](http://kbroman.org/minimal_make/) target that uses R, a brand new R session must spawn. For projects with thousands of small targets, that means more time may be spent loading R sessions than doing the actual work. With `make(..., parallelism = "mclapply, jobs = 4")`, `drake` launches 4 persistent workers up front and efficiently processes the targets in R.
- Convenient organization of output. With [Make](http://kbroman.org/minimal_make/), the user must save each target as a file. `Drake` saves all the results for you automatically in a [storr cache](https://github.com/richfitz/storr) so you do not have to micromanage the results.


## Remake

[Drake](https://github.com/ropensci/drake) overlaps with its direct predecessor, [remake](https://github.com/richfitz/remake). In fact, [drake](https://github.com/ropensci/drake) owes its core ideas to [remake](https://github.com/richfitz/remake) and [Rich Fitzjohn](https://github.com/richfitz/remake). [Remake](https://github.com/richfitz/remake)'s development repository lists several [real-world applications](https://github.com/richfitz/remake/blob/master/README.md#real-world-examples). [Drake](https://github.com/ropensci/drake) surpasses [remake](https://github.com/richfitz/remake) in several important ways, including but not limited to the following.

1. High-performance computing. [Remake](https://github.com/richfitz/remake) has no native parallel computing support. [Drake](https://github.com/ropensci/drake), on the other hand, has a [vast arsenal](https://github.com/ropensci/drake/blob/master/vignettes/parallelism.Rmd) of parallel computing options, from local multicore computing to serious distributed computing. Thanks to [future](github.com/HenrikBengtsson/future), [future.batchtools](github.com/HenrikBengtsson/future.batchtools), and [batchtools](github.com/mllg/batchtools), it is straightforward to configure a [drake](https://github.com/ropensci/drake) project for most popular job schedulers, such as [SLURM](https://github.com/ropensci/drake/tree/master/inst/examples/slurm), [TORQUE](https://github.com/ropensci/drake/tree/master/inst/examples/torque), and the [Sun/Univa Grid Engine](https://github.com/ropensci/drake/tree/master/inst/examples/sge), as well as systems contained in [Docker images](https://github.com/ropensci/drake/tree/master/inst/examples/Docker-psock).
1. A friendly interface. In [remake](https://github.com/richfitz/remake), the user must manually write a [YAML](https://github.com/richfitz/remake/blob/master/doc/remake.yml) configuration file to arrange the steps of a workflow, which leads to some of the same scalability problems as [Make](https://www.gnu.org/software/make/). [Drake](https://github.com/ropensci/drake)'s data-frame-based interface and [wildcard templating functionality](https://github.com/ropensci/drake/blob/master/vignettes/quickstart.Rmd#generate-the-workflow-plan)  easily generate workflows at scale.
1. Thorough documentation. [Drake](https://github.com/ropensci/drake) contains [several vignettes](https://github.com/ropensci/drake/tree/master/vignettes), a [comprehensive README](https://github.com/ropensci/drake/blob/master/README.md), examples in the help files of user-side functions, and [accessible example code](https://github.com/ropensci/drake/tree/master/inst/examples) that users can write with `drake::example_drake()`.
1. Active maintenance. [Drake](https://github.com/ropensci/drake) is actively developed and maintained, and [issues](https://github.com/ropensci/drake/issues) are usually solved promptly.
1. Presence on CRAN. At the time of writing, [drake](https://github.com/ropensci/drake) is [available on CRAN](https://cran.r-project.org/package=drake), but [remake](https://github.com/richfitz/remake) is not.

## Memoise

Memoization is the strategic caching of the return values of functions. Every time a memoized function is called with a new set of arguments, the return value is saved for future use. Later, whenever the same function is called with the same arguments, the previous return value is salvaged, and the function call is skipped to save time. The [memoise package](https://github.com/r-lib/memoise) is an excellent implementation of memoization in R.

However, memoization does not go far enough. In reality, the return value of a function depends not only on the function body and the arguments, but also on any nested functions and global variables, the dependencies of those dependencies, and so on upstream. `Drake` surpasses [memoise](https://github.com/r-lib/memoise) because it uses the *entire dependency network graph* of a project to decide which pieces need to be rebuilt and which ones can be skipped.

## Knitr

Much of the R community uses [knitr](https://yihui.name/knitr/) for reproducible research. The idea is to intersperse code chunks in an [R Markdown](http://rmarkdown.rstudio.com/) or `*.Rnw` file and then generate a dynamic report that weaves together code, output, and prose. [Knitr](https://yihui.name/knitr/) is not designed to be a serious [pipeline toolkit](https://github.com/pditommaso/awesome-pipeline), and it should not be the primary computational engine for medium to large data analysis projects.

1. [Knitr](https://yihui.name/knitr/) scales far worse than [Make](https://www.gnu.org/software/make/) or [remake](https://github.com/richfitz/remake). The whole point is to consolidate output and prose, so it deliberately lacks the essential modularity.
1. There is no obvious high-performance computing support.
1. While there is a way to skip chunks that are already up to date (with code chunk options `cache` and `autodep`), this functionality is not the focus of [knitr](https://yihui.name/knitr/). It is deactivated by default, and [remake](https://github.com/richfitz/remake) and `drake` are more dependable ways to skip work that is already up to date.

As in the [basic example](https://github.com/ropensci/drake/tree/master/inst/examples/basic) demonstrates, `drake` should manage the entire workflow, and any [knitr](https://yihui.name/knitr/) reports should quickly build as targets at the very end. The strategy is analogous for [knitr](https://yihui.name/knitr/) reports within [remake](https://github.com/richfitz/remake) projects.

## Factual's Drake

[Factual's Drake](https://github.com/Factual/drake) is similar in concept, but the development effort is completely unrelated to the [drake R package](https://github.com/ropensci/drake).

## Other pipeline toolkits

There are [countless other successful pipeline toolkits](https://github.com/pditommaso/awesome-pipeline). The `drake` package distinguishes itself with its R-focused approach, Tidyverse-friendly interface, and [wide selection of parallel computing backends](https://github.com/ropensci/drake/blob/master/vignettes/parallelism.Rmd#parallel-backends).

# Acknowledgements

Special thanks to [Jarad Niemi](http://www.jarad.me/), my advisor from [graduate school](http://stat.iastate.edu/), for first introducing me to the idea of [Makefiles](https://www.gnu.org/software/make/) for research. He originally set me down the path that led to `drake`.

Many thanks to [Julia Lowndes](https://github.com/jules32), [Ben Marwick](https://github.com/benmarwick), and [Peter Slaughter](https://github.com/gothub) for [reviewing drake for rOpenSci](https://github.com/ropensci/onboarding/issues/156), and to [Maëlle Salmon](https://github.com/maelle) for such active involvement as the editor. Thanks also to the following people for contributing early in development.

- [Alex Axthelm](https://github.com/AlexAxthelm)
- [Chan-Yub Park](https://github.com/mrchypark)
- [Daniel Falster](https://github.com/dfalster)
- [Eric Nantz](https://github.com/thercast)
- [Henrik Bengtsson](https://github.com/HenrikBengtsson)
- [Ian Watson](https://github.com/IanAWatson)
- [Jasper Clarkberg](https://github.com/dapperjapper)
- [Kendon Bell](https://github.com/kendonB)
- [Kirill M&uuml;ller](https://github.com/krlmlr)

Credit for images is [attributed here](https://github.com/ropensci/drake/blob/master/images/image-credit.md).

[![ropensci_footer](http://ropensci.org/public_images/github_footer.png)](https://ropensci.org)
