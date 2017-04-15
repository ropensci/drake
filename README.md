<h1 align="center">
  <img width="200" src="./inst/logo-small.png" alt="">
</h1>

[![Travis-CI Build Status](https://travis-ci.org/wlandau-lilly/drake.svg?branch=master)](https://travis-ci.org/wlandau-lilly/drake)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/wlandau-lilly/drake?branch=master&svg=true)](https://ci.appveyor.com/project/wlandau-lilly/drake)
[![codecov.io](https://codecov.io/github/wlandau-lilly/drake/coverage.svg?branch=master)](https://codecov.io/github/wlandau-lilly/drake?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/drake)](http://cran.r-project.org/package=drake)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.2.0-6666ff.svg)](https://cran.r-project.org/)

# drake - data frames in R for [Make](http://kbroman.org/minimal_make/)

Drake is a workflow manager for R. When it runs a project, it automatically builds missing and outdated results while skipping over all the up-to-date output. This automation and reproducibility is important for data analysis workflows, especially large projects under heavy development.

# Acknowledgements and history

The original idea of a time-saving reproducible build system extends back decades to [GNU Make](http://kbroman.org/minimal_make/), which today helps [data scientists](http://blog.kaggle.com/2012/10/15/make-for-data-scientists/) as well as the original user base of complied-language programmers. More recently, [Rich FitzJohn](http://richfitz.github.io/) created [remake](https://github.com/richfitz/remake), a breakthrough reimagining of [Make](http://kbroman.org/minimal_make/) for R and the most important inspiration for drake. Drake is a fresh reinterpretation of some of  [remake](https://github.com/richfitz/remake)'s pioneering fundamental concepts, scaled up for computationally-demanding workflows. Relative to [remake](https://github.com/richfitz/remake), some of drake's most prominent distinguishing features at the time of writing this document are

- a more convenient, R-focused, [YAML](http://yaml.org/)-free user interface.
- generative templating to plan large workflows without much typing.
- smoother, safer user-side handling of external files and nested commands.
- distributed computing though **seamless** integration with [Makefiles](http://kbroman.org/minimal_make/).
- single-session multiprocess parallel computing via [parallel::parLapply()](https://stat.ethz.ch/R-manual/R-devel/library/parallel/html/clusterApply.html) and [parallel::mclapply()](https://stat.ethz.ch/R-manual/R-devel/library/parallel/html/mclapply.html). (The user can choose either.)
- robust detection of dependencies from users' packages.
- a wholehearted embrace of [igraph](http://igraph.org/r/) to drive the backend.

Thanks also to [Kirill M&uuml;ller](http://krlmlr.github.io/) and [Daniel Falster](http://danielfalster.com/). They contributed code patches and enhancement ideas to my [parallelRemake](https://github.com/wlandau/parallelRemake) and [remakeGenerator](https://github.com/wlandau/remakeGenerator) packages, which I have now subsumed into drake.

# Installation

First, ensure that [R](https://www.r-project.org/) is installed, as well as the dependencies in the [`DESCRIPTION`](https://github.com/wlandau-lilly/drake/blob/master/DESCRIPTION). To install the [latest CRAN release](https://CRAN.R-project.org/package=drake), run

```r
install.packages("drake")
```

To install the development version, get the [devtools](https://CRAN.R-project.org/package=devtools) package and then run 

```r
devtools::install_github("wlandau-lilly/drake", build = TRUE)
```

If you specify a tag, you can install a GitHub release.

```r
devtools::install_github("wlandau-lilly/drake@v2.0.0", build = TRUE)
```

# Windows

Drake presents `mclapply()` as one of two single-session parallel computing backends. Unfortunately, `mclapply()` cannot run multiple parallel jobs on Windows, so Windows users should use set `parallelism = "parLapply"` rather than `parallelism = "mclapply"` inside `make()` (already the Windows default). For true distributed parallel computing over multiple R sessions, Windows users need to download and install [`Rtools`](https://cran.r-project.org/bin/windows/Rtools/). This is because drake runs [Makefiles](http://kbroman.org/minimal_make/) with `system2("make", ...)`.

# Tutorials

The [CRAN page](https://CRAN.R-project.org/package=drake) links to multiple tutorials and vignettes. With drake installed, you can load any of the vignettes in an R session.

```r
vignette(package = "drake") # List the vignettes.
vignette("drake") # High-level intro.
vignette("quickstart") # Walk through a simple example.
vignette("caution") # Drake is not perfect. Read this to be safe.
```

# Quickstart examples

Drake has small self-contained built-in examples. To see the names of the available examples, use

```{r}
examples_drake()
```

Then use `example_drake()` to write the files for the example to your working directory.

```{r}
example_drake("basic")
```

Step through the code files to get started.

# Words of caution

Drake tries to reproducibly track everything and make other obviously good decisions, but there are limitations. For example, in some edge cases, it is possible to trick drake into ignoring dependencies. Please read the "caution" vignette to use drake safely (`vignette("caution")`, also linked from the [CRAN page](https://CRAN.R-project.org/package=drake) under "vignettes").

# Help and troubleshooting

Please refer to [TROUBLESHOOTING.md](https://github.com/wlandau-lilly/drake/blob/master/TROUBLESHOOTING.md) on the [GitHub page](https://github.com/wlandau-lilly/drake) for instructions.
