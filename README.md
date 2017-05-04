<h1 align="center">
  <img width="200" src="./inst/logo.png" alt="">
</h1>

[![Travis-CI Build Status](https://travis-ci.org/wlandau-lilly/drake.svg?branch=master)](https://travis-ci.org/wlandau-lilly/drake)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/wlandau-lilly/drake?branch=master&svg=true)](https://ci.appveyor.com/project/wlandau-lilly/drake)
[![codecov.io](https://codecov.io/github/wlandau-lilly/drake/coverage.svg?branch=master)](https://codecov.io/github/wlandau-lilly/drake?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/drake)](http://cran.r-project.org/package=drake)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.2.0-6666ff.svg)](https://cran.r-project.org/)

# drake - data frames in R for [Make](http://kbroman.org/minimal_make/)

Drake is an R package for high-performance computing and reproducible data analysis. It helps you skip the work you already did, accelerate the rest with parallel processing, stay organized, and confirm that your results are up to date with your code. Drake stands on the shoulders of tools like 
[GNU Make](https://www.gnu.org/software/make) and [Rich FitzJohn](http://richfitz.github.io/)'s [remake](https://github.com/richfitz/remake) package, and it offers improvements in reproducibility, scalability, and convenience.

# Acknowledgements and history

The original idea of a time-saving reproducible build system extends back decades to [GNU Make](http://kbroman.org/minimal_make/), which today helps [data scientists](http://blog.kaggle.com/2012/10/15/make-for-data-scientists/) as well as the original user base of complied-language programmers. More recently, [Rich FitzJohn](http://richfitz.github.io/) created [remake](https://github.com/richfitz/remake), a breakthrough reimagining of [Make](http://kbroman.org/minimal_make/) for R and the most important inspiration for drake. Drake is a fresh reinterpretation of some of  [remake](https://github.com/richfitz/remake)'s pioneering fundamental concepts, scaled up for computationally-demanding workflows. 

Thanks also to [Kirill M&uuml;ller](http://krlmlr.github.io/) and [Daniel Falster](http://danielfalster.com/). They contributed code patches and enhancement ideas to my [parallelRemake](https://github.com/wlandau/parallelRemake) and [remakeGenerator](https://github.com/wlandau/remakeGenerator) packages, which I have now subsumed into drake.

# Advantages of drake

Some advantages of drake, especially relative to [remake](https://github.com/richfitz/remake) at the time of writing, are

1. reproducibility. Drake can
    - import and track arbitrary variables from your workspace.
    - track functions from packages.
2. safety.
    - If you manually break an output file target, drake will fix it.
3. parallel computing. Auto-magically run multiple targets simultaneously.
    - Switch on the `mclapply()` or `parLapply()` backend for light parallelism within a single R session.
    - Switch on a `Makefile` to parallelize over multiple R sessions, which you can distribute across several nodes of a computing cluster or a supercomputer.
4. convenience.
    - No YAML files!
    - With functions like `plan()`, `datasets()`, `analyses()`, `evaluate()`, `expand()`, and `gather()`, you can minimize typing when you set up a project.

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
devtools::install_github("wlandau-lilly/drake@v3.0.0", build = TRUE)
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

With drake, there is room for error with respect to tracking dependencies, managing environments and workspaces, etc. For example, in some edge cases, it is possible to trick drake into ignoring dependencies. Please read the "caution" vignette to use drake safely (`vignette("caution")`, also linked from the [CRAN page](https://CRAN.R-project.org/package=drake) under "vignettes"). For the most up-to-date information on unhandled edge cases, please visit the [issue tracker](https://github.com/wlandau-lilly/drake/issues), where you can submit your own bug reports as well. Be sure to search the closed issues too, especially if you are not using the most up-to-date development version.

# Help and troubleshooting

Please refer to [TROUBLESHOOTING.md](https://github.com/wlandau-lilly/drake/blob/master/TROUBLESHOOTING.md) on the [GitHub page](https://github.com/wlandau-lilly/drake) for instructions.
