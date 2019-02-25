
<!-- README.md is generated from README.Rmd. Please edit that file -->
<center>
<img src="https://ropensci.github.io/drake/figures/infographic.svg" alt="infographic" align="center" style = "border: none; float: center;">
</center>
<table class="table">
<thead>
<tr class="header">
<th align="left">
Usage
</th>
<th align="left">
Release
</th>
<th align="left">
Development
</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">
<a href="https://www.gnu.org/licenses/gpl-3.0.en.html"><img src="https://img.shields.io/badge/licence-GPL--3-blue.svg" alt="Licence"></a>
</td>
<td align="left">
<a href="https://doi.org/10.21105/joss.00550"><img src="https://joss.theoj.org/papers/10.21105/joss.00550/status.svg" alt="JOSS"></a>
</td>
<td align="left">
<a href="https://travis-ci.org/ropensci/drake"><img src="https://travis-ci.org/ropensci/drake.svg?branch=master" alt="Travis"></a>
</td>
</tr>
<tr class="even">
<td align="left">
<a href="https://cran.r-project.org/"><img src="https://img.shields.io/badge/R%3E%3D-3.4.0-blue.svg" alt="minimal R version"></a>
</td>
<td align="left">
<a href="https://github.com/ropensci/onboarding/issues/156"><img src="https://badges.ropensci.org/156_status.svg" alt="rOpenSci"></a>
</td>
<td align="left">
<a href="https://ci.appveyor.com/project/ropensci/drake"><img src="https://ci.appveyor.com/api/projects/status/4ypc9xnmqt70j94e?svg=true&amp;branch=master" alt="AppVeyor"></a>
</td>
</tr>
<tr class="odd">
<td align="left">
<a href="https://CRAN.R-project.org/package=drake"><img src="https://cranlogs.r-pkg.org/badges/drake" alt="downloads"></a>
</td>
<td align="left">
<a href="https://cran.r-project.org/package=drake"><img src="https://www.r-pkg.org/badges/version/drake" alt="CRAN"></a>
</td>
<td align="left">
<a href="https://codecov.io/github/ropensci/drake?branch=master"><img src="https://codecov.io/github/ropensci/drake/coverage.svg?branch=master" alt="Codecov"></a>
</td>
</tr>
<tr class="even">
<td align="left">
<a href="https://saythanks.io/to/drake-r-package-feedback"><img src="https://img.shields.io/badge/Say-Thanks-blue.svg" alt="SayThanks"></a>
</td>
<td align="left">
<a href="https://zenodo.org/badge/latestdoi/82609103"><img src="https://zenodo.org/badge/82609103.svg" alt="Zenodo"></a>
</td>
<td align="left">
<a href="https://www.repostatus.org/#active"><img src="https://www.repostatus.org/badges/latest/active.svg" alt="Project Status: Active – The project has reached a stable, usable state and is being actively developed." /></a>
</td>
</tr>
</tbody>
</table>
<br>

The drake R package <img src="https://ropensci.github.io/drake/figures/logo.svg" align="right" alt="logo" width="120" height = "139" style = "border: none; float: right;">
===========================================================================================================================================================================

`drake` — or, Data Frames in R for Make — is a general-purpose workflow manager for data-driven tasks. It rebuilds intermediate data objects when their dependencies change, and it skips work when the results are already up to date. Not every runthrough starts from scratch, there is native support for parallel and distributed computing, and completed workflows have tangible evidence of reproducibility.

6-minute video
==============

Visit the [first page of the manual](https://ropenscilabs.github.io/drake-manual/) to watch a short introduction.

<center>
<a href="https://ropenscilabs.github.io/drake-manual"> <img src="https://ropensci.github.io/drake/figures/video.png" alt="video" align="center" style = "border: none; float: center;"> </a>
</center>
<br>

What gets done stays done.
==========================

Too many data science projects follow a [Sisyphean loop](https://en.wikipedia.org/wiki/Sisyphus):

1.  Launch the code.
2.  Wait while it runs.
3.  Discover an issue.
4.  Rerun from scratch.

Ordinarily, it is hard to avoid rerunning the code from scratch. <br>

<center>
<img src="https://ropensci.github.io/drake/figures/tweet.png" alt="tweet" align="center" style = "border: none; float: center;">
</center>
<br>

But with `drake`, you can automatically

1.  Launch the parts that changed since last time.
2.  Skip the rest.

How it works
============

To set up a project, load your packages,

``` r
library(drake)
library(dplyr)
library(ggplot2)
```

load your custom functions,

``` r
create_plot <- function(data) {
  ggplot(data, aes(x = Petal.Width, fill = Species)) +
    geom_histogram()
}
```

check any supporting files (optional),

``` r
# Get the files with drake_example("main").
file.exists("raw_data.xlsx")
#> [1] TRUE
file.exists("report.Rmd")
#> [1] TRUE
```

and plan what you are going to do.

``` r
plan <- drake_plan(
  raw_data = readxl::read_excel(file_in("raw_data.xlsx")),
  data = raw_data %>%
    mutate(Species = forcats::fct_inorder(Species)),
  hist = create_plot(data),
  fit = lm(Sepal.Width ~ Petal.Width + Species, data),
  report = rmarkdown::render(
    knitr_in("report.Rmd"),
    output_file = file_out("report.html"),
    quiet = TRUE
  )
)
plan
#> # A tibble: 5 x 2
#>   target   command                                                         
#>   <chr>    <expr>                                                          
#> 1 raw_data readxl::read_excel(file_in("raw_data.xlsx"))                   …
#> 2 data     raw_data %>% mutate(Species = forcats::fct_inorder(Species))   …
#> 3 hist     create_plot(data)                                              …
#> 4 fit      lm(Sepal.Width ~ Petal.Width + Species, data)                  …
#> 5 report   rmarkdown::render(knitr_in("report.Rmd"), output_file = file_ou…
```

So far, we have just been setting the stage. Use `make()` to do the real work. Targets are built in the correct order regardless of the row order of `plan`.

``` r
make(plan)
#> target raw_data
#> target data
#> target fit
#> target hist
#> target report
```

Except for files like `report.html`, your output is stored in a hidden `.drake/` folder. Reading it back is easy.

``` r
readd(data) # See also loadd().
#> # A tibble: 150 x 5
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#>          <dbl>       <dbl>        <dbl>       <dbl> <fct>  
#> 1          5.1         3.5          1.4         0.2 setosa 
#> 2          4.9         3            1.4         0.2 setosa 
#> 3          4.7         3.2          1.3         0.2 setosa 
#> 4          4.6         3.1          1.5         0.2 setosa 
#> 5          5           3.6          1.4         0.2 setosa 
#> # … with 145 more rows
```

You may look back on your work and see room for improvement, but it's all good! The whole point of `drake` is to help you go back and change things quickly and painlessly. For example, we forgot to give our histogram a bin width.

``` r
readd(hist)
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="https://ropensci.github.io/drake/figures/hist1.png" alt="hist1" align="center" style = "border: none; float: center;" width = "500px">

So let's fix the plotting function.

``` r
create_plot <- function(data) {
  ggplot(data, aes(x = Petal.Width, fill = Species)) +
    geom_histogram(binwidth = 0.25) +
    theme_gray(20)
}
```

`drake` knows which results are affected.

``` r
config <- drake_config(plan)
vis_drake_graph(config) # Interactive graph: zoom, drag, etc.
```

<img src="https://ropensci.github.io/drake/figures/graph.png" alt="hist1" align="center" style = "border: none; float: center;" width = "600px">

The next `make()` just builds `hist` and `report.html`. No point in wasting time on the data or model.

``` r
make(plan)
#> target hist
#> target report
```

``` r
loadd(hist)
hist
```

<img src="https://ropensci.github.io/drake/figures/hist2.png" alt="hist1" align="center" style = "border: none; float: center;" width = "500px">

Reproducibility with confidence
===============================

The R community emphasizes reproducibility. Traditional themes include [scientific replicability](https://en.wikipedia.org/wiki/Replication_crisis), literate programming with [knitr](https://yihui.name/knitr/), and version control with [git](https://git-scm.com/book/en/v2/Getting-Started-About-Version-Control). But internal consistency is important too. Reproducibility carries the promise that your output matches the code and data you say you used. With the exception of [non-default triggers](https://ropenscilabs.github.io/drake-manual/triggers.html) and [hasty mode](https://ropenscilabs.github.io/drake-manual/hpc.html#hasty-mode), `drake` strives to keep this promise.

Evidence
--------

Suppose you are reviewing someone else's data analysis project for reproducibility. You scrutinize it carefully, checking that the datasets are available and the documentation is thorough. But could you re-create the results without the help of the original author? With `drake`, it is quick and easy to find out.

``` r
make(plan)
#> All targets are already up to date.

config <- drake_config(plan)
outdated(config)
#> character(0)
```

With everything already up to date, you have **tangible evidence** of reproducibility. Even though you did not re-create the results, you know the results are re-creatable. They **faithfully show** what the code is producing. Given the right [package environment](https://rstudio.github.io/packrat/) and [system configuration](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/sessionInfo.html), you have everything you need to reproduce all the output by yourself.

Ease
----

When it comes time to actually rerun the entire project, you have much more confidence. Starting over from scratch is trivially easy.

``` r
clean()       # Remove the original author's results.
make(plan) # Independently re-create the results from the code and input data.
#> target raw_data
#> target data
#> target fit
#> target hist
#> target report
```

Independent replication
-----------------------

With even more evidence and confidence, you can invest the time to independently replicate the original code base if necessary. Up until this point, you relied on basic `drake` functions such as `make()`, so you may not have needed to peek at any substantive author-defined code in advance. In that case, you can stay usefully ignorant as you reimplement the original author's methodology. In other words, `drake` could potentially improve the integrity of independent replication.

Readability and transparency
----------------------------

Ideally, independent observers should be able to read your code and understand it. `drake` helps in several ways.

-   The [workflow plan data frame](https://ropensci.github.io/drake/reference/drake_plan.html) explicitly outlines the steps of the analysis, and [`vis_drake_graph()`](https://ropensci.github.io/drake/reference/vis_drake_graph.html) visualizes how those steps depend on each other.
-   `drake` takes care of the parallel scheduling and high-performance computing (HPC) for you. That means the HPC code is no longer tangled up with the code that actually expresses your ideas.
-   You can [generate large collections of targets](https://ropenscilabs.github.io/drake-manual/mtcars.html#generate-the-workflow-plan) without necessarily changing your code base of imported functions, another nice separation between the concepts and the execution of your workflow

Aggressively scale up.
======================

Not every project can complete in a single R session on your laptop. Some projects need more speed or computing power. Some require a few local processor cores, and some need large high-performance computing systems. But parallel computing is hard. Your tables and figures depend on your analysis results, and your analyses depend on your datasets, so some tasks must finish before others even begin. `drake` knows what to do. Parallelism is implicit and automatic. See the [high-performance computing guide](https://ropenscilabs.github.io/drake-manual/hpc.html) for all the details.

``` r
# Use the spare cores on your local machine.
make(plan, jobs = 4)

# Or scale up to a supercomputer.
drake_batchtools_tmpl_file("slurm") # https://slurm.schedmd.com/
library(future.batchtools)
future::plan(batchtools_slurm, template = "batchtools.slurm.tmpl", workers = 100)
make(plan, parallelism = "future_lapply")
```

Installation
============

You can choose among different versions of `drake`. The CRAN release often lags behind the [online manual](https://ropenscilabs.github.io/drake-manual/) but may have fewer bugs.

``` r
# Install the latest stable release from CRAN.
install.packages("drake")

# Alternatively, install the development version from GitHub.
install.packages("devtools")
library(devtools)
install_github("ropensci/drake")
```

A few technical details:

-   You must properly install `drake` using `install.packages()`, `devtools::install_github()`, or similar. It is not enough to use `devtools::load_all()`, particularly for the parallel computing functionality, in which multiple R sessions initialize and then try to `require(drake)`.
-   For `make(parallelism = "Makefile")`, Windows users may need to download and install [`Rtools`](https://cran.r-project.org/bin/windows/Rtools/).
-   To use `make(parallelism = "future")` or `make(parallelism = "future_lapply")` to deploy your work to a computing cluster (see the [high-performance computing guide](https://ropenscilabs.github.io/drake-manual/hpc.html)), you will need the [`future.batchtools`](https://github.com/HenrikBengtsson/future.batchtools) package.

Documentation
=============

The main resources to learn `drake` are the [user manual](https://ropenscilabs.github.io/drake-manual/) and the [reference website](https://ropensci.github.io/drake/). Others are below.

Cheat sheet
-----------

Thanks to [Kirill](https://github.com/krlmlr) for preparing a [`drake` cheat sheet](https://github.com/krlmlr/drake-sib-zurich/blob/master/cheat-sheet.pdf) for the [workshop](https://github.com/krlmlr/drake-sib-zurich).

Frequently asked questions
--------------------------

The [FAQ page](https://ropenscilabs.github.io/drake-manual/faq.html) is an index of links to [appropriately-labeled issues on GitHub](https://github.com/ropensci/drake/issues?utf8=%E2%9C%93&q=is%3Aissue+label%3A%22frequently+asked+question%22+). To contribute, please [submit a new issue](https://github.com/ropensci/drake/issues/new) and ask that it be labeled as a frequently asked question.

Function reference
------------------

The [reference section](https://ropensci.github.io/drake/reference/index.html) lists all the available functions. Here are the most important ones.

-   `drake_plan()`: create a workflow data frame (like `my_plan`).
-   `make()`: build your project.
-   `loadd()`: load one or more built targets into your R session.
-   `readd()`: read and return a built target.
-   `drake_config()`: create a master configuration list for other user-side functions.
-   `vis_drake_graph()`: show an interactive visual network representation of your workflow.
-   `outdated()`: see which targets will be built in the next `make()`.
-   `deps()`: check the dependencies of a command or function.
-   `failed()`: list the targets that failed to build in the last `make()`.
-   `diagnose()`: return the full context of a build, including errors, warnings, and messages.

Tutorials
---------

Thanks to [Kirill](https://github.com/krlmlr) for constructing two interactive [`learnr`](https://rstudio.github.io/learnr/) tutorials: [one supporting `drake` itself](https://krlmlr.shinyapps.io/cooking-drake-tutorial/), and a [prerequisite walkthrough](https://krlmlr.shinyapps.io/cooking-tutorial/) of the [`cooking` package](https://github.com/krlmlr/cooking).

Examples
--------

Here are some real-world applications of `drake` in the wild.

-   [efcaguab/demografia-del-voto](https://github.com/efcaguab/demografia-del-voto)
-   [efcaguab/great-white-shark-nsw](https://github.com/efcaguab/great-white-shark-nsw)
-   [IndianaCHE/Detailed-SSP-Reports](https://github.com/IndianaCHE/Detailed-SSP-Reports)
-   [tiernanmartin/home-and-hope](https://github.com/tiernanmartin/home-and-hope)

There are also multiple `drake`-powered example projects [available here](https://github.com/wlandau/drake-examples), ranging from beginner-friendly stubs to demonstrations of high-performance computing. You can generate the files for a project with `drake_example()` (e.g. `drake_example("gsp")`), and you can list the available projects with `drake_examples()`. You can contribute your own example project with a [fork and pull request](https://github.com/wlandau/drake-examples/pulls).

Presentations
-------------

<table style="width:54%;">
<colgroup>
<col width="12%" />
<col width="12%" />
<col width="12%" />
<col width="16%" />
</colgroup>
<thead>
<tr class="header">
<th>Author</th>
<th>Venue</th>
<th>Date</th>
<th>Materials</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><a href="https://github.com/aedobbyn">Amanda Dobbyn</a></td>
<td><a href="https://www.rladiesnyc.org/">R-Ladies NYC</a></td>
<td>2019-02-12</td>
<td><a href="https://aedobbyn.github.io/nyc-fires/index.html#1">slides</a>, <a href="https://github.com/aedobbyn/nyc-fires">source</a></td>
</tr>
<tr class="even">
<td><a href="https://github.com/wlandau">Will Landau</a></td>
<td><a href="https://projects.iq.harvard.edu/datafest2019/home">Harvard DataFest</a></td>
<td>2019-01-22</td>
<td><a href="https://wlandau.github.io/drake-datafest-2019">slides</a>, <a href="https://github.com/wlandau/drake-datafest-2019">source</a></td>
</tr>
<tr class="odd">
<td><a href="https://github.com/karthik">Karthik Ram</a></td>
<td><a href="https://www.rstudio.com/conference">RStudio Conference</a></td>
<td>2019-01-18</td>
<td><a href="https://resources.rstudio.com/rstudio-conf-2019/a-guide-to-modern-reproducible-data-science-with-r">video</a>, <a href="http://inundata.org/talks/rstd19/#/">slides</a>, <a href="https://github.com/karthik/rstudio2019">resources</a></td>
</tr>
<tr class="even">
<td><a href="https://github.com/sinarueeger">Sina Rüeger</a></td>
<td><a href="https://www.meetup.com/Geneve-R-User-Group">Geneva R User Group</a></td>
<td>2018-10-04</td>
<td><a href="https://sinarueeger.github.io/20181004-geneve-rug">slides</a>, <a href="https://github.com/sinarueeger/workflow-example">example code</a></td>
</tr>
<tr class="odd">
<td><a href="https://github.com/wlandau">Will Landau</a></td>
<td><a href="http://rinpharma.com/">R in Pharma</a></td>
<td>2018-08-16</td>
<td><a href="https://ropenscilabs.github.io/drake-manual/">video</a>, <a href="https://wlandau.github.io/drake-talk">slides</a>, <a href="https://github.com/wlandau/drake-talk">source</a></td>
</tr>
<tr class="even">
<td><a href="https://github.com/cstawitz">Christine Stawitz</a></td>
<td><a href="https://www.meetup.com/rladies-seattle">R-Ladies Seattle</a></td>
<td>2018-06-25</td>
<td><a href="https://github.com/cstawitz/RLadies_Sea_drake">materials</a></td>
</tr>
<tr class="odd">
<td><a href="https://github.com/krlmlr">Kirill Müller</a></td>
<td><a href="https://www.sib.swiss">Swiss Institute of Bioinformatics</a></td>
<td>2018-03-05</td>
<td><a href="https://www.sib.swiss/training/course/2018-03-remake">workshop</a>, <a href="https://krlmlr.github.io/slides/drake-sib-zurich">slides</a>, <a href="https://github.com/krlmlr/drake-sib-zurich">source</a>, <a href="https://krlmlr.github.io/slides/drake-sib-zurich/cooking.html">exercises</a></td>
</tr>
<tr class="even">
<td><a href="https://github.com/krlmlr">Kirill Müller</a></td>
<td><a href="https://www.rstudio.com/conference">RStudio Conference</a></td>
<td>2018-02-01</td>
<td><a href="https://krlmlr.github.io/drake-pitch">slides</a>, <a href="https://github.com/krlmlr/drake-pitch">source</a></td>
</tr>
</tbody>
</table>

Context and history
-------------------

For context and history, check out [this post on the rOpenSci blog](https://ropensci.org/blog/2018/02/06/drake/) and [episode 22 of the R Podcast](https://www.r-podcast.org/episode/022-diving-in-to-drake-with-will-landau/).

Help and troubleshooting
========================

The following resources document many known issues and challenges.

-   [Frequently-asked questions](https://github.com/ropensci/drake/issues?utf8=%E2%9C%93&q=is%3Aissue+label%3A%22frequently+asked+question%22+).
-   [Cautionary notes and edge cases](https://ropenscilabs.github.io/drake-manual/caution.html)
-   [Debugging and testing drake projects](https://ropenscilabs.github.io/drake-manual/debug.html)
-   [Other known issues](https://github.com/ropensci/drake/issues) (please search both open and closed ones).

If you are still having trouble, please submit a [new issue](https://github.com/ropensci/drake/issues/new) with a bug report or feature request, along with a minimal reproducible example where appropriate.

The GitHub issue tracker is mainly intended for bug reports and feature requests. While questions about usage etc. are also highly encouraged, you may alternatively wish to post to [Stack Overflow](https://stackoverflow.com) and use the [`drake-r-package` tag](https://stackoverflow.com/tags/drake-r-package).

Contributing
============

Development is a community effort, and we encourage participation.

Code of Conduct
---------------

The environment for collaboration should be friendly, inclusive, respectful, and safe for everyone, so all participants must obey [this repository's code of conduct](https://github.com/ropensci/drake/blob/master/CONDUCT.md).

Issues
------

`drake` thrives on the suggestions, questions, and bug reports you submit to the [issue tracker](https://github.com/ropensci/drake/issues). Before posting, please search both the open and closed issues to help us avoid duplication. Usage questions are welcome, but you may also wish to post to [Stack Overflow](https://stackoverflow.com) with the [`drake-r-package` tag](https://stackoverflow.com/tags/drake-r-package).

Development
-----------

If you would like to work on the code or documentation, please [fork this repository](https://help.github.com/articles/fork-a-repo/), make the changes in your fork, and then submit a [pull request](https://github.com/ropensci/drake/pulls). We will discuss your work and then hopefully merge it into the project.

Similar work
============

GNU Make
--------

The original idea of a time-saving reproducible build system extends back at least as far as [GNU Make](https://www.gnu.org/software/make/), which still aids the work of [data scientists](http://blog.kaggle.com/2012/10/15/make-for-data-scientists/) as well as the original user base of complied language programmers. In fact, the name "drake" stands for "Data Frames in R for Make". [Make](https://kbroman.org/minimal_make/) is used widely in reproducible research. Below are some examples from [Karl Broman's website](https://kbroman.org/minimal_make/).

-   Bostock, Mike (2013). "A map of flowlines from NHDPlus." <https://github.com/mbostock/us-rivers>. Powered by the Makefile at <https://github.com/mbostock/us-rivers/blob/master/Makefile>.
-   Broman, Karl W (2012). "Halotype Probabilities in Advanced Intercross Populations." *G3* 2(2), 199-202.Powered by the `Makefile` at <https://github.com/kbroman/ailProbPaper/blob/master/Makefile>.
-   Broman, Karl W (2012). "Genotype Probabilities at Intermediate Generations in the Construction of Recombinant Inbred Lines." \*Genetics 190(2), 403-412. Powered by the Makefile at <https://github.com/kbroman/preCCProbPaper/blob/master/Makefile>.
-   Broman, Karl W and Kim, Sungjin and Sen, Saunak and Ane, Cecile and Payseur, Bret A (2012). "Mapping Quantitative Trait Loci onto a Phylogenetic Tree." *Genetics* 192(2), 267-279. Powered by the `Makefile` at <https://github.com/kbroman/phyloQTLpaper/blob/master/Makefile>.

There are several reasons for R users to prefer `drake` instead.

-   `drake` already has a [Make](https://kbroman.org/minimal_make/)-powered parallel backend. Just run `make(..., parallelism = "Makefile", jobs = 2)` to enjoy most of the original benefits of [Make](https://kbroman.org/minimal_make/) itself.
-   Improved scalability. With [Make](https://kbroman.org/minimal_make/), you must write a potentially large and cumbersome [Makefile](https://github.com/kbroman/preCCProbPaper/blob/master/Makefile) by hand. But with `drake`, you can use [wildcard templating](https://ropenscilabs.github.io/drake-manual/mtcars.html#generate-the-workflow-plan) to automatically generate massive collections of targets with minimal code.
-   Lower overhead for light-weight tasks. For each [Make](https://kbroman.org/minimal_make/) target that uses R, a brand new R session must spawn. For projects with thousands of small targets, that means more time may be spent loading R sessions than doing the actual work. With `make(..., parallelism = "mclapply, jobs = 4")`, `drake` launches 4 persistent workers up front and efficiently processes the targets in R.
-   Convenient organization of output. With [Make](https://kbroman.org/minimal_make/), the user must save each target as a file. `drake` saves all the results for you automatically in a [storr cache](https://github.com/richfitz/storr) so you do not have to micromanage the results.

Remake
------

[drake](https://github.com/ropensci/drake) overlaps with its direct predecessor, [remake](https://github.com/richfitz/remake). In fact, [drake](https://github.com/ropensci/drake) owes its core ideas to [remake](https://github.com/richfitz/remake) and [Rich FitzJohn](https://github.com/richfitz/remake). [Remake](https://github.com/richfitz/remake)'s development repository lists several [real-world applications](https://github.com/richfitz/remake/blob/master/README.md#real-world-examples). [drake](https://github.com/ropensci/drake) surpasses [remake](https://github.com/richfitz/remake) in several important ways, including but not limited to the following.

1.  High-performance computing. [Remake](https://github.com/richfitz/remake) has no native parallel computing support. [drake](https://github.com/ropensci/drake), on the other hand, has a thorough selection of parallel computing technologies and scheduling algorithms. Thanks to [future](github.com/HenrikBengtsson/future), [future.batchtools](github.com/HenrikBengtsson/future.batchtools), and [batchtools](github.com/mllg/batchtools), it is straightforward to configure a [drake](https://github.com/ropensci/drake) project for most popular job schedulers, such as [SLURM](https://slurm.schedmd.com/), [TORQUE](https://www.adaptivecomputing.com/products/torque/), and the [Grid Engine](https://www.oracle.com/technetwork/oem/grid-engine-166852.html), as well as systems contained in [Docker images](https://www.docker.com/).
2.  A friendly interface. In [remake](https://github.com/richfitz/remake), the user must manually write a [YAML](https://github.com/richfitz/remake/blob/master/doc/remake.yml) configuration file to arrange the steps of a workflow, which leads to some of the same scalability problems as [Make](https://www.gnu.org/software/make/). [drake](https://github.com/ropensci/drake)'s data-frame-based interface and [wildcard templating functionality](https://ropenscilabs.github.io/drake-manual/mtcars.html#generate-the-workflow-plan) easily generate workflows at scale.
3.  Thorough documentation. [drake](https://github.com/ropensci/drake) contains [thorough user manual](https://ropenscilabs.github.io/drake-manual/), a [reference website](https://ropensci.github.io/drake/), a [comprehensive README](https://github.com/ropensci/drake/blob/master/README.md), examples in the help files of user-side functions, and [accessible example code](https://github.com/wlandau/drake-examples) that users can write with `drake::example_drake()`.
4.  Active maintenance. [drake](https://github.com/ropensci/drake) is actively developed and maintained, and [issues](https://github.com/ropensci/drake/issues) are usually addressed promptly.
5.  Presence on CRAN. At the time of writing, [drake](https://github.com/ropensci/drake) is [available on CRAN](https://cran.r-project.org/package=drake), but [remake](https://github.com/richfitz/remake) is not.

Memoise
-------

Memoization is the strategic caching of the return values of functions. Every time a memoized function is called with a new set of arguments, the return value is saved for future use. Later, whenever the same function is called with the same arguments, the previous return value is salvaged, and the function call is skipped to save time. The [memoise package](https://github.com/r-lib/memoise) is an excellent implementation of memoization in R.

However, memoization does not go far enough. In reality, the return value of a function depends not only on the function body and the arguments, but also on any nested functions and global variables, the dependencies of those dependencies, and so on upstream. `drake` surpasses [memoise](https://github.com/r-lib/memoise) because it uses the *entire dependency network graph* of a project to decide which pieces need to be rebuilt and which ones can be skipped.

Knitr
-----

Much of the R community uses [knitr](https://yihui.name/knitr/) for reproducible research. The idea is to intersperse code chunks in an [R Markdown](https://rmarkdown.rstudio.com/) or `*.Rnw` file and then generate a dynamic report that weaves together code, output, and prose. [Knitr](https://yihui.name/knitr/) is not designed to be a serious [pipeline toolkit](https://github.com/pditommaso/awesome-pipeline), and it should not be the primary computational engine for medium to large data analysis projects.

1.  [Knitr](https://yihui.name/knitr/) scales far worse than [Make](https://www.gnu.org/software/make/) or [remake](https://github.com/richfitz/remake). The whole point is to consolidate output and prose, so it deliberately lacks the essential modularity.
2.  There is no obvious high-performance computing support.
3.  While there is a way to skip chunks that are already up to date (with code chunk options `cache` and `autodep`), this functionality is not the focus of [knitr](https://yihui.name/knitr/). It is deactivated by default, and [remake](https://github.com/richfitz/remake) and `drake` are more dependable ways to skip work that is already up to date.

`drake` was designed to manage the entire workflow with [knitr](https://yihui.name/knitr/) reports as targets. The strategy is analogous for [knitr](https://yihui.name/knitr/) reports within [remake](https://github.com/richfitz/remake) projects.

Factual's Drake
---------------

[Factual's Drake](https://github.com/Factual/drake) is similar in concept, but the development effort is completely unrelated to the [drake R package](https://github.com/ropensci/drake).

Other pipeline toolkits
-----------------------

There are [countless other successful pipeline toolkits](https://github.com/pditommaso/awesome-pipeline). The `drake` package distinguishes itself with its R-focused approach, Tidyverse-friendly interface, and a [thorough selection of parallel computing technologies and scheduling algorithms](https://ropenscilabs.github.io/drake-manual/hpc.html).

Acknowledgements
================

Special thanks to [Jarad Niemi](https://www.jarad.me/), my advisor from [graduate school](https://stat.iastate.edu/), for first introducing me to the idea of [Makefiles](https://www.gnu.org/software/make/) for research. He originally set me down the path that led to `drake`.

Many thanks to [Julia Lowndes](https://github.com/jules32), [Ben Marwick](https://github.com/benmarwick), and [Peter Slaughter](https://github.com/gothub) for [reviewing drake for rOpenSci](https://github.com/ropensci/onboarding/issues/156), and to [Maëlle Salmon](https://github.com/maelle) for such active involvement as the editor. Thanks also to the following people for contributing early in development.

-   [Alex Axthelm](https://github.com/AlexAxthelm)
-   [Chan-Yub Park](https://github.com/mrchypark)
-   [Daniel Falster](https://github.com/dfalster)
-   [Eric Nantz](https://github.com/rpodcast)
-   [Henrik Bengtsson](https://github.com/HenrikBengtsson)
-   [Ian Watson](https://github.com/IanAWatson)
-   [Jasper Clarkberg](https://github.com/dapperjapper)
-   [Kendon Bell](https://github.com/kendonB)
-   [Kirill Müller](https://github.com/krlmlr)
-   [Michael Schubert](https://github.com/mschubert)

Credit for images is [attributed here](https://ropensci.github.io/drake/figures/image-credit.md).

[![ropensci\_footer](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)
