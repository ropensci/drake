
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
<a href="https://cran.r-project.org/package=drake"><img src="https://www.r-pkg.org/badges/version/drake" alt="CRAN"></a>
</td>
<td align="left">
<a href="https://travis-ci.org/ropensci/drake"><img src="https://travis-ci.org/ropensci/drake.svg?branch=master" alt="Travis"></a>
</td>
</tr>
<tr class="even">
<td align="left">
<a href="https://cran.r-project.org/"><img src="https://img.shields.io/badge/R%3E%3D-3.3.0-blue.svg" alt="minimal R version"></a>
</td>
<td align="left">
<a href="https://cran.r-project.org/web/checks/check_results_drake.html"><img src="https://cranchecks.info/badges/summary/drake" alt="cran-checks"></a>
</td>
<td align="left">
<a href="https://ci.appveyor.com/project/ropensci/drake"><img src="https://ci.appveyor.com/api/projects/status/4ypc9xnmqt70j94e?svg=true&amp;branch=master" alt="AppVeyor"></a>
</td>
</tr>
<tr class="odd">
<td align="left">
<a href="https://CRAN.R-project.org/package=drake"><img src="https://tinyverse.netlify.com/badge/drake"></a>
</td>
<td align="left">
<a href="https://github.com/ropensci/onboarding/issues/156"><img src="https://badges.ropensci.org/156_status.svg" alt="rOpenSci"></a>
</td>
<td align="left">
<a href="https://codecov.io/github/ropensci/drake?branch=master"><img src="https://codecov.io/github/ropensci/drake/coverage.svg?branch=master" alt="Codecov"></a>
</td>
</tr>
<tr class="even">
<td align="left">
<a href="https://CRAN.R-project.org/package=drake"><img src="https://cranlogs.r-pkg.org/badges/drake" alt="downloads"></a>
</td>
<td align="left">
<a href="https://doi.org/10.21105/joss.00550"><img src="https://joss.theoj.org/papers/10.21105/joss.00550/status.svg" alt="JOSS"></a>
</td>
<td align="left">
<a href="https://bestpractices.coreinfrastructure.org/projects/2135"><img src="https://bestpractices.coreinfrastructure.org/projects/2135/badge"></a>
</td>
</tr>
<tr class="odd">
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

# The drake R package <img src="https://ropensci.github.io/drake/figures/logo.svg" align="right" alt="logo" width="120" height = "139" style = "border: none; float: right;">

Data analysis can be slow. A round of scientific computation can take
several minutes, hours, or even days to complete. After it finishes, if
you update your code or data, your hard-earned results may no longer be
valid. How much of that valuable output can you keep, and how much do
you need to update? How much runtime must you endure all over again?

For projects in R, the `drake` package can help. It [analyzes your
workflow](https://ropenscilabs.github.io/drake-manual/plans.html), skips
steps with up-to-date results, and orchestrates the rest with [optional
distributed
computing](https://ropenscilabs.github.io/drake-manual/hpc.html). At the
end, `drake` provides evidence that your results match the underlying
code and data, which increases your ability to trust your research.

# 6-minute video

Visit the [first page of the
manual](https://ropenscilabs.github.io/drake-manual/) to watch a short
introduction.

<center>

<a href="https://ropenscilabs.github.io/drake-manual">
<img src="https://ropensci.github.io/drake/figures/video.png" alt="video" align="center" style = "border: none; float: center;">
</a>

</center>

<br>

# What gets done stays done.

Too many data science projects follow a [Sisyphean
loop](https://en.wikipedia.org/wiki/Sisyphus):

1.  Launch the code.
2.  Wait while it runs.
3.  Discover an issue.
4.  Rerun from scratch.

Ordinarily, it is hard to avoid rerunning the code from scratch.
<br>

<center>

<img src="https://ropensci.github.io/drake/figures/tweet.png" alt="tweet" align="center" style = "border: none; float: center;">

</center>

<br>

But with `drake`, you can automatically

1.  Launch the parts that changed since last time.
2.  Skip the rest.

# How it works

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

So far, we have just been setting the stage. Use `make()` to do the real
work. Targets are built in the correct order regardless of the row order
of `plan`.

``` r
make(plan)
#> target raw_data
#> target data
#> target fit
#> target hist
#> target report
```

Except for files like `report.html`, your output is stored in a hidden
`.drake/` folder. Reading it back is easy.

``` r
readd(data) # See also loadd().
#> # A tibble: 150 x 5
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#>           <dbl>       <dbl>        <dbl>       <dbl> <fct>  
#>  1          5.1         3.5          1.4         0.2 setosa 
#>  2          4.9         3            1.4         0.2 setosa 
#>  3          4.7         3.2          1.3         0.2 setosa 
#>  4          4.6         3.1          1.5         0.2 setosa 
#>  5          5           3.6          1.4         0.2 setosa 
#>  6          5.4         3.9          1.7         0.4 setosa 
#>  7          4.6         3.4          1.4         0.3 setosa 
#>  8          5           3.4          1.5         0.2 setosa 
#>  9          4.4         2.9          1.4         0.2 setosa 
#> 10          4.9         3.1          1.5         0.1 setosa 
#> # … with 140 more rows
```

You may look back on your work and see room for improvement, but it’s
all good\! The whole point of `drake` is to help you go back and change
things quickly and painlessly. For example, we forgot to give our
histogram a bin width.

``` r
readd(hist)
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="https://ropensci.github.io/drake/figures/hist1.png" alt="hist1" align="center" style = "border: none; float: center;" width = "500px">

So let’s fix the plotting function.

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

The next `make()` just builds `hist` and `report.html`. No point in
wasting time on the data or model.

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

# Reproducibility with confidence

The R community emphasizes reproducibility. Traditional themes include
[scientific
replicability](https://en.wikipedia.org/wiki/Replication_crisis),
literate programming with [knitr](https://yihui.name/knitr/), and
version control with
[git](https://git-scm.com/book/en/v2/Getting-Started-About-Version-Control).
But internal consistency is important too. Reproducibility carries the
promise that your output matches the code and data you say you used.
With the exception of [non-default
triggers](https://ropenscilabs.github.io/drake-manual/triggers.html) and
[hasty
mode](https://ropenscilabs.github.io/drake-manual/hpc.html#hasty-mode),
`drake` strives to keep this promise.

## Evidence

Suppose you are reviewing someone else’s data analysis project for
reproducibility. You scrutinize it carefully, checking that the datasets
are available and the documentation is thorough. But could you re-create
the results without the help of the original author? With `drake`, it is
quick and easy to find out.

``` r
make(plan)
#> All targets are already up to date.

config <- drake_config(plan)
outdated(config)
#> character(0)
```

With everything already up to date, you have **tangible evidence** of
reproducibility. Even though you did not re-create the results, you know
the results are re-creatable. They **faithfully show** what the code is
producing. Given the right [package
environment](https://rstudio.github.io/packrat/) and [system
configuration](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/sessionInfo.html),
you have everything you need to reproduce all the output by yourself.

## Ease

When it comes time to actually rerun the entire project, you have much
more confidence. Starting over from scratch is trivially easy.

``` r
clean()    # Remove the original author's results.
make(plan) # Independently re-create the results from the code and input data.
#> target raw_data
#> target data
#> target fit
#> target hist
#> target report
```

## History and provenance

As of version 7.5.0, `drake` tracks the history and provenance of your
targets: what you built, when you built it, how you built it, the
arguments you used in your function calls, and how to get the data back.
(Disable with `make(history = FALSE)`)

``` r
history <- drake_history(analyze = TRUE)
history
#> # A tibble: 12 x 10
#>    target time  hash  exists command  runtime   seed latest quiet
#>    <chr>  <chr> <chr> <lgl>  <chr>      <dbl>  <int> <lgl>  <lgl>
#>  1 data   2019… e580… TRUE   raw_da… 0.001    1.29e9 FALSE  NA   
#>  2 data   2019… e580… TRUE   raw_da… 0        1.29e9 TRUE   NA   
#>  3 fit    2019… 62a1… TRUE   lm(Sep… 0.002    1.11e9 FALSE  NA   
#>  4 fit    2019… 62a1… TRUE   lm(Sep… 0.001000 1.11e9 TRUE   NA   
#>  5 hist   2019… 10bc… TRUE   create… 0.006    2.10e8 FALSE  NA   
#>  6 hist   2019… 5252… TRUE   create… 0.004    2.10e8 FALSE  NA   
#>  7 hist   2019… 00fa… TRUE   create… 0.007    2.10e8 TRUE   NA   
#>  8 raw_d… 2019… 6317… TRUE   "readx… 0.009    1.20e9 FALSE  NA   
#>  9 raw_d… 2019… 6317… TRUE   "readx… 0.00600  1.20e9 TRUE   NA   
#> 10 report 2019… 3e2b… TRUE   "rmark… 0.481    1.30e9 FALSE  TRUE 
#> 11 report 2019… 3e2b… TRUE   "rmark… 0.358    1.30e9 FALSE  TRUE 
#> 12 report 2019… 3e2b… TRUE   "rmark… 0.356    1.30e9 TRUE   TRUE 
#> # … with 1 more variable: output_file <chr>
```

Remarks:

  - The `quiet` column appears above because one of the `drake_plan()`
    commands has `knit(quiet = TRUE)`.
  - The `hash` column identifies all the previous the versions of your
    targets. As long as `exists` is `TRUE`, you can recover old data.
  - Advanced: if you use `make(cache_log_file = TRUE)` and put the cache
    log file under version control, you can match the hashes from
    `drake_history()` with the `git` commit history of your code.

Let’s use the history to recover the oldest histogram.

``` r
hash <- history %>%
  filter(target == "hist") %>%
  pull(hash) %>%
  head(n = 1)
cache <- drake_cache()
cache$get_value(hash)
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="https://ropensci.github.io/drake/figures/hist1.png" alt="hist1" align="center" style = "border: none; float: center;" width = "600px">

## Automated recovery and renaming

Note: this feature is still experimental.

In `drake` version 7.5.0 and above, `make(recover = TRUE)` can salvage
old targets from the distant past. This may not be a good idea if your
external dependencies have changed a lot over time (R version, package
environment, etc) but it can be useful under the right circumstances.

``` r
# Is the data really gone?
clean() # garbage_collection = FALSE

# Nope!
make(plan, recover = TRUE) # The report still builds since report.md is gone.
#> recover raw_data
#> recover data
#> recover fit
#> recover hist
#> target report

# When was the raw data *really* first built?
diagnose(raw_data)$date
#> [1] "2019-07-18 20:19:48.110163 -0400 GMT"
```

You can even rename your targets\! All you have to do is use the same  target seed as last time. Just be aware that this invalidates downstream
targets.

``` r
# Get the old seed.
old_seed <- diagnose(data)$seed

# Now rename the data and supply the old seed.
plan <- drake_plan(
  raw_data = readxl::read_excel(file_in("raw_data.xlsx")),
  
  # Previously just named "data".
  iris_data = target(
    raw_data %>%
      mutate(Species = forcats::fct_inorder(Species)),
    seed = !!old_seed
  ),

  # `iris_data` will be recovered from `data`,
  # but `hist` and `fit` have changed commands,
  # so they will build from scratch.
  hist = create_plot(iris_data),
  fit = lm(Sepal.Width ~ Petal.Width + Species, iris_data),
  report = rmarkdown::render(
    knitr_in("report.Rmd"),
    output_file = file_out("report.html"),
    quiet = TRUE
  )
)

make(plan, recover = TRUE)
#> recover iris_data
#> target fit
#> target hist
#> target report
```

## Independent replication

With even more evidence and confidence, you can invest the time to
independently replicate the original code base if necessary. Up until
this point, you relied on basic `drake` functions such as `make()`, so
you may not have needed to peek at any substantive author-defined code
in advance. In that case, you can stay usefully ignorant as you
reimplement the original author’s methodology. In other words, `drake`
could potentially improve the integrity of independent replication.

## Readability and transparency

Ideally, independent observers should be able to read your code and
understand it. `drake` helps in several ways.

  - The [workflow plan data
    frame](https://ropensci.github.io/drake/reference/drake_plan.html)
    explicitly outlines the steps of the analysis, and
    [`vis_drake_graph()`](https://ropensci.github.io/drake/reference/vis_drake_graph.html)
    visualizes how those steps depend on each other.
  - `drake` takes care of the parallel scheduling and high-performance
    computing (HPC) for you. That means the HPC code is no longer
    tangled up with the code that actually expresses your ideas.
  - You can [generate large collections of
    targets](https://ropenscilabs.github.io/drake-manual/gsp.html)
    without necessarily changing your code base of imported functions,
    another nice separation between the concepts and the execution of
    your workflow

# Scale up and out.

Not every project can complete in a single R session on your laptop.
Some projects need more speed or computing power. Some require a few
local processor cores, and some need large high-performance computing
systems. But parallel computing is hard. Your tables and figures depend
on your analysis results, and your analyses depend on your datasets, so
some tasks must finish before others even begin. `drake` knows what to
do. Parallelism is implicit and automatic. See the [high-performance
computing guide](https://ropenscilabs.github.io/drake-manual/hpc.html)
for all the details.

``` r
# Use the spare cores on your local machine.
make(plan, jobs = 4)

# Or scale up to a supercomputer.
drake_hpc_template_file("slurm_clustermq.tmpl") # https://slurm.schedmd.com/
options(
  clustermq.scheduler = "clustermq",
  clustermq.template = "slurm_clustermq.tmpl"
)
make(plan, parallelism = "clustermq", jobs = 4)
```

# Installation

You can choose among different versions of `drake`. The CRAN release
often lags behind the [online
manual](https://ropenscilabs.github.io/drake-manual/) but may have fewer
bugs.

``` r
# Install the latest stable release from CRAN.
install.packages("drake")

# Alternatively, install the development version from GitHub.
install.packages("devtools")
library(devtools)
install_github("ropensci/drake")
```

# Function reference

The [reference
section](https://ropensci.github.io/drake/reference/index.html) lists
all the available functions. Here are the most important ones.

  - `drake_plan()`: create a workflow data frame (like `my_plan`).
  - `make()`: build your project.
  - `drake_history()`: show what you built, when you built it, and the
    function arguments you used.
  - `r_make()`: launch a fresh
    [`callr::r()`](https://github.com/r-lib/callr) process to build your
    project. Called from an interactive R session, `r_make()` is more
    reproducible than `make()`.
  - `loadd()`: load one or more built targets into your R session.
  - `readd()`: read and return a built target.
  - `drake_config()`: create a master configuration list for other
    user-side functions.
  - `vis_drake_graph()`: show an interactive visual network
    representation of your workflow.
  - `recoverable()`: Which targets can we salvage using `make(recover =
    TRUE)` (experimental).
  - `outdated()`: see which targets will be built in the next `make()`.
  - `deps()`: check the dependencies of a command or function.
  - `failed()`: list the targets that failed to build in the last
    `make()`.
  - `diagnose()`: return the full context of a build, including errors,
    warnings, and messages.

# Documentation

  - The [user manual](https://ropenscilabs.github.io/drake-manual/)
  - The [reference website](https://ropensci.github.io/drake/).
  - The [official repository of example
    code](https://github.com/wlandau/drake-examples). Download an
    example workflow from here with `drake_example()`.
  - [`drakeplanner`](https://github.com/wlandau/drakeplanner), an
    R/Shiny app to help learn `drake` and create new projects. Run
    locally with `drakeplanner::drakeplanner()` or access it at
    <https://wlandau.shinyapps.io/drakeplanner>.
  - [`learndrake`](https://github.com/wlandau/learndrake), an R package
    for teaching an extended `drake` workshop. It contains notebooks,
    slides, Shiny apps, the latter two of which are publicly deployed.
    See the
    [README](https://github.com/wlandau/learndrake/blob/master/README.md)
    for instructions and links.
  - Presentations and workshops by [Will
    Landau](https://github.com/wlandau), [Kirill
    Müller](https://github.com/krlmlr), [Amanda
    Dobbyn](https://github.com/aedobbyn), [Karthik
    Ram](http://github.com/karthik), [Sina
    Rüeger](https://github.com/sinarueeger), [Christine
    Stawitz](https://github.com/cstawitz), and others. See specific
    links at
    <https://ropenscilabs.github.io/drake-manual/index.html#presentations>
  - The [FAQ
    page](https://ropenscilabs.github.io/drake-manual/faq.html), which
    links to [appropriately-labeled issues on
    GitHub](https://github.com/ropensci/drake/issues?utf8=%E2%9C%93&q=is%3Aissue+label%3A%22frequently+asked+question%22+).

## Use cases

The official [rOpenSci use cases](https://ropensci.org/usecases/) and
[associated discussion threads](https://discuss.ropensci.org/c/usecases)
describe applications of `drake` in action. Here are some more
applications of `drake` in real-world
    projects.

  - [efcaguab/demografia-del-voto](https://github.com/efcaguab/demografia-del-voto)
  - [efcaguab/great-white-shark-nsw](https://github.com/efcaguab/great-white-shark-nsw)
  - [IndianaCHE/Detailed-SSP-Reports](https://github.com/IndianaCHE/Detailed-SSP-Reports)
  - [joelnitta/pleurosoriopsis](https://github.com/joelnitta/pleurosoriopsis)
  - [pat-s/pathogen-modeling](https://github.com/pat-s/pathogen-modeling)
  - [sol-eng/tensorflow-w-r](https://github.com/sol-eng/tensorflow-w-r)
  - [tiernanmartin/home-and-hope](https://github.com/tiernanmartin/home-and-hope)

# Help and troubleshooting

The following resources document many known issues and challenges.

  - [Frequently-asked
    questions](https://github.com/ropensci/drake/issues?utf8=%E2%9C%93&q=is%3Aissue+label%3A%22frequently+asked+question%22+).
  - [Cautionary notes and edge
    cases](https://ropenscilabs.github.io/drake-manual/caution.html)
  - [Debugging and testing drake
    projects](https://ropenscilabs.github.io/drake-manual/debugging.html)
  - [Other known issues](https://github.com/ropensci/drake/issues)
    (please search both open and closed ones).

If you are still having trouble, please submit a [new
issue](https://github.com/ropensci/drake/issues/new) with a bug report
or feature request, along with a minimal reproducible example where
appropriate.

The GitHub issue tracker is mainly intended for bug reports and feature
requests. While questions about usage etc. are also highly encouraged,
you may alternatively wish to post to [Stack
Overflow](https://stackoverflow.com) and use the [`drake-r-package`
tag](https://stackoverflow.com/tags/drake-r-package).

# Contributing

Development is a community effort, and we encourage participation.
Please read
[CONTRIBUTING.md](https://github.com/ropensci/drake/blob/master/CONTRIBUTING.md)
for details.

# Similar work

`drake` enhances reproducibility and high-performance computing, but not
in all respects. [Literate programming](https://rmarkdown.rstudio.com/),
[local library managers](https://rstudio.github.io/packrat),
[containerization](https://www.docker.com/), and [strict session
managers](https://github.com/tidyverse/reprex) offer more robust
solutions in their respective domains. And for the problems `drake`
*does* solve, it stands on the shoulders of the giants that came before.

## Pipeline tools

### GNU Make

The original idea of a time-saving reproducible build system extends
back at least as far as [GNU Make](https://www.gnu.org/software/make/),
which still aids the work of [data
scientists](http://blog.kaggle.com/2012/10/15/make-for-data-scientists/)
as well as the original user base of complied language programmers. In
fact, the name “drake” stands for “Data Frames in R for Make”.
[Make](https://kbroman.org/minimal_make/) is used widely in reproducible
research. Below are some examples from [Karl Broman’s
website](https://kbroman.org/minimal_make/).

  - Bostock, Mike (2013). “A map of flowlines from NHDPlus.”
    <https://github.com/mbostock/us-rivers>. Powered by the Makefile at
    <https://github.com/mbostock/us-rivers/blob/master/Makefile>.
  - Broman, Karl W (2012). “Halotype Probabilities in Advanced
    Intercross Populations.” *G3* 2(2), 199-202.Powered by the
    `Makefile` at
    <https://github.com/kbroman/ailProbPaper/blob/master/Makefile>.
  - Broman, Karl W (2012). “Genotype Probabilities at Intermediate
    Generations in the Construction of Recombinant Inbred Lines.”
    \*Genetics 190(2), 403-412. Powered by the Makefile at
    <https://github.com/kbroman/preCCProbPaper/blob/master/Makefile>.
  - Broman, Karl W and Kim, Sungjin and Sen, Saunak and Ane, Cecile and
    Payseur, Bret A (2012). “Mapping Quantitative Trait Loci onto a
    Phylogenetic Tree.” *Genetics* 192(2), 267-279. Powered by the
    `Makefile` at
    <https://github.com/kbroman/phyloQTLpaper/blob/master/Makefile>.

Whereas [GNU Make](https://www.gnu.org/software/make/) is
language-agnostic, `drake` is fundamentally designed for R.

  - Instead of a
    [Makefile](https://github.com/kbroman/preCCProbPaper/blob/master/Makefile),
    `drake` supports an R-friendly [domain-specific
    language](https://ropenscilabs.github.io/drake-manual/plans.html#large-plans)
    for declaring targets.
  - Targets in [GNU Make](https://www.gnu.org/software/make/) are files,
    whereas targets in `drake` are arbitrary variables in memory.
    (`drake` does have opt-in support for files via `file_out()`,
    `file_in()`, and `knitr_in()`.) `drake` caches these objects in its
    own [storage system](https://github.com/richfitz/storr) so R users
    rarely have to think about output files.

### Remake

[remake](https://github.com/richfitz/remake) itself is no longer
maintained, but its founding design goals and principles live on through
[drake](https://github.com/ropensci/drake). In fact,
[drake](https://github.com/ropensci/drake) is a direct reimagining of
[remake](https://github.com/richfitz/remake) with enhanced scalability,
reproducibility, high-performance computing, visualization, and
documentation.

### Factual’s Drake

[Factual’s Drake](https://github.com/Factual/drake) is similar in
concept, but the development effort is completely unrelated to the
[drake R package](https://github.com/ropensci/drake).

### Other pipeline tools

There are [countless other successful pipeline
toolkits](https://github.com/pditommaso/awesome-pipeline). The `drake`
package distinguishes itself with its R-focused approach,
Tidyverse-friendly interface, and a [thorough selection of parallel
computing technologies and scheduling
algorithms](https://ropenscilabs.github.io/drake-manual/hpc.html).

## Memoization

Memoization is the strategic caching of the return values of functions.
It is a lightweight approach to the core problem that `drake` and other
pipeline tools are trying to solve. Every time a memoized function is
called with a new set of arguments, the return value is saved for future
use. Later, whenever the same function is called with the same
arguments, the previous return value is salvaged, and the function call
is skipped to save time. The
[`memoise`](https://github.com/r-lib/memoise) package is the primary
implementation of memoization in R.

Memoization saves time for small projects, but it arguably does not go
far enough for large reproducible pipelines. In reality, the return
value of a function depends not only on the function body and the
arguments, but also on any nested functions and global variables, the
dependencies of those dependencies, and so on upstream. `drake` tracks
this deeper context, while [memoise](https://github.com/r-lib/memoise)
does not.

## Literate programming

[Literate programming](https://rmarkdown.rstudio.com/) is the practice
of narrating code in plain vernacular. The goal is to communicate the
research process clearly, transparently, and reproducibly. Whereas
commented code is still mostly code, literate
[knitr](https://yihui.name/knitr/) / [R
Markdown](https://rmarkdown.rstudio.com/) reports can become websites,
presentation slides, lecture notes, serious scientific manuscripts, and
even books.

### knitr and R Markdown

`drake` and [knitr](https://yihui.name/knitr/) are symbiotic. `drake`’s
job is to manage large computation and orchestrate the demanding tasks
of a complex data analysis pipeline.
[knitr](https://yihui.name/knitr/)’s job is to communicate those
expensive results after `drake` computes them.
[knitr](https://yihui.name/knitr/) / [R
Markdown](https://rmarkdown.rstudio.com/) reports are small pieces of an
overarching `drake` pipeline. They should focus on communication, and
they should do as little computation as possible.

To insert a [knitr](https://yihui.name/knitr/) report in a `drake`
pipeline, use the `knitr_in()` function inside your [`drake`
plan](https://ropenscilabs.github.io/drake-manual/plans.html), and use
`loadd()` and `readd()` to refer to targets in the report itself. See an
[example
here](https://github.com/wlandau/drake-examples/tree/master/main).

### Version control

`drake` is not a version control tool. However, it is fully compatible
with [`git`](https://git-scm.com/),
[`svn`](https://en.wikipedia.org/wiki/Apache_Subversion), and similar
software. In fact, it is good practice to use
[`git`](https://git-scm.com/) alongside `drake` for reproducible
workflows.

However, data poses a challenge. The datasets created by `make()` can
get large and numerous, and it is not recommended to put the `.drake/`
cache or the `.drake_history/` logs under version control. Instead, it
is recommended to use a data storage solution such as
[DropBox](https://www.dropbox.com/) or
[OSF](https://osf.io/ka7jv/wiki/home/).

### Containerization and R package environments

`drake` does not track R packages or system dependencies for changes.
Instead, it defers to tools like [Docker](https://www.docker.com),
[Singularity](https://sylabs.io/singularity/),
[`renv`](https://github.com/rstudio/renv), and
[`packrat`](https://github.com/rstudio/packrat), which create
self-contained portable environments to reproducibly isolate and ship
data analysis projects. `drake` is fully compatible with these tools.

### workflowr

The [`workflowr`](https://github.com/jdblischak/workflowr) package is a
project manager that focuses on literate programming, sharing over the
web, file organization, and version control. Its brand of
reproducibility is all about transparency, communication, and
discoverability. For an example of
[`workflowr`](https://github.com/jdblischak/workflowr) and `drake`
working together, see [this machine learning
project](https://2019-feature-selection.pjs-web.de/report-defoliation.html)
by [Patrick Schratz](https://github.com/pat-s)
([source](https://github.com/pat-s/2019-feature-selection)).

# Acknowledgements

Special thanks to [Jarad Niemi](https://www.jarad.me/), my advisor from
[graduate school](https://stat.iastate.edu/), for first introducing me
to the idea of [Makefiles](https://www.gnu.org/software/make/) for
research. He originally set me down the path that led to `drake`.

Many thanks to [Julia Lowndes](https://github.com/jules32), [Ben
Marwick](https://github.com/benmarwick), and [Peter
Slaughter](https://github.com/gothub) for [reviewing drake for
rOpenSci](https://github.com/ropensci/onboarding/issues/156), and to
[Maëlle Salmon](https://github.com/maelle) for such active involvement
as the editor. Thanks also to the following people for contributing
early in development.

  - [Alex Axthelm](https://github.com/AlexAxthelm)
  - [Chan-Yub Park](https://github.com/mrchypark)
  - [Daniel Falster](https://github.com/dfalster)
  - [Eric Nantz](https://github.com/rpodcast)
  - [Henrik Bengtsson](https://github.com/HenrikBengtsson)
  - [Ian Watson](https://github.com/IanAWatson)
  - [Jasper Clarkberg](https://github.com/dapperjapper)
  - [Kendon Bell](https://github.com/kendonB)
  - [Kirill Müller](https://github.com/krlmlr)
  - [Michael Schubert](https://github.com/mschubert)

Credit for images is [attributed
here](https://ropensci.github.io/drake/figures/image-credit.md).

[![ropensci\_footer](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)
