---
name: Bottleneck
about: drake is too slow or consumes too many resources.
title: ''
labels: 'topic: performance'
assignees: wlandau

---

## Prework

* [ ] Read and agree to the [code of conduct](https://github.com/ropensci/drake/blob/master/CODE_OF_CONDUCT.md) and [contributing guidelines](https://github.com/ropensci/drake/blob/master/CONTRIBUTING.md).
* [ ] If there is [already a relevant issue](https://github.com/ropensci/drake/issues), whether open or closed, comment on the existing thread instead of posting a new issue.
* [ ] Post a [minimal reproducible example](https://www.tidyverse.org/help/) so the maintainer can troubleshoot the problems you identify. A reproducible example is:
    * [ ] **Runnable**: post enough R code and data so any onlooker can create the error on their own computer.
    * [ ] **Minimal**: reduce runtime wherever possible and remove complicated details that are irrelevant to the issue at hand.
    * [ ] **Readable**: format your code according to the [tidyverse style guide](https://style.tidyverse.org/).

## Description

Describe the bottleneck clearly and concisely. 

## Reproducible example

* [ ] Post a [minimal reproducible example](https://www.tidyverse.org/help/) so the maintainer can troubleshoot the problems you identify. A reproducible example is:
    * [ ] **Runnable**: post enough R code and data so any onlooker can create the error on their own computer.
    * [ ] **Minimal**: reduce runtime wherever possible and remove complicated details that are irrelevant to the issue at hand.
    * [ ] **Readable**: format your code according to the [tidyverse style guide](https://style.tidyverse.org/).

## Benchmarks

How poorly does `drake` perform? To find out, we recommend the [`proffer`](https://github.com/wlandau/proffer) package and take screenshots of the results displayed in your browser.

```r
library(drake)
library(proffer)
px <- pprof({
  # All your drake code goes here.
})
```

Alternatively, if installing [`proffer`](https://github.com/wlandau/proffer) is too cumbersome, create a zip archive of profiling data (e.g. `samples.zip` below) and upload it to this issue thread.

```r
Rprof(filename = "samples.rprof")
# Slow code goes here.
Rprof(NULL)
zip(zipfile = "samples.zip", files = "samples.rprof")
```
