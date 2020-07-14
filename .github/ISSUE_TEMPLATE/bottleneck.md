---
name: Bottleneck
about: drake is too slow or consumes too many resources.
title: ''
labels: 'topic: performance'
assignees: wlandau

---

## Prework

- [ ] Read and abide by `drake`'s [code of conduct](https://github.com/ropensci/drake/blob/master/CODE_OF_CONDUCT.md).
- [ ] Search for duplicates among the [existing issues](https://github.com/ropensci/drake/issues), both open and closed.
- [ ] Be considerate of the maintainer's time and make it as easy as possible to troubleshoot any problems you identify. Read [here](https://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example) and [here](https://www.tidyverse.org/help/) to learn about minimal reproducible examples. Format your code according to the [tidyverse style guide](https://style.tidyverse.org/) to make it easier for others to read.

## Description

Describe the bottleneck clearly and concisely. 

## Reproducible example

Be considerate of the maintainer's time and make it as easy as possible to troubleshoot any problems you identify. Read [here](https://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example) and [here](https://www.tidyverse.org/help/) to learn about minimal reproducible examples. Format your code according to the [tidyverse style guide](https://style.tidyverse.org/) to make it easier for others to read.

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
