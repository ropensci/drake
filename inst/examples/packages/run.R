# This analysis explores some trends in R package downloads over time.
# The data change often, but drake makes sure
# the project does not restart from scratch.
#
# Also see the example-packages.Rmd vignette,
# https://github.com/wlandau-lilly/drake/blob/master/vignettes/example-packages.Rmd

library(drake)
library(cranlogs)
library(knitr)
library(ggplot2)

packages <- c(
  "knitr",
  "Rcpp",
  "ggplot2"
)

data_plan <- drake_plan(
  recent = cran_downloads(packages = "PACKAGE", when = "last-month"),
  older = cran_downloads(
    packages = "PACKAGE",
    from = "2016-11-01",
    to = "2016-12-01"
  ),
  strings_in_dots = "literals"
)

data_plan$trigger <- c("always", "any")

data_plan <- evaluate_plan(
  plan = data_plan,
  wildcard = "PACKAGE",
  values = c(
    "ggplot2",
    "knitr",
    "Rcpp"
  )
)

whole_plan <- rbind(
  data_plan
)

make(whole_plan)
