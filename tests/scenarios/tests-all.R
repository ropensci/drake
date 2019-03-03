Sys.setenv(NOT_CRAN = "true")
Sys.setenv(drake_skip_callr = "true")
library(testthat)
devtools::load_all()
args <- commandArgs(TRUE)
with_options(
  list(drake_no_processx = TRUE),
  test_scenarios()
)
