Sys.setenv(NOT_CRAN = "true")
library(testthat)
devtools::load_all()
args <- commandArgs(TRUE)
withr::with_options(
  list(drake_no_processx = TRUE),
  test_scenarios(scenario_names = args)
)
