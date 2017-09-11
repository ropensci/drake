# Quickest way to run all the unit tests.
# Does not work on Windows.

library(magrittr)
library(testthat)
devtools::load_all()

dir <- file.path("testthat", "workspaces")
setwd("..")

system.time({
  set_test_opt("local_mcl_1")
  test_dir("testthat")
})
