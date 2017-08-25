# Quickest way to run all the unit tests.
# Does not work on Windows.

library(magrittr)
library(testthat)
devtools::load_all()

dir <- file.path("testthat", "workspaces")
setwd("..")

system.time({
  set_test_opt("local_mcl_8")
  test_dir("testthat")
})
