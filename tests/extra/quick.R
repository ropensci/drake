# Quickest way to run all the unit tests.
# Does not work on Windows.

library(magrittr)
library(testthat)
devtools::load_all()

dir <- file.path("testthat", "workspaces")
setwd("..")

system.time({
  opt_name <- "local_mcl_8"
  set_test_opt(opt_name)
  cat(opt_name, "\n")
  unlink(dir, recursive = TRUE, force = TRUE)
  test_dir("testthat")
  unlink(dir, recursive = TRUE, force = TRUE)
})
