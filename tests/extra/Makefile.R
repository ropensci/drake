# Quickest way to run all the unit tests.
# Does not work on Windows.

library(magrittr)
library(testthat)
devtools::load_all()

dir <- file.path("testthat", "workspaces")
setwd("..")

system.time({
  for(opt in c("local_Make_1", "global_Make_16")){
    set_test_opt(opt)
    test_dir("testthat")
  }
})
