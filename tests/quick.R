# Quickest way to run all the unit tests.
# Does not work on Windows.

library(magrittr)
library(testthat)
devtools::load_all()

os <- Sys.info()['sysname'] %>% tolower %>% unname

system.time(
  opt_name = "global_mcl_8"
  set_test_opt(opt_name)
  cat(opt_name)
  test_dir("testthat")
)
