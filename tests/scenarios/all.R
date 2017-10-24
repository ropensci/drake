library(testthat)
devtools::load_all()
options(mc.cores = 4)
withr::with_options(list(mc.cores = 4), test_scenarios())
