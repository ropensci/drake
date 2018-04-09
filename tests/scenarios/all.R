library(testthat)
devtools::load_all()
withr::with_options(
  list(
    mc.cores = 4,
    warnPartialMatchArgs = FALSE
  ),
  test_scenarios()
)
