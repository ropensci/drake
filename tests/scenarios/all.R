library(testthat)
devtools::load_all()
withr::with_options(
  list(warnPartialMatchArgs = FALSE),
  test_scenarios()
)
