library(testthat)
devtools::load_all()
scenarios <- c(
  "local_Make_1",
  "global_Make_16"
)
test_scenarios(scenarios = scenarios)
