library(testthat)
devtools::load_all()
scenario_names <- c(
  "global_Makefile_2",
  "local_Makefile_9"
)
test_scenarios(scenario_names = scenario_names)
