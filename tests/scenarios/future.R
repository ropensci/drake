library(testthat)
devtools::load_all()
scenario_names <- Filter(
  x = testing_scenario_names(),
  f = function(name){
    # grepl("future", name)
    grepl("master$", name) | grepl("worker$", name)
  }
)
withr::with_options(
  list(mc.cores = 4),
  test_scenarios(scenario_names = scenario_names)
)
