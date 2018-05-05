library(testthat)
devtools::load_all()
scenario_names <- Filter(
  x = testing_scenario_names(),
  f = function(name){
    grepl("future", name)
  }
)
test_scenarios(scenario_names = scenario_names)
