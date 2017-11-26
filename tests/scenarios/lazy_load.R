# Your working directory needs to be drake/tests/scenarios for this one.
library(testthat)
devtools::load_all()
test_file <- file.path("..", "testthat", "test-lazy-load.R")
scenario_table <- testing_scenarios()
scenarios <- Filter(
  x = testing_scenario_names(),
  f = function(x){
    !should_skip(x)
  }
)
for (scenario in scenarios[-1:-2]){
  set_testing_scenario(scenario = scenario)
  cat(get_testing_scenario_name(), "\n")
  source(test_file)
}
