test_option_name <- "drake_test_scenario"

get_testing_scenario <- function() {
  scenario <- get_testing_scenario_name()
  testing_scenarios[[scenario]]
}

get_testing_scenario_name <- function() {
  scenario <- getOption(test_option_name)
  if (!length(scenario))
    scenario <- names(testing_scenarios)[1]
  scenario
}

set_testing_scenario <- function(scenario = NULL) {
  scenario <- match.arg(scenario, choices = names(testing_scenarios))
  new <- list()
  new[[test_option_name]] <- scenario
  options(new)
}

test_scenarios <- function(
  scenarios = names(testing_scenarios),
  unit_test_dir = unit_test_files(),
  skip_criterion = should_skip,
  ...
){
  for (index in seq_along(scenarios)){
    scenarios[[index]] <- match.arg(
      arg = scenarios[[index]],
      choices = names(testing_scenarios)
    )
  }
  for (scenario_name in scenarios){
    skip <- skip_criterion(scenario_name)
    msg <- ifelse(skip, "skip", "run")
    cat(scenario_name, ": ", msg, "\n", sep = "")
    new <- list()
    new[[test_option_name]] <- scenario_name
    if (!skip) {
      with_options(new = new, testthat::test_dir(unit_test_dir))
    }
  }
}
