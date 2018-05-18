library(testthat)
devtools::load_all()
args <- commandArgs(TRUE)
test_scenarios(scenario_names = args)
