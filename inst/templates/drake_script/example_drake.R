# This file serves the r_*() functions (e.g. r_make()) documented at
# https://books.ropensci.org/drake/projects.html#safer-interactivity # nolint
# and
# https://docs.ropensci.org/drake/reference/r_make.html

# Load all your packages before calling make().
library(drake)
library(tibble)

# Custom functions are an important part of a drake workflow.
# This is where you write them.
# Details: https://books.ropensci.org/drake/plans.html#functions

generate_data <- function() {
  tibble(x = rnorm(1e5), y = rnorm(1e5))
}

fit_model <- function(data) {
  summary(lm(y ~ x, data = data))
}

# This is where you write your drake plan.
# Details: https://books.ropensci.org/drake/plans.html

plan <- drake_plan(
  data = generate_data(),
  model = fit_model(data)
)

# You could have put all the above into separate script files and
# source()'d them as below:

# source("R/packages.R")  # Load your packages, e.g. library(drake). # nolint
# source("R/functions.R") # Define your custom code as a bunch of functions. # nolint
# source("R/plan.R")      # Create your drake plan. # nolint

# _drake.R must end with a call to drake_config().
# The arguments to drake_config() are basically the same as those to make().
drake_config(plan)
