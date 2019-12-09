# Custom functions are an important part of a drake workflow.
# This is where you write them.
# Details: https://books.ropensci.org/drake/plans.html#functions

generate_data <- function() {
  tibble(x = rnorm(1e5), y = rnorm(1e5))
}

fit_model <- function(data) {
  summary(lm(y ~ x, data = data))
}
