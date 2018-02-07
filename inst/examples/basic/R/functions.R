# This file contains all the functions of the workflow.
# If needed, you could split it up into multiple files.

# Pick a random subset of n rows from a dataset
random_rows <- function(data, n){
  data[sample.int(n = nrow(data), size = n, replace = TRUE), ]
}

# Bootstrapped datasets from mtcars.
simulate <- function(n){
  # Pick a random set of cars to bootstrap from the mtcars data.
  data <- random_rows(data = mtcars, n = n)

  # x is the car's weight, and y is the fuel efficiency.
  data.frame(
    x = data$wt,
    y = data$mpg
  )
}

# Try a couple different regression models.

# Is fuel efficiency linearly related to weight?
reg1 <- function(d){
  lm(y ~ + x, data = d)
}

# Is fuel efficiency related to the SQUARE of the weight?
reg2 <- function(d){
  d$x2 <- d$x ^ 2
  lm(y ~ x2, data = d)
}
