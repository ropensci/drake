cat(get_testing_scenario_name(), ": ", sep = "")
context("reproducible random numbers")

test_with_dir("Objects are reproducible", {
  scenario <- get_testing_scenario()
  env <- eval(parse(text = scenario$envir))
  parallelism <- scenario$parallelism
  jobs <- scenario$jobs
  
  data <- plan(
    x = runif(20),
    y = runif(20),
    z = rnorm(20)
  )
  make(
    data,
    envir = env,
    parallelism = parallelism,
    jobs = jobs,
    verbose = FALSE
  )
  old_x <- readd(x)
  old_y <- readd(y)
  old_z <- readd(z)
  
  # Oh no, I've accidentally deleted some data that needs to be reproducible
  clean(x, y, z)
  make(
    data,
    envir = env,
    parallelism = parallelism,
    jobs = jobs,
    verbose = FALSE
  )
  expect_identical(readd(x), old_x)
  expect_identical(readd(y), old_y)
  expect_identical(readd(z), old_z)
})

test_with_dir("Random targets are distinct", {
  scenario <- get_testing_scenario()
  env <- eval(parse(text = scenario$envir))
  parallelism <- scenario$parallelism
  jobs <- scenario$jobs
  data <- plan(
    x = runif(20)
  )
  data_exp <- expand(data, values = c("a", "b"))
  make(
    data_exp,
    envir = env,
    parallelism = parallelism,
    jobs = jobs,
    verbose = FALSE
  )
  expect_false(identical(readd(x_a), readd(x_b)))
})

test_with_dir("Random sequential targets reproduce correctly", {
  scenario <- get_testing_scenario()
  env <- eval(parse(text = scenario$envir))
  parallelism <- scenario$parallelism
  jobs <- scenario$jobs
  first_stage <- plan(
    x = runif(20)
  )
  methods <- plan(
    xbar = mean(..dataset..),  # nolint
    y = sample(..dataset.., 5)  # nolint
  )
  second_stage <- analyses(
    methods,
    data = first_stage
  )
  make(
    rbind(first_stage, second_stage),
    envir = env,
    parallelism = parallelism,
    jobs = jobs,
    verbose = FALSE
  )
  old_x <- readd(x)
  old_xbax_x <- readd(xbar_x)
  old_y_y <- readd(y_x)
  
  # How do I keep deleting my data like this? So clumsy.
  clean(x, xbar_x, y_x)
  
  make(
    rbind(first_stage, second_stage),
    envir = env,
    parallelism = parallelism,
    jobs = jobs,
    verbose = FALSE
  )
  expect_identical(old_x, readd(x))
  expect_identical(old_xbax_x, readd(xbar_x))
  expect_identical(old_y_y, readd(y_x))
})
