drake_context("reproducible random numbers")

test_with_dir("get_valid_seed() gets a valid seed", {
  expect_true(is.integer(get_valid_seed(seed = NULL)))
})

test_with_dir("Random targets are reproducible", {
  scenario <- get_testing_scenario()
  env <- eval(parse(text = scenario$envir))
  parallelism <- scenario$parallelism
  jobs <- scenario$jobs

  data <- drake_plan(
    x = runif(20),
    y = runif(20),
    z = rnorm(20),
    mx = mean(x),
    my = mean(y),
    mz = mean(z)
  )
  con <- make(
    data,
    envir = env,
    parallelism = parallelism,
    jobs = jobs,
    verbose = FALSE,
    session_info = FALSE
  )
  old_x <- readd(x)
  old_y <- readd(y)
  old_z <- readd(z)
  old_mx <- readd(mx)
  old_my <- readd(my)
  old_mz <- readd(mz)
  expect_false(identical(old_x, old_y))

  # Deleted and reproduce some random data.
  clean(destroy = TRUE)
  con2 <- make(
    data,
    envir = env,
    parallelism = parallelism,
    jobs = jobs,
    verbose = FALSE,
    session_info = FALSE
  )

  expect_identical(con$seed, con2$seed)
  expect_identical(readd(x), old_x)
  expect_identical(readd(y), old_y)
  expect_identical(readd(z), old_z)
  expect_identical(readd(mx), old_mx)
  expect_identical(readd(my), old_my)
  expect_identical(readd(mz), old_mz)

  # Change the seed and check that the targets are different.
  tmp <- runif(1)
  clean(destroy = TRUE)
  con3 <- make(
    data,
    envir = env,
    parallelism = parallelism,
    jobs = jobs,
    verbose = FALSE,
    session_info = FALSE
  )
  expect_false(identical(con$seed, con3$seed))
  expect_false(identical(readd(x), old_x))
  expect_false(identical(readd(y), old_y))
  expect_false(identical(readd(z), old_z))
  expect_false(identical(readd(mx), old_mx))
  expect_false(identical(readd(my), old_my))
  expect_false(identical(readd(mz), old_mz))
})
