drake_context("reproducible random numbers")

test_with_dir("Random targets are reproducible", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
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
  # Should not change the session's seed
  seed0 <- .Random.seed # nolint
  con <- make(
    data,
    envir = env,
    parallelism = parallelism,
    jobs = jobs,
    verbose = FALSE,
    session_info = FALSE
  )
  expect_true(identical(seed0, .Random.seed)) # nolint
  old_x <- readd(x)
  old_y <- readd(y)
  old_z <- readd(z)
  old_mx <- readd(mx)
  old_my <- readd(my)
  old_mz <- readd(mz)
  expect_false(identical(old_x, old_y))

  # Delete and reproduce some random data.
  clean()
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

  # Change the session's seed
  # and check that y and my are the same as before.
  tmp <- runif(1)
  clean(y)
  con3 <- make(
    data,
    envir = env,
    parallelism = parallelism,
    jobs = jobs,
    verbose = FALSE,
    session_info = FALSE
  )
  expect_equal(justbuilt(con3), "y")
  expect_true(identical(con$seed, con3$seed))
  expect_true(identical(readd(y), old_y))
  expect_true(identical(readd(my), old_my))

  # Set the same seed as in the cache
  # and check that things are the same as before.
  tmp <- runif(1)
  clean(y)
  con4 <- make(
    data,
    envir = env,
    seed = con2$seed,
    parallelism = parallelism,
    jobs = jobs,
    verbose = FALSE,
    session_info = FALSE
  )
  expect_equal(justbuilt(con4), "y")
  expect_true(identical(con4$seed, con$seed))
  expect_true(identical(readd(y), old_y))
  expect_true(identical(readd(my), old_my))

  # Change the supplied seed, destroy the cache,
  # and check that the results are different.
  tmp <- runif(1)
  clean(destroy = TRUE)
  con5 <- make(
    data,
    envir = env,
    seed = con2$seed + 1,
    parallelism = parallelism,
    jobs = jobs,
    verbose = FALSE,
    session_info = FALSE
  )
  expect_equal(sort(justbuilt(con5)), sort(con5$plan$target))
  expect_false(identical(con$seed, con5$seed))
  expect_false(identical(readd(x), old_x))
  expect_false(identical(readd(y), old_y))
  expect_false(identical(readd(z), old_z))
  expect_false(identical(readd(mx), old_mx))
  expect_false(identical(readd(my), old_my))
  expect_false(identical(readd(mz), old_mz))

  # Cannot supply a conflicting seed.
  expect_error(
    make(
      data,
      envir = env,
      seed = con5$seed + 1,
      parallelism = parallelism,
      jobs = jobs,
      verbose = FALSE,
      session_info = FALSE
    ),
    regexp = "already has a different seed"
  )
})
