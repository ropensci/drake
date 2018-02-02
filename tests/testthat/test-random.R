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

  # Change the session's seed, destroy the cache.
  # and check that the results are different.
  tmp <- runif(1)
  clean(destroy = TRUE)
  con4 <- make(
    data,
    envir = env,
    parallelism = parallelism,
    jobs = jobs,
    verbose = FALSE,
    session_info = FALSE
  )
  expect_equal(sort(justbuilt(con4)), sort(con4$plan$target))
  expect_false(identical(con$seed, con4$seed))
  expect_false(identical(readd(x), old_x))
  expect_false(identical(readd(y), old_y))
  expect_false(identical(readd(z), old_z))
  expect_false(identical(readd(mx), old_mx))
  expect_false(identical(readd(my), old_my))
  expect_false(identical(readd(mz), old_mz))
})

test_that("random seed can be read", {
  cache <- storr::storr_environment()
  my_plan <- drake_plan(
    target1 = sqrt(1234),
    target2 = rnorm(n = 1, mean = target1)
  )
  digest::digest(.Random.seed) # nolint
  make(my_plan, cache = cache, session_info = FALSE)
  rs0 <- digest::digest(.Random.seed) # nolint
  ds0 <- digest::digest(read_drake_seed(cache = cache))
  targ0 <- readd(target2, cache = cache)
  clean(target2, cache = cache)
  x <- runif(1) # Maybe I also changed the R session's seed.
  rs1 <- digest::digest(.Random.seed) # nolint
  make(my_plan, cache = cache, session_info = FALSE)
  ds2 <- digest::digest(read_drake_seed(cache = cache))
  rs2 <- digest::digest(.Random.seed) # nolint
  targ2 <- readd(target2, cache = cache)

  expect_equal(rs1, rs2)
  expect_false(rs0 == rs1)
  expect_equal(ds0, ds2)
  expect_equal(targ0, targ2)
  cache$del(key = "seed", namespace = "config")
  expect_error(read_drake_seed(cache = cache), regexp = "seed not found")
})
