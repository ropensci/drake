drake_context("rng")

test_with_dir("seed_from_basic_types", {
  x <- sample.int(1)
  seed <- get(".Random.seed", envir = globalenv()) # nolint
  s1 <- seed_from_basic_types(seed, "abc")
  s2 <- seed_from_basic_types(seed, "abc")
  s3 <- seed_from_basic_types(seed, "xyz")
  seed[length(seed)] <- seed[length(seed)] + 1
  s4 <- seed_from_basic_types(seed, "xyz")
  expect_true(identical(s1, s2))
  expect_false(identical(s1, s3))
  expect_false(identical(s1, s4))
  expect_false(identical(s2, s4))
  expect_false(identical(s3, s4))
})

test_with_dir("Random targets are reproducible", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  scenario <- get_testing_scenario()
  env <- eval(parse(text = scenario$envir))
  parallelism <- scenario$parallelism
  jobs <- scenario$jobs

  data <- drake_plan(
    x = sample.int(n = 200, size = 3),
    y = sample.int(n = 200, size = 3),
    z = sample.int(n = 200, size = 3),
    mx = mean(x),
    my = mean(y),
    mz = mean(z)
  )
  # Should not change the session's seed
  x <- sample.int(1)
  seed <- get(".Random.seed", envir = globalenv())
  make(
    data,
    envir = env,
    parallelism = parallelism,
    jobs = jobs,
    verbose = 0L,
    session_info = FALSE
  )
  con <- drake_config(
    data,
    envir = env,
    parallelism = parallelism,
    jobs = jobs,
    verbose = 0L,
    session_info = FALSE
  )
  # clustermq modifies the global random seed
  # and it does not affect the way drake builds targets.
  # expect_true(identical(seed0, .Random.seed)) # nolint
  old_x <- readd(x)
  old_y <- readd(y)
  old_z <- readd(z)
  old_mx <- readd(mx)
  old_my <- readd(my)
  old_mz <- readd(mz)
  expect_false(identical(old_x, old_y))

  # Delete and reproduce some random data.
  clean()
  make(
    data,
    envir = env,
    parallelism = parallelism,
    jobs = jobs,
    verbose = 0L,
    session_info = FALSE
  )
  con2 <- drake_config(
    data,
    envir = env,
    parallelism = parallelism,
    jobs = jobs,
    verbose = 0L,
    session_info = FALSE
  )

  expect_identical(con$settings$seed, con2$settings$seed)
  expect_identical(readd(x), old_x)
  expect_identical(readd(y), old_y)
  expect_identical(readd(z), old_z)
  expect_identical(readd(mx), old_mx)
  expect_identical(readd(my), old_my)
  expect_identical(readd(mz), old_mz)

  # Change the session's seed
  # and check that y and my are the same as before.
  tmp <- sample.int(1)
  clean(y)
  make(
    data,
    envir = env,
    parallelism = parallelism,
    jobs = jobs,
    verbose = 0L,
    session_info = FALSE
  )
  con3 <- drake_config(
    data,
    envir = env,
    parallelism = parallelism,
    jobs = jobs,
    verbose = 0L,
    session_info = FALSE
  )
  expect_equal(justbuilt(con3), "y")
  expect_true(identical(con$settings$seed, con3$settings$seed))
  expect_true(identical(readd(y), old_y))
  expect_true(identical(readd(my), old_my))

  # Set the same seed as in the cache
  # and check that things are the same as before.
  tmp <- sample.int(1)
  clean(y)
  make(
    data,
    envir = env,
    seed = con2$settings$seed,
    parallelism = parallelism,
    jobs = jobs,
    verbose = 0L,
    session_info = FALSE
  )
  con4 <- drake_config(
    data,
    envir = env,
    seed = con2$settings$seed,
    parallelism = parallelism,
    jobs = jobs,
    verbose = 0L,
    session_info = FALSE
  )
  expect_equal(justbuilt(con4), "y")
  expect_true(identical(con4$settings$seed, con$settings$seed))
  expect_true(identical(readd(y), old_y))
  expect_true(identical(readd(my), old_my))

  # Change the supplied seed, destroy the cache,
  # and check that the results are different.
  tmp <- sample.int(1)
  clean(destroy = TRUE)
  make(
    data,
    envir = env,
    seed = con2$settings$seed + 1,
    parallelism = parallelism,
    jobs = jobs,
    verbose = 0L,
    session_info = FALSE
  )
  con5 <- drake_config(
    data,
    envir = env,
    seed = con2$settings$seed + 1,
    parallelism = parallelism,
    jobs = jobs,
    verbose = 0L,
    session_info = FALSE
  )
  expect_equal(sort(justbuilt(con5)), sort(data$target))
  expect_false(identical(con$settings$seed, con5$settings$seed))
  expect_false(identical(readd(x), old_x))
  expect_false(identical(readd(y), old_y))
  expect_false(identical(readd(z), old_z))
  expect_false(identical(readd(mx), old_mx))
  expect_false(identical(readd(my), old_my))
  expect_false(identical(readd(mz), old_mz))

  # New seed invalidates old targets.
  make(
    data,
    envir = env,
    seed = con2$settings$seed + 2,
    parallelism = parallelism,
    jobs = jobs,
    verbose = 0L,
    session_info = FALSE
  )
  expect_equal(sort(justbuilt(con5)), sort(data$target))

  make(
    data,
    envir = env,
    parallelism = parallelism,
    jobs = jobs,
    verbose = 0L,
    session_info = FALSE
  )
  expect_equal(justbuilt(con5), character(0))
  expect_equal(con2$settings$seed + 2, read_drake_seed())
  expect_true(con2$settings$seed + 2 > 1L)
})

test_with_dir("custom seeds (#947)", {
  plan <- drake_plan(
    x = sample.int(n = 200, size = 3),
    y = sample.int(n = 200, size = 3),
    z = sample.int(n = 200, size = 3),
    mx = mean(x),
    my = mean(y),
    mz = mean(z)
  )
  config <- drake_config(
    plan,
    cache = storr::storr_environment(),
    session_info = FALSE
  )
  make_impl(config = config)
  expect_equal(sort(justbuilt(config)), sort(plan$target))
  old_x <- readd(x, cache = config$cache)
  old_y <- readd(y, cache = config$cache)
  old_z <- readd(z, cache = config$cache)
  old_mx <- readd(mx, cache = config$cache)
  old_my <- readd(my, cache = config$cache)
  old_mz <- readd(mz, cache = config$cache)
  expect_false(all(old_x == old_y))
  expect_false(all(old_x == old_z))
  expect_false(all(old_mx == old_my))
  expect_false(all(old_mx == old_mz))
  s <- diagnose(y, cache = config$cache)$seed

  plan <- drake_plan(
    x = target(
      sample.int(n = 200, size = 3),
      seed = s,
      trigger = trigger(seed = FALSE)
    ),
    y = sample.int(n = 200, size = 3),
    z = sample.int(n = 200, size = 3),
    mx = mean(x),
    my = mean(y),
    mz = mean(z)
  )
  config <- drake_config(
    plan,
    cache = config$cache,
    session_info = FALSE
  )
  make_impl(config = config)
  expect_equal(justbuilt(config), character(0))

  plan <- drake_plan(
    x = target(
      sample.int(n = 200, size = 3),
      seed = s
    ),
    y = sample.int(n = 200, size = 3),
    z = sample.int(n = 200, size = 3),
    mx = mean(x),
    my = mean(y),
    mz = mean(z)
  )
  config <- drake_config(
    plan,
    trigger = trigger(seed = FALSE),
    cache = config$cache,
    session_info = FALSE
  )
  make_impl(config = config)
  expect_equal(justbuilt(config), character(0))

  plan <- drake_plan(
    x = target(
      sample.int(n = 200, size = 3),
      seed = s
    ),
    y = sample.int(n = 200, size = 3),
    z = sample.int(n = 200, size = 3),
    mx = mean(x),
    my = mean(y),
    mz = mean(z)
  )
  config <- drake_config(
    plan,
    cache = config$cache,
    session_info = FALSE
  )
  make_impl(config = config)
  expect_equal(sort(justbuilt(config)), sort(c("x", "mx")))
  new_x <- readd(x, cache = config$cache)
  new_mx <- readd(mx, cache = config$cache)
  expect_false(all(old_x == new_mx))
  expect_false(all(old_mx == new_mx))
  expect_equal(old_y, new_x)
  expect_equal(old_my, new_mx)
})

test_with_dir("revert the seed trigger and end up with a new seed (#947)", {
  plan <- drake_plan(
    x = sample.int(n = 200, size = 3),
    y = sample.int(n = 200, size = 3),
    z = sample.int(n = 200, size = 3),
    mx = mean(x),
    my = mean(y),
    mz = mean(z)
  )
  config <- drake_config(
    plan,
    cache = storr::storr_environment(),
    session_info = FALSE
  )
  make_impl(config = config)
  expect_equal(sort(justbuilt(config)), sort(plan$target))

  s <- diagnose(x, cache = config$cache)$seed
  expect_true(s > 5)
  expect_true(is.integer(s))

  plan <- drake_plan(
    x = target(
      sample.int(n = 201, size = 3),
      seed = 2,
      trigger = trigger(seed = FALSE)
    ),
    y = sample.int(n = 200, size = 3),
    z = sample.int(n = 200, size = 3),
    mx = mean(x),
    my = mean(y),
    mz = mean(z)
  )
  config <- drake_config(
    plan,
    cache = config$cache,
    session_info = FALSE
  )
  make_impl(config = config)
  expect_equal(sort(justbuilt(config)), sort(c("x", "mx")))

  plan <- drake_plan(
    x = target(
      sample.int(n = 201, size = 3),
      seed = 3
    ),
    y = sample.int(n = 200, size = 3),
    z = sample.int(n = 200, size = 3),
    mx = mean(x),
    my = mean(y),
    mz = mean(z)
  )
  config <- drake_config(
    plan,
    cache = config$cache,
    session_info = FALSE
  )
  make_impl(config = config)
  expect_equal(sort(justbuilt(config)), sort(c("x", "mx")))
})
