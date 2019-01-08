drake_context("time")

test_with_dir("can ignore a bad time", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("lubridate")
  x <- drake_plan(a = 1, b = 2)
  make(x, verbose = FALSE)
  cache <- get_cache()
  expect_equal(nrow(build_times()), 2)
  set_in_subspaces(
    key = "a",
    subspaces = "time_build",
    namespace = "meta",
    values = NA,
    cache = cache
  )
  expect_equal(nrow(build_times()), 1)
})

test_with_dir("proc_time runtimes can be fetched", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("lubridate")
  cache <- storr::storr_rds("cache")
  key <- "x"
  t <- system.time({
    z <- 1
  })
  set_in_subspaces(
    key = key,
    values = list(x = t),
    subspaces = "time_build",
    namespace = "meta",
    cache = cache
  )
  y <- fetch_runtime(key = key, cache = cache, type = "build")
  expect_true(nrow(y) > 0)
})

test_with_dir("build times works if no targets are built", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("lubridate")
  expect_equal(cached(), character(0))
  expect_equal(nrow(build_times(search = FALSE)), 0)
  my_plan <- drake_plan(x = 1)
  con <- drake_config(my_plan, verbose = FALSE)
  process_imports(con)
  expect_equal(nrow(build_times(search = FALSE)), 0)
})

test_with_dir("build time the same after superfluous make", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("lubridate")
  x <- drake_plan(y = Sys.sleep(0.25))
  make(x, verbose = FALSE, session_info = FALSE)
  c1 <- drake_config(x, verbose = FALSE, session_info = FALSE)
  expect_equal(justbuilt(c1), "y")
  b1 <- build_times(search = FALSE)
  expect_true(all(complete.cases(b1)))

  make(x, verbose = FALSE, session_info = FALSE)
  c2 <- drake_config(x, verbose = FALSE, session_info = FALSE)
  expect_equal(justbuilt(c2), character(0))
  b2 <- build_times(search = FALSE)
  expect_true(all(complete.cases(b2)))
  expect_equal(b1[b1$target == "y", ], b2[b2$target == "y", ])
})

test_with_dir("runtime predictions", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("lubridate")
  con <- dbug()
  expect_warning(p0 <- as.numeric(predict_runtime(con)))
  expect_true(p0 < 1e4)
  expect_warning(p0 <- as.numeric(predict_runtime(con, targets_only = TRUE)))
  expect_equal(p0, 0, tolerance = 1e-2)
  expect_warning(p0 <- as.numeric(predict_runtime(con, default_time = 1e4)))
  expect_true(p0 > 6e4 - 10 && p0 < 7e4)
  expect_warning(
    p0 <- as.numeric(
      predict_runtime(con, default_time = 1e4, jobs = 2)
    )
  )
  expect_true(p0 > 4e4 - 10 && p0 < 6e4 + 10)
  testrun(con)
  p1 <- as.numeric(predict_runtime(config = con, jobs = 1))
  p2 <- predict_runtime(
    config = con,
    jobs = 1,
    default_time = Inf,
    from_scratch = FALSE
  )
  p2 <- as.numeric(p2)
  p3 <- predict_runtime(
    config = con,
    jobs = 1,
    default_time = Inf,
    from_scratch = TRUE
  )
  p3 <- as.numeric(p3)
  p4 <- predict_runtime(
    config = con,
    jobs = 2,
    default_time = Inf,
    from_scratch = TRUE
  )
  p4 <- as.numeric(p4)
  known_times <- c(
    myinput = 10,
    nextone = 33,
    yourinput = 27,
    final = Inf
  )
  targets <- c("nextone", "yourinput")
  p5 <- predict_runtime(
    config = con,
    jobs = 1,
    default_time = Inf,
    from_scratch = FALSE,
    known_times = known_times,
    targets = targets
  )
  p5 <- as.numeric(p5)
  p6 <- predict_runtime(
    config = con,
    jobs = 1,
    default_time = Inf,
    from_scratch = TRUE,
    known_times = known_times,
    targets = targets
  )
  p6 <- as.numeric(p6)
  p7 <- predict_runtime(
    config = con,
    jobs = 2,
    default_time = Inf,
    from_scratch = TRUE,
    known_times = known_times,
    targets = targets
  )
  p7 <- as.numeric(p7)
  expect_true(all(is.finite(c(p1, p2, p3, p4))))
  expect_equal(p5, 0, tolerance = 1e-6)
  expect_equal(p6, 70, tolerance = 1e-6)
  expect_equal(p7, 43, tolerance = 1e-6)
})
