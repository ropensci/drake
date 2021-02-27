drake_context("time")

test_with_dir("can ignore a bad time", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("lubridate")
  x <- drake_plan(a = 1, b = 2)
  make(x, verbose = 0L)
  cache <- drake_cache()
  expect_equal(nrow(build_times()), 2)
  meta <- diagnose(a)
  meta$time_build <- NA
  cache$set(key = "a", value = meta, namespace = "meta")
  expect_equal(nrow(build_times()), 1)
})

test_with_dir("proc_time runtimes can be fetched", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("lubridate")
  cache <- new_cache("cache")
  t <- system.time({
    z <- 1
  })
  meta <- list(time_build = t)
  cache$set(key = "x", value = meta, namespace = "meta")
  y <- extract_runtime(meta, type = "time_build")
  expect_true(is.list(y))
  expect_equal(sort(names(y)), sort(c("target", "elapsed", "user", "system")))
})

test_with_dir("build times works if no targets are built", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("lubridate")
  expect_equal(cached(), character(0))
  expect_equal(nrow(build_times()), 0)
  my_plan <- drake_plan(x = 1)
  con <- drake_config(my_plan, verbose = 0L)
  process_imports(con)
  expect_equal(nrow(build_times()), 0)
})

test_with_dir("build time the same after superfluous make", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("lubridate")
  x <- drake_plan(y = Sys.sleep(0.25))
  make(x, verbose = 0L, session_info = FALSE)
  c1 <- drake_config(x, verbose = 0L, session_info = FALSE)
  expect_equal(justbuilt(c1), "y")
  b1 <- build_times()
  expect_true(all(complete.cases(b1)))

  make(x, verbose = 0L, session_info = FALSE)
  c2 <- drake_config(x, verbose = 0L, session_info = FALSE)
  expect_equal(justbuilt(c2), character(0))
  b2 <- build_times()
  expect_true(all(complete.cases(b2)))
  expect_equal(b1[b1$target == "y", ], b2[b2$target == "y", ])
})

test_with_dir("runtime predictions", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("lubridate")
  con <- dbug()
  expect_warning(p0 <- as.numeric(predict_runtime_impl(con)))
  expect_true(p0 < 1e4)
  expect_warning(
    p0 <- as.numeric(predict_runtime_impl(con, targets_only = TRUE))
  )
  expect_equal(p0, 0, tolerance = 1e-2)
  expect_warning(
    p0 <- as.numeric(predict_runtime_impl(con, default_time = 1e4))
  )
  expect_true(p0 > 6e4 - 10 && p0 < 7e4)
  expect_warning(
    p0 <- as.numeric(
      predict_runtime_impl(con, default_time = 1e4, jobs = 2)
    )
  )
  expect_true(p0 > 4e4 - 10 && p0 < 6e4 + 10)
  testrun(con)
  p1 <- as.numeric(predict_runtime_impl(config = con, jobs = 1))
  p2 <- predict_runtime_impl(
    config = con,
    jobs_predict = 1,
    default_time = Inf,
    from_scratch = FALSE
  )
  p2 <- as.numeric(p2)
  p3 <- predict_runtime_impl(
    config = con,
    jobs = 1,
    default_time = Inf,
    from_scratch = TRUE
  )
  p3 <- as.numeric(p3)
  p4 <- predict_runtime_impl(
    config = con,
    jobs_predict = 2,
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
  p5 <- predict_runtime_impl(
    config = con,
    jobs_predict = 1,
    default_time = Inf,
    from_scratch = FALSE,
    known_times = known_times,
    targets_predict = targets
  )
  p5 <- as.numeric(p5)
  p6 <- predict_runtime_impl(
    config = con,
    jobs_predict = 1,
    default_time = Inf,
    from_scratch = TRUE,
    known_times = known_times,
    targets_predict = targets
  )
  p6 <- as.numeric(p6)
  p7 <- predict_runtime_impl(
    config = con,
    jobs_predict = 2,
    default_time = Inf,
    from_scratch = TRUE,
    known_times = known_times,
    targets_predict = targets
  )
  p7 <- as.numeric(p7)
  expect_true(all(is.finite(c(p1, p2, p3, p4))))
  expect_equal(p5, 0, tolerance = 1e-6)
  expect_equal(p6, 70, tolerance = 1e-6)
  expect_equal(p7, 43, tolerance = 1e-6)
})

test_with_dir("predict_workers_impl()", {
  skip_on_cran()
  skip_if_not_installed("knitr")
  skip_if_not_installed("lubridate")
  load_mtcars_example()
  cache <- storr::storr_environment()
  config <- drake_config(my_plan, cache = cache, session_info = FALSE)
  make(my_plan, cache = config$cache)
  out <- predict_workers_impl(config, jobs = 4)
  expect_equal(sort(unique(out$worker)), sort(as.integer(1:4)))
  expect_equal(dim(out), dim(my_plan))
  expect_equal(sort(colnames(out)), sort(c("target", "worker")))
})

test_with_dir("can disable build times (#1078)", {
  skip_on_cran()
  skip_if_not_installed("lubridate")
  plan <- drake_plan(x = 1)
  make(plan, log_build_times = FALSE)
  expect_equal(nrow(build_times(type = "build")), 0L)
  expect_equal(nrow(build_times(type = "command")), 0L)
})
