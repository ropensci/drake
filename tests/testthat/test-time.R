drake_context("time")

test_with_dir("can ignore a bad time", {
  x <- drake_plan(a = 1, b = 2)
  make(x, verbose = FALSE)
  cache <- get_cache()
  expect_equal(nrow(build_times()), 2)
  set_in_subspace(
    key = "a",
    subspace = "time_build",
    namespace = "meta",
    value = NA,
    cache = cache
  )
  expect_equal(nrow(build_times()), 1)
})

test_with_dir("proc_time runtimes can be fetched", {
  cache <- storr::storr_rds("cache")
  key <- "x"
  t <- system.time({
    z <- 1
  })
  set_in_subspace(
    key = key,
    value = t,
    subspace = "time_build",
    namespace = "meta",
    cache = cache
  )
  y <- fetch_runtime(key = key, cache = cache, type = "build")
  expect_true(nrow(y) > 0)
})

test_with_dir("build times works if no targets are built", {
  expect_equal(cached(), character(0))
  expect_equal(nrow(build_times(search = FALSE)), 0)
  my_plan <- drake_plan(x = 1)
  con <- drake_config(my_plan, verbose = FALSE)
  make_imports(con)
  expect_equal(nrow(build_times(search = FALSE)), 0)
})

test_with_dir("build time the same after superfluous make", {
  x <- drake_plan(y = Sys.sleep(0.25))
  c1 <- make(x, verbose = FALSE, session_info = FALSE)
  expect_equal(justbuilt(c1), "y")
  b1 <- build_times(search = FALSE)
  expect_true(all(complete.cases(b1)))

  c2 <- make(x, verbose = FALSE, session_info = FALSE)
  expect_equal(justbuilt(c2), character(0))
  b2 <- build_times(search = FALSE)
  expect_true(all(complete.cases(b2)))
  expect_equal(b1[b1$item == "y", ], b2[b2$item == "y", ])
})

test_with_dir("runtime predictions", {
  con <- dbug()
  testrun(con)
  p1 <- predict_runtime(config = con, jobs = 1)
  p2 <- predict_runtime(
    config = con,
    jobs = 1,
    default_time = 1e3,
    from_scratch = FALSE
  )
  p3 <- predict_runtime(
    config = con,
    jobs = 1,
    default_time = 1e3,
    from_scratch = TRUE
  )
  p4 <- predict_runtime(
    config = con,
    jobs = 2,
    default_time = 1e3,
    from_scratch = TRUE
  )
  expect_true(p1 < p3)
  expect_true(p2 < p3)
  expect_true(p4 < p3)
})
