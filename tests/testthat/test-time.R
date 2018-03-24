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

test_with_dir("empty time predictions", {
  min_df <- function(df){
    df <- df[!grepl("covr", df$item, fixed = TRUE), ]
    df <- df[!grepl(":::", df$item, fixed = TRUE), ]
    df
  }

  my_plan <- drake_plan(y = 1)
  config <- drake_config(my_plan)
  expect_warning(
    x <- rate_limiting_times(config) %>%
      min_df
  )
  expect_equal(nrow(x), 0)
  config <- make(my_plan, verbose = FALSE, session_info = FALSE)
  x <- rate_limiting_times(config) %>%
    min_df
  expect_equal(nrow(x), 0)
  x <- rate_limiting_times(config,
    targets_only = TRUE) %>%
    min_df
  expect_equal(nrow(x), 0)
  x <- rate_limiting_times(config,
    from_scratch = TRUE) %>%
    min_df
  expect_equal(nrow(x), 1)
  expect_true(all(complete.cases(x)))
  x <- rate_limiting_times(config,
    targets_only = TRUE, from_scratch = TRUE) %>%
    min_df
  expect_equal(nrow(x), 1)
  expect_true(all(complete.cases(x)))
})

test_with_dir("time predictions: incomplete targets", {
  min_df <- function(df){
    df <- df[!grepl("covr", df$item, fixed = TRUE), ]
    df <- df[!grepl(":::", df$item, fixed = TRUE), ]
    df
  }

  eval(parse(text = "require(methods, quietly = TRUE)"))
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs

  load_basic_example(envir = e)
  my_plan <- e$my_plan
  config <- drake_config(my_plan, envir = e,
    jobs = 1, verbose = FALSE)
  config <- make_imports(config)

  dats <- c("small", "large")
  expect_warning(
    x <- rate_limiting_times(
      config,
      targets = dats
    ) %>%
    min_df
  )
  expect_equal(nrow(x), 6)
  expect_warning(
    x <- rate_limiting_times(
      config,
      targets = dats,
      targets_only = TRUE
    ) %>%
    min_df
  )
  expect_equal(nrow(x), 0)
  expect_warning(
    x <- rate_limiting_times(config) %>%
    min_df
  )
  expect_equal(nrow(x), 13)
  expect_warning(
    y <- predict_runtime(
      config,
      targets = dats,
      digits = Inf
    )
  )
  expect_equal(length(y), 1)
  expect_warning(
    y <- predict_runtime(
      config,
      targets = dats,
      digits = Inf,
      targets_only = TRUE
    )
  )
  expect_equal(length(y), 1)

  config$targets <- dats
  con <- testrun(config)

  expect_silent(
    x <- rate_limiting_times(
      con,
      targets = dats,
      from_scratch = TRUE
    ) %>%
    min_df
  )
  expect_equal(nrow(x), 8)

  config <- drake_config(plan = con$plan, envir = con$envir, verbose = FALSE)
  testrun(config)
  expect_silent(
    x <- rate_limiting_times(
      config,
      from_scratch = TRUE,
      future_jobs = 2
    ) %>%
    min_df
  )
  expect_true(nrow(x) >= 14 & nrow(x) < 27)
  expect_silent(
    x <- rate_limiting_times(
      config,
      from_scratch = TRUE
    ) %>%
    min_df
  )
  expect_equal(nrow(x), 28)
  expect_silent(
    y <- predict_runtime(
      config,
      from_scratch = TRUE,
      digits = Inf
    )
  )
  expect_equal(length(y), 1)
})

test_with_dir("timing predictions with realistic build", {
  min_df <- function(df){
    df <- df[!grepl("covr", df$item, fixed = TRUE), ]
    df <- df[!grepl(":::", df$item, fixed = TRUE), ]
    df
  }

  eval(parse(text = "require(methods, quietly = TRUE)"))
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs

  load_basic_example(envir = e)
  my_plan <- e$my_plan
  my_plan$command <- paste("Sys.sleep(0.001);", my_plan$command)
  config <- drake_config(my_plan, envir = e, parallelism = "mclapply",
    jobs = 1, verbose = FALSE)
  config <- testrun(config)
  config$envir$reg2 <- function(d){
    d$x3 <- d$x ^ 3
    lm(y ~ x3, data = d)
  }

  # should not really use config after
  # manually setting things out of date
  scratch_df <- rate_limiting_times(
    config,
    from_scratch = TRUE,
    digits = Inf
  ) %>%
    min_df
  resume_df <- rate_limiting_times(
    config,
    digits = Inf
  ) %>%
  min_df
  resume_df_targets <- rate_limiting_times(
    config,
    digits = Inf,
    targets_only = TRUE
  ) %>%
    min_df
  jobs_4_df <- rate_limiting_times(
    config,
    future_jobs = 4
  ) %>%
    min_df
  jobs_4_df_targets <- rate_limiting_times(
    config,
    from_scratch = TRUE,
    future_jobs = 4,
    targets_only = TRUE
  ) %>%
    min_df
  jobs_2_df <- rate_limiting_times(
    config,
    from_scratch = TRUE,
    future_jobs = 2
  ) %>%
    min_df

  expect_true(all(complete.cases(scratch_df)))
  expect_true(all(complete.cases(resume_df)))
  expect_true(all(complete.cases(resume_df_targets)))
  expect_true(all(complete.cases(jobs_2_df)))
  expect_true(all(complete.cases(jobs_4_df)))
  expect_true(all(complete.cases(jobs_4_df_targets)))

  expect_equal(nrow(scratch_df), 29)
  expect_equal(nrow(resume_df), nrow(scratch_df) - 8)
  expect_equal(nrow(resume_df_targets), nrow(scratch_df) - 22)
  expect_true(nrow(jobs_2_df) < nrow(scratch_df))
  expect_true(nrow(jobs_4_df) < nrow(jobs_2_df))
  expect_true(nrow(jobs_4_df_targets) < nrow(jobs_4_df))
})
