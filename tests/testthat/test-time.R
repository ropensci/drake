cat(get_testing_scenario_name(), ": ", sep = "")
context("time")

test_with_dir("build times works if no targets are built", {
  expect_equal(nrow(build_times(search = FALSE)), 0)
  my_plan <- plan(x = 1)
  make(my_plan, verbose = FALSE, imports_only = TRUE)
  expect_equal(nrow(build_times(search = FALSE)), 0)
})

test_with_dir("build time the same after superfluous make", {
  x <- plan(y = Sys.sleep(0.25))
  c1 <- make(x, verbose = FALSE, return_config = TRUE)
  expect_equal(justbuilt(c1), "y")
  b1 <- build_times(search = FALSE)
  expect_true(all(complete.cases(b1)))

  c2 <- make(x, verbose = FALSE, return_config = TRUE)
  expect_equal(justbuilt(c2), character(0))
  b2 <- build_times(search = FALSE)
  expect_true(all(complete.cases(b2)))
  expect_equal(b1[b1$item == "y", ], b2[b2$item == "y", ])
})

test_with_dir("empty time predictions", {
  min_df <- function(df){
    df <- df[!grepl("covr", df$item), ]
    df <- df[!grepl(":::", df$item), ]
    df
  }

  my_plan <- plan(y = 1)
  expect_warning(
    x <- rate_limiting_times(plan = my_plan, verbose = FALSE) %>%
      min_df
  )
  expect_equal(nrow(x), 0)
  make(my_plan, verbose = FALSE)
  x <- rate_limiting_times(plan = my_plan, verbose = FALSE) %>%
    min_df
  expect_equal(nrow(x), 0)
  x <- rate_limiting_times(plan = my_plan, verbose = FALSE,
    targets_only = TRUE) %>%
    min_df
  expect_equal(nrow(x), 0)
  x <- rate_limiting_times(plan = my_plan, verbose = FALSE,
    from_scratch = TRUE) %>%
    min_df
  expect_equal(nrow(x), 1)
  expect_true(all(complete.cases(x)))
  x <- rate_limiting_times(plan = my_plan, verbose = FALSE,
    targets_only = TRUE, from_scratch = TRUE) %>%
    min_df
  expect_equal(nrow(x), 1)
  expect_true(all(complete.cases(x)))
})

test_with_dir("time predictions: incomplete targets", {
  min_df <- function(df){
    df <- df[!grepl("covr", df$item), ]
    df <- df[!grepl(":::", df$item), ]
    df
  }

  eval(parse(text = "require(methods, quietly = TRUE)"))
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs

  load_basic_example(envir = e)
  my_plan <- e$my_plan
  config <- config(my_plan, envir = e,
    jobs = 1, verbose = FALSE)

  dats <- c("small", "large")
  expect_warning(
    x <- rate_limiting_times(
      plan = config$plan,
      targets = dats,
      envir = config$envir,
      verbose = FALSE
    ) %>%
    min_df
  )
  expect_equal(nrow(x), 6)
  expect_warning(
    x <- rate_limiting_times(
      plan = config$plan,
      targets = dats,
      envir = config$envir,
      verbose = FALSE,
      targets_only = TRUE
    ) %>%
    min_df
  )
  expect_equal(nrow(x), 0)
  expect_warning(
    x <- rate_limiting_times(
      plan = config$plan,
      envir = config$envir,
      verbose = FALSE
    ) %>%
    min_df
  )
  expect_equal(nrow(x), 15)
  expect_warning(
    y <- predict_runtime(
      plan = config$plan,
      targets = dats,
      envir = config$envir,
      verbose = FALSE,
      digits = Inf
    )
  )
  expect_equal(length(y), 1)
  expect_warning(
    y <- predict_runtime(
      plan = config$plan,
      targets = dats,
      envir = config$envir,
      verbose = FALSE,
      digits = Inf,
      targets_only = TRUE
    )
  )
  expect_equal(length(y), 1)

  config$targets <- dats
  con <- testrun(config)

  expect_silent(
    x <- rate_limiting_times(
      plan = config$plan,
      targets = dats,
      envir = config$envir,
      verbose = FALSE,
      from_scratch = TRUE
    ) %>%
    min_df
  )
  expect_equal(nrow(x), 8)
  expect_warning(
    x <- rate_limiting_times(
      plan = config$plan,
      envir = config$envir,
      verbose = FALSE
    ) %>%
    min_df
  )
  expect_equal(nrow(x), 15)
  expect_warning(
    x <- rate_limiting_times(
      plan = config$plan,
      envir = config$envir,
      verbose = FALSE,
      from_scratch = TRUE
    ) %>%
    min_df
  )
  expect_equal(nrow(x), 17)
  expect_silent(
    y <- predict_runtime(
      plan = config$plan,
      targets = dats,
      envir = config$envir,
      verbose = FALSE,
      from_scratch = TRUE,
      digits = Inf
    )
  )
  expect_equal(length(y), 1)
})

test_with_dir("timing predictions with realistic build", {
  min_df <- function(df){
    df <- df[!grepl("covr", df$item), ]
    df <- df[!grepl(":::", df$item), ]
    df
  }

  eval(parse(text = "require(methods, quietly = TRUE)"))
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs

  load_basic_example(envir = e)
  my_plan <- e$my_plan
  config <- config(my_plan, envir = e, parallelism = "mclapply",
    jobs = 1, verbose = FALSE)
  config <- testrun(config)
  config$envir$reg2 <- function(d){
    d$x3 <- d$x ^ 3
    lm(y ~ x3, data = d)
  }

  # should not really use config after
  # manually setting things out of date
  config_df <- rate_limiting_times(
    plan = config$plan,
    envir = config$envir,
    from_scratch = TRUE,
    config = config,
    digits = 4,
    verbose = FALSE
  ) %>%
  min_df
  scratch_df <- rate_limiting_times(
    plan = config$plan,
    envir = config$envir,
    from_scratch = TRUE,
    digits = Inf,
    verbose = FALSE
  ) %>%
  min_df
  resume_df <- rate_limiting_times(
    plan = config$plan,
    envir = config$envir,
    verbose = FALSE,
    digits = Inf
  ) %>%
  min_df
  resume_df_targets <- rate_limiting_times(
    plan = config$plan,
    envir = config$envir,
    verbose = FALSE,
    digits = Inf,
    targets_only = TRUE
  ) %>%
  min_df
  jobs_4_df <- rate_limiting_times(
    plan = config$plan,
    envir = config$envir,
    from_scratch = TRUE,
    future_jobs = 4,
    jobs = 1,
    verbose = FALSE
  ) %>%
  min_df
  jobs_4_df_targets <- rate_limiting_times(
    plan = config$plan,
    envir = config$envir,
    from_scratch = TRUE,
    future_jobs = 4,
    jobs = 1,
    verbose = FALSE,
    targets_only = TRUE
  ) %>%
  min_df
  jobs_2_df <- rate_limiting_times(
    plan = config$plan,
    envir = config$envir,
    from_scratch = TRUE,
    future_jobs = 2,
    jobs = 1,
    verbose = FALSE
  ) %>%
  min_df

  scratch_time <- predict_runtime(
    plan = config$plan,
    config = config,
    envir = config$envir,
    from_scratch = TRUE,
    digits = Inf
  )
  resume_time <- predict_runtime(
    plan = config$plan,
    envir = config$envir,
    config = config,
    digits = Inf
  )
  resume_time_targets <- predict_runtime(
    plan = config$plan,
    envir = config$envir,
    config = config,
    digits = Inf,
    targets_only = TRUE
  )
  jobs_2_time <- predict_runtime(
    plan = config$plan,
    envir = config$envir,
    config = config,
    from_scratch = TRUE,
    future_jobs = 2,
    jobs = 1,
    digits = Inf
  )
  jobs_4_time <- predict_runtime(
    plan = config$plan,
    envir = config$envir,
    config = config,
    from_scratch = TRUE,
    future_jobs = 4,
    jobs = 1,
    digits = Inf
  )
  jobs_4_time_targets <- predict_runtime(
    plan = config$plan,
    envir = config$envir,
    config = config,
    from_scratch = TRUE,
    future_jobs = 4,
    jobs = 1,
    digits = Inf,
    targets_only = TRUE
  )

  expect_true(all(complete.cases(scratch_df)))
  expect_true(all(complete.cases(resume_df)))
  expect_true(all(complete.cases(resume_df_targets)))
  expect_true(all(complete.cases(jobs_2_df)))
  expect_true(all(complete.cases(jobs_4_df)))
  expect_true(all(complete.cases(jobs_4_df_targets)))

  expect_equal(nrow(scratch_df), 30)
  expect_equal(nrow(resume_df), nrow(scratch_df) - 8)
  expect_equal(nrow(resume_df_targets), nrow(scratch_df) - 23)
  expect_true(nrow(jobs_2_df) < nrow(scratch_df))
  expect_true(nrow(jobs_4_df) < nrow(jobs_2_df))
  expect_true(nrow(jobs_4_df_targets) < nrow(jobs_4_df))
  expect_true(resume_time <= scratch_time)
  expect_true(resume_time_targets <= resume_time)
  expect_true(jobs_2_time <= scratch_time)
  expect_true(jobs_4_time <= jobs_2_time)
  expect_true(jobs_4_time_targets <= jobs_4_time)
})
