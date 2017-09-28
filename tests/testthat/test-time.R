cat(get_testing_scenario_name(), ": ", sep = "")
context("time")

test_with_dir("build times works if no targets are built", {
  expect_equal(nrow(build_times(search = FALSE)), 0)
  my_plan <- plan(x = 1)
  make(my_plan, verbose = FALSE, imports_only = TRUE)
  expect_equal(nrow(build_times(search = FALSE)), 0)
})

test_with_dir("time predictions: incomplete targets", {
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
    )
  )
  expect_equal(nrow(x), 0)
  expect_warning(
    y <- predict_runtime(
      plan = config$plan,
      targets = dats,
      envir = config$envir,
      verbose = FALSE,
      digits = Inf
    )
  )
  expect_equal(lubridate::dseconds(0), y)
  expect_warning(
    x <- rate_limiting_times(
      plan = config$plan,
      envir = config$envir,
      verbose = FALSE
    )
  )
  expect_equal(nrow(x), 0)

  config$targets <- dats
  con <- testrun(config)

  expect_silent(
    x <- rate_limiting_times(
      plan = config$plan,
      targets = dats,
      envir = config$envir,
      verbose = FALSE,
      from_scratch = TRUE
    )
  )
  expect_equal(nrow(x), 2)
  expect_warning(
    x <- rate_limiting_times(
      plan = config$plan,
      envir = config$envir,
      verbose = FALSE
    )
  )
  expect_equal(nrow(x), 0)
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
  expect_true(y > lubridate::as.duration(0))
})

test_with_dir("timing predictions with realistic build", {
  eval(parse(text = "require(methods, quietly = TRUE)"))
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs

  load_basic_example(envir = e)
  my_plan <- e$my_plan
  config <- config(my_plan, envir = e,
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
  )
  scratch_df <- rate_limiting_times(
    plan = config$plan,
    envir = config$envir,
    from_scratch = TRUE,
    digits = 4,
    verbose = FALSE
  )
  resume_df <- rate_limiting_times(
    plan = config$plan,
    envir = config$envir,
    verbose = FALSE
  )
  jobs_4_df <- rate_limiting_times(
    plan = config$plan,
    envir = config$envir,
    from_scratch = TRUE,
    jobs = 4,
    verbose = FALSE
  )
  jobs_2_df <- rate_limiting_times(
    plan = config$plan,
    envir = config$envir,
    from_scratch = TRUE,
    jobs = 2,
    verbose = FALSE
  )

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
  jobs_4_time <- predict_runtime(
    plan = config$plan,
    envir = config$envir,
    config = config,
    from_scratch = TRUE,
    jobs = 4,
    digits = Inf
  )
  jobs_2_time <- predict_runtime(
    plan = config$plan,
    envir = config$envir,
    config = config,
    from_scratch = TRUE,
    jobs = 2,
    digits = Inf
  )

  expect_equal(nrow(scratch_df), nrow(config$plan))
  expect_equal(nrow(resume_df), 8)
  expect_equal(nrow(jobs_4_df), 6)
  expect_equal(nrow(jobs_2_df), 9)
  expect_true(resume_time < scratch_time)
  expect_true(jobs_4_time < jobs_2_time)
  expect_true(jobs_2_time < scratch_time)
})
