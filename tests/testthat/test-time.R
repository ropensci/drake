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
