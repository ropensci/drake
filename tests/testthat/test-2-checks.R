drake_context("checks")

test_with_dir("top-level checks", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  config <- list()
  expect_error(config_checks(config))
  expect_error(plan_checks(data.frame(x = 1, y = 2)), "columns")
})

test_with_dir("config and make without safety checks", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  x <- drake_plan(
    file = readRDS(file_in("my_file.rds"))
  )
  tmp <- drake_config(x, verbose = 0L)
  expect_silent(
    tmp <- drake_config(x, skip_safety_checks = TRUE, verbose = 0L))
  expect_silent(config_checks(config = tmp))
})

test_with_dir("config_checks() via make()", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  config <- dbug()
  y <- data.frame(x = 1, y = 2)
  suppressWarnings(
    expect_error(
      make(y, envir = config$envir, session_info = FALSE, verbose = 0L)))
  y <- data.frame(target = character(0), command = character(0))
  suppressWarnings(
    expect_error(
      make(y, envir = config$envir,
           session_info = FALSE, verbose = 0L)))
  suppressWarnings(expect_error(
    make(
      config$plan,
      targets = character(0),
      envir = config$envir,
      session_info = FALSE,
      verbose = 0L
    ),
    regexp = "valid targets"
  ))
})

test_with_dir(".onLoad() warns correctly and .onAttach() works", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("tibble")
  f <- ".RData"
  expect_false(file.exists(f))
  expect_silent(drake:::.onLoad())
  save.image()
  expect_true(file.exists(f))
  expect_warning(drake:::.onLoad())
  unlink(f, force = TRUE)
  set.seed(0)
  expect_silent(suppressPackageStartupMessages(drake:::.onAttach()))
})

test_with_dir("warnings and messages are caught", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("tibble")
  expect_equal(nrow(drake_progress()), 0)
  f <- function(x) {
    warning("my first warn")
    message("my first mess")
    warning("my second warn")
    message("my second mess")
    123
  }
  bad_plan <- drake_plan(x = f(), y = x)
  expect_warning(make(bad_plan, verbose = 1L, session_info = FALSE))
  x <- diagnose(x)
  expect_true(grepl("my first warn", x$warnings[1], fixed = TRUE))
  expect_true(grepl("my second warn", x$warnings[2], fixed = TRUE))
  expect_true(grepl("my first mess", x$messages[1], fixed = TRUE))
  expect_true(grepl("my second mess", x$messages[2], fixed = TRUE))
})
