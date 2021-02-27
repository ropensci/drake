drake_context("prework")

test_with_dir("packages are loaded and prework is run", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("abind")
  on.exit(options(test_drake_option_12345 = NULL))

  options(test_drake_option_12345 = "unset")
  expect_equal(getOption("test_drake_option_12345"), "unset")
  config <- dbug()
  try(detach("package:abind", unload = TRUE), silent = TRUE) # nolint
  expect_error(abind(1))

  # Load packages with the 'packages' argument
  config$packages <- "abind"
  config$prework <- "options(test_drake_option_12345 = 'set')"
  config$plan <- drake_plan(
    x = getOption("test_drake_option_12345"),
    y = c(deparse(body(abind)), x)
  )
  config$targets <- config$plan$target
  expect_false(any(c("x", "y") %in% config$cache$list()))
  testrun(config)
  expect_true(all(c("x", "y") %in% config$cache$list()))
  expect_equal(readd(x), "set")
  expect_true(length(readd(y)) > 0)
  clean()

  # load packages the usual way
  options(test_drake_option_12345 = "unset")
  expect_equal(getOption("test_drake_option_12345"), "unset")
  try(detach("package:abind", unload = TRUE), silent = TRUE) # nolint
  expect_error(abind(1))
  library(abind) # nolint
  config$packages <- NULL
  expect_false(any(c("x", "y") %in% config$cache$list()))

  # drake may be loaded with devtools::load_all() but not
  # installed.
  scenario <- get_testing_scenario()
  suppressWarnings(
    make(
      plan = config$plan,
      targets = config$targets,
      envir = config$envir,
      verbose = 0L,
      parallelism = scenario$parallelism,
      jobs = scenario$jobs,
      prework = config$prework,
      command = config$command,
      session_info = FALSE
    )
  )
  expect_true(all(c("x", "y") %in% config$cache$list()))
  expect_equal(readd(x), "set")
  expect_true(length(readd(y)) > 0)
})

test_with_dir("prework can be an expression", {
  skip_on_cran()
  on.exit(options(test_drake_option_12345 = NULL))
  options(test_drake_option_12345 = "unset")
  expect_equal(getOption("test_drake_option_12345"), "unset")
  config <- dbug()
  config$plan <- drake_plan(x = getOption("test_drake_option_12345"))
  config$targets <- config$plan$target
  config$prework <- quote(options(test_drake_option_12345 = "set"))
  testrun(config)
  expect_equal(readd(x), "set")
})

test_with_dir("prework can be an expression", {
  skip_on_cran()
  on.exit(
    options(test_drake_option_12345 = NULL, test_drake_option_6789 = NULL)
  )
  options(test_drake_option_12345 = "unset", test_drake_option_6789 = "unset")
  expect_equal(getOption("test_drake_option_12345"), "unset")
  expect_equal(getOption("test_drake_option_6789"), "unset")
  config <- dbug()
  config$plan <- drake_plan(
    x = getOption("test_drake_option_12345"),
    y = getOption("test_drake_option_6789")
  )
  config$targets <- config$plan$target
  config$prework <- list(
    quote(options(test_drake_option_12345 = "set")),
    quote(options(test_drake_option_6789 = "set"))
  )
  testrun(config)
  expect_equal(readd(x), "set")
  expect_equal(readd(y), "set")
})

test_with_dir("prework must be interpretable as a language object", {
  skip_on_cran()
  plan <- drake_plan(x = 1)
  f <- function(x) {
    x + 1
  }
  expect_error(
    make(plan, prework = f),
    regexp = "prework must be an expression"
  )
})
