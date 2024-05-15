drake_context("drake_build")

test_with_dir("drake_build() works as expected", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  pl <- drake_plan(a = 1, b = a)
  con <- drake_config(
    plan = pl,
    session_info = FALSE,
    envir = e
  )
  # can run before any make()
  o <- drake_build_impl(
    target = "a",
    character_only = TRUE,
    config = con
  )
  x <- cached()
  expect_equal(x, "a")
  make(pl, envir = e)
  o <- drake_config(pl, envir = e)
  expect_equal(justbuilt(o), "b")

  # Replacing deps in environment
  con$envir_targets$a <- 2
  o <- drake_build_impl(b, config = con)
  expect_equal(o, 2)
  expect_equal(con$envir_targets$a, 2)
  expect_equal(readd(a), 1)
  o <- drake_build_impl(b, config = con, replace = FALSE)
  expect_equal(con$envir_targets$a, 2)
  expect_equal(readd(a), 1)
  con$envir_targets$a <- 3
  o <- drake_build_impl(b, config = con, replace = TRUE)
  expect_equal(con$envir_targets$a, 1)
  expect_equal(o, 1)

  # `replace` in loadd()
  e$b <- 1
  expect_equal(e$b, 1)
  e$b <- 5
  loadd(b, envir = e, replace = FALSE)
  expect_equal(e$b, 5)
  loadd(b, envir = e, replace = TRUE)
  expect_equal(e$b, 1)
  e$b <- 5
  loadd(b, envir = e)
  expect_equal(e$b, 1)
})

test_with_dir("debug_command()", {
  skip_on_cran()
  txt <- "    f(x + 2) + 2"
  txt2 <- "drake::debug_and_run(function() {\n    f(x + 2) + 2\n})"
  x <- parse(text = txt)[[1]]
  out1 <- debug_command(x)
  out2 <- debug_command(txt)
  txt3 <- safe_deparse(out1)
  expect_equal(out2, txt2)
  expect_equal(out2, txt3)
})
