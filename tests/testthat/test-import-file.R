drake_context("import file")

test_with_dir("responses to imported file", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  config <- dbug()
  expect_silent(check_plan(plan = config$plan, envir = config$envir))
  expect_warning(
    check_plan(plan = config$plan[-1, ], envir = config$envir,
          verbose = FALSE))
  expect_silent(
    check_plan(plan = config$plan[c(-1, -6), ], envir = config$envir,
          verbose = FALSE))
  testrun(config)
  expect_true(length(justbuilt(config)) > 0)
  testrun(config)
  nobuild(config)

  # check missing and then replace file exactly as before
  contents <- readRDS("input.rds")
  unlink("input.rds", force = TRUE)
  expect_warning(tmp <- capture.output(check_plan(plan = config$plan,
    envir = config$envir)))
  saveRDS(contents, "input.rds")
  testrun(config)
  nobuild(config)
  final0 <- readd(final, search = FALSE)

  # actually change file
  saveRDS(2:10, "input.rds")
  testrun(config)
  expect_equal(justbuilt(config), sort(c("\"intermediatefile.rds\"",
    "combined", "final", "myinput", "nextone")))
  expect_false(length(final0) == length(readd(final, search = FALSE)))
})
