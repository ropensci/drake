drake_context("import file")

test_with_dir("responses to imported file", {
  config <- dbug()
  con2 <- drake_config(plan = config$plan[-1, ], envir = config$envir)
  expect_warning(runtime_checks(con2))
  testrun(config)
  expect_true(length(justbuilt(config)) > 0)
  testrun(config)
  nobuild(config)

  # check missing and then replace file exactly as before
  contents <- readRDS("input.rds")
  unlink("input.rds", force = TRUE)
  con3 <- drake_config(plan = config$plan, envir = config$envir)
  expect_warning(tmp <- runtime_checks(con3))
  saveRDS(contents, "input.rds")
  testrun(config)
  nobuild(config)
  final0 <- readd(final)

  # actually change file
  saveRDS(2:10, "input.rds")
  testrun(config)
  expect_equal(justbuilt(config), sort(c(
    "drake_target_1", "combined", "final", "myinput", "nextone")))
  expect_false(length(final0) == length(readd(final)))
})

test_with_dir("same with an imported directory", {
  scenario <- get_testing_scenario()
  envir <- eval(parse(text = scenario$envir))
  envir <- dbug_envir(envir)
  dbug_files()
  dir.create("inputdir")
  tmp <- file.copy("input.rds", "inputdir/input.rds")
  tmp <- file.remove("input.rds")
  plan <- dbug_plan()
  plan$command[plan$target == "myinput"][[1]] <- quote(
    readRDS(file.path(file_in("inputdir"), "input.rds"))
  )
  config <- drake_config(
    plan = plan,
    targets = plan$target,
    envir = envir,
    parallelism = scenario$parallelism,
    jobs = scenario$jobs,
    verbose = 0L,
    session_info = FALSE,
    log_progress = TRUE,
    caching = scenario$caching
  )
  config$plan <- plan
  testrun(config)
  final0 <- readd(final)

  # add another file to the directory
  saveRDS(2:10, "inputdir/otherinput.rds")
  testrun(config)
  expect_equal(justbuilt(config), "myinput")
  expect_equal(length(final0), length(readd(final)))

  # change the real input file
  saveRDS(2:10, "inputdir/input.rds")
  testrun(config)
  expect_equal(justbuilt(config), sort(c(
    "drake_target_1", "combined", "final", "myinput", "nextone")))
  expect_false(length(final0) == length(readd(final)))

})

test_with_dir("drake_config() memoizes against knitr files (#887)", {
  # Setup
  plan <- drake_plan(
    a = TRUE,
    b = TRUE,
    report_step = knitr_in("report1.Rmd", "report2.Rmd")
  )
  lines_a <- c(
    "---",
    "title: abc",
    "---",
    "",
    "```{r}",
    "readd(a)",
    "```"
  )
  lines_b <- c(
    "---",
    "title: abc",
    "---",
    "",
    "```{r}",
    "readd(b)",
    "```"
  )
  writeLines(lines_a, "report1.Rmd")
  writeLines(lines_a, "report2.Rmd")
  envir <- new.env(parent = globalenv())
  cache <- storr::storr_environment()
  for (i in 1:2) {
    config <- drake_config(
      plan,
      envir = envir,
      cache = cache,
      session_info = FALSE
    )
  }

  # Now switch `a` to `b` in the report.
  writeLines(lines_b, "report2.Rmd")
  config <- drake_config(
    plan,
    envir = envir,
    cache = cache,
    session_info = FALSE
  )
  deps <- deps_target(report_step, config)
  expect_true("a" %in% deps$name)
  expect_true("b" %in% deps$name)

  # make() first so file times and hashes are in the cache.
  make(config = config)
  writeLines(lines_b, "report1.Rmd")
  config <- drake_config(
    plan,
    envir = envir,
    cache = cache,
    session_info = FALSE
  )
  deps <- deps_target(report_step, config)
  expect_false("a" %in% deps$name)
  expect_true("b" %in% deps$name)

  # check if things work if a knitr file is missing.
  unlink("report1.Rmd")
  expect_warning(
    config <- drake_config(
      plan,
      envir = envir,
      cache = cache,
      session_info = FALSE
    ),
    regexp = "does not exist"
  )
})
