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
  final0 <- readd(final, search = FALSE)

  # actually change file
  saveRDS(2:10, "input.rds")
  testrun(config)
  expect_equal(justbuilt(config), sort(c(
    "drake_target_1", "combined", "final", "myinput", "nextone")))
  expect_false(length(final0) == length(readd(final, search = FALSE)))
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
  testrun(config)
  final0 <- readd(final)

  # add another file to the directory
  saveRDS(2:10, "inputdir/otherinput.rds")
  testrun(config)
  expect_equal(justbuilt(config), "myinput")
  expect_equal(length(final0), length(readd(final, search = FALSE)))

  # change the real input file
  saveRDS(2:10, "inputdir/input.rds")
  testrun(config)
  expect_equal(justbuilt(config), sort(c(
    "drake_target_1", "combined", "final", "myinput", "nextone")))
  expect_false(length(final0) == length(readd(final, search = FALSE)))

})
