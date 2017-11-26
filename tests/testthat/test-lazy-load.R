drake_context("lazy load")

test_with_dir("no overt errors lazy load for the debug example", {
  config <- dbug()
  config$verbose <- TRUE
  config$lazy_load <- TRUE
  expect_equal(sort(outdated(config)), sort(config$plan$target))

  config <- testrun(config)
  expect_equal(sort(justbuilt(config)), sort(config$plan$target))
  expect_equal(outdated(config), character(0))

  unload_these <- intersect(config$plan$target, ls(envir = config$envir))
  remove(list = unload_these, envir = config$envir)

  config <- testrun(config)
  expect_equal(sort(justbuilt(config)), character(0))
  expect_equal(outdated(config), character(0))

  unload_these <- intersect(config$plan$target, ls(envir = config$envir))
  remove(list = unload_these, envir = config$envir)

  config$plan$command[config$plan$target == "combined"] <-
    "nextone + yourinput + 1"
  config <- testrun(config)
  expect_equal(sort(justbuilt(config)), sort(c(
    "combined", "final", "'intermediatefile.rds'"
  )))
  expect_equal(outdated(config), character(0))
})

test_with_dir("lazy loading is actually lazy", {
  config <- dbug()
  config <- make(
    lazy_load = TRUE,
    plan = config$plan,
    parallelism = config$parallelism,
    jobs = config$jobs,
    targets = "combined",
    envir = config$envir,
    verbose = TRUE
  )
  loaded <- c(
    "a", "b", "c", "f", "g", "h", "i", "j", "nextone", "yourinput"
  )
  expect_equal(sort(loaded), sort(ls(envir = config$envir)))
  clean()
  config <- make(
    lazy_load = FALSE,
    plan = config$plan,
    parallelism = config$parallelism,
    jobs = config$jobs,
    targets = "combined",
    envir = config$envir,
    verbose = TRUE
  )
  loaded <- c(loaded, "combined", "nextone") %>%
    setdiff(y = "myinput")
  expect_equal(sort(loaded), sort(ls(envir = config$envir)))
})
