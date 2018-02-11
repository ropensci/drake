drake_context("lazy load")

test_with_dir("no overt errors lazy load for the debug example", {
  config <- dbug()
  config$verbose <- FALSE
  config$lazy_load <- TRUE
  if (config$parallelism == "parLapply"){
    config$jobs <- 1
  }
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
  lazily_loaded <- c("nextone", "yourinput")
  eagerly_loaded <- "combined"
  config <- dbug()
  unload_these <- c(lazily_loaded, eagerly_loaded) %>%
    intersect(y = ls(envir = config$envir))
  remove(list = unload_these, envir = config$envir)
  config <- make(
    lazy_load = TRUE,
    plan = config$plan,
    targets = "combined",
    envir = config$envir,
    verbose = FALSE,
    session_info = FALSE
  )
  loaded <- ls(envir = config$envir)
  expect_true(all(lazily_loaded %in% loaded))
  expect_false(any(eagerly_loaded %in% loaded))
  clean()
  config <- make(
    lazy_load = FALSE,
    plan = config$plan,
    targets = "combined",
    envir = config$envir,
    verbose = FALSE,
    session_info = FALSE
  )
  loaded <- ls(envir = config$envir)
  expect_true(all(lazily_loaded %in% loaded))
  expect_true(all(eagerly_loaded %in% loaded))
})

test_with_dir("active bindings", {
  config <- dbug()
  testrun(config)

  rm(final, envir = config$envir)
  expect_false("final" %in% ls(config$envir))
  loadd(final, envir = config$envir, lazy = "bind", cache = config$cache)

  # `final` should be loaded when it is referenced.
  tmp <- config$envir$final
  expect_true(is.numeric(tmp))
  expect_equal(config$envir$final, readd(final, cache = config$cache))

  # Allow active bindings to overwrite existing variables.
  expect_message(
    loadd(
      final, envir = config$envir, lazy = "bind",
      verbose = FALSE, cache = config$cache),
    regexp = "active binding"
  )

  # Active bindings react to make()
  old_final <- config$envir$final
  config$plan$command[6] <- paste0(sum(old_final), "+ 1")
  testrun(config)
  expect_false(identical(config$envir$final, old_final))

  expect_false("nextone" %in% ls(config$envir))
  loadd(envir = config$envir, lazy = "bind", cache = config$cache)
  tmp <- config$envir$nextone
  expect_true("nextone" %in% ls(config$envir))
  expect_equal(config$envir$nextone, readd(nextone, cache = config$cache))
})
