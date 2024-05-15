drake_context("clustermq")

test_with_dir("clustermq parallelism for CRAN", {
  skip_on_cran()
  skip_if_not_installed("clustermq", minimum_version = "0.9.1")
  skip_on_os("windows")
  options(clustermq.scheduler = "multicore")
  plan <- drake_plan(x = {
    # Test relaying.
    message("message")
    warning("warning")
  })
  for (caching in c("main", "worker")) {
    clean()
    suppressWarnings(make(plan, parallelism = "clustermq", caching = caching))
    config <- drake_config(plan)
    expect_equal(justbuilt(config), "x")
  }
  if ("package:clustermq" %in% search()) {
    detach("package:clustermq", unload = TRUE) # nolint
  }
})

test_with_dir("clustermq parallelism", {
  skip_on_cran()
  skip_if_not_installed("clustermq", minimum_version = "0.9.1")
  skip_on_os("windows")
  if ("package:clustermq" %in% search()) {
    detach("package:clustermq", unload = TRUE) # nolint
  }
  options(clustermq.scheduler = "multicore")
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs # ignoring for now, using 2 jobs
  load_mtcars_example(envir = e)
  e$my_plan$hpc <- e$my_plan$target != "regression1_large"
  parallelism <- "clustermq"
  for (caching in c("main", "worker")) {
    clean(destroy = TRUE)
    config <- drake_config(e$my_plan, envir = e)
    expect_equal(length(outdated_impl(config)), nrow(e$my_plan))
    make(
      e$my_plan,
      parallelism = parallelism,
      jobs = jobs,
      caching = caching,
      envir = e,
      verbose = 1L,
      garbage_collection = TRUE
    )
    expect_equal(outdated_impl(config), character(0))
    make(
      e$my_plan,
      parallelism = parallelism,
      jobs = jobs,
      caching = caching,
      envir = e,
      verbose = 1L
    )
    expect_equal(justbuilt(config), character(0))
    e$my_plan$command[[2]] <- as.call(
      c(quote(identity), unname(e$my_plan$command[2]))
    )
    make(
      e$my_plan,
      parallelism = parallelism,
      jobs = jobs,
      caching = caching,
      envir = e,
      verbose = 1L
    )
    expect_equal(justbuilt(config), "small")
    clean(small, cache = config$cache)
    make(
      e$my_plan,
      parallelism = parallelism,
      jobs = jobs,
      caching = caching,
      envir = e,
      verbose = 1L
    )
    expect_equal(justbuilt(config), "small")
  }
  if ("package:clustermq" %in% search()) {
    detach("package:clustermq", unload = TRUE) # nolint
  }
})

test_with_dir("No hpc targets? No workers.", {
  skip_on_cran()
  skip_if_not_installed("clustermq", minimum_version = "0.9.1")
  skip_on_os("windows")
  if ("package:clustermq" %in% search()) {
    detach("package:clustermq", unload = TRUE) # nolint
  }
  options(clustermq.scheduler = "multicore")
  plan <- drake_plan(x = target(1L, hpc = FALSE), y = target(x, hpc = FALSE))
  cache <- storr::storr_environment()
  drake:::with_options(
    list(clustermq.scheduler = "does_not_exist"),
    make(
      plan,
      parallelism = "clustermq",
      jobs = 2,
      session_info = FALSE,
      cache = cache
    )
  )
  expect_equal(readd(x, cache = cache), 1L)
  if ("package:clustermq" %in% search()) {
    detach("package:clustermq", unload = TRUE) # nolint
  }
})

test_with_dir("All hpc targets up to date? No workers.", {
  skip_on_cran()
  skip_if_not_installed("clustermq", minimum_version = "0.9.1")
  skip_on_os("windows")
  if ("package:clustermq" %in% search()) {
    detach("package:clustermq", unload = TRUE) # nolint
  }
  options(clustermq.scheduler = "multicore")
  plan <- drake_plan(x = target(1L, hpc = FALSE), y = target(x, hpc = TRUE))
  cache <- storr::storr_environment()
  make(plan, session_info = FALSE, cache = cache)
  config <- drake_config(plan, cache = cache)
  expect_equal(sort(justbuilt(config)), c("x", "y"))
  expect_equal(outdated_impl(config), character(0))
  plan <- drake_plan(
    x = target(2L - 1L, hpc = FALSE),
    y = target(x, hpc = TRUE)
  )
  config <- drake_config(plan, cache = cache)
  expect_equal(sort(outdated_impl(config)), c("x", "y"))
  drake:::with_options(
    list(clustermq.scheduler = "does_not_exist"),
    make(
      plan,
      parallelism = "clustermq",
      jobs = 2,
      session_info = FALSE,
      cache = cache
    )
  )
  expect_equal(justbuilt(config), "x")
  if ("package:clustermq" %in% search()) {
    detach("package:clustermq", unload = TRUE) # nolint
  }
})

test_with_dir("Start off with non-HPC targets, then go to HPC targets.", {
  skip_on_cran()
  skip_if_not_installed("clustermq", minimum_version = "0.9.1")
  skip_on_os("windows")
  if ("package:clustermq" %in% search()) {
    detach("package:clustermq", unload = TRUE) # nolint
  }
  options(clustermq.scheduler = "multicore")
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs # ignoring for now, using 2 jobs
  load_mtcars_example(envir = e)
  e$my_plan$hpc <- !(e$my_plan$target %in% c("small", "large"))
  # Run interactively with 1 job to verify that clustermq workers
  # only get submitted after targets `large` and `small` are built.
  make(
    e$my_plan,
    parallelism = "clustermq",
    jobs = jobs,
    envir = e,
    verbose = 1L,
    garbage_collection = TRUE
  )
  config <- drake_config(e$my_plan, envir = e)
  expect_equal(sort(justbuilt(config)), sort(e$my_plan$target))
  expect_equal(outdated_impl(config), character(0))
  if ("package:clustermq" %in% search()) {
    detach("package:clustermq", unload = TRUE) # nolint
  }
})

test_with_dir("clustermq warnings (worker caching)", {
  skip_on_cran()
  build <- list(target = "x", warnings = "y")
  expect_warning(cmq_report_warnings(build))
})
