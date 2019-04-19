drake_context("clustermq")

test_with_dir("clustermq parallelism", {
  skip_on_cran()
  if ("package:clustermq" %in% search()) {
    detach("package:clustermq", unload = TRUE) # nolint
  }
  options(clustermq.scheduler = "multicore")
  skip_if_not_installed("clustermq")
  skip_on_os("windows")
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs # ignoring for now, using 2 jobs
  load_mtcars_example(envir = e)
  e$my_plan$hpc <- e$my_plan$target != "regression1_large"
  parallelism <- "clustermq"
  for (caching in c("master", "worker")) {
    clean(destroy = TRUE)
    config <- drake_config(e$my_plan, envir = e)
    expect_equal(length(outdated(config)), nrow(e$my_plan))
    make(
      e$my_plan,
      parallelism = parallelism,
      jobs = jobs,
      caching = caching,
      envir = e,
      verbose = 1L,
      garbage_collection = TRUE,
      lock_envir = TRUE
    )
    expect_equal(outdated(config), character(0))
    make(
      e$my_plan,
      parallelism = parallelism,
      jobs = jobs,
      caching = caching,
      envir = e,
      verbose = 1L,
      lock_envir = TRUE
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
      verbose = 1L,
      lock_envir = TRUE
    )
    expect_equal(justbuilt(config), "small")
    clean(small, cache = config$cache)
    make(
      e$my_plan,
      parallelism = parallelism,
      jobs = jobs,
      caching = caching,
      envir = e,
      verbose = 1L,
      lock_envir = TRUE
    )
    expect_equal(justbuilt(config), "small")
  }
  if ("package:clustermq" %in% search()) {
    detach("package:clustermq", unload = TRUE) # nolint
  }
})

test_with_dir("No hpc targets? No workers.", {
  plan <- drake_plan(x = target(1, hpc = FALSE), y = target(x, hpc = FALSE))
  drake:::with_options(
    list(clustermq.scheduler = "does_not_exist"),
    make(
      plan,
      parallelism = "clustermq",
      jobs = 2,
      session_info = FALSE,
      cache = storr::storr_environment()
    )
  )
})

test_with_dir("Start off with non-HPC targets, then go to HPC targets.", {
  skip_on_cran()
  if ("package:clustermq" %in% search()) {
    detach("package:clustermq", unload = TRUE) # nolint
  }
  options(clustermq.scheduler = "multicore")
  skip_if_not_installed("clustermq")
  skip_on_os("windows")
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs # ignoring for now, using 2 jobs
  load_mtcars_example(envir = e)
  e$my_plan$hpc <- !(e$my_plan$target %in% c("small", "large"))
  make(
    e$my_plan,
    parallelism = "clustermq",
    jobs = jobs,
    envir = e,
    verbose = 1L,
    garbage_collection = TRUE,
    lock_envir = TRUE
  )
  config <- drake_config(e$my_plan, envir = e)
  expect_equal(sort(justbuilt(config)), sort(e$my_plan$target))
  expect_equal(outdated(config), character(0))
  if ("package:clustermq" %in% search()) {
    detach("package:clustermq", unload = TRUE) # nolint
  }
})
