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
  parallelism <- "clustermq"
  for (caching in c("master", "worker")) {
    clean(destroy = TRUE)
    config <- drake_config(e$my_plan, envir = e)
    expect_equal(length(outdated(config)), nrow(config$plan))
    make(
      e$my_plan,
      parallelism = parallelism,
      jobs = jobs,
      caching = caching,
      envir = e,
      verbose = 4,
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
      verbose = 4,
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
      verbose = 4,
      lock_envir = TRUE
    )
    expect_equal(justbuilt(config), "small")
  }
  if ("package:clustermq" %in% search()) {
    detach("package:clustermq", unload = TRUE) # nolint
  }
})
