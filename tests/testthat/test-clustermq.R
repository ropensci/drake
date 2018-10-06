drake_context("clustermq")

test_with_dir("clustermq parallelism", {
  skip_on_cran()
  if ("package:clustermq" %in% search()){
    eval(parse(text = "detach('package:clustermq', unload = TRUE)"))
  }
  options(clustermq.scheduler = "multicore")
  skip_if_not_installed("clustermq")
  skip_on_os("windows")
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs # ignoring for now, using 2 jobs
  load_mtcars_example(envir = e)
  for (parallelism in c("clustermq", "clustermq_staged")){
    for (caching in c("master", "worker")){
      config <- drake_config(e$my_plan, envir = e)
      expect_equal(length(outdated(config)), nrow(config$plan))
      make(
        e$my_plan,
        parallelism = parallelism,
        jobs = jobs,
        caching = caching,
        envir = e,
        verbose = 4,
        garbage_collection = TRUE
      )
      expect_equal(outdated(config), character(0))
      make(
        e$my_plan,
        parallelism = parallelism,
        jobs = jobs,
        caching = caching,
        envir = e,
        verbose = 4
      )
      expect_equal(justbuilt(config), character(0))
      clean(destroy = TRUE)
    }
  }
  if ("package:clustermq" %in% search()){
    eval(parse(text = "detach('package:clustermq', unload = TRUE)"))
  }
})
