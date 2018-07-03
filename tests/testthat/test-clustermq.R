drake_context("clustermq")

test_with_dir("clustermq_staged parallelism", {
  skip_on_cran()
  if ("package:clustermq" %in% search()){
    detach("package:clustermq", unload = TRUE)
  }
  options(clustermq.scheduler = "multicore")
  skip_if_not_installed("clustermq")
  skip_on_os("windows")
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs
  load_mtcars_example(envir = e)
  make(
    e$my_plan,
    parallelism = "clustermq_staged",
    jobs = jobs,
    envir = e,
    verbose = 4
  )
  config <- drake_config(e$my_plan, envir = e)
  expect_equal(outdated(config), character(0))
  make(
    e$my_plan,
    parallelism = "clustermq_staged",
    jobs = jobs,
    envir = e,
    verbose = 4
  )
  expect_equal(justbuilt(config), character(0))
})
