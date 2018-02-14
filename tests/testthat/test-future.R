drake_context("future")

test_with_dir("future package functionality", {
  future::plan(future::sequential)
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  load_basic_example(envir = e)
  backends <- c("future_lapply", rep("future", 2))
  caching <- c(rep("worker", 2), "master")
  for (i in 1:3){
    clean(destroy = TRUE)
    withr::with_options(
      new = list(mc.cores = 2), code = {
        config <- make(
          e$my_plan,
          envir = e,
          parallelism = backends[i],
          caching = caching[i],
          jobs = 1,
          verbose = FALSE,
          session_info = FALSE
        )
      }
    )
    expect_equal(
      outdated(config),
      character(0)
    )
  }
})

test_with_dir("prepare_distributed() writes cache folder if nonexistent", {
  config <- dbug()
  config$cache_path <- "nope"
  prepare_distributed(config)
  expect_true(file.exists("nope"))
})
