context("storr_environment")

test_with_dir("basic example with environment storr", {
  opt <- test_opt()
  e <- eval(parse(text = opt$envir))
  jobs <- opt$jobs
  parallelism <- default_parallelism()

  load_basic_example(envir = e)
  my_plan <- e$my_plan
  cache_envir <- new.env(parent = globalenv())
  cache <- storr::storr_environment(envir = cache_envir)
  config <- config(my_plan, envir = e,
    jobs = jobs, parallelism = parallelism,
    verbose = FALSE, cache = cache)
  
  make(
    my_plan,
    envir = config$envir,
    cache = cache,
    parallelism = parallelism,
    jobs = jobs
  )

  expect_false(file.exists(default_cache_path()))
  o1 <- outdated(my_plan, envir = e)
  o2 <- outdated(my_plan, envir = e, cache = cache)

  e$reg2 <- function(d) {
    d$x3 <- d$x ^ 3
    lm(y ~ x3, data = d)
  }
  o1 <- outdated(my_plan, envir = e)
  o2 <- outdated(my_plan, envir = e, cache = cache)

  con2 <- testrun(config)
  o1 <- outdated(my_plan, envir = e)
  o2 <- outdated(my_plan, envir = e, cache = cache)
  expect_false(file.exists(default_cache_path()))
})
