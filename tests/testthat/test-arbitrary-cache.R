context("arbitrary cache")

test_with_dir("arbitrary storr file cache", {
  expect_false(file.exists(default_cache_path()))
  parallelism <- default_parallelism()
  jobs <- 2
  envir <- eval(parse(text = test_opt()$envir))
  cache <- storr::storr_rds("arbitrary_storr", mangle_key = TRUE)
  load_basic_example(envir = envir)
  my_plan <- envir$my_plan
  con <- make(
    my_plan,
    envir = envir,
    cache = cache,
    parallelism = parallelism,
    jobs = jobs,
    verbose = FALSE,
    return_config = TRUE
  )
  envir$reg2 <- function(d){
    d$x3 <- d$x ^ 3
    lm(y ~ x3, data = d)
  }
  expect_false(file.exists(default_cache_path()))

  expect_equal(cached(), character(0))
  targets <- my_plan$target
  expect_true(all(targets %in% cached(cache = cache)))
  expect_false(file.exists(default_cache_path()))

  expect_error(session())
  expect_true(is.list(session(cache = cache)))
  expect_false(file.exists(default_cache_path()))

  expect_equal(length(imported()), 0)
  expect_true(length(imported(cache = cache)) > 0)
  expect_false(file.exists(default_cache_path()))

  expect_equal(length(built()), 0)
  expect_true(length(built(cache = cache)) > 0)
  expect_false(file.exists(default_cache_path()))

  expect_equal(nrow(build_times()), 0)
  expect_true(nrow(build_times(cache = cache)) > 0)
  expect_false(file.exists(default_cache_path()))

  o1 <- outdated(my_plan, envir = envir, verbose = FALSE)
  unlink(default_cache_path(), recursive = TRUE)
  o2 <- outdated(my_plan, jobs = 2, cache = cache,
    envir = envir, verbose = FALSE)
  expect_true(length(o1) > length(o2))
  expect_false(file.exists(default_cache_path()))

  p <- plot_graph(my_plan, envir = envir,
    cache = cache, verbose = FALSE, file = "graph.html")
  expect_false(file.exists(default_cache_path()))

  m1 <- max_useful_jobs(my_plan, envir = envir, verbose = F)
  unlink(default_cache_path(), recursive = TRUE)
  m2 <- max_useful_jobs(my_plan, envir = envir, verbose = F, cache = cache)
  expect_equal(m1, 8)
  expect_equal(m2, 4)
  expect_false(file.exists(default_cache_path()))

  p1 <- progress()
  unlink(default_cache_path(), recursive = TRUE)
  p2 <- progress(cache = cache)
  expect_true(length(p2) > length(p1))
  expect_false(file.exists(default_cache_path()))

  expect_error(read_config())
  expect_true(is.list(read_config(cache = cache)))
  expect_false(file.exists(default_cache_path()))

  expect_error(read_graph())
  expect_equal(class(read_graph(cache = cache)), "igraph")
  expect_false(file.exists(default_cache_path()))

  expect_error(read_plan())
  expect_true(is.data.frame(read_plan(cache = cache)))
  expect_false(file.exists(default_cache_path()))

  expect_error(readd(small))
  expect_true(is.data.frame(readd(small, cache = cache)))
  expect_false(file.exists(default_cache_path()))

  expect_error(loadd(large))
  expect_error(nrow(large))
  expect_silent(loadd(large, cache = cache))
  expect_true(nrow(large) > 0)
  expect_false(file.exists(default_cache_path()))
})
