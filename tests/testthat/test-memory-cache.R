drake_context("memory cache")

test_with_dir("storr_environment is usable", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- storr_environment(hash_algorithm = "murmur32")
  expect_false(file.exists(default_cache_path()))
  expect_equal(x$driver$hash_algorithm, "murmur32")
  expect_error(drake_get_session_info(cache = x))
  pln <- drake_plan(y = 1)
  make(pln, cache = x, verbose = 0L, session_info = FALSE)
  config <- drake_config(
    pln, cache = x, verbose = 0L, session_info = FALSE)
  expect_equal(cached(cache = x), "y")
  expect_false(file.exists(default_cache_path()))
  expect_equal(outdated(config), character(0))
  expect_false(file.exists(default_cache_path()))
})

test_with_dir("arbitrary storr in-memory cache", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("lubridate")
  expect_false(file.exists(default_cache_path()))
  parallelism <- "loop"
  jobs <- 1
  envir <- eval(parse(text = get_testing_scenario()$envir))
  cache <- storr::storr_environment(hash_algorithm = "murmur32")
  load_mtcars_example(envir = envir)
  my_plan <- envir$my_plan
  make(
    my_plan,
    envir = envir,
    cache = cache,
    parallelism = parallelism,
    jobs = jobs,
    verbose = 0L
  )
  con <- drake_config(
    my_plan,
    envir = envir,
    cache = cache,
    parallelism = parallelism,
    jobs = jobs,
    verbose = 0L
  )
  envir$reg2 <- function(d) {
    d$x3 <- d$x ^ 3
    lm(y ~ x3, data = d)
  }
  expect_false(file.exists(default_cache_path()))
  expect_equal(con$cache$driver$hash_algorithm, "murmur32")

  expect_equal(cached(verbose = 0L), character(0))
  targets <- con$plan$target
  expect_true(all(targets %in% cached(cache = cache, verbose = 0L)))
  expect_false(file.exists(default_cache_path()))

  expect_error(drake_get_session_info(verbose = 0L))
  expect_true(is.list(drake_get_session_info(cache = cache, verbose = 0L)))
  expect_false(file.exists(default_cache_path()))

  imp <- setdiff(cached(targets_only = FALSE), cached(targets_only = TRUE))
  expect_equal(length(imp), 0)
  imp <- setdiff(cached(cache = cache, targets_only = FALSE),
                 cached(cache = cache, targets_only = TRUE))
  expect_true(length(imp) > 0)
  expect_false(file.exists(default_cache_path()))

  expect_equal(length(cached(verbose = 0L)), 0)
  expect_true(length(cached(cache = cache)) > 0)
  expect_false(file.exists(default_cache_path()))

  expect_equal(nrow(build_times(verbose = 0L)), 0)
  expect_true(nrow(build_times(cache = cache)) > 0)
  expect_false(file.exists(default_cache_path()))

  o1 <- outdated(con)
  expect_equal(length(o1), 7)
  expect_false(file.exists(default_cache_path()))

  p1 <- progress(verbose = 0L)
  unlink(default_cache_path(), recursive = TRUE)
  p2 <- progress(cache = cache, verbose = 0L)
  expect_true(nrow(p2) > nrow(p1))
  expect_false(file.exists(default_cache_path()))

  expect_error(readd(small, verbose = 0L))
  expect_true(is.data.frame(readd(small, cache = cache, verbose = 0L)))
  expect_false(file.exists(default_cache_path()))

  expect_error(loadd(large, verbose = 0L))
  expect_silent(loadd(large, cache = cache, verbose = 0L))
  expect_true(nrow(large) > 0)
  rm(large)
  expect_false(file.exists(default_cache_path()))
})
