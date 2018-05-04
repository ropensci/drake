drake_context("memory cache")

test_with_dir("storr_environment is usable", {
  x <- storr_environment(hash_algorithm = "murmur32") %>%
    configure_cache(
      long_hash_algo = "sha1",
      overwrite_hash_algos = TRUE
    )
  expect_false(file.exists(default_cache_path()))
  expect_equal(short_hash(x), "murmur32")
  expect_equal(long_hash(x), "sha1")
  expect_error(drake_session(cache = x))
  pln <- drake_plan(y = 1)
  config <- make(pln, cache = x, verbose = FALSE, session_info = FALSE)
  expect_equal(cached(cache = x), "y")
  expect_false(file.exists(default_cache_path()))
  expect_equal(outdated(config), character(0))
  expect_false(file.exists(default_cache_path()))
})

test_with_dir("arbitrary storr in-memory cache", {
  expect_false(file.exists(default_cache_path()))
  parallelism <- default_parallelism()
  jobs <- 1
  envir <- eval(parse(text = get_testing_scenario()$envir))
  cache <- storr::storr_environment(hash_algorithm = "murmur32")
  load_mtcars_example(envir = envir, cache = cache)
  my_plan <- envir$my_plan
  con <- make(
    my_plan,
    envir = envir,
    cache = cache,
    parallelism = parallelism,
    jobs = jobs,
    verbose = FALSE
  )
  envir$reg2 <- function(d){
    d$x3 <- d$x ^ 3
    lm(y ~ x3, data = d)
  }
  expect_false(file.exists(default_cache_path()))
  expect_equal(short_hash(con$cache), "murmur32")
  expect_equal(long_hash(con$cache), default_long_hash_algo())

  expect_equal(cached(verbose = FALSE), character(0))
  targets <- con$plan$target
  expect_true(all(targets %in% cached(cache = cache, verbose = FALSE)))
  expect_false(file.exists(default_cache_path()))

  expect_error(drake_session(verbose = FALSE))
  expect_true(is.list(drake_session(cache = cache, verbose = FALSE)))
  expect_false(file.exists(default_cache_path()))

  expect_equal(length(imported(verbose = FALSE)), 0)
  expect_true(length(imported(cache = cache, verbose = FALSE)) > 0)
  expect_false(file.exists(default_cache_path()))

  expect_equal(length(built(verbose = FALSE)), 0)
  expect_true(length(built(cache = cache)) > 0)
  expect_false(file.exists(default_cache_path()))

  expect_equal(nrow(build_times(verbose = FALSE)), 0)
  expect_true(nrow(build_times(cache = cache)) > 0)
  expect_false(file.exists(default_cache_path()))

  o1 <- outdated(con)
  expect_equal(length(o1), 7)
  expect_false(file.exists(default_cache_path()))

  p <- vis_drake_graph(con, file = "graph.html")
  expect_false(file.exists(default_cache_path()))

  expect_equal(m1, 4)
  expect_false(file.exists(default_cache_path()))

  p1 <- progress(verbose = FALSE)
  unlink(default_cache_path(), recursive = TRUE)
  p2 <- progress(cache = cache, verbose = FALSE)
  expect_true(length(p2) > length(p1))
  expect_false(file.exists(default_cache_path()))

  expect_error(read_drake_config(verbose = FALSE))
  expect_true(is.list(read_drake_config(cache = cache, verbose = FALSE)))
  expect_false(file.exists(default_cache_path()))

  expect_error(read_drake_graph(verbose = FALSE))
  expect_equal(class(
    read_drake_graph(cache = cache, verbose = FALSE)), "igraph")
  expect_false(file.exists(default_cache_path()))

  expect_error(read_drake_plan(verbose = FALSE))
  expect_true(is.data.frame(read_drake_plan(cache = cache, verbose = FALSE)))
  expect_false(file.exists(default_cache_path()))

  expect_error(readd(small, verbose = FALSE))
  expect_true(is.data.frame(readd(small, cache = cache, verbose = FALSE)))
  expect_false(file.exists(default_cache_path()))

  expect_error(loadd(large, verbose = FALSE))
  expect_silent(loadd(large, cache = cache, verbose = FALSE))
  expect_true(nrow(large) > 0)
  rm(large)
  expect_false(file.exists(default_cache_path()))
})
