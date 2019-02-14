drake_context("custom caches")

test_with_dir("cache_path finding", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- new_cache("x")
  expect_true(is.character(cache_path_(x)))
  expect_null(cache_path_(NULL))
  expect_null(cache_path_(1234))
})

test_with_dir("fancy cache features, bad paths", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  saveRDS(1, file = "exists")
  suppressWarnings(expect_error(x <- new_cache("exists")))
})

test_with_dir("Pick the hash", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- new_cache("new", hash_algorithm = "murmur32")
  expect_true(file.exists("new"))
  y <- storr::storr_rds(path = "new")
  expect_true(file.exists("new"))
  expect_equal(y$driver$hash_algorithm, "murmur32")
})

test_with_dir("totally off the default cache", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  saveRDS("stuff", file = "some_file")
  con <- dbug()
  unlink(default_cache_path(), recursive = TRUE)
  con$plan <- data.frame(target = "a", command = "file_in(\"some_file\")")
  con$targets <- con$plan$target
  con$cache <- new_cache(
    path = "my_new_cache",
    hash_algorithm = "murmur32"
  )
  make(
    con$plan,
    cache = con$cache,
    verbose = FALSE,
    parallelism = get_testing_scenario()$parallelism,
    jobs = get_testing_scenario()$jobs,
    session_info = FALSE
  )
  expect_false(file.exists(default_cache_path()))
})

test_with_dir("use two differnt file system caches", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  saveRDS("stuff", file = "some_file")
  targ <- "DRAKE_TEST_target"
  my_plan <- data.frame(
    target = targ,
    command = "my_function(file_in(\"some_file\"))"
  )
  scenario <- get_testing_scenario()
  parallelism <- scenario$parallelism
  jobs <- scenario$jobs
  envir <- eval(parse(text = scenario$envir))
  if (targ %in% ls(envir)) {
    rm(list = targ, envir = envir)
  }
  envir$my_function <- function(x) {
    x
  }
  cache <- new_cache(path = "cache1", hash_algorithm = "murmur32")
  make(
    my_plan,
    cache = cache,
    envir = envir,
    verbose = FALSE,
    parallelism = parallelism,
    jobs = jobs,
    session_info = FALSE
  )
  con <- drake_config(
    my_plan,
    cache = cache,
    envir = envir,
    verbose = FALSE,
    parallelism = parallelism,
    jobs = jobs,
    session_info = FALSE
  )

  o1 <- outdated(con)

  expect_equal(o1, character(0))
  expect_equal(
    cache$driver$hash_algorithm,
    "murmur32"
  )

  cache2 <- new_cache(
    path = "my_new_cache",
    hash_algorithm = "crc32"
  )
  con2 <- con
  con2$cache <- cache2
  o2 <- outdated(con2)
  make(
    my_plan,
    cache = cache2,
    envir = envir,
    verbose = FALSE,
    parallelism = parallelism,
    jobs = jobs,
    session_info = FALSE
  )
  con2 <- drake_config(
    my_plan,
    cache = cache2,
    envir = envir,
    verbose = FALSE,
    parallelism = parallelism,
    jobs = jobs,
    session_info = FALSE
  )
  o3 <- outdated(con2)
  expect_equal(o2, targ)
  expect_equal(o3, character(0))
  expect_equal(
    cache2$driver$hash_algorithm,
    "crc32"
  )
  expect_false(file.exists(".drake"))
  expect_true(file.exists("cache1"))
  expect_true(file.exists("my_new_cache"))
  expect_true(grepl("my_new_cache", con2$cache$driver$path, fixed = TRUE))
  expect_true(grepl("my_new_cache", cache_path_(cache2), fixed = TRUE))
})
