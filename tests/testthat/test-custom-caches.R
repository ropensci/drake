drake_context("custom caches")

test_with_dir("cache_path finding", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- new_cache("x")
  expect_true(is.character(cache_path(x)))
  expect_null(cache_path(NULL))
  expect_null(cache_path(1234))
})

test_with_dir("fancy cache features, bad paths", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  saveRDS(1, file = "exists")
  suppressWarnings(expect_error(x <- new_cache("exists")))
  expect_silent(tmp <- uncache(targets = "targ", cache = NULL))
})

test_with_dir("null hashes", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- new_cache("x")
  x$del("short_hash_algo", namespace = "config")
  expect_null(short_hash(x))
  expect_false(is.null(long_hash(x)))
  y <- new_cache("y")
  y$del("long_hash_algo", namespace = "config")
  expect_false(is.null(short_hash(y)))
  expect_null(long_hash(y))
})

test_with_dir("First configure", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- new_cache()
  expect_equal(short_hash(x), default_short_hash_algo())
  expect_equal(long_hash(x), default_long_hash_algo())

  x <- configure_cache(
    cache = x,
    short_hash_algo = "crc32",
    long_hash_algo = "sha1"
  )
  expect_equal(short_hash(x), default_short_hash_algo())
  expect_equal(long_hash(x), default_long_hash_algo())

  expect_warning(
    x <- configure_cache(
      cache = x,
      short_hash_algo = "crc32",
      long_hash_algo = "sha1",
      overwrite_hash_algos = TRUE
    )
  )

  expect_equal(short_hash(x), default_short_hash_algo())
  expect_equal(long_hash(x), "sha1")

  expect_silent(
    x <- configure_cache(
      cache = x,
      long_hash_algo = "murmur32",
      overwrite_hash_algos = TRUE
    )
  )

  expect_equal(short_hash(x), default_short_hash_algo())
  expect_equal(long_hash(x), "murmur32")
})

test_with_dir("Pick the hashes", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- new_cache("new",
    short_hash_algo = "murmur32",
    long_hash_algo = "crc32"
  )
  expect_true(file.exists("new"))
  y <- this_cache(path = "new")
  expect_true(file.exists("new"))
  expect_equal(short_hash(x), "murmur32")
  expect_equal(long_hash(x), "crc32")
  expect_equal(short_hash(y), "murmur32")
  expect_equal(long_hash(y), "crc32")
  expect_equal(x$driver$hash_algorithm, "murmur32")
  expect_equal(y$driver$hash_algorithm, "murmur32")

  x$del("long_hash_algo", namespace = "config")
  x <- configure_cache(x, long_hash_algo = "sha1")
  expect_equal(long_hash(x), "sha1")
  expect_error(configure_cache(x, long_hash_algo = "not found"))
  expect_error(configure_cache(x, short_hash_algo = "not found"))

  s <- short_hash(x)
  l <- long_hash(x)
  expect_silent(configure_cache(x, overwrite_hash_algos = TRUE))
  expect_equal(s, short_hash(x))
  expect_equal(l, long_hash(x))
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
    short_hash_algo = "murmur32",
    long_hash_algo = "crc32"
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
  if (targ %in% ls(envir)){
    rm(list = targ, envir = envir)
  }
  envir$my_function <- function(x){
    x
  }
  cache <- dbug()$cache
  con <- make(
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
    short_hash(cache),
    con$short_hash_algo,
    cache$get("short_hash_algo", namespace = "config"),
    default_short_hash_algo()
  )
  expect_equal(
    long_hash(cache),
    con$long_hash_algo,
    cache$get("long_hash_algo", namespace = "config"),
    default_long_hash_algo()
  )
  expect_equal(
    short_hash(cache),
    cache$driver$hash_algorithm,
    default_short_hash_algo()
  )

  cache2 <- new_cache(
    path = "my_new_cache",
    short_hash_algo = "murmur32",
    long_hash_algo = "crc32"
  )
  con2 <- con
  con2$cache <- cache2
  o2 <- outdated(con2)
  con2 <- make(
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
    short_hash(cache2),
    con2$short_hash_algo,
    cache2$get("short_hash_algo", namespace = "config"),
    "murmur32"
  )
  expect_equal(
    long_hash(cache2),
    con2$long_hash_algo,
    cache$get("long_hash_algo", namespace = "config"),
    "crc32"
  )
  expect_equal(
    short_hash(cache2),
    con2$cache$driver$hash_algorithm,
    "murmur32"
  )
  expect_true(grepl("my_new_cache", con2$cache$driver$path, fixed = TRUE))
  expect_true(grepl("my_new_cache", cache_path(cache2), fixed = TRUE))
})
