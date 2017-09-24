context("customize caches")

test_with_dir("fancy cache features, bad paths", {
  saveRDS(1, file = "exists")
  expect_error(x <- new_cache("exists"))
  expect_equal(type_of_cache("not_found"), NULL)
  expect_silent(tmp <- uncache(target = "targ", cache = NULL))
  expect_equal(get_storr_rds_cache("not_found"), NULL)
})

test_with_dir("First configure", {
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
  x <- new_cache("new",
    short_hash_algo = "murmur32",
    long_hash_algo = "crc32"
  )
  expect_true(file.exists("new"))
  expect_equal(short_hash(x), "murmur32")
  expect_equal(long_hash(x), "crc32")
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
  saveRDS("stuff", file = "some_file")
  con <- dbug()
  unlink(default_cache_path(), recursive = TRUE)
  con$plan <- data.frame(target = "a", command = "c('some_file')")
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
    parallelism = test_opt()$parallelism,
    jobs = test_opt()$jobs
  )
  expect_false(file.exists(default_cache_path()))
})

test_with_dir("use two differnt file system caches", {
  saveRDS("stuff", file = "some_file")
  con <- dbug()
  con$plan <- data.frame(target = "a", command = "c('some_file')")
  con$targets <- con$plan$target
  testrun(con)
  o1 <- outdated(
    con$plan,
    envir = con$envir,
    verbose = FALSE,
    cache = con$cache
  )

  expect_equal(o1, character(0))
  expect_equal(
    con$short_hash_algo,
    con$cache$get("short_hash_algo", namespace = "config"),
    default_short_hash_algo()
  )
  expect_equal(
    con$long_hash_algo,
    con$cache$get("long_hash_algo", namespace = "config"),
    default_long_hash_algo()
  )
  expect_equal(
    con$cache$driver$hash_algorithm,
    default_short_hash_algo()
  )

  con$cache <- new_cache(
    path = "my_new_cache",
    short_hash_algo = "murmur32",
    long_hash_algo = "crc32"
  )
  o2 <- outdated(
    con$plan,
    envir = con$envir,
    verbose = FALSE,
    cache = con$cache
  )
  con <- make(
    con$plan,
    cache = con$cache,
    verbose = FALSE,
    parallelism = test_opt()$parallelism,
    jobs = test_opt()$jobs,
    return_config = TRUE
  )
  o3 <- outdated(
    con$plan,
    envir = con$envir,
    verbose = FALSE,
    cache = con$cache
  )
  expect_equal(o2, "a")
  expect_equal(o3, character(0))
  expect_equal(
    con$short_hash_algo,
    con$cache$get("short_hash_algo", namespace = "config"),
    "murmur32"
  )
  expect_equal(
    con$long_hash_algo,
    con$cache$get("long_hash_algo", namespace = "config"),
    "crc32"
  )
  expect_equal(
    con$cache$driver$hash_algorithm,
    "murmur32"
  )
  expect_true(grepl("my_new_cache", con$cache$driver$path))
})
