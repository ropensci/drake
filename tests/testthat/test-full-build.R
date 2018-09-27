drake_context("full build")

test_with_dir("scratch build with custom filesystem cache.", {
  config <- dbug()
  unlink(default_cache_path(), recursive = TRUE)
  path <- "my_cache"
  config$cache <- cache <- new_cache(
    path = path,
    short_hash_algo = "murmur32",
    long_hash_algo = "sha512"
  )
  expect_error(drake_session(cache = cache))
  expect_true(length(progress(cache = cache)) == 0)
  expect_equal(config$cache$list(), character(0))

  testrun(config)

  expect_true(is.numeric(readd(final, cache = cache)))
  expect_true(length(config$cache$list()) > 2)
  expect_false(any(c("f", "final") %in% ls()))
  expect_true(
    all(read_drake_plan(cache = cache)$target %in% config$plan$target))

  cache <- this_cache(path = path)
  expect_equal(short_hash(cache), "murmur32")
  expect_equal(long_hash(cache), "sha512")

  # changed nothing
  testrun(config)
  nobuild(config)

  cache <- this_cache(path = path)

  # take this opportunity to test clean() and prune()
  all <- sort(c("\"input.rds\"",
    "\"intermediatefile.rds\"", "drake_target_1", "a",
    "b", "c", "combined", "f", "final", "g", "h", "i", "j",
    "myinput", "nextone", "yourinput"))
  expect_equal(config$cache$list(), all)
  expect_true(file.exists("intermediatefile.rds"))
  expect_true(file.exists("input.rds"))
  expect_false(file.exists(default_cache_path()))
  expect_true(file.exists(path))

  # clean specific targets
  clean(b, c, list = c("\"intermediatefile.rds\"", "nextone"),
    cache = cache)
  expect_false(file.exists("intermediatefile.rds"))
  expect_true(file.exists("input.rds"))
  expect_equal(
    sort(config$cache$list()),
    sort(setdiff(
      all,
      c("b", "c", "\"intermediatefile.rds\"", "nextone")
    ))
  )

  # clean does not remove imported files
  expect_true(file.exists("input.rds"))
  expect_true("\"input.rds\"" %in%
    config$cache$list())
  clean("\"input.rds\"", cache = cache)
  expect_true(file.exists("input.rds"))
  expect_false("\"input.rds\"" %in%
    config$cache$list())

  # clean removes imported functions and cleans up 'functions'
  # namespace
  expect_true(cached(f, cache = cache))
  for (n in c(cache$default_namespace, "kernels")) {
    expect_true("f" %in% config$cache$list(namespace = n))
  }
  clean(f, cache = cache)
  for (n in c(cache$default_namespace, "kernels")) {
    expect_false("f" %in% config$cache$list(namespace = n))
  }

  clean(destroy = FALSE, cache = cache)
  expect_equal(config$cache$list(), character(0))
  expect_equal(config$cache$list("kernels"), character(0))
  expect_false(file.exists("intermediatefile.rds"))
  expect_true(file.exists("input.rds"))
  expect_false(file.exists(default_cache_path()))
  expect_true(file.exists(path))

  clean(destroy = TRUE, cache = cache)
  expect_false(file.exists(path))
})

test_with_dir("clean in full build.", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  config <- dbug()
  make(config$plan, envir = config$envir, verbose = FALSE)
  expect_true("final" %in% config$cache$list())
  clean(final, search = TRUE)
  expect_false("final" %in% config$cache$list())
  clean(search = TRUE)
  expect_equal(config$cache$list(), character(0))
  expect_true(file.exists(default_cache_path()))
  clean(search = TRUE, destroy = TRUE)
  expect_false(file.exists(default_cache_path()))
})
