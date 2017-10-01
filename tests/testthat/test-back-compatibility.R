cat(get_testing_scenario_name(), ": ", sep = "")
context("back compatibility")

test_with_dir("back-compatible with a tiny v4.1.0 project", {
  root <- file.path("back-compatibility", "v4.1.0")
  cache_files <- system.file(
    file.path(root, "cache"),
    package = "drake",
    mustWork = TRUE
  )
  infile <- system.file(
    file.path(root, "my_file.rds"),
    package = "drake",
    mustWork = TRUE
  )
  expect_true(file.copy(
    from = cache_files,
    to = getwd(),
    recursive = TRUE,
    overwrite = TRUE
  ))
  file.rename(from = "cache", to = ".drake")
  bt <- build_times()
  expect_true(is.data.frame(bt))
  expect_equal(dim(bt), c(1, 5))
  expect_false(is.na(bt$type))
  expect_equal(bt$type, "target")
  expect_equal(bt$item, "x")
  expect_true(file.copy(
    from = infile,
    to = getwd(),
    recursive = TRUE,
    overwrite = TRUE
  ))
  old_plan <- data.frame(
    target = "x",
    command = "eply::strings(my_function('my_file.rds'))"
  )
  envir <- eval(parse(text = get_testing_scenario()$envir))
  envir$my_function <- function(x){
    x
  }
  version <- session()$otherPkgs$drake$Version # nolint
  expect_equal(version, "4.1.0")
  o <- outdated(old_plan, envir = envir, verbose = FALSE)
  expect_equal(o, character(0))
  newconfig <- read_config(search = FALSE)
  expect_equal(newconfig$short_hash_algo, "md5")
  expect_equal(newconfig$long_hash_algo, "md5")
  expect_equal(newconfig$cache$driver$hash_algorithm, "md5")
  con <- make(
    old_plan,
    verbose = FALSE,
    return_config = TRUE,
    envir = envir,
    parallelism = get_testing_scenario()$parallelism,
    jobs = get_testing_scenario()$jobs
  )
  expect_equal(justbuilt(con), character(0))
  expect_equal(con$short_hash_algo, "md5")
  expect_equal(con$long_hash_algo, "md5")
  expect_equal(con$cache$driver$hash_algorithm, "md5")
  storr_hash <- scan(
    file.path(default_cache_path(), "config", "hash_algorithm"),
    what = character(),
    quiet = TRUE
  )
  expect_equal(storr_hash, "md5")
  obj <- c("'my_file.rds'", "eply::strings", "my_function", "x")
  expect_equal(sort(cached(cache = con$cache)), sort(obj))
    
  # Make from scratch. Should now depend on package:eply.
  clean(destroy = TRUE, cache = con$cache)
  con <- make(
    old_plan,
    verbose = FALSE,
    return_config = TRUE,
    envir = envir,
    parallelism = get_testing_scenario()$parallelism,
    jobs = get_testing_scenario()$jobs
  )
  obj <- c(obj, "package:eply")
  expect_equal(sort(cached(cache = con$cache)), sort(obj))

  rm(list = "my_function", envir = envir)
})
