drake_context("migrate")

test_with_dir("force loading a non-back-compatible cache", {
  expect_null(assert_compatible_cache(NULL))
  expect_null(get_cache())
  expect_null(this_cache())
  expect_true(inherits(recover_cache(), "storr"))
  write_v4.3.0_project() # nolint
  suppressWarnings({
    expect_error(get_cache())
    expect_error(this_cache())
    expect_error(recover_cache())
  })
  expect_true(inherits(get_cache(force = TRUE), "storr"))
  expect_true(inherits(this_cache(force = TRUE), "storr"))
  expect_true(inherits(recover_cache(force = TRUE), "storr"))
  config <- load_mtcars_example(force = TRUE)
  expect_true(length(outdated(config)) > 0)
  expect_error(make(my_plan, verbose = FALSE, session_info = FALSE))
  make(my_plan, verbose = FALSE, force = TRUE)
  expect_equal(outdated(config), character(0))
  expect_true(length(cached()) > 0)
  clean()
  expect_true(length(cached()) == 0)
})

test_with_dir("null cases for migrate_drake_project()", {
  expect_true(migrate_drake_project(path = "not_found"))
  x <- new_cache(path = "path")
  expect_true(migrate_drake_project(path = "path"))
  expect_silent(tmp <- null_proc_time())
})

test_with_dir("migrate_drake_project() an up to date cache", {
  write_v4.3.0_project() # nolint
  file.rename(from = default_cache_path(), to = "old")
  expect_error(this_cache(path = "old"))
  cache <- this_cache(path = "old", force = TRUE)
  expect_true(migrate_drake_project(path = "old"))
  load_mtcars_example()
  config <- make(my_plan, cache = cache, session_info = FALSE)
 # expect_equal(justbuilt(config = config), character(0)) # r-lib/covr#289 # nolint
  clean(cache = cache)
  expect_equal(cached(cache = cache), character(0))
  # Already migrated, nothing to do
  expect_true(suppressWarnings(migrate_drake_project(path = "old", jobs = 2)))
})

test_with_dir("migrate_drake_project() a partially outdated cache", {
  write_v4.3.0_project() # nolint
  file.rename(from = default_cache_path(), to = "old")
  cache <- this_cache(path = "old", force = TRUE)
  for (namespace in cache$list_namespaces()){
    cache$del(key = "report_dependencies", namespace = namespace)
  }
  plan <- cache$get("plan", namespace = "config")
  plan$command[plan$target == "small"] <- "simulate(6)"
  cache$set(key = "plan", value = plan, namespace = "config")
  expect_true(suppressWarnings(migrate_drake_project(path = "old", jobs = 2)))
  out <- c("\"report.md\"", plan$target[grep("small", plan$target)])
  config <- load_mtcars_example(cache = cache)
  out2 <- outdated(config = config)
  # expect_equal(sort(out), sort(out2)) # r-lib/covr#289 # nolint
})

test_with_dir("migration_result()", {
  expect_error(migration_result(FALSE, "backup"))
  expect_message(migration_result(TRUE, "backup"))
})

test_with_dir("Null cases in legacy functions", {
  write_v4.3.0_project() # nolint
  cache <- this_cache(force = TRUE)
  cache$set(key = "\"report.md\"", value = Inf, namespace = "filemtime")
  config <- read_drake_config(cache = cache)
  expect_true(is.na(legacy_self_hash(target = "ok123", config = config)))
  x <- legacy_file_hash(target = "'report.md'", config = config,
    size_cutoff = -1)
  expect_true(!is.na(x) && is.character(x) && nchar(x) > 10)
  expect_false(
    legacy_target_current(target = "ok123", hashes = NULL, config = config))
  unlink("report.md")
  expect_false(
    legacy_target_current(
      target = "\"report.md\"", hashes = NULL, config = config))
  expect_true(is.na(error_na()))
})

test_with_dir("edge cases in the legacy functions. (no glaring errors)", {
  con <- dbug()
  testrun(con)
  target <- "\"intermediatefile.rds\""

  # Force legacy_file_hash() to rehash a file.
  con$cache$set(key = target, value = Inf, namespace = "filemtime")
  expect_true(
    is.character(
      legacy_file_hash(
        target = target, config = con, size_cutoff = -1
      )
    )
  )

  # Force legacy_target_current() to show a target out of date.
  expect_false(legacy_target_current(target, hashes = list(),
    config = con))
})
