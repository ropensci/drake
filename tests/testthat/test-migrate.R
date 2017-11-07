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
  load_basic_example()
  expect_error(outdated(my_plan))
  expect_error(make(my_plan, verbose = FALSE))
  make(my_plan, verbose = FALSE, force = TRUE)
  expect_equal(outdated(my_plan, verbose = FALSE), character(0))
  expect_true(length(cached()) > 0)
  clean()
  expect_true(length(cached()) == 0)
})

test_with_dir("null cases for migrate()", {
  expect_true(migrate(path = "not_found"))
  x <- new_cache(path = "path")
  expect_true(migrate(path = "path"))
  expect_silent(tmp <- null_proc_time())
})

test_with_dir("migrate() an up to date cache", {
  write_v4.3.0_project() # nolint
  file.rename(from = default_cache_path(), to = "old")
  expect_error(this_cache(path = "old"))
  cache <- this_cache(path = "old", force = TRUE)
  expect_true(migrate(path = "old", jobs = 2))
  load_basic_example()
  config <- make(my_plan, cache = cache)
 # expect_equal(justbuilt(config = config), character(0)) # r-lib/covr#289 # nolint
  clean(cache = cache)
  expect_equal(cached(cache = cache), character(0))
  # Already migrated, nothing to do
  expect_true(migrate(path = "old", jobs = 2))
})

test_with_dir("migrate() a partially outdated cache", {
  write_v4.3.0_project() # nolint
  file.rename(from = default_cache_path(), to = "old")
  cache <- this_cache(path = "old", force = TRUE)
  for (namespace in cache$list_namespaces()){
    cache$del(key = "report_dependencies", namespace = namespace)
  }
  plan <- cache$get("plan", namespace = "config")
  plan$command[plan$target == "small"] <- "simulate(6)"
  cache$set(key = "plan", value = plan, namespace = "config")
  expect_true(migrate(path = "old", jobs = 2))
  out <- c("'report.md'", plan$target[grep("small", plan$target)])
  load_basic_example()
  out2 <- outdated(plan, cache = cache)
#  expect_equal(sort(out), sort(out2)) # r-lib/covr#289 # nolint
})

test_with_dir("migration_result()", {
  expect_error(migration_result(FALSE, "backup"))
  expect_output(migration_result(TRUE, "backup"))
})

test_with_dir("Null cases in legacy functions", {
  write_v4.3.0_project() # nolint
  cache <- this_cache(force = TRUE)
  cache$set(key = "'report.md'", value = Inf, namespace = "filemtime")
  config <- read_config(cache = cache)
  expect_true(is.na(legacy_self_hash(target = "ok123", config = config)))
  x <- legacy_file_hash(target = "'report.md'", config = config,
    size_cutoff = -1)
  expect_true(!is.na(x) && is.character(x) && nchar(x) > 10)
  expect_false(
    legacy_target_current(target = "ok123", hashes = NULL, config = config))
  unlink("report.md")
  expect_false(
    legacy_target_current(
      target = "'report.md'", hashes = NULL, config = config))
  expect_true(is.na(error_na()))
})

test_with_dir("more legacy functions", {
  con <- dbug()
  testrun(con)
  file.rename("intermediatefile.rds", "tmp")
  expect_false(legacy_target_current("'intermediatefile.rds'", hashes = list(),
    config = list(inventory = "'intermediatefile.rds'")))
})
