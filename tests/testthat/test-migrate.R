drake_context("migrate")

test_with_dir("force loading a non-back-compatible cache", {
  expect_null(assert_compatible_cache(NULL))
  expect_null(get_cache())
  expect_null(this_cache())
  expect_true(inherits(recover_cache(), "storr"))
  write_v4.1.0_cache() # nolint
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
  expect_silent(make(my_plan, verbose = FALSE, force = TRUE))
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
  write_v4.1.0_cache()
  file.rename(from = default_cache_path(), to = "old")
  expect_error(this_cache(path = "old"))
  cache <- this_cache(path = "old", force = TRUE)
  plan <- cache$get(key = "plan", namespace = "config")
  plan <- plan[plan$target != "'report.md'", ]
  cache$set(key = "plan", value = plan, namespace = "config")
  expect_true(migrate(path = "old", jobs = 2))
  load_basic_example()
  con <- make(plan, cache = cache)
  expect_equal(justbuilt(config = con), character(0))
})

test_with_dir("migrate() a partially outdated cache", {
  write_v4.1.0_cache()
  file.rename(from = default_cache_path(), to = "old")
  cache <- this_cache(path = "old", force = TRUE)
  for (namespace in cache$list_namespaces()){
    cache$del(key = "report_dependencies", namespace = namespace)
  }
  plan <- cache$get("plan", namespace = "config")
  plan$command[plan$target == "small"] <- "simulate(6)"
  cache$set(key = "plan", value = plan, namespace = "config")
  expect_true(migrate(path = "old", jobs = 2))
  e <- new.env(parent = globalenv())
  load_basic_example(envir = e)
  out <- outdated(plan = plan, envir = e, cache = cache)
  out2 <- c("'report.md'", "report_dependencies",
    plan$target[grepl("small", plan$target)])
  expect_equal(sort(out), sort(out2))
})

test_with_dir("migration_result()", {
  expect_error(migration_result(FALSE, "backup"))
  expect_output(migration_result(TRUE, "backup"))
})
