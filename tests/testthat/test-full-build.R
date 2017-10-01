cat(get_testing_scenario_name(), ": ", sep = "")
context("full build")

test_with_dir("scratch build with contained envir.", {
  config <- dbug()
  expect_error(session(search = FALSE))
  expect_true(length(progress(search = FALSE)) == 0)
  expect_equal(config$cache$list(), character(0))
  testrun(config)
  expect_true(is.numeric(readd(final, search = FALSE)))
  expect_true(length(config$cache$list()) > 2)
  expect_false(any(c("f", "final") %in% ls()))
  expect_true(is.list(session(search = FALSE)))
  expect_true(all(session(search = FALSE)$target %in% config$plan$target))

  # changed nothing
  testrun(config)
  nobuild(config)

  # take this opportunity to test clean() and prune()
  all <- sort(c("package:base", "'input.rds'",
    "'intermediatefile.rds'", "a",
    "b", "c", "combined", "f", "final", "g", "h", "i", "j",
    "myinput", "nextone", "readRDS", "saveRDS", "yourinput"))
  expect_equal(config$cache$list(), all)
  expect_true(file.exists("intermediatefile.rds"))
  expect_true(file.exists("input.rds"))
  expect_true(file.exists(default_cache_path()))

  # prune
  expect_warning(prune(config$plan[config$plan$target != "final", ]))
  expect_true(file.exists("intermediatefile.rds"))
  expect_true(file.exists("input.rds"))
  expect_equal(config$cache$list(), setdiff(all, "final"))

  # clean specific targets
  clean(b, c, list = c("'intermediatefile.rds'", "nextone"),
    search = FALSE)
  expect_false(file.exists("intermediatefile.rds"))
  expect_true(file.exists("input.rds"))
  expect_equal(config$cache$list(), setdiff(all, c("b", "c",
    "'intermediatefile.rds'", "nextone", "final")))

  # clean does not remove imported files
  expect_true(file.exists("input.rds"))
  expect_true("'input.rds'" %in% config$cache$list())
  clean("'input.rds'", search = FALSE)
  expect_true(file.exists("input.rds"))
  expect_false("'input.rds'" %in% config$cache$list())

  # clean removes imported functions and cleans up 'functions'
  # namespace
  expect_true(cached(f))
  for (n in c("objects", "depends", "functions")) {
    expect_true("f" %in% config$cache$list(namespace = n))
  }
  clean(f)
  for (n in c("objects", "depends", "functions")) {
    expect_false("f" %in% config$cache$list(namespace = n))
  }

  clean(destroy = FALSE, search = FALSE)
  expect_equal(config$cache$list(), character(0))
  expect_equal(config$cache$list("depends"), character(0))
  expect_equal(config$cache$list("functions"), character(0))
  expect_false(file.exists("intermediatefile.rds"))
  expect_true(file.exists("input.rds"))
  expect_true(file.exists(default_cache_path()))
  expect_equal(config$cache$list("filemtime"), character(0))

  clean(destroy = TRUE, search = FALSE)
  expect_false(file.exists(default_cache_path()))
  clean(destroy = TRUE, search = FALSE)
  clean(destroy = FALSE, search = FALSE)
})

test_with_dir("clean in full build.", {
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
