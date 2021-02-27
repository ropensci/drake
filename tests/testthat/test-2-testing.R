drake_context("testing")

test_with_dir("test_with_dir() clears out files", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("tibble")
  for (i in 1:10) {
    expect_silent(
      test_with_dir("test", {
        skip_on_cran()
        expect_false(file.exists("x"))
        file.create("x")
        expect_true(file.exists("x"))
      })
    )
  }
})

test_with_dir("set_testing_scenario", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  original <- get_testing_scenario_name()
  original_opt <- getOption(test_option_name)
  with_all_options <- function(code) {
    old <- options()
    on.exit(restore_options(old))
    force(code)
  }
  restore_options <- function(old) {
    current <- options()
    remove_these <- setdiff(names(current), names(old))
    removal_list <- as.list(old[remove_these])
    names(removal_list) <- remove_these
    do.call(options, removal_list)
    options(old)
  }
  with_all_options({
    expect_equal(get_testing_scenario_name(), original)
    expect_error(set_testing_scenario("lskdjf"))
    set_testing_scenario("local_future_9_future::multisession_worker")
    expect_equal(
      get_testing_scenario_name(),
      "local_future_9_future::multisession_worker"
    )
    expect_equal(get_testing_scenario()$parallelism, "future")
    expect_equal(get_testing_scenario()$jobs, 9)
  })
  expect_equal(original, get_testing_scenario_name())
  expect_equal(original_opt, getOption(test_option_name))
})

test_with_dir("testing utils", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  expect_true(is.character(this_os()))
  scenario <- default_testing_scenario
  expect_true(is.data.frame(get_testing_scenario()))
  expect_true(is.data.frame(testing_scenarios()))
  expect_false(should_skip(scenario, os = "windows"))
  expect_false(should_skip(scenario, os = "linux"))
  scenario <- "local_clustermq_2"
  expect_true(should_skip(scenario, os = "windows"))
  expect_false(should_skip(scenario, os = "linux"))
  expect_error(should_skip("scenario not found"))
})

test_with_dir("test_with_dir() evaluates inside the testing envir", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  some_outside_object <- 4
  test_with_dir("nested test", {
    skip_on_cran()
    some_nested_object <- 1
    expect_true("some_nested_object" %in% ls())
    expect_false("some_outside_object" %in% ls())
  })
  expect_false("some_nested_object" %in% ls())
  expect_true("some_outside_object" %in% ls())
})

test_with_dir("test_scenarios()", capture.output(suppressMessages({
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  old_scenario <- getOption(test_option_name)
  wd <- getwd()
  some_outside_object <- 4
  subdir <- "subdir"
  if (!file.exists(subdir)) {
    dir.create(subdir)
  }
  file <- file.path(subdir, "test-small.R")

  writeLines(
    text = "write(getOption('drake_test_scenario'), 'log.txt', append = TRUE)",
    con = file
  )

  always_skip <- function(...) {
    TRUE
  }
  never_skip <- function(...) {
    FALSE
  }
  test_scenarios(
    unit_test_dir = subdir,
    skip_criterion = never_skip
  )

  expect_false("some_nested_object" %in% ls())
  expect_true("some_outside_object" %in% ls())
  expect_equal(getwd(), wd)
  expect_equal(old_scenario, getOption(test_option_name))

  # Check if we tested with all the options
  log <- readLines(file.path("subdir", "log.txt"))
  expect_equal(sort(log), sort(testing_scenario_names()))
})))

test_with_dir("unit_test_files works", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).

  # Find package root
  path <- system.file("DESCRIPTION", package = "drake")
  testfiles <- unit_test_files(path = path)
  expect_equal(basename(testfiles), "testthat")

  # Does unit_test_files find a root with DESCRIPTION?
  wd <- getwd()
  writeLines(
    text = "Package: drake",
    con = "DESCRIPTION"
  )
  subdir <- "subdir"
  if (!file.exists(subdir)) {
    dir.create(subdir)
  }
  expect_equal(basename(unit_test_files(subdir, max_depth = 2)), "testthat")

  # DESCRIPTION without 'drake' in first line.  For this and next test,
  # max_depth = 1 so we don't accidentally find a DESCRIPTION left over
  # in temp directory directory above us
  writeLines(
    text = "Package: drakebutnotdrake",
    con = "DESCRIPTION"
  )
  expect_error(unit_test_files(wd, max_depth = 1))

  # Shouldn't find anything if there's no DESCRIPTION
  unlink("DESCRIPTION")
  expect_error(unit_test_files(wd, max_depth = 1))
})
