drake_context("testing")

test_with_dir("test_with_dir() clears out files", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
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
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  original <- get_testing_scenario_name()
  original_opt <- getOption(test_option_name)
  with_all_options({
    expect_equal(get_testing_scenario_name(), original)
    expect_error(set_testing_scenario("lskdjf"))
    set_testing_scenario("local_mclapply_9")
    expect_equal(get_testing_scenario_name(), "local_mclapply_9")
    expect_equal(get_testing_scenario()$parallelism, "mclapply")
    expect_equal(get_testing_scenario()$jobs, 9)
  })
  expect_equal(original, get_testing_scenario_name())
  expect_equal(original_opt, getOption(test_option_name))
})

test_with_dir("testing utils", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  path <- system.file("DESCRIPTION", package = "drake")
  testfiles <- unit_test_files(path = path)
  expect_equal(basename(testfiles), "testthat")
  expect_true(is.character(this_os()))
  scenario <- default_testing_scenario
  expect_true(is.data.frame(get_testing_scenario()))
  expect_true(is.data.frame(testing_scenarios()))
  expect_false(should_skip(scenario, os = "windows"))
  expect_false(should_skip(scenario, os = "linux"))
  scenario <- "global_mclapply_2"
  expect_true(should_skip(scenario, os = "windows"))
  expect_false(should_skip(scenario, os = "linux"))
  expect_error(should_skip("scenario not found"))
})

test_with_dir("test_with_dir() evaluates inside the testing envir", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
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

test_with_dir("test_scenarios()", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  old_scenario <- getOption(test_option_name)
  wd <- getwd()
  some_outside_object <- 4
  subdir <- "subdir"
  if (!file.exists(subdir)) {
    dir.create(subdir)
  }
  file <- file.path(subdir, "test-small.R")

  writeLines(
    text = "cat('logged scenario', getOption('drake_test_scenario'), ' ')",
    con = file
  )

  always_skip <- function(...) {
    TRUE
  }
  never_skip <- function(...) {
    FALSE
  }
  log <- capture.output(
    test_scenarios(
      unit_test_dir = subdir,
      skip_criterion = never_skip
    )
  )

  expect_false("some_nested_object" %in% ls())
  expect_true("some_outside_object" %in% ls())
  expect_equal(getwd(), wd)
  expect_equal(old_scenario, getOption(test_option_name))

  # Check if we tested with all the options
  loggings <- grepl("logged scenario", log, fixed = TRUE)
  expect_true(any(loggings))
  log <- log[loggings]
  log <- gsub("logged scenario ", "", log)
  log <- gsub(" .*", "", log)
  expect_equal(sort(log), sort(testing_scenario_names()))

  log <- evaluate_promise(
    test_scenarios(
      unit_test_dir = subdir,
      skip_criterion = always_skip
    ),
    print = TRUE
  )
  log <- c(log$output, log$messages)

  loggings <- grepl("logged scenario", log, fixed = TRUE)
  expect_false(any(loggings))
  expect_true(any(grepl("skip", log, fixed = TRUE)))
})
