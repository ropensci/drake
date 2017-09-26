cat(get_testing_scenario_name(), ": ", sep = "")
context("testing")

test_with_dir("set_testing_scenario", {
  original <- get_testing_scenario_name()
  original_opt <- getOption(test_option_name)
  with_all_options({
    expect_equal(get_testing_scenario_name(), original)
    expect_error(set_testing_scenario("lskdjf"))
    set_testing_scenario("local_mcl_8")
    expect_equal(get_testing_scenario_name(), "local_mcl_8")
    expect_equal(get_testing_scenario()$parallelism, "mclapply")
    expect_equal(get_testing_scenario()$jobs, 8)
  })
  expect_equal(original, get_testing_scenario_name())
  expect_equal(original_opt, getOption(test_option_name))
})

test_with_dir("testing utils", {
  path <- system.file("DESCRIPTION", package = "drake")
  testfiles <- unit_test_files(path = path)
  expect_equal(basename(testfiles), "testthat")
  expect_true(is.character(this_os()))
  scenario <- names(testing_scenarios)[1]
  expect_true(is.list(get_testing_scenario()))
  expect_true(is.list(testing_scenarios))
  expect_false(should_skip(scenario, os = "windows"))
  expect_false(should_skip(scenario, os = "linux"))
  scenario <- "global_mcl_2"
  expect_true(should_skip(scenario, os = "windows"))
  expect_false(should_skip(scenario, os = "linux"))
  expect_error(should_skip("scenario not found"))
})

test_with_dir("test_with_dir() evaluates inside the testing envir", {
  some_outside_object <- 4
  test_with_dir("nested test", {
    some_nested_object <- 1
    expect_true("some_nested_object" %in% ls())
    expect_false("some_outside_object" %in% ls())
  })
  expect_false("some_nested_object" %in% ls())
  expect_true("some_outside_object" %in% ls())
})

test_with_dir("test_scenarios()", {
  old_scenario <- getOption(test_option_name)
  wd <- getwd()
  some_outside_object <- 4
  subdir <- "subdir"
  if (!file.exists(subdir)){
    dir.create(subdir)
  }
  file <- file.path(subdir, "test-small.R")
  writeLines(
    text = c(
      "test_with_dir('super nested test', {",
      "  cat('logged scenario', getOption(test_option_name))",
      "  some_nested_object <- 1",
      "  expect_true('some_nested_object' %in% ls())",
      "  expect_false('some_outside_object' %in% ls())",
      "})"
    ),
    con = file
  )

  always_skip <- function(...){
    TRUE
  }
  never_skip <- function(...){
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
  loggings <- grepl("logged scenario", log)
  expect_true(any(loggings))
  log <- log[loggings]
  log <- gsub("logged scenario", "", log)
  log <- gsub(" ", "", log)
  log <- gsub(".", "", log, fixed = TRUE)
  expect_equal(sort(log), sort(names(testing_scenarios)))

  log <- capture.output(
    test_scenarios(
      unit_test_dir = subdir,
      skip_criterion = always_skip
    )
  )
  loggings <- grepl("logged scenario", log)
  expect_false(any(loggings))
  expect_true(any(grepl("skip", log)))
})
