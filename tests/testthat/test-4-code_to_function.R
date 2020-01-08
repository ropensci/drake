drake_context("code to function")

test_with_dir("Scripts eval'd to fns returning times & file paths", {
  skip_on_cran()
  script1 <- tempfile()
  writeLines(
    c(
      "data <- mtcars",
      "data$newcol <- 1"
    ),
    script1
  )

  script1_function <- code_to_function(script1)

  function_contents <- as.character(body(script1_function))
  function_results <- script1_function()

  expect_equal(
    function_contents[-length(function_contents)],
    c("{", readLines(script1))
  )
  expect_equal(
    length(function_results),
    2
  )
  expect_s3_class(
    function_results$time,
    "POSIXct"
  )
})

test_with_dir("Returned fns include only *active* scripts lines", {
  skip_on_cran()
  script1 <- tempfile()
  script2 <- tempfile()

  script1_contents <- c(
    "data <- mtcars",
    "data$newcol <- 1"
  )
  script2_contents <- c(
    "data <- mtcars",
    "# Comment with un-necessary information",
    "data$newcol <- 1"
  )

  writeLines(script1_contents, script1)
  writeLines(script2_contents, script2)

  script1_function <- code_to_function(script1)
  script2_function <- code_to_function(script2)

  function1_contents <- as.character(body(script1_function))
  function2_contents <- as.character(body(script2_function))

  expect_equal(
    function1_contents[-length(function1_contents)],
    c("{", readLines(script1))
  )
  expect_equal(
    function1_contents,
    function2_contents
  )
  expect_false(
    isTRUE(
      all.equal(
        function2_contents[-length(function2_contents)],
        c("{", readLines(script2))
      )
    )
  )
})

test_with_dir("Returned fns include only *active* Rmd code lines", {
  skip_on_cran()
  skip_if_not_installed("knitr")
  rmd <- tempfile()

  writeLines(
    c(
      "---",
      "title: \"Test RMD 1\"",
      "author: \"Unknown\"",
      "output: html_document",
      "---",
      "This is a test rmd to load and save the `mtcars` dataset.",
      "```{r load_data}",
      "data <- mtcars",
      "```",
      "We will now add a new column!",
      "```{r manipulate}",
      "data$newcol <- 1",
      "```"),
    rmd
  )

  expected_contents <- c(
    "data <- mtcars",
    "data$newcol <- 1"
  )

  rmd_function <- code_to_function(rmd)
  rmd_function_contents <- as.character(body(rmd_function))

  expect_equal(
    rmd_function_contents[-length(rmd_function_contents)],
    c("{", expected_contents)
  )
})

test_with_dir("drake tracks and updates for scripted functions", {
  skip_on_cran()
  #Setup scripts
  script_list <- setup_scripts()
  script1_function <- code_to_function(script_list$script1)
  script2_function <- code_to_function(script_list$script2)
  script3_function <- code_to_function(script_list$script3)

  # create plan
  plan <- create_scripts_plan()
  config <- drake_config(plan)
  make(plan)
  expect_equal(
    justbuilt(config),
    c(
      "step1",
      "step2",
      "step3"
    )
  )

  # Change script2 trivially, there should be no updates
  script2_function <- update_script2_trivial(
    script = script_list$script2,
    temp_dir = script_list$temp_dir
  )
  expect_equal(
    length(outdated_impl(config)),
    0
  )

  # change script2 non-trivially to cause update to all downstream targets.
  script2_function <- update_script2_non_trivial(
    script = script_list$script2,
    temp_dir = script_list$temp_dir
  )

  expect_equal(
    outdated_impl(config),
    c(
      "step2",
      "step3"
    )
  )
  make(plan)

  expect_equal(
    justbuilt(config),
    c(
      "step2",
      "step3"
    )
  )

  # change script2 non-trivially but output would be the same.
  # behavior is like `gnumaker`, in that it will rerun all
  # downstream functions
  script2_function <- update_script2_same_output(
    script = script_list$script2,
    temp_dir = script_list$temp_dir
  )

  expect_equal(
    outdated_impl(config),
    c(
      "step2",
      "step3"
    )
  )
  make(plan)
  expect_equal(
    justbuilt(config),
    c(
      "step2",
      "step3"
    )
  )

  # nothing to do
  final_output <- readd(step3)

  make(plan)
  expect_equal(
    final_output,
    readd(step3)
  )
  expect_equal(
    justbuilt(config),
    character(0)
  )
})

test_with_dir("drake tracks/updates for Rmd files that act like scripts", {
  skip_on_cran()
  skip_if_not_installed("knitr")

  #Setup scripts
  rmd_list <- setup_rmd()
  rmd1_function <- code_to_function(rmd_list$rmd1)
  rmd2_function <- code_to_function(rmd_list$rmd2)
  rmd3_function <- code_to_function(rmd_list$rmd3)

  # create plan
  plan <- create_rmd_plan()
  config <- drake_config(plan)
  make(plan)

  expect_equal(
    justbuilt(config),
    c(
      "step1",
      "step2",
      "step3"
    )
  )

  # Change rmd2 trivially, there should be no updates
  rmd2_function <- update_rmd2_trivial(
    rmd = rmd_list$rmd2,
    temp_dir = rmd_list$temp_dir
  )

  expect_equal(
    length(outdated_impl(config)),
    0
  )

  # change rmd2 non-trivially to cause update to all downstream targets.
  rmd2_function <- update_rmd2_non_trivial(
    rmd = rmd_list$rmd2,
    temp_dir = rmd_list$temp_dir
  )

  expect_equal(
    outdated_impl(config),
    c(
      "step2",
      "step3"
    )
  )
  make(plan)
  expect_equal(
    justbuilt(config),
    c(
      "step2",
      "step3"
    )
  )

  # change rmd2 non-trivially but output would be the same.
  # behavior is like `gnumaker`, in that it will rerun all
  # downstream functions
  rmd2_function <- update_rmd2_same_output(
    rmd = rmd_list$rmd2,
    temp_dir = rmd_list$temp_dir
  )

  expect_equal(
    outdated_impl(config),
    c(
      "step2",
      "step3"
    )
  )
  make(plan)
  expect_equal(
    justbuilt(config),
    c(
      "step2",
      "step3"
    )
  )

  # nothing to do
  final_output <- readd(step3)

  make(plan)
  expect_equal(
    final_output,
    readd(step3)
  )
  expect_equal(
    justbuilt(config),
    character(0)
  )
})
