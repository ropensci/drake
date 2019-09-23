drake_context("test-code_to_function")



test_that("Scripts are evaluated into function that returns a time and file path", {

  script1 <- tempfile()
  script2 <- tempfile()
  writeLines(c("data <- mtcars", "data$newcol <- 1"), script1)

  script1_function <- code_to_function(script1)

  function_contents <- as.character(body(script1_function))
  function_results <- script1_function()

  expect_equal(function_contents[-length(function_contents)],
               c("{",readLines(script1)))
  expect_equal(length(function_results),2)
  expect_s3_class(function_results$time,"POSIXct")

})

test_that("Returned functions include only the scripts lines that need to be evaluated", {

  script1 <- tempfile()
  script2 <- tempfile()

  script1_contents<-c("data <- mtcars",
                      "data$newcol <- 1")
  script2_contents<-c("data <- mtcars",
                      "# Comment with un-necessary information",
                      "data$newcol <- 1")

  writeLines(script1_contents, script1)
  writeLines(script2_contents, script2)

  script1_function <- code_to_function(script1)
  script2_function <- code_to_function(script2)

  function1_contents <- as.character(body(script1_function))
  function2_contents <- as.character(body(script2_function))

  expect_equal(function1_contents[-length(function1_contents)],
               c("{",readLines(script1)))
  expect_equal(function1_contents,
               function2_contents)
  expect_false(isTRUE(all.equal(
    function2_contents[-length(function2_contents)],
    c("{",readLines(script2)))))
})

test_with_dir("drake tracks and updates for scripted functions", {

  #Setup scripts
  script_list <- setup_scripts()
  script1_function<-code_to_function(script_list$script1)
  script2_function<-code_to_function(script_list$script2)
  script3_function<-code_to_function(script_list$script3)

  # create plan
  plan <- create_scripts_plan()
  config<-drake_config(plan)
  make(plan)
  expect_equal(
    justbuilt(config),
    c("step1","step2","step3")
  )


  # Change script2 trivially, there should be no updates
  script2_function<-update_script2_trivial(script = script_list$script2,
                                           tempDir = script_list$tempDir)
  expect_equal(length(outdated(config)),0)


  # change script2 non-trivially to cause update to all downstream targets.
  script2_function<-update_script2_non_trivial(script = script_list$script2,
                             tempDir = script_list$tempDir)

  expect_equal(
    outdated(config),
    c("step2","step3")
  )
  make(plan)
  expect_equal(
    justbuilt(config),
    c("step2","step3")
  )

  # change script2 non-trivially but output would be the same.
  # behavior is like `gnumaker`, in that it will rerun all downstream functions
  script2_function<-update_script2_same_output(script = script_list$script2,
                                               tempDir = script_list$tempDir)
  expect_equal(
    outdated(config),
    c("step2","step3")
  )
  make(plan)
  expect_equal(
    justbuilt(config),
    c("step2","step3")
  )

  # nothing to do
  finalOutput <- readd(step3)

  make(plan)
  expect_true(identical(finalOutput, readd(step3)))
})
