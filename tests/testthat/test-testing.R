context("testing")

test_with_dir("testing configs are there", {
  old_opt <- getOption("drake_test_opt")
  expect_true(is.list(test_opt()))
  expect_true(is.list(test_opts))
  expect_true(is.character(capture.output(show_config_opts(dbug()$config))))
  set_test_opt(old_opt)
  expect_true(!is.null(getOption("drake_test_opt")))
  options(drake_test_opt = old_opt)
  expect_equal(old_opt, getOption("drake_test_opt"))
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
