drake_context("examples")

test_with_dir("examples are listed and written", {
  x <- examples_drake()
  expect_true(is.character(x) & length(x) > 0)
  for (i in x){
    expect_false(file.exists(i))
    example_drake(i)
    expect_true(file.exists(i))
    expect_true(file.info(i)$isdir)
    unlink(i, recursive = TRUE, force = TRUE)
  }
})

test_with_dir("overwrites of report.Rmd handled correctly", {
  expect_silent(load_basic_example(overwrite = TRUE))
  expect_silent(load_basic_example(overwrite = FALSE))
  expect_warning(load_basic_example(overwrite = TRUE))
  expect_warning(load_basic_example(to = "a", report_file = "b"))
  expect_true(file.exists("b"))
  expect_false(file.exists("a"))
})
