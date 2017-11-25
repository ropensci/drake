drake_context("examples")

test_with_dir("examples are listed and written", {
  x <- drake_examples()
  expect_true(is.character(x) & length(x) > 0)
  for (i in x){
    expect_false(file.exists(i))
    drake_example(i)
    expect_true(file.exists(i))
    expect_true(file.info(i)$isdir)
    unlink(i, recursive = TRUE, force = TRUE)
  }
  expect_warning(drake_example(destination = getwd()))
  expect_silent(batchtools_drake_tmpl_file("slurm"))
  expect_error(batchtools_drake_tmpl_file("basic"))
})

test_with_dir("overwrites of report.Rmd handled correctly", {
  load_basic_example(overwrite = TRUE)
  load_basic_example(overwrite = FALSE)
  expect_warning(load_basic_example(overwrite = TRUE))
  expect_warning(load_basic_example(to = "a", report_file = "b"))
  expect_true(file.exists("b"))
  expect_false(file.exists("a"))
})
