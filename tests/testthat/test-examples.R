# library(testthat); library(devtools); load_all()
context("examples")

test_that("examples are listed and written", {
  dclean()
  x = examples_drake()
  expect_true(is.character(x) & length(x) > 0)
  for(i in x){
    expect_false(file.exists(i))
    example_drake(i)
    expect_true(file.exists(i))
    expect_true(file.info(i)$isdir)
    unlink(i, recursive = TRUE)
  }
  dclean()
})
