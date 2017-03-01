 # library(testthat); library(devtools); load_all()
context("importobject")
source("utils.R")

test_that("responses to imported objects and functions", {
  dclean()
  args = dbug()
  testrun(args)
  args$envir$c = args$envir$c + 1
  testrun(args)
  expect_equal(justbuilt(), setdiff(sort(args$plan$target), "myinput"))
  dclean()
})
