# library(testthat); library(devtools); load_all()
context("edge-cases")
source("utils.R")

test_that("target conflicts with current import", {
  dclean()
  args = dbug()
  args$plan = rbind(args$plan, data.frame(target = "f", command = "1+1"))
  expect_error(testrun(args)) # f is a function, and it's not found
  dclean()
})

test_that("target conflicts with previous import", {
  dclean()
  args = dbug()
  testrun(args)
  args$plan$command[2] = "g(1+1)"
  args$plan = rbind(args$plan, data.frame(target = "f", command = "1+1"))
  args$targets = args$plan$target
  testrun(args)
  expect_equal(justbuilt(), c("'intermediatefile.rds'", "combined", "f",
    "final", "yourinput"))
  dclean()
})
