# library(testthat); library(devtools); load_all()
context("full-build")
source("utils.R")

test_that("scratch build with contained envir.", {
  dclean()
  args = dbug()
  expect_error(session())
  expect_equal(nrow(status()), 0)
  expect_equal(cached(), character(0))
  testrun(args)
  expect_true(is.numeric(readd(final)))
  expect_true(length(cached()) > 2)
  expect_false(any(c("f", "final") %in% ls()))
  expect_true(is.list(session()))
  expect_true(all(session()$target %in% args$plan$target))
  
  # changed nothing
  testrun(args)
  nobuild(args)
  dclean()
})

test_that("calling environment is unaffected in scratch build.", {
  dclean()
  args = dbug()
  for(x in ls(args$envir)) assign(x, args$envir[[x]], environment())
  rm(obj)
  obj = ls()
  expect_equal(cached(), character(0))
  run(args$plan, verbose = FALSE)
  expect_equal(sort(c(obj, "obj")), ls())
  dclean()
})
