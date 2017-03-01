# library(testthat); library(devtools); load_all()
context("command-changes")
source("utils.R")

test_that("changes to commands are handled well", {
  dclean()
  args = dbug()
  testrun(args)
  args$plan$command[2] = "f(1+ 1) # nothing should rebuild"
  testrun(args)
  nobuild(args)
  args$plan$command[2] = "f(1+ 1 -2 + 2) -1 + 1 # only this target should rebuild"
  testrun(args)
  expect_equal(justbuilt(), "yourinput")
  args$plan$command[2] = "f(1+2) # now, everything downstream should rebuild"
  testrun(args)
  expect_equal(justbuilt(), 
    c("'intermediatefile.rds'", "combined", "final", "yourinput"))
  testrun(args)
  nobuild(args)
  
  # command changed for an intermediate file
  args$plan$command[1] = "saveRDS(combined + 1, \"intermediatefile.rds\")"
  testrun(args)
  expect_equal(justbuilt(), c("'intermediatefile.rds'", "final"))
  dclean()
})
