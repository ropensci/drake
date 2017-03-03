# library(testthat); library(devtools); load_all()
context("command-changes")
source("utils.R")

test_that("changes to commands are handled well", {
  dclean()
  config = dbug()
  testrun(config)
  config$plan$command[2] = "f(1+ 1) # nothing should rebuild"
  testrun(config)
  nobuild(config)
  config$plan$command[2] = "f(1+ 1 -2 + 2) -1 + 1 # only yourinput changed"
  testrun(config)
  expect_equal(justbuilt(), "yourinput")
  config$plan$command[2] = "f(1+2) # now downstream should rebuild"
  testrun(config)
  expect_equal(justbuilt(), 
    c("'intermediatefile.rds'", "combined", "final", "yourinput"))
  testrun(config)
  nobuild(config)
  
  # command changed for an intermediate file
  config$plan$command[1] = "saveRDS(combined + 1, \"intermediatefile.rds\")"
  testrun(config)
  expect_equal(justbuilt(), c("'intermediatefile.rds'", "final"))
  dclean()
})
