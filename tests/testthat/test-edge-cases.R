# library(testthat); library(devtools); load_all()
context("edge-cases")
source("utils.R")

test_that("target conflicts with current import", {
  dclean()
  args = dbug()
  args$plan = rbind(args$plan, data.frame(target = "f", command = "1+1"))
  suppressWarnings( # for jobs > 1, mclapply gives a warning too. 
    tryCatch(testrun(args), # desired error doesn't go to R with Makefiles
      error = function(e){}))
  expect_true("final" %in% args$targets)
  expect_false("final" %in% cached()) # this should be enough
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
