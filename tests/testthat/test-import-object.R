 # library(testthat); library(devtools); load_all()
context("import-object")
source("utils.R")

test_that("responses to imported objects and functions", {
  dclean()
  config = dbug()
  testrun(config)
  
  # change imported object
  config$envir$c = config$envir$c + 1
  testrun(config)
  expect_equal(justbuilt(config), setdiff(sort(config$plan$target), "myinput"))

  # change nested function trivially
  config$envir$g = function(y){
    
    h(  y)+b # comment
  }
  testrun(config)
  nobuild(config)
  
  # change nested function so that it gives the same answer
  config$envir$g = function(y){
    h(y)+b + 1-1 - 0
  }
  testrun(config)
  expect_equal(justbuilt(config), c("nextone", "yourinput"))
  
  # nested function gives different answer
  config$envir$g = function(y){
    h(y)+b + 16
  }
  testrun(config)
  expect_true("final" %in% justbuilt(config))

  # test a deeper nested function
  config$envir$i = function(x){
    2*x + sqrt(13)
  }
  testrun(config)
  expect_true("final" %in% justbuilt(config))
  
  # command depends on imported object k
  expect_false("k" %in% ls())
  config$plan$command[2] = "f(1+1) + k"
  expect_error(testrun(config))
  
  # set k
  config$envir$k = 5
  testrun(config)
  final0 = readd(final, search = FALSE)
  builds = c("'intermediatefile.rds'", "combined", "final", "yourinput")
  expect_equal(justbuilt(config), builds)

  # nothing to do
  testrun(config)
  nobuild(config)
  expect_true(identical(final0, readd(final, search = FALSE)))
  
  # change k
  config$envir$k = 10
  testrun(config)
  expect_equal(justbuilt(config), builds)
  expect_false(identical(final0, readd(final, search = FALSE)))
  dclean()
})
