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
  dclean()
})
