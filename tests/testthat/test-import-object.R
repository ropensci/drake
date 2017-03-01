 # library(testthat); library(devtools); load_all()
context("import-object")
source("utils.R")

test_that("responses to imported objects and functions", {
  dclean()
  args = dbug()
  testrun(args)
  
  # change imported object
  args$envir$c = args$envir$c + 1
  testrun(args)
  expect_equal(justbuilt(), setdiff(sort(args$plan$target), "myinput"))

  # change nested function trivially
  args$envir$g = function(y){
    
    h(  y)+b # comment
  }
  testrun(args)
  nobuild(args)
  
  # change nested function so that it gives the same answer
  args$envir$g = function(y){
    h(y)+b + 1-1 - 0
  }
  testrun(args)
  expect_equal(justbuilt(), c("nextone", "yourinput"))
  
  # nested function gives different answer
  args$envir$g = function(y){
    h(y)+b + 16
  }
  testrun(args)
  expect_true("final" %in% justbuilt())

  # test a deeper nested function
  args$envir$i = function(x){
    2*x + sqrt(13)
  }
  testrun(args)
  expect_true("final" %in% justbuilt())
  dclean()
})
