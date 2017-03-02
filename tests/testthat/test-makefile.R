# library(testthat); library(devtools); load_all()
context("makefile")
source("utils.R")

test_that("plan", {
  dclean()
  args = dbug()
  run(args$plan, targets = "combined", 
    envir = args$envir, verbose = FALSE)
  args$verbose = FALSE
  run_makefile(args, T)
  stamps = list.files(file.path(timestampdir))
  expect_equal(stamps, c("combined", "myinput", "nextone", 
    "yourinput"))
  dclean()
})
