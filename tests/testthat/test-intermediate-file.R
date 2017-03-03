# library(testthat); library(devtools); load_all()
context("intermediate-file")
source("utils.R")

test_that("responses to intermediate file", {
  dclean()
  config = dbug()
  testrun(config)
  
  # check missing and then replace file exactly as before
  final0 = readd(final)
  val = readRDS("intermediatefile.rds")
  unlink("intermediatefile.rds")
  saveRDS(val, "intermediatefile.rds")
  testrun(config)
  nobuild(config)
  expect_equal(final0, readd(final))
  
  # actually change file
  saveRDS(val + 1, "intermediatefile.rds")
  testrun(config)
  expect_equal(justbuilt(), "'intermediatefile.rds'")
  expect_equal(final0, readd(final))
  
  # break the intermediate file
  unlink("intermediatefile.rds")
  testrun(config)
  expect_equal(justbuilt(), "'intermediatefile.rds'")
  expect_equal(final0, readd(final))
  dclean()
})
