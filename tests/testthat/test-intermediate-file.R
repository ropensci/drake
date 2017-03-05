# library(testthat); library(devtools); load_all()
context("intermediate-file")

test_that("responses to intermediate file", {
  dclean()
  config = dbug()
  testrun(config)
  
  # check missing and then replace file exactly as before
  final0 = readd(final, search = FALSE)
  val = readRDS("intermediatefile.rds")
  unlink("intermediatefile.rds")
  saveRDS(val, "intermediatefile.rds")
  testrun(config)
  nobuild(config)
  expect_equal(final0, readd(final, search = FALSE))
  
  # actually change file
  saveRDS(val + 1, "intermediatefile.rds")
  testrun(config)
  expect_equal(justbuilt(config), "'intermediatefile.rds'")
  expect_equal(final0, readd(final, search = FALSE))
  
  # break the intermediate file
  unlink("intermediatefile.rds")
  testrun(config)
  expect_equal(justbuilt(config), "'intermediatefile.rds'")
  expect_equal(final0, readd(final, search = FALSE))
  dclean()
})
