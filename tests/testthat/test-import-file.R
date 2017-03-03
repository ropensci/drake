# library(testthat); library(devtools); load_all()
context("import-file")
source("utils.R")

test_that("responses to imported file", {
  dclean()
  args = dbug()
  expect_output(check(plan = args$plan, envir = args$envir))
  expect_error(check(plan = args$plan[-1,], envir = args$envir))
  expect_silent(check(plan = args$plan[c(-1, -6),], envir = args$envir))
  testrun(args)
  expect_true(length(justbuilt()) > 0)
  testrun(args)
  nobuild(args)
  
  # check missing and then replace file exactly as before
  contents = readRDS("input.rds")
  unlink("input.rds")
  expect_error(check(plan = args$plan, envir = args$envir))
  expect_error(testrun(args))
  saveRDS(contents, "input.rds")
  testrun(args)
  nobuild(args)
  final0 = readd(final)
  
  # actually change file
  saveRDS(2:10, "input.rds")
  testrun(args)
  expect_equal(justbuilt(), c("'intermediatefile.rds'",
    "combined", "final", "myinput", "nextone"))
  expect_false(length(final0) == length(readd(final)))
  dclean()
})
