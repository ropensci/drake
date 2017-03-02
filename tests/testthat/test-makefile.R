# library(testthat); library(devtools); load_all()
context("makefile")
source("utils.R")

test_that("basic makefile stuff works", {
  dclean()
  args = dbug()
  run(args$plan, targets = "combined", 
    envir = args$envir, verbose = FALSE)
  args$verbose = FALSE
  run_makefile(args, T)
  expect_true(file.exists("Makefile"))
  stamps = list.files(file.path(timestampdir))
  expect_equal(stamps, c("combined", "myinput", "nextone", 
    "yourinput"))
  expect_false(file.exists("intermediatefile.rds"))
  mk("'intermediatefile.rds'")
  expect_true(file.exists("intermediatefile.rds"))
  dclean()
  expect_false(file.exists("Makefile"))
})

test_that("packages are loaded in prework", {
  dclean()
  original = getOption("testdrake")
  options(testdrake = "unset")
  expect_equal(getOption("testdrake"), "unset")
  args = dbug()
  args$packages = c("eply", "digest")
  args$prework = "options(testdrake = 'set')"
  args$plan = plan(x = quotes(getOption("testdrake"), single = TRUE),
    y = c(digest("option"), x), strings_in_dots = "literals")
  args$targets = args$plan$target
  expect_false(any(c("x", "y") %in% cached()))
  testrun(args)
  expect_true(all(c("x", "y") %in% cached()))
  expect_equal(readd(x), "'set'")
  options(testdrake = original)
  dclean()
})
