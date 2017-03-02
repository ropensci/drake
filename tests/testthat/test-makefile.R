# library(testthat); library(devtools); load_all()
context("makefile")
source("utils.R")

test_that("basic makefile stuff works", {
  dclean()
  args = dbug()
  run(args$plan, targets = "combined", 
    envir = args$envir, verbose = FALSE)
  args$verbose = FALSE
  run_makefile(args, run = FALSE)
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
  if(R.utils::isPackageLoaded("abind"))
    detach("package:abind")
  if(R.utils::isPackageLoaded("MASS"))
    detach("package:MASS")
  expect_error(abind(1))
  expect_error(deparse(body(lda)))
  
  # Load packages with the 'packages' argument
  args$packages = c("abind", "MASS")
  args$prework = "options(testdrake = 'set')"
  args$plan = plan(x = getOption("testdrake"),
    y = c(abind("option"), deparse(body(lda)), x), 
    strings_in_dots = "literals")
  args$targets = args$plan$target
  expect_false(any(c("x", "y") %in% cached()))
  testrun(args)
  expect_true(all(c("x", "y") %in% cached()))
  expect_equal(readd(x), "set")
  expect_true(length(readd(y)) > 0)
  options(testdrake = original)
  clean()
  
  # load packages the usual way
  options(testdrake = "unset")
  expect_equal(getOption("testdrake"), "unset")
  if(R.utils::isPackageLoaded("abind"))
    detach("package:abind")
  if(R.utils::isPackageLoaded("MASS"))
    detach("package:MASS")
  expect_error(abind(1))
  expect_error(deparse(body(lda)))
  library(abind)
  library(MASS)
  args$packages = NULL
  expect_false(any(c("x", "y") %in% cached()))
  testrun_automatic_packages(args)
  expect_true(all(c("x", "y") %in% cached()))
  expect_equal(readd(x), "set")
  expect_true(length(readd(y)) > 0)
  options(testdrake = original)
  dclean()
})
