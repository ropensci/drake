# library(testthat); library(devtools); load_all()
context("Makefile")
source("utils.R")

test_that("prepend arg works", {
  dclean()
  config = dbug()
  config$verbose = FALSE
  config$prepend = "# add"
  run_Makefile(config, run = FALSE)
  lines = readLines("Makefile")
  expect_true(grepl("# add", lines[1]))
  dclean()
})

test_that("basic Makefile stuff works", {
  dclean()
  config = dbug()
  make(config$plan, targets = "combined", 
    envir = config$envir, verbose = FALSE)
  config$verbose = FALSE
  run_Makefile(config, run = FALSE)
  expect_true(file.exists("Makefile"))
  stamps = list.files(file.path(time_stamp_dir))
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
  config = dbug()
  if(R.utils::isPackageLoaded("abind"))
    detach("package:abind")
  if(R.utils::isPackageLoaded("MASS"))
    detach("package:MASS")
  expect_error(abind(1))
  expect_error(deparse(body(lda)))
  
  # Load packages with the 'packages' argument
  config$packages = c("abind", "MASS")
  config$prework = "options(testdrake = 'set')"
  config$plan = plan(x = getOption("testdrake"),
    y = c(abind("option"), deparse(body(lda)), x), 
    strings_in_dots = "literals")
  config$targets = config$plan$target
  expect_false(any(c("x", "y") %in% config$cache$list()))
  testrun(config)
  expect_true(all(c("x", "y") %in% config$cache$list()))
  expect_equal(readd(x, search = FALSE), "set")
  expect_true(length(readd(y, search = FALSE)) > 0)
  options(testdrake = original)
  clean(search = FALSE)
  
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
  config$packages = NULL
  expect_false(any(c("x", "y") %in% config$cache$list()))
  testrun_automatic_packages(config)
  expect_true(all(c("x", "y") %in% config$cache$list()))
  expect_equal(readd(x, search = FALSE), "set")
  expect_true(length(readd(y, search = FALSE)) > 0)
  options(testdrake = original)
  dclean()
})
