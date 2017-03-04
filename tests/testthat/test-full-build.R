# library(testthat); library(devtools); load_all()
context("full-build")
source("utils.R")

test_that("scratch build with contained envir.", {
  dclean()
  config = dbug()
  expect_error(session(search = FALSE))
  expect_true(length(status(search = FALSE)) == 0)
  expect_equal(config$cache$list(), character(0))
  testrun(config)
  expect_true(is.numeric(readd(final, search = FALSE)))
  expect_true(length(config$cache$list()) > 2)
  expect_false(any(c("f", "final") %in% ls()))
  expect_true(is.list(session(search = FALSE)))
  expect_true(all(session(search = FALSE)$target %in% config$plan$target))
  
  # changed nothing
  testrun(config)
  nobuild(config)
  
  # take this opportunity to test clean() and prune()
  all = c("'input.rds'", "'intermediatefile.rds'", "a",
    "b", "c", "combined", "f", "final", "g", "h", "i",
    "j", "myinput", "nextone", "readRDS", "saveRDS",
    "yourinput")
  expect_equal(config$cache$list(), all)
  expect_true(file.exists("intermediatefile.rds"))
  expect_true(file.exists("input.rds"))
  expect_true(file.exists(cachepath))
  
  # prune
  expect_warning(prune(config$plan[config$plan$target != "final",]))
  expect_true(file.exists("intermediatefile.rds"))
  expect_true(file.exists("input.rds"))
  expect_equal(config$cache$list(), setdiff(all, "final"))
  
  # clean specific targets
  clean(b, c, list = c("'intermediatefile.rds'", "nextone"), search = FALSE)
  expect_false(file.exists("intermediatefile.rds"))
  expect_true(file.exists("input.rds"))
  expect_equal(config$cache$list(), setdiff(all, 
    c("b", "c", "'intermediatefile.rds'", "nextone", "final")))
  
  # clean does not remove imported files
  expect_true(file.exists("input.rds"))
  expect_true("'input.rds'" %in% config$cache$list())
  clean("'input.rds'", search = FALSE)
  expect_true(file.exists("input.rds"))
  expect_false("'input.rds'" %in% config$cache$list())  
  
  clean(destroy = FALSE, search = FALSE)
  expect_equal(config$cache$list(), character(0))
  expect_false(file.exists("intermediatefile.rds"))
  expect_true(file.exists("input.rds"))
  expect_true(file.exists(cachepath))
  expect_equal(config$cache$list("filemtime"), character(0))
  
  clean(destroy = TRUE, search = FALSE)
  expect_false(file.exists(cachepath))
  clean(destroy = TRUE, search = FALSE)
  clean(destroy = FALSE, search = FALSE)
  dclean()
})

test_that("calling environment is unaffected in scratch build.", {
  dclean()
  config = dbug()
  for(x in ls(config$envir)) assign(x, config$envir[[x]], environment())
  if("obj" %in% ls()) rm(obj)
  obj = ls()
  expect_equal(config$cache$list(), character(0))
  make(config$plan, verbose = FALSE)
  expect_equal(sort(c(obj, "obj")), ls())
  expect_true(length(config$cache$list()) > 0)
  
  # Take this opportunity to test clean() some more.
  expect_true("final" %in% config$cache$list())
  clean(final, search = TRUE)
  expect_false("final" %in% config$cache$list())
  clean(search = TRUE)
  expect_equal(config$cache$list(), character(0))
  expect_true(file.exists(cachepath))
  clean(search = TRUE, destroy = TRUE)
  expect_false(file.exists(cachepath))
  
  dclean()
  expect_false(file.exists("input.rds"))
})
