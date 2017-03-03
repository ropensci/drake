# library(testthat); library(devtools); load_all()
context("full-build")
source("utils.R")

test_that("scratch build with contained envir.", {
  dclean()
  config = dbug()
  expect_error(session())
  expect_error(status())
  expect_equal(cached(), character(0))
  testrun(config)
  expect_true(is.numeric(readd(final)))
  expect_true(length(cached()) > 2)
  expect_false(any(c("f", "final") %in% ls()))
  expect_true(is.list(session()))
  expect_true(all(session()$target %in% config$plan$target))
  
  # changed nothing
  testrun(config)
  nobuild(config)
  
  # take this opportunity to test prune
  all = c("'input.rds'", "'intermediatefile.rds'", "a",
    "b", "c", "combined", "f", "final", "g", "h", "i",
    "j", "myinput", "nextone", "readRDS", "saveRDS",
    "yourinput")
  expect_equal(cached(), all)
  expect_true(file.exists("intermediatefile.rds"))
  expect_true(file.exists("input.rds"))
  expect_true(file.exists(cachepath))
  prune(plan = config$plan, targets = "nextone", 
    envir = config$envir)
  pruned = c("'input.rds'", "b", "c", "g", "h", "i",
    "j", "myinput", "nextone", "readRDS")
  expect_equal(cached(), pruned)
  expect_true(file.exists("input.rds"))
  expect_false(file.exists("intermediatefile.rds"))
  dclean()
})

test_that("calling environment is unaffected in scratch build.", {
  dclean()
  config = dbug()
  for(x in ls(config$envir)) assign(x, config$envir[[x]], environment())
  if("obj" %in% ls()) rm(obj)
  obj = ls()
  expect_equal(cached(), character(0))
  run(config$plan, verbose = FALSE)
  expect_equal(sort(c(obj, "obj")), ls())
  expect_true(length(cached()) > 0)
  
  # test clean: keep the cache and remove the objects
  clean(destroy = FALSE)
  expect_equal(cached(), character(0))
  expect_false(file.exists("intermediatefile.rds"))
  expect_true(file.exists("input.rds"))
  expect_true(file.exists(cachepath))
  expect_equal(config$cache$list("filemtime"), character(0))
  
  # destroy the cache
  clean(destroy = TRUE)
  expect_true(file.exists("input.rds"))
  expect_false(file.exists(cachepath))
  dclean()
  expect_false(file.exists("input.rds"))
})
