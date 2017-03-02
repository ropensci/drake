# library(testthat); library(devtools); load_all()
context("cache")
source("utils.R")

test_that("cache functions work", {
  dclean()
  args = dbug()
  testrun(args)
  
  # find in current directory
  all = c("'input.rds'", "'intermediatefile.rds'", "a",
        "b", "c", "combined", "f", "final", "g", "h", "i",
        "j", "myinput", "nextone", "readRDS", "saveRDS",
        "yourinput")
  expect_true(is.list(session()))
  expect_true(nrow(status()) > 0)
  twopiece = sort(c(built(), imported()))
  expect_equal(cached(), all, twopiece)
  expect_true(all(cached(list = all)))
  expect_equal(length(cached(i, list = imported())), length(imported()))
  expect_equal(cached(i, bla, list = c("final", "run")), 
    c(i = TRUE, bla = FALSE, final = TRUE, run = FALSE))
  expect_equal(find_project(), getwd())
  expect_equal(find_cache(), file.path(getwd(), cachepath))
  expect_true(is.numeric(readd(a)))
  expect_error(h(1))
  loadd(h, i, j, c)
  expect_true(is.numeric(h(1)))
  rm(h, i, j, c)
  expect_error(h(1))
  
  # search from a different directory
  if(!file.exists("searchfrom")){
    dir.create("searchfrom")
    dir.create(file.path("searchfrom", "here"))
  }
  setwd("..")
  s = file.path("testthat", "searchfrom", "here")
  expect_true(is.list(session(search = T, path = s)))
  expect_true(nrow(status(search = T, path = s)) > 0)
  twopiece = sort(c(built(path = s, search = T), 
                  imported(path = s, search = T)))
  expect_equal(cached(path = s, search = T), all, twopiece)
  expect_true(all(cached(list = all, path = s, search = T)))
  expect_equal(find_project(path = s), "testthat")
  expect_equal(find_cache(path = s), 
    file.path("testthat", cachepath))
  expect_true(is.numeric(readd(a, path = s, search = T)))
  expect_error(h(1))
  expect_error(j(1))
  loadd(h, i, j, c, path = s, search = T)
  expect_true(is.numeric(h(1)))
  rm(h, i, j, c)
  expect_error(h(1))
  setwd("testthat")
  unlink("searchfrom", recursive = TRUE)
  dclean()
})
