# library(testthat); library(devtools); load_all()
context("plan")

test_that("plan set 1", {
  x = plan(a = c, b = "c", list = c(c = "d", d = "readRDS('e')"))
  y = data.frame(target = letters[1:4], 
    command = c("c", "'c'", "d", "readRDS('e')"), 
    stringsAsFactors = F)
  expect_equal(x, y)
})  

test_that("plan set 2", {
  x = plan(a = c, b = "c", list = c(c = "d", d = "readRDS('e')"),
      strings_in_dots = "literals")
  y = data.frame(target = letters[1:4], 
                 command = c("c", "\"c\"", "d", "readRDS('e')"), 
                 stringsAsFactors = F)
  expect_equal(x, y)
})  

test_that("plan set 3", {
  x = plan(a = c, b = "c", list = c(c = "d", d = "readRDS('e')"),
           strings_in_dots = "literals", file_targets = TRUE)
  y = data.frame(target = eply::quotes(letters[1:4], single = TRUE), 
                 command = c("c", "\"c\"", "d", "readRDS('e')"), 
                 stringsAsFactors = F)
  expect_equal(x, y)
})

test_that("plan set 4", {
  x = plan(a = c, b = "c", list = c(c = "d", d = "readRDS('e')"),
           strings_in_dots = "filenames", file_targets = TRUE)
  y = data.frame(target = eply::quotes(letters[1:4], single = TRUE), 
                 command = c("c", "'c'", "d", "readRDS('e')"), 
                 stringsAsFactors = F)
  expect_equal(x, y)
  expect_error(check(x))
  dclean()
})

test_that("plan() trims outer whitespace in target names", {
  x = plan(list = c(" a" = 1, "b \t\n" = 2))
  y = x
  y$output = y$target
  y$target = NULL
  z = plan(a = 1, b = 2)
  expect_equal(x$target, y$output, z$target)
})

test_that("make() trims outer whitespace in target names", {
  dclean()
  x = data.frame(target = c("a\n", "  b", "c ", "\t  d   "), command = 1)
  make(x, verbose = FALSE)
  expect_equal(sort(cached()), letters[1:4])
  stat = c(a = "finished", b = "finished", c = "finished", d = "finished")
  expect_equal(status(), stat)
  dclean()
})

test_that("make() plays nicely with tibbles", {
  dclean()
  x = tibble::tribble(~target, ~command, "a", 1)
  expect_silent(check(x))
  expect_silent(make(x, verbose = FALSE))
  dclean()
})
