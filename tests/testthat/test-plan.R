# library(testthat); library(devtools); load_all()
context("plan")

test_that("plan", {
  dclean()
  
  x = plan(a = c, b = "c", list = c(c = "d", d = "readRDS('e')"))
  y = data.frame(target = letters[1:4], 
    command = c("c", "'c'", "d", "readRDS('e')"), 
    stringsAsFactors = F)
  expect_equal(x, y)
  
  x = plan(a = c, b = "c", list = c(c = "d", d = "readRDS('e')"),
      strings_in_dots = "literals")
  y = data.frame(target = letters[1:4], 
                 command = c("c", "\"c\"", "d", "readRDS('e')"), 
                 stringsAsFactors = F)
  expect_equal(x, y)
  
  x = plan(a = c, b = "c", list = c(c = "d", d = "readRDS('e')"),
           strings_in_dots = "literals", file_targets = TRUE)
  y = data.frame(target = eply::quotes(letters[1:4], single = TRUE), 
                 command = c("c", "\"c\"", "d", "readRDS('e')"), 
                 stringsAsFactors = F)
  expect_equal(x, y)
  
  x = plan(a = c, b = "c", list = c(c = "d", d = "readRDS('e')"),
           strings_in_dots = "filenames", file_targets = TRUE)
  y = data.frame(target = eply::quotes(letters[1:4], single = TRUE), 
                 command = c("c", "'c'", "d", "readRDS('e')"), 
                 stringsAsFactors = F)
  expect_equal(x, y)
  expect_error(check(x))

  dclean()
})
