# library(testthat); library(devtools); load_all()
context("importobject")

test_that("responses to imported objects and functions", {
  dclean()
  args = dbug()
  run(args$plan, envir = args$envir, verbose = F)
  args$envir$c = args$envir$c + 1
  run(args$plan, envir = args$envir, verbose = F)
  dclean()
})
