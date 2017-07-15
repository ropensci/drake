# library(testthat); library(devtools); load_all()
context("examples")

test_that("examples are listed and written", {
  dclean()
  x = examples_drake()
  expect_true(is.character(x) & length(x) > 0)
  for(i in x){
    expect_false(file.exists(i))
    example_drake(i)
    expect_true(file.exists(i))
    expect_true(file.info(i)$isdir)
    unlink(i, recursive = TRUE)
  }
  dclean()
})

test_that("basic example loads and runs", {
  e = new.env()
  load_basic_example(envir = e)
  make(plan = e$my_plan, envir = e, jobs = testopts()$jobs, 
       parallelism = testopts()$parallelism, verbose = FALSE)
  expect_true(file.exists("report.md"))
  expect_true(cached("'report.md'"))
  clean(destroy = TRUE)
  unlink("report.Rmd")
})
