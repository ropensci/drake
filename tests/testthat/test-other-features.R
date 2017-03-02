# library(testthat); library(devtools); load_all()
context("other-features")
source("utils.R")

test_that(".onLoad() warns correctly", {
  f = ".RData"
  expect_false(file.exists(f))
  expect_silent(drake:::.onLoad())
  save.image()
  expect_true(file.exists(f))
  expect_warning(drake:::.onLoad())
  unlink(f)
})

test_that("graph function empty runthrough throws no errors", {
  dclean()
  args = dbug()
  expect_silent(graph(plan = args$plan, envir = args$envir))
  unlink("Rplots.pdf")
  dclean()
})

test_that("console", {
  dclean()
  args = dbug()
  expect_output(console("build", "myinput", args))
  x50 = paste(rep(0:9, 5), collapse = "")
  x51 = paste0(x50, 0)
  o1 = capture.output(console("build", x50, args))
  o2 = capture.output(console("build", x51, args))
  expect_equal(nchar(o1), nchar(o2), 50)
  dots = "\\.\\.\\.$"
  expect_false(grepl(dots, o1))
  expect_true(grepl(dots, o2))
  dclean()
})

test_that("check_args() via check() and run()", {
  dclean()
  args = dbug()
  y = data.frame(x = 1, y = 2)
  expect_error(check(y, envir = args$envir))
  expect_error(run(y, envir = args$envir))
  y = data.frame(target = character(0), command = character(0))
  expect_error(check(y, envir = args$envir))
  expect_error(run(y, envir = args$envir))
  expect_error(check(args$plan, targets = character(0),
    envir = args$envir))
  expect_error(run(args$plan, targets = character(0), envir = args$envir))
  dclean()
})

test_that("missing files via check()", {
  dclean()
  args = dbug()
  expect_output(check(args$plan, envir = args$envir))
  expect_silent(find_files(args))
  unlink("input.rds")
  expect_error(check(args$plan, envir = args$envir))
  expect_error(find_files(args))
  dclean()
})

test_that("deprecation", {
  dclean()
  plan = data.frame(code = 1, output = "x")
  expect_warning(run(plan, verbose = FALSE))
  dclean()
  expect_warning(make(plan, verbose = FALSE))
  dclean()
})

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

test_that("targets can be partially specified", {
  dclean()
  args = dbug()
  args$targets = "'intermediatefile.rds'"
  testrun(args)
  expect_true(file.exists("intermediatefile.rds"))
  expect_error(readd(final))
  args$targets = "final"
  testrun(args)
  expect_true(is.numeric(readd(final)))
  dclean()
})

test_that("misc stuff", {
  expect_equal(as_file("x"), "'x'")
})

