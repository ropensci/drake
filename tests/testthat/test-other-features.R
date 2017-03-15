# library(testthat); library(devtools); load_all()
context("other-features")

test_that("mclapply and lapply", {
  dclean()
  config = dbug()
  config$jobs = 1
  config$parallelism = "mclapply"
  config$verbose = FALSE
  testrun(config)
  expect_true(is.numeric(readd(final)))
  clean()
  config$parallelism = "parLapply"
  testrun(config) # runs run_lapply since jobs == 1
  expect_true(is.numeric(readd(final)))
  dclean()
})

test_that(".onLoad() warns correctly", {
  f = ".RData"
  expect_false(file.exists(f))
  expect_silent(drake:::.onLoad())
  save.image()
  expect_true(file.exists(f))
  expect_warning(drake:::.onLoad())
  unlink(f)
})

test_that("graph functions work", {
  dclean()
  config = dbug()
  expect_equal(class(build_graph(config$plan)), "igraph")
  pdf(NULL)
  expect_silent(plot_graph(plan = config$plan, envir = config$envir))
  dev.off()
  unlink("Rplots.pdf")
  dclean()
})

test_that("console", {
  dclean()
  config = dbug()
  expect_output(console(imported = FALSE, 
    target = "myinput", config = config))
  x50 = paste(rep(0:9, 5), collapse = "")
  x51 = paste0(x50, 0)
  o1 = capture.output(console(imported = FALSE, 
    target = x50, config = config))
  o2 = capture.output(console(imported = FALSE, 
    target = x51, config = config))
  expect_equal(nchar(o1), nchar(o2), 50)
  dots = "\\.\\.\\.$"
  expect_false(grepl(dots, o1))
  expect_true(grepl(dots, o2))
  dclean()
})

test_that("check_config() via check() and make()", {
  dclean()
  config = dbug()
  y = data.frame(x = 1, y = 2)
  expect_error(check(y, envir = config$envir))
  expect_error(make(y, envir = config$envir))
  y = data.frame(target = character(0), command = character(0))
  expect_error(check(y, envir = config$envir))
  expect_error(make(y, envir = config$envir))
  expect_error(check(config$plan, targets = character(0),
    envir = config$envir))
  expect_error(make(config$plan, targets = character(0), 
    envir = config$envir))
  dclean()
})

test_that("missing files via check()", {
  dclean()
  config = dbug()
  expect_output(check(config$plan, envir = config$envir))
  expect_silent(find_files(config))
  unlink("input.rds")
  expect_error(check(config$plan, envir = config$envir))
  expect_error(find_files(config))
  dclean()
})

test_that("deprecation", {
  dclean()
  plan = data.frame(code = 1, output = "x")
  expect_warning(make(plan, verbose = FALSE))
  dclean()
  expect_warning(make(plan, verbose = FALSE))
  expect_true(is.numeric(readd(x, search = FALSE)))
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
  config = dbug()
  config$targets = "'intermediatefile.rds'"
  testrun(config)
  expect_true(file.exists("intermediatefile.rds"))
  expect_error(readd(final, search = FALSE))
  config$targets = "final"
  testrun(config)
  expect_true(is.numeric(readd(final, search = FALSE)))
  dclean()
})

test_that("misc stuff", {
  expect_equal(as_file("x"), "'x'")
})
