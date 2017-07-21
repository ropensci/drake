# library(testthat); library(devtools); load_all()
context("other-features")

test_that("shell_file() writes correctly", {
  expect_false(file.exists("shell.sh"))
  shell_file()
  expect_true(file.exists("shell.sh"))
  unlink("shell.sh")
  d = "exdir"
  dir.create(d)
  p = file.path(d, "script.txt")
  expect_false(file.exists(p))
  shell_file(p)
  expect_true(file.exists(p))
  unlink(d, recursive = TRUE)
})

test_that("deps() correctly reports dependencies of functions and commands", {
  expect_equal(deps(c), character(0))
  expect_equal(deps(list), character(0))
  f <- function(x, y){
    out <- x + y + g(x)
    saveRDS(out, 'out.rds')
  }
  expect_equal(deps(f), c("g", "saveRDS"))
  my_plan <- plan(
    x = 1 + some_object,
    my_target = x + readRDS('tracked_input_file.rds'),
    return_value = f(x, y, g(z + w))
  )
  expect_equal(deps(my_plan$command[1]), "some_object")
  expect_equal(deps(my_plan$command[2]), 
               c("'tracked_input_file.rds'", "readRDS", "x"))
  expect_equal(deps(my_plan$command[3]),
               c("f", "g", "w", "x", "y", "z"))
})

test_that("tracked() works", {
  dclean()
  config = dbug()
  x = sort(tracked(plan = config$plan, envir = config$envir))
  y = sort(c("'intermediatefile.rds'", "yourinput", "nextone", "combined",
    "myinput", "final", "j", "i", "h", "g", "f", "c", "b", "a", "saveRDS",
    "'input.rds'", "readRDS"))
  expect_equal(x, y)
  x = sort(tracked(plan = config$plan, targets = "myinput", 
    envir = config$envir))
  y = sort(c("myinput", "'input.rds'", "readRDS"))
  expect_equal(x, y)
  dclean()
})

test_that("mclapply and lapply", {
  dclean()
  config = dbug()
  make(plan = config$plan, envir = config$envir,
    verbose = FALSE, jobs = 1, parallelism = "mclapply")  
  expect_true(is.numeric(readd(final)))
  clean()
  make(plan = config$plan, envir = config$envir,
    verbose = FALSE, jobs = 1, parallelism = "parLapply")
  expect_true(is.numeric(readd(final)))
  dclean()
})

test_that(".onLoad() warns correctly and .onAttach() works", {
  f = ".RData"
  expect_false(file.exists(f))
  expect_silent(drake:::.onLoad())
  save.image()
  expect_true(file.exists(f))
  expect_warning(drake:::.onLoad())
  unlink(f)
  set.seed(0)
  expect_true(is.character(drake_tip()))
  expect_silent(suppressPackageStartupMessages(drake:::.onAttach()))
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
  config$verbose = TRUE
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
  expect_warning(status())
  expect_true(is.numeric(readd(x, search = FALSE)))
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
