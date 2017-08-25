context("other-features")

test_that("in_progress() works", {
  dclean()
  expect_equal(in_progress(), character(0))
  bad_plan <- plan(x = function_doesnt_exist())
  expect_error(make(bad_plan, verbose = FALSE))
  expect_equal(in_progress(), "x")
  dclean()
})

test_that("missed() works", {
  dclean()
  o <- dbug()
  expect_equal(character(0), missed(o$plan, envir = o$envir,
    verbose = F))
  rm(list = c("f", "g"), envir = o$envir)
  expect_equal(sort(c("f", "g")), sort(missed(o$plan, envir = o$envir,
    verbose = F)))
  dclean()
})

test_that("shell_file() writes correctly", {
  expect_false(file.exists("shell.sh"))
  shell_file()
  expect_true(file.exists("shell.sh"))
  unlink("shell.sh")
  d <- "exdir"
  dir.create(d)
  p <- file.path(d, "script.txt")
  expect_false(file.exists(p))
  shell_file(p)
  expect_true(file.exists(p))
  unlink(d, recursive = TRUE)
})

test_that("deps() correctly reports dependencies of functions and commands", {
  expect_equal(deps(""), character(0))
  expect_equal(length(command_dependencies(NA)), 0)
  expect_equal(length(command_dependencies(NULL)), 0)
  expect_equal(length(command_dependencies(character(0))), 0)
  expect_equal(deps(base::c), character(0))
  expect_equal(deps(base::list), character(0))
  expect_error(deps(NA))
  f <- function(x, y) {
    out <- x + y + g(x)
    saveRDS(out, "out.rds")
  }
  expect_false(is_vectorized(f))
  expect_false(is_vectorized("char"))
  expect_equal(sort(deps(f)), sort(c("g", "saveRDS")))
  my_plan <- plan(
    x = 1 + some_object,
    my_target = x + readRDS("tracked_input_file.rds"),
    return_value = f(x, y, g(z + w)))
  expect_equal(deps(my_plan$command[1]), "some_object")
  expect_equal(sort(deps(my_plan$command[2])),
    sort(c("'tracked_input_file.rds'", "readRDS", "x")))
  expect_equal(sort(deps(my_plan$command[3])), sort(c("f", "g", "w",
    "x", "y", "z")))
})

test_that("tracked() works", {
  dclean()
  config <- dbug()
  x <- sort(tracked(plan = config$plan, envir = config$envir))
  y <- sort(c("'intermediatefile.rds'", "yourinput", "nextone",
    "combined", "myinput", "final", "j", "i", "h", "g", "f",
    "c", "b", "a", "saveRDS", "'input.rds'", "readRDS"))
  expect_equal(x, y)
  x <- sort(tracked(plan = config$plan, targets = "myinput",
    envir = config$envir))
  y <- sort(c("myinput", "'input.rds'", "readRDS"))
  expect_equal(x, y)
  dclean()
})

test_that("mclapply and lapply", {
  dclean()
  config <- dbug()
  make(plan = config$plan, envir = config$envir, verbose = FALSE,
    jobs = 1, parallelism = "mclapply")
  expect_true(is.numeric(readd(final)))
  clean()
  make(plan = config$plan, envir = config$envir, verbose = FALSE,
    jobs = 1, parallelism = "parLapply")
  expect_true(is.numeric(readd(final)))
  dclean()
})

test_that(".onLoad() warns correctly and .onAttach() works", {
  f <- ".RData"
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
  config <- dbug()
  expect_equal(class(build_graph(config$plan)), "igraph")
  pdf(NULL)
  tmp <- plot_graph(plan = config$plan, envir = config$envir,
    verbose = FALSE)
  dev.off()
  unlink("Rplots.pdf")
  expect_true(is.character(default_graph_title(
    parallelism = parallelism_choices()[1], split_columns = FALSE)))
  expect_true(is.character(default_graph_title(
    parallelism = parallelism_choices()[1], split_columns = TRUE)))
  dclean()
})

test_that("console", {
  dclean()
  config <- dbug()
  config$verbose <- TRUE
  expect_output(console(imported = FALSE, target = "myinput",
    config = config))
  expect_output(
    console_parallel_stage(targets = letters, config = config))
  x50 <- paste(rep(0:9, 5), collapse = "")
  x51 <- paste0(x50, 0)
  o1 <- capture.output(console(imported = FALSE, target = x50,
    config = config))
  o2 <- capture.output(console(imported = FALSE, target = x51,
    config = config))
  expect_equal(nchar(o1), nchar(o2), 50)
  dots <- "\\.\\.\\.$"
  expect_false(grepl(dots, o1))
  expect_true(grepl(dots, o2))
  dclean()
})

test_that("check_config() via check() and make()", {
  dclean()
  config <- dbug()
  y <- data.frame(x = 1, y = 2)
  expect_error(check(y, envir = config$envir))
  expect_error(make(y, envir = config$envir))
  y <- data.frame(target = character(0), command = character(0))
  expect_error(check(y, envir = config$envir))
  expect_error(make(y, envir = config$envir))
  expect_error(
    check(config$plan, targets = character(0), envir = config$envir))
  expect_error(
    make(config$plan, targets = character(0), envir = config$envir))
  dclean()
})

test_that("missing files via check()", {
  dclean()
  config <- dbug()
  expect_output(check(config$plan, envir = config$envir))
  expect_silent(tmp <- missing_input_files(config))
  unlink("input.rds")
  expect_warning(
    tmp <- capture.output(check(config$plan, envir = config$envir)))
  expect_warning(tmp <- missing_input_files(config))
  dclean()
})

test_that("deprecation", {
  dclean()
  plan <- data.frame(code = 1, output = "x")
  expect_warning(make(plan, verbose = FALSE))
  dclean()
  expect_warning(make(plan, verbose = FALSE))
  expect_warning(status())
  expect_true(is.numeric(readd(x, search = FALSE)))
  dclean()
})

test_that("targets can be partially specified", {
  dclean()
  config <- dbug()
  config$targets <- "'intermediatefile.rds'"
  testrun(config)
  expect_true(file.exists("intermediatefile.rds"))
  expect_error(readd(final, search = FALSE))
  config$targets <- "final"
  testrun(config)
  expect_true(is.numeric(readd(final, search = FALSE)))
  dclean()
})

test_that("misc stuff", {
  expect_equal(as_file("x"), "'x'")
})

test_that("misc empty/NULL cases", {
  dclean()
  clean(list = "no_cache")
  dclean()
})
