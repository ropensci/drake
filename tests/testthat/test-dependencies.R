drake_context("dependencies")

test_with_dir("unparsable commands are handled correctly", {
  x <- "bluh$"
  expect_false(is_parsable(x))
  expect_error(deps(x))
})

test_with_dir(
  "deps() correctly reports dependencies of functions and commands", {
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

  load_basic_example()
  expect_equal(sort(deps("'report.Rmd'")), sort(c(
    "coef_regression2_small", "large", "small"
  )))
  f <- function(x){
    knit(x)
  }
  expect_equal(deps(f), "knit")
})

test_with_dir("tracked() works", {
  config <- dbug()
  x <- sort(tracked(plan = config$plan, envir = config$envir))
  y <- sort(c("'intermediatefile.rds'",
    "yourinput", "nextone",
    "combined", "myinput", "final", "j", "i", "h", "g", "f",
    "c", "b", "a", "saveRDS", "'input.rds'", "readRDS"))
  expect_equal(x, y)
  x <- sort(tracked(plan = config$plan, targets = "myinput",
    envir = config$envir))
  y <- sort(c("myinput", "'input.rds'", "readRDS"))
  expect_equal(x, y)
})

test_with_dir("missing files via check()", {
  config <- dbug()
  expect_output(check(config$plan, envir = config$envir))
  expect_silent(tmp <- missing_input_files(config))
  unlink("input.rds", force = TRUE)
  expect_warning(
    tmp <- capture.output(check(config$plan, envir = config$envir)))
  expect_warning(tmp <- missing_input_files(config))
})
