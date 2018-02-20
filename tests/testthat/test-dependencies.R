drake_context("dependencies")

test_with_dir("unparsable commands are handled correctly", {
  x <- "bluh$"
  expect_false(is_parsable(x))
  expect_error(deps(x))
})

test_with_dir("file_out() and knitr_in(): commands vs imports", {
  cmd <- "file_in(\"x\"); file_out(\"y\"); knitr_in(\"report.Rmd\")"
  f <- function(){
    file_in("x")
    file_out("y")
    knitr_in("report.Rmd")
  }
  file.create("x")
  file.create("y")
  path <- system.file(
    file.path("examples", "basic", "report.Rmd"),
    package = "drake",
    mustWork = TRUE
  )
  file.copy(from = path, to = getwd(), overwrite = TRUE)
  x <- commands_edges("\"y\"", cmd)
  y <- imports_edges("f", f)
  expect_equal(
    sort(x$from),
    sort(
      c("coef_regression2_small", "large",
        "\"report.Rmd\"", "small", "\"x\""
      )
    )
  )
  expect_equal(x$to, rep("\"y\"", 5))
  expect_equal(
    sort(y$from),
    sort(c("\"report.Rmd\"", "\"x\""))
  )
  expect_equal(y$to, rep("f", 2))
  expect_equal(sort(deps(f)), sort(c("\"report.Rmd\"", "\"x\"")))
  expect_equal(
    sort(deps(cmd)),
    sort(
      c("coef_regression2_small", "large",
        "\"report.Rmd\"", "small", "\"x\"", "\"y\""
      )
    )
  )
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
  my_plan <- drake_plan(
    x = 1 + some_object,
    my_target = x + readRDS(file_in("tracked_input_file.rds")),
    return_value = f(x, y, g(z + w)),
    botched = read.csv(file_in(nothing)),
    meta = read.table(file_in("file_in")))
  expect_equal(deps(my_plan$command[1]), "some_object")
  expect_equal(sort(deps(my_plan$command[2])),
    sort(c("\"tracked_input_file.rds\"", "readRDS", "x")))
  expect_equal(sort(deps(my_plan$command[3])), sort(c("f", "g", "w",
    "x", "y", "z")))
  expect_equal(sort(deps(my_plan$command[4])), sort(c("read.csv")))
  expect_equal(sort(deps(my_plan$command[5])),
    sort(c("read.table", "\"file_in\"")))
})

test_with_dir("tracked() works", {
  config <- dbug()
  x <- sort(
    tracked(plan = config$plan, envir = config$envir, verbose = FALSE))
  y <- sort(c("\"intermediatefile.rds\"",
    "yourinput", "nextone",
    "combined", "myinput", "final", "j", "i", "h", "g", "f",
    "c", "b", "a", "saveRDS", "\"input.rds\"", "readRDS"))
  expect_equal(x, y)
  x <- sort(tracked(plan = config$plan, targets = "myinput",
    envir = config$envir, verbose = FALSE))
  y <- sort(c("myinput", "\"input.rds\"", "readRDS"))
  expect_equal(x, y)
})

test_with_dir("missing files via check_plan()", {
  config <- dbug()
  expect_message(check_plan(config$plan, envir = config$envir))
  expect_silent(tmp <- missing_input_files(config))
  unlink("input.rds", force = TRUE)
  expect_warning(
    tmp <- capture.output(check_plan(config$plan, envir = config$envir)))
  expect_warning(tmp <- missing_input_files(config))
})

test_with_dir("Vectorized nested functions work", {
  e <- new.env(parent = globalenv())
  eval(parse(text = "f <- Vectorize(function(x) g(x), \"x\")"),
       envir = e)
  eval(parse(text = "g <- function(x) x + y"), envir = e)
  e$y <- 7
  config <- dbug()
  config$envir <- e
  config$plan <- drake_plan(a = f(1:10))
  config$targets <- "a"
  expect_equal(deps(e$f), "g")
  expect_equal(deps(e$g), "y")

  config <- testrun(config)
  if ("a" %in% ls(config$envir)){
    rm(a, envir = config$envir)
  }
  expect_equal(readd(a), 8:17)
  k <- readd(f)
  expect_equal(k(2:5), 9:12)
  expect_equal(character(0), outdated(config))
  config$envir$y <- 8
  expect_equal("a", outdated(config))

  # Target "a" should react.
  config <- testrun(config)
  expect_equal(character(0), outdated(config))
  expect_equal(readd(a), 9:18)

  # Change a vectorized function and see target "a" react.
  eval(parse(text = "f <- Vectorize(function(x){g(x) + 3}, \"x\")"),
       envir = e)
  config <- testrun(config)
  expect_equal(justbuilt(config), "a")
  expect_equal(readd(a), 12:21)
})
