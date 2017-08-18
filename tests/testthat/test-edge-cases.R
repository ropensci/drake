context("edge-cases")

test_that("graph does not fail if input file is binary", {
  dclean()
  x <- plan(y = readRDS("input.rds"))
  saveRDS(as.list(mtcars), "input.rds")
  expect_silent(out <- plot_graph(x, verbose = FALSE))
  unlink("input.rds")
  dclean()
})

test_that("different graphical arrangements for Makefile parallelism", {
  dclean()
  e <- new.env()
  x <- plan(a = 1, b = f(2))
  e$f <- function(x) x
  con <- config(x, envir = e, verbose = FALSE)
  expect_equal(1, max_useful_jobs(x, envir = e, config = con,
    parallelism = "mclapply", jobs = 1))
  expect_equal(1, max_useful_jobs(x, envir = e, config = con,
    parallelism = "parLapply", jobs = 1))
  expect_equal(2, max_useful_jobs(x, envir = e, config = con,
    parallelism = "Makefile", jobs = 1))
  dclean()
})

test_that("Vectorized nested functions work", {
  dclean()
  e <- new.env(parent = globalenv())
  eval(parse(text = "f <- Vectorize(function(x) g(x), \"x\")"),
    envir = e)
  eval(parse(text = "g <- function(x) x + y"), envir = e)
  e$y <- 7
  config <- dbug()
  config$envir <- e
  config$plan <- plan(a = f(1:10))
  config$targets <- "a"
  expect_equal(deps(e$f), "g")
  expect_equal(deps(e$g), "y")
  testrun(config)
  expect_equal(readd(a), 8:17)
  k <- readd(f)
  expect_equal(k(2:5), 9:12)
  expect_equal(character(0), outdated(config$plan, envir = config$envir,
    verbose = FALSE))
  config$envir$y <- 8
  expect_equal("a", outdated(config$plan, envir = config$envir,
    verbose = FALSE))
  testrun(config)
  expect_equal(character(0), outdated(config$plan, envir = config$envir,
    verbose = FALSE))
  dclean()
})

test_that("stringsAsFactors can be TRUE", {
  dclean()
  f <- function(x) {
    return(x)
  }
  myplan <- data.frame(target = "a", command = "f(\"helloworld\")",
    stringsAsFactors = TRUE)
  expect_true(is.factor(myplan$target))
  expect_true(is.factor(myplan$command))
  make(myplan, verbose = FALSE)
  expect_equal(readd(a), "helloworld")
  dclean()
})

test_that("circular non-DAG workflows quit in error", {
  dclean()
  p <- plan(a = b, b = c, c = a)
  expect_error(check(p))
  expect_error(make(p))
  dclean()
})

# Target/import conflicts are unpredictable. A warning should
# be enough.
test_that("target conflicts with current import or another target", {
  dclean()
  config <- dbug()
  config$plan <- rbind(config$plan, data.frame(target = "f",
    command = "1+1"))
  expect_silent(tmp <- capture.output(check(plan = config$plan,
    envir = config$envir)))
  config$plan$target <- "repeated"
  expect_error(check(plan = config$plan))
})

test_that("target conflicts with previous import", {
  dclean()
  config <- dbug()
  testrun(config)
  config$plan$command[2] <- "g(1+1)"
  config$plan <- rbind(config$plan, data.frame(target = "f",
    command = "1+1"))
  config$targets <- config$plan$target
  testrun(config)
  expect_equal(justbuilt(config), sort(c("'intermediatefile.rds'",
    "combined", "f", "final", "yourinput")))
  dclean()
})

test_that("can use semicolons and multi-line commands", {
  dclean()
  plan <- plan(list = c(x = "a<-1; a", y = "b<-2\nb"))
  make(plan, verbose = FALSE)
  expect_false(any(c("a", "b") %in% ls()))
  expect_true(all(cached(x, y, search = FALSE)))
  expect_equal(cached(search = FALSE), c("x", "y"))
  dclean()
})

test_that("true targets can be functions", {
  dclean()
  generator <- function() return(function(x) {
    x + 1
  })
  plan <- plan(myfunction = generator(), output = myfunction(1))
  config <- make(plan, verbose = FALSE, return_config = TRUE)
  expect_equal(readd(output), 2)
  expect_true(is.list(config$cache$get("myfunction")))
  myfunction <- readd(myfunction)
  expect_equal(myfunction(4), 5)
  dclean()
})
