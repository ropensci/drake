drake_context("retries and timeouts")

test_with_dir("retries", {
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs
  parallelism <- scenario$parallelism
  e$f <- function(){
    file <- "file.rds"
    if (!file.exists(file)){
      n <- 0
      saveRDS(0, file)
    } else {
      n <- readRDS(file)
      saveRDS(n + 1, file)
    }
    if (n < 5){
      stop("this error is deliberate and expected.")
    }
  }
  pl <- workflow(x = f())
  expect_equal(diagnose(), character(0))
  tmp <- capture.output(capture.output(
    make(
      pl, parallelism = parallelism, jobs = jobs,
      envir = e, verbose = FALSE, retries = 10
    ),
    type = "message"), type = "output"
  )
  expect_true(cached(x))
  expect_equal(diagnose(), "x")
  expect_error(diagnose("notfound"))
  expect_true(inherits(diagnose(x), "error"))
  y <- "x"
  expect_true(inherits(diagnose(y, character_only = TRUE), "error"))
})

# Only use the default parallelism
# because errors are thrown in different
# directions for different modes of parallelism.
# Too much trouble to catch and handle the
# intentional errors in tests.
test_with_dir("timouts", {
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  # From R.utils examples and tests
  foo <- function() {
    print("Tic")
    for (kk in 1:20) {
      print(kk)
      Sys.sleep(0.1)
    }
    print("Tac")
  }
  pl <- data.frame(target = "x", command = "foo()")
  expect_error(
    tmp <- capture.output(capture.output(
      make(
      pl,
      envir = e,
      verbose = TRUE,
      timeout = 1e-3
    ), type = "message"), type = "output")
  )
  expect_false(cached(x))
  expect_error(
    tmp <- capture.output(capture.output(
      make(
      pl,
      envir = e,
      verbose = TRUE,
      timeout = 1e-3,
      retries = 2
    ), type = "message"), type = "output")
  )
  expect_false(cached(x))
  pl <- workflow(x = 1 + 1)
  expect_silent(
    tmp <- capture.output(make(
      pl,
      envir = e,
      verbose = TRUE,
      timeout = 1000
    ), type = "output")
  )
})
