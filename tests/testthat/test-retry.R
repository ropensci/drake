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
  make(
    pl, parallelism = parallelism, jobs = jobs,
    envir = e, verbose = FALSE, retries = 10,
    hook = function(code){
      withr::with_message_sink("sink.txt", code)
    }
  )
  expect_true(cached(x))
  expect_equal(diagnose(), "x")
  expect_error(diagnose("notfound"))
  expect_true(inherits(diagnose(x), "error"))
  y <- "x"
  expect_true(inherits(diagnose(y, character_only = TRUE), "error"))
})

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
    tmp <- capture.output(
      make(
        pl,
        envir = e,
        verbose = TRUE,
        timeout = 1e-3,
        hook = function(code){
          withr::with_output_sink("out.txt",
            withr::with_message_sink("err.txt", code)
          )
        }
      )
    )
  )
  expect_false(cached(x))
  expect_error(
    tmp <- capture.output(
      make(
        pl,
        envir = e,
        verbose = TRUE,
        timeout = 1e-3,
        retries = 2,
        hook = function(code){
          withr::with_output_sink("out.txt",
            withr::with_message_sink("err.txt", code)
          )
        }
      )
    )
  )
  expect_false(cached(x))
  pl <- workflow(x = 1 + 1)
  expect_silent(
    tmp <- capture.output(
      make(
        pl,
        envir = e,
        verbose = TRUE,
        timeout = 1000
      )
    )
  )
})
