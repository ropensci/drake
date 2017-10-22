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
      stop("Function f failed.")
    }
  }
  pl <- plan(x = f())
  tmp <- "test-retry-log.txt"
  if (!file.exists(tmp)){
    file.create(tmp)
  }
  on.exit({
    suppressWarnings(sink(type = "output"))
    suppressWarnings(sink(type = "message"))
  })
  make(
    pl, parallelism = parallelism, jobs = jobs,
    envir = e, verbose = FALSE, retries = 10,
    prework = paste0(
      "sink(file = \"", tmp,
      "\"); sink(file = stdout(), type = \"message\")"
    )
  )
  expect_true(cached(x))
})

# Only use the default parallelism
# because errors are thrown in different
# directions for different modes of parallelism.
# Too much trouble to catch and handle the
# intentional errors in tests.
test_with_dir("timouts", {
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  pl <- data.frame(target = "x", command = "Sys.sleep(0.25)")
  expect_error(
    tmp <- capture.output(capture.output(make(
      pl,
      envir = e,
      verbose = TRUE,
      timeout = 1e-3
    ), type = "message"), type = "output")
  )
  expect_error(
    tmp <- capture.output(capture.output(make(
      pl,
      envir = e,
      verbose = TRUE,
      timeout = 1e-3,
      retries = 2
    ), type = "message"), type = "output")
  )
  pl <- drake::plan(x = 1 + 1)
  expect_silent(
    tmp <- capture.output(make(
      pl,
      envir = e,
      verbose = TRUE,
      timeout = 1000
    ), type = "output")
  )
})
