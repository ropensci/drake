drake_context("retries and timeouts")

test_with_dir("retries", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs
  parallelism <- scenario$parallelism
  e$f <- function() {
    file <- "file.rds"
    if (!file.exists(file)) {
      n <- 0
      saveRDS(0, file)
    } else {
      n <- readRDS(file)
      saveRDS(n + 1, file)
    }
    if (n < 5) {
      file.create("failed_once.txt")
      stop("this error is deliberate and expected.")
    }
  }

  pl <- drake_plan(x = f())
  expect_equal(diagnose(), character(0))

  debrief_retries <- function() {
    expect_true(file.exists("failed_once.txt"))
    expect_true("x" %in% cached())
    expect_null(diagnose(x)$error)
  }

  make(
    pl, parallelism = parallelism, jobs = jobs,
    envir = e, retries = 10, verbose = FALSE,
    session_info = FALSE
  )
  debrief_retries()

  # The 'retries' in the workflow plan should override
  # the 'retries' argument to make().
  clean(destroy = TRUE)
  unlink(c("failed_once.txt", "file.rds"))
  pl$retries <- 10
  make(
    pl, parallelism = parallelism, jobs = jobs,
    envir = e, retries = 0, verbose = FALSE,
    session_info = FALSE
  )
  debrief_retries()

  # And the `retries` argument to `make()` should step in
  # wherever plan$retries has NAs
  clean(destroy = TRUE)
  unlink(c("failed_once.txt", "file.rds"))
  pl$retries <- NA
  make(
    pl, parallelism = parallelism, jobs = jobs,
    envir = e, retries = 10, verbose = FALSE,
    session_info = FALSE
  )
  debrief_retries()
})

test_with_dir("timeouts", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs
  parallelism <- scenario$parallelism
  pl <- data.frame(target = "x", command = "Sys.sleep(0.25)")

  # Should have no errors.
  tmp <- capture.output(
    make(
      pl,
      envir = e,
      verbose = FALSE,
      session_info = FALSE
    )
  )
  expect_true("x" %in% cached())

  # Should time out.
  clean()
  expect_error(
    tmp <- capture.output(
      make(
        pl,
        envir = e,
        verbose = FALSE,
        elapsed = 1e-3,
        retries = 2,
        session_info = FALSE
      )
    )
  )
  expect_false(cached(x))

  # Should time out too. The workflow plan should override
  # the arguments to make().
  # CPU time should be similar, but testing it is elusive.
  for (field in c("elapsed")) {
    clean()
    pl2 <- pl
    pl2[[field]] <- 1e-3
    args <- list(
      plan = pl2,
      envir = e,
      verbose = FALSE,
      retries = 2
    )
    args[[field]] <- Inf
    expect_error(
      tmp <- capture.output(
        do.call(what = make, args = args)
      )
    )
    expect_false(cached(x))
  }
})
