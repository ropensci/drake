drake_context("console")

test_with_dir("crop_lines() crops lines", {
  expect_equal(length(crop_lines(letters, n = 10)), 10)
})

test_with_dir("multiline message cap", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  n <- 100
  x1 <- "aldksjf"
  x2 <- rep(x1, n)
  y1 <- capture.output(multiline_message(x1))
  y2 <- capture.output(multiline_message(x2))
  dots <- "\\.\\.\\."
  expect_false(grepl(dots, y1))
  expect_true(grepl(dots, y2))
  expect_true(nchar(y2) < nchar(x1) * n)
})

test_with_dir("console to file", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("knitr")
  load_mtcars_example()
  cache <- storr::storr_environment()
  expect_false(file.exists("log.txt"))
  tmp <- capture.output({
      make(
        my_plan, cache = cache, verbose = 1L, session_info = FALSE,
        log_make = "log.txt"
      )
      make(
        my_plan, cache = cache, verbose = 1L, session_info = FALSE,
        log_make = "log.txt"
      )
      make(
        my_plan, cache = cache, verbose = 1L, session_info = FALSE,
        trigger = trigger(condition = TRUE), log_make = "log.txt"
      )
    },
    type = "message"
  )
  expect_equal(tmp, character(0))
  expect_true(length(readLines("log.txt")) > 0)
})

test_with_dir("drake_warning() and drake_error()", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  plan <- drake_plan(
    x = {
      message("some_message")
      warning("some_warning")
      stop("some_error")
    }
  )
  cache <- storr::storr_environment()
  expect_error(
    expect_warning(
      make(
        plan, cache = cache,
        session_info = FALSE,
        verbose = 1L
      ),
      regexp = "some_warning"
    ),
    regexp = "some_error"
  )
  o <- diagnose(x, cache = cache)
  expect_equal(o$warnings, "some_warning")
  expect_equal(o$messages, "some_message")
  expect_equal(o$error$message, "some_error")
  expect_true(any(grepl("some_error", as.character(unlist(o$error$calls)))))
  expect_false(file.exists("log.txt"))
  expect_error(expect_warning(make(
    plan, log_make = "log.txt",
    cache = storr::storr_environment(),
    session_info = FALSE
  )))
  x <- readLines("log.txt")
  expect_true(any(grepl("some_message", x)))
  expect_true(any(grepl("some_warning", x)))
  expect_true(any(grepl("some_error", x)))
})

test_with_dir("show_source()", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  plan <- drake_plan(x = base::sqrt(15))
  cache <- storr::storr_environment()
  make(plan, cache = cache, session_info = FALSE)
  config <- drake_config(plan, cache = cache)
  expect_message(show_source(x, config), regexp = "command")
  expect_message(
    show_source("base::sqrt", config, character_only = TRUE), regexp = "import"
  )
  expect_message(
    loadd(x, cache = cache, show_source = TRUE),
    regexp = "was built from command"
  )
  expect_message(
    y <- readd(x, cache = cache, show_source = TRUE),
    regexp = "was built from command"
  )
  expect_true(is.numeric(x))
  expect_true(is.numeric(y))
})

test_with_dir("progress bar does not break things", {
  skip_on_cran()
  plan <- drake_plan(
    x = target(
      1,
      transform = map(y = !!seq_len(2))
    )
  )
  make(plan, verbose = 2)
  plan <- drake_plan(
    x = target(
      1,
      transform = map(y = !!seq_len(2))
    ),
    w = seq_len(2),
    z = target(
      1,
      dynamic = map(y = w)
    )
  )
  make(plan, verbose = 2)
  clean()
  skip_if_not_installed("future")
  make(plan, verbose = 2, parallelism = "future")
  skip_on_os("windows")
  clean()
  options(clustermq.scheduler = "multicore")
  make(plan, verbose = 2, parallelism = "clustermq")
  expect_equal(readd(w), seq_len(2))
  if ("package:clustermq" %in% search()) {
    detach("package:clustermq", unload = TRUE) # nolint
  }
})
