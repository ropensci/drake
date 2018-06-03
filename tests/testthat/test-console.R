drake_context("console")

test_with_dir("console_cache", {
  expect_silent(
    console_cache(config = list(cache_path = "123", verbose = TRUE)))
  expect_message(
    console_cache(config = list(cache_path = "123", verbose = 2)))
  expect_silent(console_cache(config = list(verbose = TRUE)))
  expect_message(console_cache(config = list(verbose = 2)))
})

test_with_dir("console_up_to_date", {
  pl <- drake_plan(a = 1)
  con <- make(pl, verbose = FALSE, session_info = FALSE)
  expect_silent(console_up_to_date(con))
  con$verbose <- TRUE
  expect_silent(console_up_to_date(con))
  con$cache$clear(namespace = "attempt")
  con <- make(pl, verbose = FALSE, session_info = FALSE)
  con$verbose <- TRUE
  expect_message(console_up_to_date(con))
})

test_with_dir("verbose consoles", {
  config <- list(verbose = 2)
  expect_silent(console_missing("\"myfile\"", config))
  expect_silent(console_import("\"myfile\"", config))
  config$verbose <- 3
  expect_message(console_missing("\"myfile\"", config))
  expect_silent(console_import("\"myfile\"", config))
  config$verbose <- 4
  expect_message(console_missing("\"myfile\"", config))
  expect_message(console_import("\"myfile\"", config))
})

test_with_dir("console_parLapply", {
  config <- list(verbose = TRUE)
  expect_message(console_parLapply(config = config)) # nolint
  config <- list(verbose = FALSE)
  expect_silent(console_parLapply(config = config)) # nolint
})

test_with_dir("multiline message cap", {
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

test_with_dir("console", {
  config <- dbug()
  config$verbose <- TRUE
  expect_silent(console(imported = TRUE, target = "myinput",
    config = config))
  config$verbose <- 3
  expect_message(console(imported = FALSE, target = "myinput",
    config = config))
  expect_silent(console(imported = TRUE, target = "myinput",
    config = config))
  expect_message(console(imported = NA, target = "myinput",
    config = config))
  expect_message(
    console_many_targets(targets = letters,
      pattern = "check", config = config
    )
  )
  x1 <- "12345"
  x2 <- paste(rep(0:9, length.out = getOption("width") + 400), collapse = "")
  expect_equal(x1, color(x = x1, color = NULL))
  o1 <- evaluate_promise(
    console(
      imported = FALSE,
      target = x1,
      config = config
    ),
    print = TRUE
  )$messages
  o2 <- evaluate_promise(
    console(
      imported = FALSE,
      target = x2,
      config = config
    ),
    print = TRUE
  )$messages
  expect_true(nchar(o1) < getOption("width"))
  expect_true(nchar(o2) < getOption("width") + 20)
  dots <- "\\.\\.\\.\n$"
  expect_false(grepl(dots, o1))
  expect_true(grepl(dots, o2))
})

test_with_dir("console_many_targets() works", {
  config <- list(verbose = FALSE)
  expect_silent(console_many_targets(
    targets = character(0), pattern = "check", config = config))
  expect_silent(console_many_targets(
    targets = "my_target", pattern = "check", config = config))
  config <- list(verbose = TRUE)
  expect_silent(console_many_targets(
    targets = character(0), pattern = "check", config = config))
  expect_silent(console_many_targets(
    targets = "my_target", pattern = "check", config = config))
  config$verbose <- 2
  expect_silent(console_many_targets(
    targets = character(0), pattern = "check", config = config))
  tmp <- evaluate_promise(
    console_many_targets(
      targets = LETTERS,
      pattern = unique_random_string(n = 400),
      config = config
    ),
    print = TRUE
  )$messages
  expect_true(is.character(tmp))
  expect_true(nchar(tmp) <= getOption("width") + 20)
})

test_with_dir("console_persistent_workers", {
  con <- dbug()
  expect_silent(console_persistent_workers(con))
  con$verbose <- 4
  expect_message(console_persistent_workers(con))
})

test_with_dir("console_skip", {
  con <- dbug()
  expect_silent(console_skip("bla", con))
  con$verbose <- 4
  expect_message(console_skip(file_store("bla"), con))
})

test_with_dir("console to file", {
  load_mtcars_example()
  cache <- storr::storr_environment()
  expect_false(file.exists("log.txt"))
  tmp <- capture.output({
      make(
        my_plan, cache = cache, verbose = 4, session_info = FALSE,
        console = "log.txt"
      )
      make(
        my_plan, cache = cache, verbose = 4, session_info = FALSE,
        console = "log.txt"
      )
      make(
        my_plan, cache = cache, verbose = 4, session_info = FALSE,
        trigger = "always", console = "log.txt"
      )
    },
    type = "message"
  )
  expect_equal(tmp, character(0))
  expect_true(length(readLines("log.txt")) > 0)
})

test_with_dir("drake_warning() and drake_error()", {
  plan <- drake_plan(x = {
      message("some_message")
      warning("some_warning")
      stop("some_error")
    },
    strings_in_dots = "literals"
  )
  expect_error(expect_warning(make(
    plan, cache = storr::storr_environment(),
    session_info = FALSE
  )))
  expect_false(file.exists("log.txt"))
  expect_error(expect_warning(make(
    plan, console = "log.txt",
    cache = storr::storr_environment(),
    session_info = FALSE
  )))
  x <- readLines("log.txt")
  expect_true(any(grepl("some_message", x)))
  expect_true(any(grepl("some_warning", x)))
  expect_true(any(grepl("some_error", x)))
})
