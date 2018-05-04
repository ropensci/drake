drake_context("console")

test_with_dir("console_cache", {
  expect_silent(console_cache("12345", verbose = TRUE))
  expect_message(console_cache("12345", verbose = 2))
  expect_silent(console_cache(NULL, verbose = TRUE))
  expect_message(console_cache(NULL, verbose = 2))
})

test_with_dir("console_up_to_date", {
  pl <- drake_plan(a = 1)
  con <- make(pl, verbose = FALSE, session_info = FALSE)
  expect_silent(console_up_to_date(con))
  con$verbose <- TRUE
  expect_silent(console_up_to_date(con))
  con$cache$clear(namespace = "session")
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
  expect_message(console_persistent_workers("bla", con))
})
