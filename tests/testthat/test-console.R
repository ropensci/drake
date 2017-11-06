drake_context("console")

test_with_dir("console_up_to_date", {
  pl <- workplan(a = 1)
  con <- make(pl, verbose = FALSE)
  expect_silent(console_up_to_date(con))
  con$verbose <- TRUE
  expect_silent(console_up_to_date(con))
  con <- make(pl, verbose = FALSE)
  con$verbose <- TRUE
  expect_output(console_up_to_date(con))
})

test_with_dir("console_parLapply", {
  config <- list(verbose = TRUE)
  expect_output(console_parLapply(config = config)) # nolint
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
  expect_output(console(imported = FALSE, target = "myinput",
    config = config))
  expect_output(console(imported = TRUE, target = "myinput",
    config = config))
  expect_output(console(imported = NA, target = "myinput",
    config = config))
  expect_output(
    console_many_targets(targets = letters,
      pattern = "check", config = config
    )
  )
  x1 <- "12345"
  x2 <- paste(rep(0:9, length.out = console_length + 400), collapse = "")
  expect_equal(x1, color(x = x1, color = NULL))
  o1 <- capture.output(console(imported = FALSE, target = x1,
    config = config))
  o2 <- capture.output(console(imported = FALSE, target = x2,
    config = config))
  expect_true(nchar(o1) < console_length)
  expect_true(nchar(o2) < console_length + 20)
  dots <- "\\.\\.\\.$"
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
  expect_output(console_many_targets(
    targets = "my_target", pattern = "check", config = config))
  tmp <- capture.output(
    console_many_targets(
      targets = LETTERS,
      pattern = unique_random_string(n = 400),
      config = config
    )
  )
  expect_true(is.character(tmp))
  expect_true(nchar(tmp) <= console_length + 20)
})
