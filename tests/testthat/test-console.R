cat(get_testing_scenario_name(), ": ", sep = "")
context("console")

test_with_dir("console", {
  config <- dbug()
  config$verbose <- TRUE
  expect_output(console(imported = FALSE, target = "myinput",
    config = config))
  expect_output(
    console_many_targets(targets = letters,
      message = "check", config = config
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
    targets = character(0), message = "check", config = config))
  expect_silent(console_many_targets(
    targets = "my_target", message = "check", config = config))
  config <- list(verbose = TRUE)
  expect_silent(console_many_targets(
    targets = character(0), message = "check", config = config))
  expect_output(console_many_targets(
    targets = "my_target", message = "check", config = config))
  tmp <- capture.output(
    console_many_targets(
      targets = LETTERS,
      message = unique_random_string(n = 400),
      config = config
    )
  )
  expect_true(is.character(tmp))
  expect_true(nchar(tmp) <= console_length + 20)
})
