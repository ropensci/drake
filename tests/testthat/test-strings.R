drake_context("strings")

# All tests in this file are taken from eply:
# https://github.com/wlandau-lilly/eply

test_that("Functions drake_quotes() and drake_unquote() are correct.", {
  expect_equal(drake_quotes(), "\"\"")
  expect_equal(drake_quotes(single = T), "\'\'")
  expect_equal(drake_quotes(drake_strings(x, y)), c("\"x\"", "\"y\""))
  expect_equal(drake_quotes(c("x", "y")), c("\"x\"", "\"y\""))
  expect_equal(drake_quotes(c("x", "y"), single = F), c("\"x\"", "\"y\""))
  expect_equal(drake_quotes(c("x", "y"), single = T), c("\'x\'", "\'y\'"))

  expect_equal(drake_unquote(), character(0))
  expect_equal(drake_unquote(drake_strings(x, y)), drake_strings(x, y))
  expect_equal(drake_unquote(
    deep = TRUE,
    x = drake_quotes(
      drake_quotes(
        drake_quotes(c("x", "y")), single = T),
        single = F
      )
    ),
    c("x", "y")
  )

  a <- c(
    "\"x\"", "\"y\"", "return(a)",
    "return(\"a\")", "\"x", "y\"",
    "\"x\"", "\"return(\"a\")\"")
  b <- c(
    "x", "y", "return(a)",
    "return(\"a\")", "x", "y",
    "x", "return(\"a\")"
  )
  expect_equal(drake_unquote(a), b)

  x <- c("'x'", '"y"', "\"'x'\"", "'\"y\"'")
  y <- c("x", "y", "'x'", "\"y\"")
  z <- c("x", "y", "x", "y")
  expect_equal(drake_unquote(x), y)
  expect_equal(drake_unquote(x, deep = F), y)
  expect_equal(drake_unquote(x, deep = T), z)
})

test_that("Function drake_strings() is correct.", {
  expect_equal(character(0), drake_strings())
  expect_equal("1", drake_strings(1))
  expect_equal(
    "list(\"foo\", f(c(\"bar\", \"baz\")))",
    drake_strings(list("foo", f(c("bar", "baz"))))
  )
})
