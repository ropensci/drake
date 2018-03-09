drake_context("strings")

# All tests in this file are taken from eply:
# https://github.com/ropensci/eply

test_with_dir("Functions drake_quotes() and drake_unquote() are correct.", {
  expect_equal(drake_quotes(), character(0))
  expect_equal(drake_quotes(single = T), character(0))
  expect_equal(drake_quotes(drake_strings(x, y)), c("\"x\"", "\"y\""))
  expect_equal(drake_quotes(c("x", "y")), c("\"x\"", "\"y\""))
  expect_equal(drake_quotes(c("x", "y"), single = F), c("\"x\"", "\"y\""))
  expect_equal(drake_quotes(c("x", "y"), single = T), c("\'x\'", "\'y\'"))

  expect_equal(drake_unquote(), character(0))
  expect_equal(drake_unquote(drake_strings(x, y)), drake_strings(x, y))

  a <- c(
    "\"x\"", "\"y\"", "'z'", "return(a)",
    "return(\"a\")",
    "\"x\"", "\"return(\"a\")\"")
  b <- c(
    "x", "y", "z", "return(a)",
    "return(\"a\")",
    "x", "return(\"a\")"
  )
  expect_equal(drake_unquote(a), b)
})

test_with_dir("Function drake_strings() is correct.", {
  expect_equal(character(0), drake_strings())
  expect_equal("1", drake_strings(1))
  expect_equal(
    "list(\"foo\", f(c(\"bar\", \"baz\")))",
    drake_strings(list("foo", f(c("bar", "baz"))))
  )
})
