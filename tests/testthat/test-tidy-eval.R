drake_context("tidy eval")

# From Kendon Bell: https://github.com/ropensci/drake/issues/200
test_with_dir("make() does tidy eval in commands", {
  con <- dbug()
  con$plan <- drake_plan(list = c(
    little_b = "\"b\"",
    letter = "!!little_b"
  ))
  con$targets <- con$plan$target
  testrun(con)
  expect_equal(readd(letter), "b")
})
