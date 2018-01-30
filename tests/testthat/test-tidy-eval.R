drake_context("tidy eval")

# From Kendon Bell: https://github.com/ropensci/drake/issues/200
test_with_dir("drake_plan does tidy eval in `...` argument", {
  plan1 <- drake_plan(
    a = !!my_variable,
    b = !!my_variable + 1,
    list = c(d = "!!my_variable")
  )  
  plan2 <- data.frame(
    target = c("a", "b", "d"),
    command = c("5", "6", "!!my_variable"),
    stringsAsFactors = FALSE
  )
  expect_equal(plan1, plan2)
})

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
