drake_context("tidy eval")

# From Kendon Bell: https://github.com/ropensci/drake/issues/200
test_with_dir("drake_plan does tidy eval", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  my_variable <- 5
  plan1 <- drake_plan(a = !!my_variable)
  plan2 <- weak_tibble(target = "a", command = "5")
  expect_equal(plan1, plan2)
})

# From Alex Axthelm: https://github.com/ropensci/drake/issues/200
test_with_dir("drake_plan tidy eval can be disabled", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  my_variable <- 5
  plan1 <- drake_plan(
    a = !!my_variable,
    b = !!my_variable + 1,
    tidy_evaluation = FALSE
  )
  plan2 <- drake_plan(
    a = !!my_variable,
    b = !!my_variable + 1,
    tidy_evaluation = TRUE
  )
  expect_equal(plan1$target, plan2$target)
  expect_false(identical(plan1$command, plan2$command))
})

# From Kendon Bell: https://github.com/ropensci/drake/issues/200
test_with_dir("make() does tidy eval in commands", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  plan <- drake_plan(
    little_b = "b",
    letter = !!little_b,
    tidy_evaluation = FALSE
  )
  make(plan)
  expect_equal(readd(letter), "b")
})
