drake_context("tidy eval")

# From Kendon Bell: https://github.com/ropensci/drake/issues/200
test_with_dir("drake_plan does tidy eval", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  my_variable <- 5
  plan1 <- drake_plan(a = !!my_variable)
  plan2 <- weak_tibble(target = "a", command = "5")
  equivalent_plans(plan1, plan2)
})

# From Alex Axthelm: https://github.com/ropensci/drake/issues/200
test_with_dir("drake_plan tidy eval can be customized and disabled", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  my_variable <- 5
  plan1 <- drake_plan(
    a = !!my_variable,
    b = !!my_variable + 1,
    c = target(1 + 1, custom = !!my_variable),
    tidy_evaluation = FALSE
  )
  plan1$custom <- unlist(lapply(plan1[["custom"]], rlang::expr_text))
  plan2 <- drake_plan(
    a = !!my_variable,
    b = !!my_variable + 1,
    c = target(1 + 1, custom = !!my_variable),
    tidy_evaluation = TRUE
  )
  plan1$command <- unclass(deparse_lang_col(plan1$command))
  plan2$command <- unclass(deparse_lang_col(plan2$command))
  expect_equal(plan1$target, plan2$target)
  expect_false(any(grepl("5", plan1$command)))
  expect_equal(plan2$command, c("5", "5 + 1", "1 + 1"))
  expect_false(any(grepl("5", plan1[["custom"]])))
  expect_equal(plan2$custom, c(NA, NA, 5))
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
