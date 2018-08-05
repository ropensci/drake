drake_context("plan printing")

test_with_dir("drake_plan_call() produces the correct calls", {
  skip_on_cran()
  load_mtcars_example()
  my_plan$trigger <- NA
  my_plan$trigger[4] <- "trigger(condition = is_tuesday(), file = FALSE)"
  my_plan$non_standard_column <- 1234
  pkgconfig::set_config("drake::strings_in_dots" = "literals")
  new_plan <- eval(drake_plan_call(my_plan))
  expected <- my_plan[, c("target", "command", "trigger")]
  expect_equal(new_plan, expected)
})
