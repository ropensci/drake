# Makefile recipe tesing assumes you can acually run
# the Makefile, so it belongs outside the regular
# untit tests.

library(testthat)
devtools::load_all()

set_testing_scenario("local_Make_2")
cat(get_testing_scenario_name(), ": ", sep = "")
context("recipes")

test_with_dir("custom Makefile recipes work", {
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs
  parallelism <- scenario$parallelism
  load_basic_example(envir = e)
  my_plan <- e$my_plan
  verbose <- TRUE

  con <- make(my_plan,
    envir = e, jobs = jobs, parallelism = parallelism,
    verbose = verbose, return_config = TRUE)
  expect_equal(sort(justbuilt(con)), sort(my_plan$target))
  clean()

  cmds <- c(
    "Rscript -e",
    "R -e 'R_RECIPE' -q" # does not work on Windows
  )
  for (cmd in cmds){
    con <- make(my_plan, recipe_command = cmd,
      envir = e, jobs = jobs, parallelism = parallelism,
      verbose = verbose, return_config = TRUE)
    expect_equal(sort(justbuilt(con)), sort(my_plan$target))
    clean()
  }
})
