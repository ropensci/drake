# Makefile recipe tesing assumes you can acually run
# the Makefile, so it belongs outside the regular
# untit tests.

library(testthat)
devtools::load_all()

set_testing_scenario("local_Makefile_9")
cat(get_testing_scenario_name(), ": ", sep = "")
context("recipes")

test_with_dir("custom Makefile recipes work", {
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs
  parallelism <- scenario$parallelism
  load_mtcars_example(envir = e)
  my_plan <- e$my_plan
  verbose <- TRUE

  con <- make(my_plan,
    envir = e, jobs = jobs, parallelism = parallelism,
    verbose = verbose, return_config = TRUE)
  expect_equal(sort(justbuilt(con)), sort(con$plan$target))
  clean()

  cmds <- c(
    "Rscript -e"
  )
  if (!on_windows()){
    cmds <- c(cmds,
      "R -e 'R_RECIPE' -q"
    )
  }
  for (cmd in cmds){
    con <- make(my_plan, recipe_command = cmd,
      envir = e, jobs = jobs, parallelism = parallelism,
      verbose = verbose, return_config = TRUE)
    expect_equal(sort(justbuilt(con)), sort(con$plan$target))
    clean()
  }
})
