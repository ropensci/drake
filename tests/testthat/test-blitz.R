drake_context("blitz")

test_with_dir("blitz parallelism", {
  skip_on_cran()
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  load_mtcars_example(envir = e)
  e$my_plan$command[e$my_plan$target == "report"] <-
    "utils::write.csv(coef_regression2_large, file = file_out(\"coef.csv\"))"
  expect_false(file.exists("coef.csv"))
  expect_warning(
    make(e$my_plan, envir = e, parallelism = "blitz"),
    regexp = "USE AT YOUR OWN RISK"
  )
  expect_true(file.exists("coef.csv"))
  expect_equal(length(cached()), 0)
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  if ("package:clustermq" %in% search()){
    eval(parse(text = "detach('package:clustermq', unload = TRUE)"))
  }
  options(clustermq.scheduler = "multicore")
  unlink("coef.csv")
  expect_false(file.exists("coef.csv"))
  expect_warning(
    make(e$my_plan, envir = e, parallelism = "blitz", jobs = 2),
    regexp = "USE AT YOUR OWN RISK"
  )
  expect_true(file.exists("coef.csv"))
  if ("package:clustermq" %in% search()){
    eval(parse(text = "detach('package:clustermq', unload = TRUE)"))
  }
})
