# library(testthat); devtools::load_all()

context("parallel")

test_that("max_useful_jobs() gives correct answers", {
  dclean()
  e = new.env(parent = globalenv())
  load_basic_example(envir = e)
  my_plan = e$my_plan
  tmp = plot_graph(my_plan, envir = e)
  tmp = dataframes_graph(my_plan, envir = e)
  expect_true(all(sapply(tmp, is.data.frame)))

  expect_equal(max_useful_jobs(my_plan, envir = e), 8)
  expect_equal(max_useful_jobs(my_plan, envir = e, imports = "files"), 8)
  expect_equal(max_useful_jobs(my_plan, envir = e, imports = "all"), 10)
  expect_equal(max_useful_jobs(my_plan, envir = e, imports = "none"), 8)

  make(my_plan, envir = e, verbose = FALSE)
  expect_equal(max_useful_jobs(my_plan, envir = e), 1)
  expect_equal(max_useful_jobs(my_plan, envir = e, imports = "files"), 1)
  expect_equal(max_useful_jobs(my_plan, envir = e, imports = "all"), 10)
  expect_equal(max_useful_jobs(my_plan, envir = e, imports = "none"), 0)

  e$reg2 = function(d){
    d$x3 = d$x^3
    lm(y ~ x3, data = d)
  }
  expect_equal(max_useful_jobs(my_plan, envir = e), 4)
  expect_equal(max_useful_jobs(my_plan, envir = e, imports = "files"), 4)
  expect_equal(max_useful_jobs(my_plan, envir = e, imports = "all"), 10)
  expect_equal(max_useful_jobs(my_plan, envir = e, imports = "none"), 4)
  dclean()
})
