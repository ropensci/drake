# library(testthat); devtools::load_all()

context("basic")

test_that("basic example works", {
  dclean()
  e = dbug()$envir
  jobs = testopts()$jobs
  parallelism = testopts()$parallelism
  dclean()

  load_basic_example(envir = e)
  my_plan = e$my_plan
  tmp = plot_graph(my_plan, envir = e, jobs = jobs, parallelism = parallelism,
    verbose = FALSE)
  expect_false(file.exists("Makefile"))
  tmp = dataframes_graph(my_plan, envir = e, jobs = jobs, parallelism = parallelism,
    verbose = FALSE)
  expect_false(file.exists("Makefile"))
  expect_true(all(sapply(tmp, is.data.frame)))
  expect_equal(sort(outdated(my_plan, envir = e, jobs = jobs,
    parallelism = parallelism, verbose = FALSE)),
    sort(c(my_plan$target)))
  expect_false(file.exists("Makefile"))

  expect_equal(max_useful_jobs(my_plan, envir = e, 
    jobs = jobs, parallelism = parallelism, verbose = FALSE), 8)
  expect_false(file.exists("Makefile"))
  expect_equal(max_useful_jobs(my_plan, envir = e, imports = "files",
    jobs = jobs, parallelism = parallelism, verbose = FALSE), 8)
  expect_true(max_useful_jobs(my_plan, envir = e, imports = "all",
    jobs = jobs, parallelism = parallelism, verbose = FALSE) > 8)
  expect_equal(max_useful_jobs(my_plan, envir = e, imports = "none",
    jobs = jobs, parallelism = parallelism, verbose = FALSE), 8)

  make(my_plan, envir = e, verbose = FALSE,
    jobs = jobs, parallelism = parallelism)
  expect_equal(parallelism == "Makefile", file.exists("Makefile"))
  expect_equal(outdated(my_plan, envir = e, jobs = jobs,
    parallelism = parallelism, verbose = FALSE), character(0))
  expect_equal(max_useful_jobs(my_plan, envir = e,
    jobs = jobs, parallelism = parallelism, verbose = FALSE), 1)
  expect_equal(max_useful_jobs(my_plan, envir = e, imports = "files",
    jobs = jobs, parallelism = parallelism, verbose = FALSE), 1)
  expect_true(max_useful_jobs(my_plan, envir = e, imports = "all",
    jobs = jobs, parallelism = parallelism, verbose = FALSE) > 8)
  expect_equal(max_useful_jobs(my_plan, envir = e, imports = "none",
    jobs = jobs, parallelism = parallelism, verbose = FALSE), 0)

  e$reg2 = function(d){
    d$x3 = d$x^3
    lm(y ~ x3, data = d)
  }
  expect_equal(sort(outdated(my_plan, envir = e, jobs = jobs,
    parallelism = parallelism, verbose = FALSE)),
    sort(c("'report.md'", "coef_regression2_large", "coef_regression2_small",
      "regression2_large", "regression2_small", "report_dependencies", 
      "summ_regression2_large", "summ_regression2_small")))
  expect_equal(max_useful_jobs(my_plan, envir = e,
    jobs = jobs, parallelism = parallelism, verbose = FALSE), 4)
  expect_equal(max_useful_jobs(my_plan, envir = e, imports = "files",
    jobs = jobs, parallelism = parallelism, verbose = FALSE), 4)
  expect_true(max_useful_jobs(my_plan, envir = e, imports = "all",
    jobs = jobs, parallelism = parallelism, verbose = FALSE) > 8)
  expect_equal(max_useful_jobs(my_plan, envir = e, imports = "none",
    jobs = jobs, parallelism = parallelism, verbose = FALSE), 4)

  make(my_plan, envir = e, verbose = FALSE,
    jobs = jobs, parallelism = parallelism)
  expect_equal(sort(outdated(my_plan, envir = e, jobs = jobs,
    parallelism = parallelism, verbose = FALSE)), character(0))
  tmp = NULL
  tmp = plot_graph(my_plan, envir = e, jobs = jobs, parallelism = parallelism,
    verbose = FALSE)
  tmp = dataframes_graph(my_plan, envir = e, jobs = jobs, parallelism = parallelism,
    verbose = FALSE)
  expect_true(all(sapply(tmp, is.data.frame)))
  dclean()
})
