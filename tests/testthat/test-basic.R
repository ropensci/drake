context("basic")

test_that("basic example works", {
  dclean()
  opt <- test_opt()
  e <- eval(parse(text = opt$envir))
  jobs <- opt$jobs
  parallelism <- opt$parallelism
  dclean()

  load_basic_example(envir = e)
  my_plan <- e$my_plan
  config <- config(my_plan, envir = e,
    jobs = jobs, parallelism = parallelism,
    verbose = FALSE)

  tmp <- plot_graph(my_plan, envir = e, config = config)
  expect_false(file.exists("Makefile"))
  tmp <- dataframes_graph(my_plan, envir = e, config = config)
  tmp <- dataframes_graph(my_plan, envir = e, config = config,
    targets_only = TRUE)
  expect_false(file.exists("Makefile"))
  expect_true(is.data.frame(tmp$nodes))
  expect_equal(sort(outdated(my_plan, envir = e, config = config)),
    sort(c(my_plan$target)))
  expect_false(file.exists("Makefile"))

  file <- "graph.html"
  expect_false(file.exists(file))
  plot_graph(my_plan, envir = e, config = config, file = file)
  expect_true(file.exists(file))
  unlink(file)
  unlink("graph_files", recursive = TRUE)
  expect_false(file.exists(file))

  expect_equal(max_useful_jobs(my_plan, envir = e, jobs = jobs,
    parallelism = parallelism, verbose = FALSE), 8)
  expect_false(file.exists("Makefile"))
  expect_equal(max_useful_jobs(my_plan, envir = e, imports = "files",
    config = config), 8)
  expect_true(max_useful_jobs(my_plan, envir = e, imports = "all",
    config = config) > 8)
  expect_equal(max_useful_jobs(my_plan, envir = e, imports = "none",
    config = config), 8)

  con <- testrun(config)

  # Check that file is not rehashed.
  # Code coverage should cover every line of file_hash().
  expect_true(is.character(file_hash(
    target = "'report.Rmd'", config = con, size_cutoff = -1)))

  config <- config(my_plan, envir = e, jobs = jobs, parallelism = parallelism,
    verbose = FALSE)
  expect_equal(parallelism == "Makefile", file.exists("Makefile"))
  expect_equal(outdated(my_plan, envir = e, jobs = jobs,
    parallelism = parallelism,
    verbose = FALSE), character(0))
  expect_equal(max_useful_jobs(my_plan, envir = e, config = config),
    1)
  expect_equal(max_useful_jobs(my_plan, envir = e, imports = "files",
    config = config), 1)
  expect_true(max_useful_jobs(my_plan, envir = e, imports = "all",
    config = config) > 8)
  expect_equal(max_useful_jobs(my_plan, envir = e, imports = "none",
    config = config), 0)

  e$reg2 <- function(d) {
    d$x3 <- d$x ^ 3
    lm(y ~ x3, data = d)
  }
  config <- config(my_plan, envir = e, jobs = jobs, parallelism = parallelism,
    verbose = FALSE)
  expect_equal(sort(outdated(my_plan, envir = e, jobs = jobs,
    config = config)), sort(c("'report.md'", "coef_regression2_large",
    "coef_regression2_small", "regression2_large", "regression2_small",
    "report_dependencies", "summ_regression2_large",
    "summ_regression2_small")))
  expect_equal(max_useful_jobs(my_plan, envir = e, config = config),
    4)
  expect_equal(max_useful_jobs(my_plan, envir = e, imports = "files",
    config = config), 4)
  expect_true(max_useful_jobs(my_plan, envir = e, imports = "all",
    config = config) > 8)
  expect_equal(max_useful_jobs(my_plan, envir = e, imports = "none",
    config = config), 4)

  testrun(config)
  config <- config(my_plan, envir = e, jobs = jobs, parallelism = parallelism,
    verbose = FALSE)
  expect_equal(sort(outdated(my_plan, envir = e, config = config)),
    character(0))
  tmp <- plot_graph(my_plan, envir = e, jobs = jobs, parallelism = parallelism,
    verbose = FALSE)
  tmp <- dataframes_graph(my_plan, envir = e, jobs = jobs,
    parallelism = parallelism, verbose = FALSE)
  expect_true(is.data.frame(tmp$nodes))
  dclean()
})
