drake_context("basic")

test_with_dir("basic example works", {
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs
  parallelism <- scenario$parallelism

  load_basic_example(envir = e)
  my_plan <- e$my_plan
  config <- drake_config(my_plan, envir = e,
    jobs = jobs, parallelism = parallelism,
    verbose = FALSE)

  expect_equal(max_useful_jobs(config), 8)
  expect_false(file.exists("Makefile"))
  expect_equal(max_useful_jobs(imports = "files",
    config = config), 8)
  expect_true(max_useful_jobs(imports = "all",
    config = config) >= 8)
  expect_equal(max_useful_jobs(imports = "none",
    config = config), 8)

  dats <- c("small", "large")
  config$targets <- dats
  con <- testrun(config)

  expect_true(is.list(dependency_profile(
    target = "small", config = con)))
  expect_equal(parallelism == "Makefile", file.exists("Makefile"))
  tmp1 <- dataframes_graph(config = config,
    make_imports = FALSE)
  tmp2 <- dataframes_graph(config = config)
  expect_true(is.data.frame(tmp1$nodes))
  expect_true(is.data.frame(tmp2$nodes))

  expect_equal(sort(justbuilt(con)), sort(dats))
  remove_these <- intersect(dats, ls(config$envir))
  rm(list = remove_these, envir = config$envir)
  config$targets <- config$plan$target
  con <- testrun(config)
  jb <- justbuilt(con)
  expect_true("'report.md'" %in% jb)
  expect_false(any(dats %in% jb))

  # Check that file is not rehashed.
  # Code coverage should cover every line of file_hash().
  expect_true(is.character(file_hash(
    target = "'report.Rmd'", config = con, size_cutoff = -1)))

  config <- drake_config(
    my_plan, envir = e, jobs = jobs, parallelism = parallelism,
    verbose = FALSE)

  expect_equal(outdated(config), character(0))
  expect_equal(max_useful_jobs(config = config), 1)
  expect_equal(max_useful_jobs(imports = "files",
    config = config), 1)
  expect_true(max_useful_jobs(imports = "all",
    config = config) >= 8)
  expect_equal(max_useful_jobs(imports = "none",
    config = config), 0)

  e$reg2 <- function(d) {
    d$x3 <- d$x ^ 3
    lm(y ~ x3, data = d)
  }
  config <- drake_config(
    my_plan, envir = e, jobs = jobs, parallelism = parallelism,
    verbose = FALSE)
  expect_equal(
    sort(outdated(config = config)),
    sort(c("'report.md'", "coef_regression2_large",
      "coef_regression2_small", "regression2_large", "regression2_small",
      "summ_regression2_large", "summ_regression2_small")))
  expect_equal(max_useful_jobs(config = config), 4)
  expect_equal(max_useful_jobs(config = config,
    from_scratch = TRUE), 8)
  expect_equal(max_useful_jobs(imports = "files",
    config = config), 4)
  expect_true(max_useful_jobs(imports = "all",
    config = config) >= 8)
  expect_equal(max_useful_jobs(imports = "none",
    config = config), 4)

  testrun(config)
  config <- drake_config(
    my_plan, envir = e, jobs = jobs, parallelism = parallelism,
    verbose = FALSE)
  expect_equal(sort(outdated(config = config)),
    character(0))
})
