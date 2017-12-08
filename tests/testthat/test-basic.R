drake_context("basic")

test_with_dir("basic example works", {
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs
  parallelism <- scenario$parallelism
  
  load_basic_example(envir = e)
  my_plan <- e$my_plan
  config <- drake_config(my_plan, envir = e,
    jobs = jobs, parallelism = parallelism)

  expect_true(is.list(dependency_profile(
    target = "'report.md'", config = config)))
  expect_true(is.list(dependency_profile(
    target = "'report.Rmd'", config = config)))
  tmp <- vis_drake_graph(config = config)
  expect_false(file.exists("Makefile"))

  # Different graph configurations should be checked manually.
  tmp <- dataframes_graph(config = config, build_times = FALSE)
  tmpcopy <- dataframes_graph(config = config,
    make_imports = FALSE, build_times = FALSE)
  tmp0 <- dataframes_graph(config = config, build_times = FALSE,
    subset = c("small", "regression2_large"))
  tmp1 <- dataframes_graph(config = config, build_times = FALSE,
    from = "small")
  tmp2 <- dataframes_graph(config = config, build_times = FALSE,
    from = "small", targets_only = TRUE)
  tmp3 <- dataframes_graph(config = config, build_times = FALSE,
    targets_only = TRUE)
  tmp4 <- dataframes_graph(config = config, build_times = FALSE,
    split_columns = TRUE)
  tmp5 <- dataframes_graph(config = config, build_times = FALSE,
    targets_only = TRUE, split_columns = TRUE)
  tmp6 <- dataframes_graph(config = config, build_times = TRUE,
    targets_only = TRUE, split_columns = TRUE)
  tmp7 <- dataframes_graph(config = config, build_times = TRUE,
    targets_only = TRUE, split_columns = TRUE, from_scratch = TRUE)
  expect_warning(
    tmp8 <- dataframes_graph(config = config, build_times = FALSE,
      from = c("small", "not_found"))
  )
  expect_error(
    tmp9 <- dataframes_graph(config = config, build_times = FALSE,
      from = "not_found")
  )
  expect_equal(nrow(tmp0$nodes), 2)
  expect_true(identical(tmp$nodes, tmpcopy$nodes))
  expect_false(identical(tmp$nodes, tmp0$nodes))
  expect_false(identical(tmp$nodes, tmp1$nodes))
  expect_false(identical(tmp$nodes, tmp2$nodes))
  expect_false(identical(tmp$nodes, tmp3$nodes))
  expect_false(identical(tmp$nodes, tmp4$nodes))
  expect_false(identical(tmp$nodes, tmp5$nodes))
  expect_false(identical(tmp$nodes, tmp6$nodes))

  expect_false(file.exists("Makefile"))
  expect_true(is.data.frame(tmp$nodes))
  expect_equal(sort(outdated(config = config)),
    sort(c(my_plan$target)))
  expect_false(file.exists("Makefile"))

  file <- "graph.html"
  expect_false(file.exists(file))
  vis_drake_graph(config = config, file = file)
  expect_true(file.exists(file))
  unlink(file, force = TRUE)
  unlink("graph_files", recursive = TRUE, force = TRUE)
  expect_false(file.exists(file))

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
