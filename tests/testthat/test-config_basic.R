for (config_outer in test_configs()){
  envir <- eval(parse(text = config_outer[["envir"]]))
  jobs <- config_outer$jobs
  parallelism <- config_outer$parallelism


context(paste("basic -", config_outer[["label"]]))

test_that("basic make", {
  dclean()
  load_basic_example(envir = envir)
  my_plan <- envir$my_plan

  config <- config(
    envir = envir,
    plan = my_plan,
    jobs = jobs,
    parallelism = parallelism,
    verbose = FALSE
    )

  tmp <- plot_graph(
    my_plan,
    envir = envir,
    config = config
    )
  expect_false(file.exists("Makefile"))

  tmp <- dataframes_graph(
    my_plan,
    envir = envir,
    config = config
    )
  expect_false(file.exists("Makefile"))
  expect_true(all(sapply(tmp, is.data.frame)))

  tmp <- dataframes_graph(
    my_plan,
    envir = envir,
    config = config
    )
  expect_false(file.exists("Makefile"))
  expect_true(all(sapply(tmp, is.data.frame)))

  expect_equal(
    sort(outdated(
        my_plan,
        envir = envir,
        config = config
        )),
    sort(c(my_plan$target))
    )
  expect_false(file.exists("Makefile"))

  file <- "graph.html"
  expect_false(file.exists(file))
  plot_graph(
    my_plan,
    envir = envir,
    config = config,
    file = file
    )
  expect_true(file.exists(file))
  unlink(file)
  unlink("graph_files", recursive = TRUE)
  expect_false(file.exists(file))

  expect_equal(
    max_useful_jobs(
      envir = envir,
      plan = my_plan,
      jobs = jobs,
      parallelism = parallelism,
      verbose = FALSE
      ),
    8
    )
  expect_false(file.exists("Makefile"))
  expect_equal(
    max_useful_jobs(
      envir = envir,
      plan = my_plan,
      imports = "files",
      config = config
      ),
    8
    )
  expect_true(
    max_useful_jobs(
      envir = envir,
      plan = my_plan,
      imports = "all",
      config = config
      ) > 8
    )
  expect_equal(
    max_useful_jobs(
      envir = envir,
      plan = my_plan,
      imports = "none",
      config = config
      ),
    8
    )

  make(
    envir = envir,
    plan = my_plan,
    jobs = jobs,
    parallelism = parallelism,
    verbose = FALSE
    )
  config <- config(
    envir = envir,
    plan = my_plan,
    jobs = jobs,
    parallelism = parallelism,
    verbose = FALSE
    )
  expect_equal(
    config_outer$parallelism == "Makefile",
    file.exists("Makefile")
    )
  expect_equal(
    outdated(
      my_plan,
      envir = envir,
      config = config
      ),
    character(0)
    )
  expect_equal(
    max_useful_jobs(
      envir = envir,
      plan = my_plan,
      config = config
      ),
    1
    )
  expect_equal(
    max_useful_jobs(
      envir = envir,
      plan = my_plan,
      imports = "files",
      config = config
      ),
    1
    )
  expect_true(
    max_useful_jobs(
      envir = envir,
      plan = my_plan,
      imports = "all",
      config = config
      ) > 8
    )
  expect_equal(
    max_useful_jobs(
      envir = envir,
      plan = my_plan,
      imports = "none",
      config = config
      ),
    0
    )


  envir$reg2 <- function(d){
    d$x3 <- d$x ^ 3
    lm(y ~ x3, data = d)
  }
  config <- config(
    my_plan,
    envir = envir,
    jobs = jobs,
    parallelism = parallelism,
    verbose = FALSE)
  expect_equal(
    sort(outdated(
        my_plan,
        envir = envir,
        jobs = jobs,
        config = config
        )),
    sort(c(
      "'report.md'",
      "coef_regression2_large",
      "coef_regression2_small",
      "regression2_large",
      "regression2_small",
      "report_dependencies",
      "summ_regression2_large",
      "summ_regression2_small"
      ))
    )
  expect_equal(
    max_useful_jobs(
      my_plan,
      envir = envir,
      config = config
      ),
    4
    )
  expect_equal(
    max_useful_jobs(
      my_plan,
      envir = envir,
      imports = "files",
      config = config
      ),
    4
    )
  expect_true(
    max_useful_jobs(
      my_plan,
      envir = envir,
      imports = "all",
      config = config
      ) > 8
    )
  expect_equal(
    max_useful_jobs(
      my_plan,
      envir = envir,
      imports = "none",
      config = config
      ),
    4
    )

  make(
    envir = envir,
    plan = my_plan,
    jobs = jobs,
    parallelism = parallelism,
    verbose = FALSE
    )
  config <- config(
    envir = envir,
    plan = my_plan,
    jobs = jobs,
    parallelism = parallelism,
    verbose = FALSE
    )
  expect_equal(
    config_outer$parallelism == "Makefile",
    file.exists("Makefile")
    )
  expect_equal(
    outdated(
      my_plan,
      envir = envir,
      config = config
      ),
    character(0)
    )

  tmp <- plot_graph(
    envir = envir,
    plan = my_plan,
    jobs = jobs,
    parallelism = parallelism,
    verbose = FALSE
    )
  tmp <- dataframes_graph(
    envir = envir,
    plan = my_plan,
    jobs = jobs,
    parallelism = parallelism,
    verbose = FALSE
    )
  expect_true(all(sapply(tmp, is.data.frame)))

  dclean()
})

}
