drake_context("graph")

test_with_dir("Supplied graph is not an igraph.", {
  expect_error(prune_drake_graph(12345, to = "node"))
})

test_with_dir("graph does not fail if input file is binary", {
  x <- workplan(y = readRDS("input.rds"))
  saveRDS(as.list(mtcars), "input.rds")
  expect_silent(out <- vis_drake_graph(x, verbose = FALSE))
  unlink("input.rds", force = TRUE)
})

test_with_dir("null graph", {
  x <- dataframes_graph(config = list(graph = igraph::make_empty_graph()))
  expect_equal(x, null_graph())
})

test_with_dir("circular non-DAG workplans quit in error", {
  p <- workplan(a = b, b = c, c = a)
  expect_error(tmp <- capture.output(check_plan(p)))
  expect_error(make(p, verbose = FALSE))
})

test_with_dir("Supplied graph disagrees with the workflow plan", {
  con <- dbug()
  con2 <- drake_config(workplan(a = 1), verbose = FALSE)
  expect_warning(
    make(
      plan = con$plan,
      envir = con$envir,
      graph = con2$graph,
      verbose = FALSE
    )
  )
})

test_with_dir("graph functions work", {
  config <- dbug()
  expect_equal(class(build_drake_graph(config$plan, verbose = FALSE)), "igraph")
  pdf(NULL)
  tmp <- vis_drake_graph(plan = config$plan, envir = config$envir,
                    verbose = FALSE)
  dev.off()
  unlink("Rplots.pdf", force = TRUE)
  expect_true(is.character(default_graph_title(
    parallelism = parallelism_choices()[1], split_columns = FALSE)))
  expect_true(is.character(default_graph_title(
    parallelism = parallelism_choices()[1], split_columns = TRUE)))
})

test_with_dir("Supplied graph is pruned.", {
  load_basic_example()
  graph <- build_drake_graph(my_plan)
  con <- drake_config(my_plan, targets = c("small", "large"), graph = graph)
  vertices <- V(con$graph)$name
  include <- c("small", "simulate", "data.frame", "rpois",
               "stats::rnorm", "large")
  exclude <- setdiff(my_plan$target, include)
  expect_true(all(include %in% vertices))
  expect_false(any(exclude %in% vertices))
})

test_with_dir("different graphical arrangements for distributed parallelism", {
  e <- new.env()
  x <- workplan(a = 1, b = f(2))
  e$f <- function(x) x
  con <- drake_config(x, envir = e, verbose = FALSE)
  expect_equal(1, max_useful_jobs(x, envir = e, config = con,
                                  parallelism = "mclapply", jobs = 1))
  expect_equal(1, max_useful_jobs(x, envir = e, config = con,
                                  parallelism = "parLapply", jobs = 1))
  con$parallelism <- "Makefile"
  expect_equal(2, max_useful_jobs(x, envir = e, config = con,
                                  parallelism = "Makefile", jobs = 1))
  expect_equal(2, max_useful_jobs(x, envir = e, config = con,
                                  parallelism = "future_lapply", jobs = 1))
  y <- workplan(a = 1, b = 2)
  tmp <- dataframes_graph(y, parallelism = "Makefile", verbose = FALSE)
  expect_true(is.list(tmp))
})
