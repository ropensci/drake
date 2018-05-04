drake_context("graph")

test_with_dir("Recursive functions are okay", {
  factorial <- function(n){
    if (n == 0){
      1
    } else {
      n * factorial(n - 1)
    }
  }
  x <- drake_plan(output = factorial(10))
  cache <- storr::storr_environment()
  make(x, cache = cache, session_info = FALSE)
})

test_with_dir("Supplied graph is not an igraph.", {
  expect_error(prune_drake_graph(12345, to = "node"))
})

test_with_dir("graph does not fail if input file is binary", {
  x <- drake_plan(
    y = readRDS(file_in("input.rds")),
    strings_in_dots = "literals"
  )
  saveRDS(as.list(mtcars), "input.rds")
  con <- drake_config(x, verbose = FALSE)
  expect_silent(out <- vis_drake_graph(con))
  unlink("input.rds", force = TRUE)
})

test_with_dir("null graph", {
  x <- dataframes_graph(config = list(graph = igraph::make_empty_graph()))
  expect_equal(x, null_graph())
})

test_with_dir("circular non-DAG drake_plans quit in error", {
  x <- drake_plan(a = b, b = c, c = a)
  expect_error(tmp <- capture.output(check_plan(x)))
  expect_error(
    make(x, verbose = FALSE, session_info = FALSE),
    regexp = "[Cc]ircular workflow"
  )
  x <- drake_plan(
    a = b, b = c, c = a, d = 4, e = d,
    A = B, B = C, C = A, mytarget = e
  )
  expect_error(tmp <- capture.output(check_plan(x)))
  expect_error(
    make(x, verbose = FALSE, session_info = FALSE),
    regexp = "[Cc]ircular workflow"
  )
})

test_with_dir("Supplied graph disagrees with the workflow plan", {
  con <- dbug()
  con2 <- drake_config(drake_plan(a = 1), verbose = FALSE)
  expect_warning(
    make(
      plan = con$plan,
      envir = con$envir,
      graph = con2$graph,
      verbose = FALSE,
      session_info = FALSE
    )
  )
})

test_with_dir("graph functions work", {
  config <- dbug()
  expect_equal(
    class(build_drake_graph(config$plan, verbose = FALSE)), "igraph")
  pdf(NULL)
  tmp <- vis_drake_graph(config)
  dev.off()
  pdf(NULL)
  tmp <- vis_drake_graph(config, full_legend = FALSE)
  dev.off()
  unlink("Rplots.pdf", force = TRUE)
  expect_true(is.character(default_graph_title()))
})

test_with_dir("Supplied graph is pruned.", {
  load_mtcars_example()
  graph <- build_drake_graph(my_plan)
  con <- drake_config(my_plan, targets = c("small", "large"), graph = graph)
  vertices <- V(con$graph)$name
  include <- c("small", "simulate", "data.frame", "sample.int", "large")
  exclude <- setdiff(my_plan$target, include)
  expect_true(all(include %in% vertices))
  expect_false(any(exclude %in% vertices))
})

test_with_dir("same graphical arrangements for distributed parallelism", {
  e <- new.env()
  x <- drake_plan(a = 1, b = f(2))
  e$f <- function(x) x
  con <- drake_config(x, envir = e, verbose = FALSE)
  expect_equal(2, max_useful_jobs(config = con))
  expect_equal(2, max_useful_jobs(config = con))
  con$parallelism <- "Makefile"
  expect_equal(2, max_useful_jobs(config = con))
  expect_equal(2, max_useful_jobs(config = con))
  y <- drake_plan(a = 1, b = 2)
  tmp <- dataframes_graph(config = con)
  expect_true(is.list(tmp))
})

test_with_dir("graphing args are not ignored (mtcars example)", {
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs
  parallelism <- scenario$parallelism

  load_mtcars_example(envir = e)
  my_plan <- e$my_plan
  config <- drake_config(my_plan, envir = e,
                         jobs = jobs, parallelism = parallelism,
                         verbose = FALSE)

  tmp <- vis_drake_graph(config = config)
  expect_false(file.exists("Makefile"))

  # Different graph configurations should be checked manually.
  expect_warning(
    tmp <- dataframes_graph(
      config = config, build_times = FALSE, from_scratch = TRUE))
  expect_warning(
    tmp <- dataframes_graph(config = config, split_columns = TRUE))
  expect_warning(
    tmp <- dataframes_graph(config = config, build_times = FALSE))
  tmpcopy <- dataframes_graph(config = config,
    make_imports = FALSE, build_times = "none")
  tmp0 <- dataframes_graph(config = config, build_times = "none",
    subset = c("small", "regression2_large"))
  tmp1 <- dataframes_graph(config = config, build_times = "none",
    from = "small")
  tmp2 <- dataframes_graph(config = config, build_times = "none",
    from = "small", targets_only = TRUE)
  tmp3 <- dataframes_graph(config = config, build_times = "none",
    targets_only = TRUE)
  tmp4 <- dataframes_graph(config = config, build_times = "none",
    targets_only = TRUE)
  tmp5 <- dataframes_graph(config = config, build_times = "build",
    targets_only = TRUE)
  tmp6 <- dataframes_graph(config = config, build_times = "build",
    targets_only = TRUE, from_scratch = FALSE)
  expect_warning(
    tmp7 <- dataframes_graph(config = config, build_times = "none",
                             from = c("small", "not_found"))
  )
  expect_error(
    tmp8 <- dataframes_graph(config = config, build_times = "none",
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
               sort(c(config$plan$target)))
  expect_false(file.exists("Makefile"))

  file <- "graph.html"
  expect_false(file.exists(file))
  vis_drake_graph(config = config, file = file)
  expect_true(file.exists(file))
})
