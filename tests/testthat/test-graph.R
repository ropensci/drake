drake_context("graph")

test_with_dir("Recursive functions are okay", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
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
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_error(prune_drake_graph(12345, to = "node"))
})

test_with_dir("graph does not fail if input file is binary", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
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
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- drake_graph_info(config = list(graph = igraph::make_empty_graph()))
  expect_equal(x, null_graph())
})

test_with_dir("circular non-DAG drake_plans quit in error", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
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
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
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
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  config <- dbug()
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
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  load_mtcars_example()
  con0 <- drake_config(my_plan)
  con <- drake_config(
    my_plan, targets = c("small", "large"), graph = con0$graph)
  vertices <- V(con$graph)$name
  include <- c("small", "simulate", "data.frame", "sample.int", "large")
  exclude <- setdiff(my_plan$target, include)
  expect_true(all(include %in% vertices))
  expect_false(any(exclude %in% vertices))
})

test_with_dir("graphing args are not ignored (mtcars example)", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
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
    tmp <- drake_graph_info(
      config = config, build_times = FALSE, from_scratch = TRUE))
  expect_warning(
    tmp <- drake_graph_info(config = config, split_columns = TRUE))
  expect_warning(
    tmp <- drake_graph_info(config = config, build_times = FALSE))
  tmpcopy <- drake_graph_info(config = config,
    make_imports = FALSE, build_times = "none")
  tmp0 <- drake_graph_info(config = config, build_times = "none",
    subset = c("small", "regression2_large"))
  tmp1 <- drake_graph_info(config = config, build_times = "none",
    from = "small")
  tmp2 <- drake_graph_info(config = config, build_times = "none",
    from = "small", targets_only = TRUE)
  tmp3 <- drake_graph_info(config = config, build_times = "none",
    targets_only = TRUE)
  tmp4 <- drake_graph_info(config = config, build_times = "none",
    targets_only = TRUE)
  tmp5 <- drake_graph_info(config = config, build_times = "build",
    targets_only = TRUE)
  tmp6 <- drake_graph_info(config = config, build_times = "build",
    targets_only = TRUE, from_scratch = FALSE)
  expect_warning(
    tmp7 <- drake_graph_info(config = config, build_times = "none",
                             from = c("small", "not_found"))
  )
  expect_error(
    tmp8 <- drake_graph_info(config = config, build_times = "none",
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
