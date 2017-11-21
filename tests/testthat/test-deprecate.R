drake_context("deprecation")

test_with_dir("deprecation: future", {
  expect_warning(backend())
})

test_with_dir("deprecation: make() and config()", {
  expect_warning(default_system2_args(jobs = 1, verbose = FALSE))
  expect_warning(make(workplan(x = 1), return_config = TRUE,
    verbose = FALSE))
  expect_warning(make(workplan(x = 1), clear_progress = TRUE,
    verbose = FALSE))
  expect_warning(config(workplan(x = 1)))
})

test_with_dir("deprecation: cache functions", {
  plan <- workplan(x = 1)
  expect_silent(make(plan, verbose = FALSE))
  expect_true(is.numeric(readd(x, search = FALSE)))
  expect_equal(cached(), "x")
  expect_warning(session())
  expect_warning(read_config())
  expect_warning(read_graph())
  expect_warning(read_plan())
})

test_with_dir("workplan deprecation", {
  pl1 <- expect_warning(drake::plan(x = 1, y = x))
  pl2 <- workplan(x = 1, y = x)
  expect_warning(drake::plan())
  expect_warning(drake::plan(x = y, file_targets = TRUE))
  expect_warning(drake::workflow())
  expect_warning(drake::workflow(x = y, file_targets = TRUE))
  expect_warning(check(workplan(a = 1)))
})

test_with_dir("drake version checks in previous caches", {
  # We need to be able to set the drake version
  # to check back compatibility.
  plan <- workplan(x = 1)
  expect_silent(make(plan, verbose = FALSE))
  x <- get_cache()
  x$del(key = "initial_drake_version", namespace = "session")
  expect_false("initial_drake_version" %in% x$list(namespace = "session"))
  set_initial_drake_version(cache = x)
  expect_true("initial_drake_version" %in% x$list(namespace = "session"))
})

test_with_dir("generative templating deprecation", {
  expect_warning(drake::evaluate(workplan()))
  expect_warning(drake::expand(workplan()))
  expect_warning(drake::gather(workplan()))
  datasets <- workplan(
    small = simulate(5),
    large = simulate(50))
  methods <- workplan(
    regression1 = reg1(..dataset..), # nolint
    regression2 = reg2(..dataset..)) # nolint
  expect_warning(
    analyses <- analyses(methods, datasets = datasets))
  summary_types <- workplan(
    summ = summary(..analysis..), # nolint
    coef = coefficients(..analysis..)) # nolint
  expect_warning(
    summaries(summary_types, analyses, datasets))
})

test_with_dir("deprecated graphing functions", {
  pl <- workplan(a = 1)
  expect_warning(build_graph(pl))
  expect_warning(out <- plot_graph(pl))
  df <- dataframes_graph(pl)
  expect_warning(out <- render_graph(df))
})

test_with_dir("deprecated example(s)_drake functions", {
  expect_warning(example_drake())
  expect_warning(examples_drake())
})

test_with_dir("deprecate misc utilities", {
  expect_warning(as_file("x"))
})
