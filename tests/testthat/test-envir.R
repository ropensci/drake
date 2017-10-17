drake_context("envir")

test_with_dir("prune_envir in full build", {
  # workflow with lots of nested deps This will fail if
  # prune_envir() doesn't work.
  datasets <- plan(x = 1, y = 2, z = 3)
  methods <- plan(
    a = ..dataset.., # nolint
    b = ..dataset.., # nolint
    c = ..dataset.. # nolint
  )
  analyses <- analyses(methods, datasets)
  heuristics <- plan(
    s = c(..dataset.., ..analysis..), # nolint
    t = ..analysis..) # nolint
  summaries <- summaries(
    heuristics,
    datasets = datasets,
    analyses = analyses,
    gather = c("rbind", "rbind")
  )
  output <- plan(
    final1 = mean(s) + mean(t),
    final2 = mean(s) - mean(t),
    waitforme = c(a_x, c_y, s_b_x, t_a_z),
    waitformetoo = c(waitforme, y)
  )
  plan <- rbind(datasets, analyses, summaries, output)

  # set up a workspace to test prune_envir()
  # set verbose to TRUE to see log of loading
  config <- build_config(
    plan,
    targets = plan$target,
    envir = new.env(parent = globalenv()),
    parallelism = "mclapply",
    jobs = 1,
    prepend = character(0),
    verbose = FALSE,
    packages = character(0),
    prework = character(0),
    command = "make",
    args = character(0),
    recipe_command = default_recipe_command(),
    cache = NULL,
    clear_progress = FALSE
  )

  # actually run
  testrun(config)
  expect_true(all(plan$target %in% cached()))

  # Check that the right things are loaded and the right things
  # are discarded
  remove(list = ls(config$envir), envir = config$envir)
  expect_equal(ls(config$envir), character(0))
  prune_envir(datasets$target, config)
  expect_equal(ls(config$envir), character(0))
  prune_envir(analyses$target, config)
  expect_equal(ls(config$envir), c("x", "y", "z"))
  prune_envir("waitforme", config)

  # keep y around for waitformetoo
  expect_equal(
    ls(config$envir),
    c("a_x", "c_y", "s_b_x", "t_a_z", "y")
  )
})
