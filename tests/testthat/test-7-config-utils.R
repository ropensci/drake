drake_context("config utils")

test_with_dir("config_util_body() (#1118)", {
  body <- config_util_body(outdated_impl)
  vars <- all.vars(body)
  expect_true("outdated_impl" %in% vars)
  expect_false("impl_fun" %in% vars)
})

test_with_dir("drake_config_parent() (#1118)", {
  fun <- drake_config_parent(n = 17L)
  expect_equal(formals(fun)$envir$n, 17L)
})

test_with_dir("outdated(plan) (#1118)", {
  skip_on_cran()
  a <- "x"
  plan <- drake_plan(x = a, y = a)
  make(plan)
  # legacy fun(config) syntax
  config <- drake_config(plan)
  # with informal config arg
  expect_warning(tmp <- outdated(config), "deprecated")
  # with formal config arg
  expect_warning(tmp <- outdated(config = config), "deprecated")
  expect_equal(tmp, character(0))
  a <- "y"
  # informal secondary arg
  expect_warning(tmp <- outdated(config, FALSE), "deprecated")
  expect_equal(tmp, character(0))
  # formal secondary arg
  expect_warning(tmp <- outdated(config, make_imports = FALSE), "deprecated")
  expect_equal(tmp, character(0))
  expect_warning(tmp <- outdated(config), "deprecated")
  expect_equal(sort(tmp), sort(plan$target))
  make(plan)
  # with plan
  tmp <- outdated(plan)
  expect_equal(tmp, character(0))
  a <- "z"
  # with plan and formal fun() args
  tmp <- outdated(plan, make_imports = FALSE)
  expect_equal(tmp, character(0))
  # with plan and informal make() args
  tmp <- outdated(plan, "x")
  expect_equal(tmp, "x")
  expect_equal(sort(outdated(plan)), sort(plan$target))
  make(plan)
  tmp <- outdated(plan)
  expect_equal(tmp, character(0))
  # envir is forced at the proper time
  tmp <- outdated(plan, envir = environment())
  expect_equal(tmp, character(0))
  # envir is used, formally and informally
  tmp <- outdated(plan, "x", new.env())
  expect_equal(tmp, "x")
  tmp <- outdated(plan, "x", envir = new.env())
  expect_equal(tmp, "x")
})

test_with_dir("drake_build(plan) (#1118)", {
  skip_on_cran()
  a <- "x"
  plan <- drake_plan(x = a, y = a)
  make(plan)
  # legacy fun(config) syntax
  config <- drake_config(plan)
  # with informal config arg
  expect_warning(tmp <- drake_build(x, config), "deprecated")
  expect_equal(tmp, "x")
  expect_warning(tmp <- drake_build(target = x, config), "deprecated")
  expect_equal(tmp, "x")
  expect_warning(
    tmp <- drake_build("x", config, character_only = TRUE),
    "deprecated"
  )
  expect_equal(tmp, "x")
  # with formal config arg
  expect_warning(tmp <- drake_build(x, config = config), "deprecated")
  expect_equal(tmp, "x")
  expect_warning(
    tmp <- drake_build("x", config = config, character_only = TRUE),
    "deprecated"
  )
  expect_equal(tmp, "x")
  a <- "y"
  # with plan
  tmp <- drake_build(x, plan)
  expect_equal(tmp, "y")
  tmp <- drake_build("x", plan, character_only = TRUE)
  expect_equal(tmp, "y")
})

test_with_dir("recoverable(plan) (#1118)", {
  skip_on_cran()
  plan <- drake_plan(x = "x", y = x)
  make(plan)
  clean()
  # legacy fun(config) syntax
  config <- drake_config(plan)
  # with informal config arg
  expect_warning(tmp <- recoverable(config), "deprecated")
  expect_equal(tmp, "x")
  # with formal config arg
  expect_warning(tmp <- recoverable(config = config), "deprecated")
  expect_equal(tmp, "x")
  # informal secondary arg
  expect_warning(tmp <- recoverable(config, FALSE), "deprecated")
  expect_equal(tmp, "x")
  # formal secondary arg
  expect_warning(
    tmp <- recoverable(config, make_imports = FALSE),
    "deprecated"
  )
  expect_equal(tmp, "x")
  expect_warning(tmp <- recoverable(config), "deprecated")
  expect_equal(tmp, "x")
  # with plan
  tmp <- recoverable(plan)
  expect_equal(tmp, "x")
  # with plan and formal fun() args
  tmp <- recoverable(plan, make_imports = FALSE)
  expect_equal(tmp, "x")
  # with plan and informal make() args
  tmp <- recoverable(plan, "x")
  expect_equal(tmp, "x")
  expect_equal(recoverable(plan), "x")
  tmp <- recoverable(plan)
  expect_equal(tmp, "x")
  # envir is forced at the proper time
  tmp <- recoverable(plan, envir = environment())
  expect_equal(tmp, "x")
  # envir is used, formally and informally
  tmp <- recoverable(plan, "x", new.env())
  expect_equal(tmp, "x")
  tmp <- recoverable(plan, "x", envir = new.env())
  expect_equal(tmp, "x")
})

test_with_dir("missed(plan) (#1118)", {
  skip_on_cran()
  plan <- drake_plan(x = missing::fun(arg), y = mis2::fun2(x))
  expect_equal(missed(plan, targets = "x"), "missing::fun")
  config <- drake_config(plan, targets = "x")
  expect_warning(tmp <- missed(config))
  expect_equal(tmp, "missing::fun")
})

test_with_dir("deps_target(plan) (#1118)", {
  skip_on_cran()
  plan <- drake_plan(x = 1, y = x)
  expect_equal(nrow(deps_target(x, plan)), 0L)
  expect_equal(deps_target(y, plan)$name, "x")
  config <- drake_config(plan, targets = "x")
  expect_warning(tmp <- deps_target(x, config))
  expect_warning(tmp <- nrow(deps_target(x, config)))
  expect_equal(tmp, 0L)
  expect_warning(tmp <- deps_target(y, config)$name)
  expect_equal(tmp, "x")
})

test_with_dir("deps_profile(plan) (#1118)", {
  skip_on_cran()
  plan <- drake_plan(x = 1, y = x)
  make(plan)
  expect_equal(nrow(deps_profile(x, plan)), 5L)
  config <- drake_config(plan)
  expect_warning(tmp <- deps_profile(x, config))
})

test_with_dir("drake_graph_info(plan) (#1118)", {
  skip_on_cran()
  skip_if_not_installed("visNetwork")
  plan <- drake_plan(x = 1, y = x)
  tmp <- drake_graph_info(plan, targets = "x")
  expect_equal(nrow(tmp$nodes), 1L)
  config <- drake_config(plan)
  expect_warning(tmp <- drake_graph_info(config))
  expect_equal(nrow(tmp$nodes), 2L)
})

test_with_dir("predict_runtime(plan) etc. (#1118)", {
  skip_on_cran()
  skip_if_not_installed("lubridate")
  plan <- drake_plan(x = 1, y = x)
  make(plan)
  tmp <- predict_runtime(plan, targets_predict = "x")
  expect_equal(length(tmp), 1L)
  tmp <- predict_workers(plan, targets_predict = "x")
  expect_equal(nrow(tmp), 1L)
  config <- drake_config(plan)
  expect_warning(tmp <- predict_runtime(config))
  expect_equal(length(tmp), 1L)
  expect_warning(tmp <- predict_workers(config))
  expect_equal(nrow(tmp), 2L)
})

test_with_dir("vis_drake_graph(plan) etc. (#1118)", {
  skip_on_cran()
  skip_if_not_installed("ggraph")
  skip_if_not_installed("networkD3")
  skip_if_not_installed("txtplot")
  skip_if_not_installed("visNetwork")
  plan <- drake_plan(x = 1, y = x)
  make(plan)
  config <- drake_config(plan)
  tmp <- vis_drake_graph(plan, targets = "x")
  tmp <- sankey_drake_graph(plan)
  tmp <- drake_ggraph(plan)
  tmp <- text_drake_graph(plan, targets = "x")
  expect_warning(tmp <- vis_drake_graph(config))
  expect_warning(tmp <- sankey_drake_graph(config))
  expect_warning(tmp <- drake_ggraph(config))
  expect_warning(tmp <- text_drake_graph(config))
})
