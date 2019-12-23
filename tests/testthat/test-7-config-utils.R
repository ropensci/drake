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
