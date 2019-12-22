drake_context("deprecate config")

test_with_dir("drake_config_parent() (#1118)", {
  fun <- drake_config_parent(n = 17L)
  expect_equal(formals(fun)$envir$n, 17L)
})

test_with_dir("deprecate outdated(config) (#1118)", {
  skip_on_cran()
  a <- "x"
  plan <- drake_plan(x = a, y = a)
  make(plan)
  # legacy outdated(config) syntax
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
  # with plan and formal outdated() args
  tmp <- outdated(plan, make_imports = FALSE)
  expect_equal(tmp, character(0))
  # with plan and informal make() args
  tmp <- outdated(plan, "x")
  expect_equal(tmp, "x")
  expect_equal(sort(outdated(plan)), sort(plan$target))
  make(plan)
  expect_equal(outdated(plan), character(0))
  tmp <- outdated(plan, "x", new.env())
  expect_equal(tmp, "x")
})
