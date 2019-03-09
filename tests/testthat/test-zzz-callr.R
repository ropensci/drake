drake_context("callr")

test_with_dir("config file missing", {
  skip_on_cran()
  skip_if_not_installed("callr")
  expect_error(r_make(r_args = list(show = FALSE)), "need an R script file")
})

test_with_dir("basic functions with default _drake.R file", {
  skip_on_cran()
  skip_if_not_installed("callr")
  skip_if_not_installed("knitr")
  if (identical(Sys.getenv("drake_skip_callr"), "true")) {
    skip("Skipping callr tests.")
  }
  writeLines(
    c(
      "library(drake)",
      "load_mtcars_example()",
      "drake_config(my_plan)"
    ),
    default_drake_source
  )
  expect_true(length(r_outdated(r_args = list(show = FALSE))) > 1)
  expect_equal(r_missed(r_args = list(show = FALSE)), character(0))
  r_make(r_args = list(show = FALSE))
  expect_true(is.data.frame(readd(small)))
  expect_equal(r_outdated(r_args = list(show = FALSE)), character(0))
  expect_true(
    is.data.frame(r_drake_build("small", r_args = list(show = FALSE)))
  )
  info <- r_drake_graph_info(r_args = list(show = FALSE))
  expect_true(all(c("nodes", "edges") %in% names(info)))
  out <- r_predict_workers(r_args = list(show = FALSE))
  expect_equal(sort(colnames(out)), c("target", "worker"))
  skip_if_not_installed("lubridate")
  rt1 <- r_predict_runtime(r_args = list(show = FALSE))
  rt2 <- r_predict_runtime(
    default_time = Inf,
    from_scratch = TRUE,
    r_args = list(show = FALSE)
  )
  expect_true(as.numeric(rt2) > as.numeric(rt1))
  # need automated visual tests
  skip_if_not_installed("ggraph")
  skip_if_not_installed("networkD3")
  skip_if_not_installed("visNetwork")
  expect_silent({
    r_vis_drake_graph(r_args = list(show = FALSE))
    r_vis_drake_graph(
      targets_only = TRUE,
      main = "new title",
      r_args = list(show = FALSE)
    )
    r_sankey_drake_graph(r_args = list(show = FALSE))
    r_drake_ggraph(r_args = list(show = FALSE))
    invisible()
  })
})

test_with_dir("supply the source with a global option", {
  skip_on_cran()
  skip_if_not_installed("callr")
  skip_if_not_installed("knitr")
  if (identical(Sys.getenv("drake_skip_callr"), "true")) {
    skip("Skipping callr tests.")
  }
  writeLines(
    c(
      "library(drake)",
      "load_mtcars_example()",
      "drake_config(my_plan)"
    ),
    "my_script.R"
  )
  old <- getOption("drake_source")
  options(drake_source = "my_script.R")
  on.exit(options(drake_source = old))
  expect_false(file.exists(default_drake_source))
  expect_true(length(r_outdated()) > 1)
  r_make(r_args = list(show = FALSE))
  expect_true(is.data.frame(readd(small)))
  expect_equal(r_outdated(), character(0))
})

test_with_dir("supply the source explicitly", {
  skip_on_cran()
  skip_if_not_installed("callr")
  skip_if_not_installed("knitr")
  if (identical(Sys.getenv("drake_skip_callr"), "true")) {
    skip("Skipping callr tests.")
  }
  writeLines(
    c(
      "library(drake)",
      "load_mtcars_example()",
      "drake_config(my_plan, console_log_file = \"log.txt\")"
    ),
    "my_script.R"
  )
  old <- getOption("drake_source")
  options(drake_source = "overridden.R")
  on.exit(options(drake_source = old))
  expect_false(file.exists(default_drake_source))
  expect_false(file.exists("overridden.R"))
  expect_true(length(r_outdated(source = "my_script.R")) > 1)
  expect_false(file.exists("log.txt"))
  r_make(source = "my_script.R", r_args = list(show = FALSE))
  expect_true(file.exists("log.txt"))
  expect_true(is.data.frame(readd(small)))
  expect_equal(r_outdated(source = "my_script.R"), character(0))
})

test_with_dir("r_make() loads packages and sets options", {
  skip_on_cran()
  skip_if_not_installed("abind")
  skip_if_not_installed("callr")
  if (identical(Sys.getenv("drake_skip_callr"), "true")) {
    skip("Skipping callr tests.")
  }
  writeLines(
    c(
      "library(drake)",
      "library(abind)",
      "options(drake_abind_opt = 2L)",
      "plan <- drake_plan(x = abind(1L, getOption(\"drake_abind_opt\")))",
      "drake_config(plan, console_log_file = \"log.txt\")"
    ),
    default_drake_source
  )
  options(drake_abind_opt = NULL)
  expect_null(getOption("drake_abind_opt"))
  r_make(r_args = list(show = FALSE))
  expect_equal(sort(as.integer(readd(x))), sort(c(1L, 2L)))
  options(drake_abind_opt = NULL)
})

test_with_dir("configuring callr", {
  skip_on_cran()
  skip_if_not_installed("callr")
  skip_if_not_installed("knitr")
  if (identical(Sys.getenv("drake_skip_callr"), "true")) {
    skip("Skipping callr tests.")
  }
  writeLines(
    c(
      "library(drake)",
      "load_mtcars_example()",
      "drake_config(my_plan, verbose = 6)"
    ),
    default_drake_source
  )
  expect_false(file.exists("stdout.log"))
  r_make(r_fn = callr::r, r_args = list(stdout = "stdout.log", show = FALSE))
  expect_true(file.exists("stdout.log"))
  expect_true(is.data.frame(readd(small)))
  expect_equal(r_outdated(r_args = list(show = FALSE)), character(0))
})
