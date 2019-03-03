drake_context("callr")

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
  expect_true(length(r_outdated()) > 1)
  expect_equal(r_missed(), character(0))
  r_make()
  expect_true(is.data.frame(readd(small)))
  expect_equal(r_outdated(), character(0))
  expect_true(is.data.frame(r_drake_build("small")))
  info <- r_drake_graph_info()
  expect_true(all(c("nodes", "edges") %in% names(info)))
  out <- r_predict_workers()
  expect_equal(sort(colnames(out)), c("target", "worker"))
  skip_if_not_installed("lubridate")
  rt1 <- r_predict_runtime()
  rt2 <- r_predict_runtime(default_time = Inf, from_scratch = TRUE)
  expect_true(as.numeric(rt2) > as.numeric(rt1))
  # need automated visual tests
  skip_if_not_installed("ggraph")
  skip_if_not_installed("networkD3")
  skip_if_not_installed("visNetwork")
  expect_silent({
    r_vis_drake_graph()
    r_vis_drake_graph(targets_only = TRUE, main = "new title")
    r_sankey_drake_graph()
    r_drake_ggraph()
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
  expect_equal(r_missed(), character(0))
  r_make()
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
      "drake_config(my_plan)"
    ),
    "my_script.R"
  )
  old <- getOption("drake_source")
  options(drake_source = "overridden.R")
  on.exit(options(drake_source = old))
  expect_false(file.exists(default_drake_source))
  expect_false(file.exists("overridden.R"))
  expect_true(length(r_outdated(source = "my_script.R")) > 1)
  expect_equal(r_missed(source = "my_script.R"), character(0))
  r_make(source = "my_script.R")
  expect_true(is.data.frame(readd(small)))
  expect_equal(r_outdated(source = "my_script.R"), character(0))
})
