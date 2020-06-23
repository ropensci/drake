drake_context("callr")

test_with_dir("r_make_message", {
  skip_on_cran()
  expect_message(r_make_message(force = TRUE))
})

test_with_dir("config file missing", {
  skip_on_cran()
  skip_if_not_installed("callr")
  expect_error(
    r_make(r_args = list(show = FALSE)),
    "file _drake.R does not exist"
  )
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
  expect_true(length(r_recoverable(r_args = list(show = FALSE))) == 0L)
  expect_equal(r_missed(r_args = list(show = FALSE)), character(0))
  deps <- r_deps_target(regression1_small, r_args = list(show = FALSE))
  expect_equal(sort(deps$name), sort(c("reg1", "small")))
  expect_equal(unique(deps$type), "globals")
  r_make(r_args = list(show = FALSE))
  expect_true(nrow(drake_progress()) > 0L)
  expect_true(is.data.frame(readd(small)))
  expect_equal(r_outdated(r_args = list(show = FALSE)), character(0))
  expect_true(
    is.data.frame(r_drake_build(small, r_args = list(show = FALSE)))
  )
  skip_if_not_installed("visNetwork")
  info <- r_drake_graph_info(
    r_args = list(show = FALSE),
    build_times = "none"
  )
  expect_true(all(c("nodes", "edges") %in% names(info)))
  skip_if_not_installed("lubridate")
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
  skip_if_not_installed("txtplot")
  expect_silent({
    graph <- r_vis_drake_graph(r_args = list(show = FALSE))
    r_vis_drake_graph(
      targets_only = TRUE,
      main = "new title",
      r_args = list(show = FALSE)
    )
    r_sankey_drake_graph(r_args = list(show = FALSE))
    r_drake_ggraph(r_args = list(show = FALSE))
    r_text_drake_graph(r_args = list(show = FALSE))
    invisible()
  })
  expect_true(inherits(graph, "visNetwork"))
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
      "drake_config(my_plan, log_make = \"log.txt\")"
    ),
    "my_script.R"
  )
  expect_false(file.exists("log.txt"))
  old <- getOption("drake_source")
  options(drake_source = "overridden.R")
  on.exit(options(drake_source = old))
  expect_false(file.exists(default_drake_source))
  expect_false(file.exists("overridden.R"))
  expect_true(length(r_outdated(source = "my_script.R")) > 1)
  expect_true(file.exists("log.txt"))
  unlink("log.txt")
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
      "drake_config(plan, log_make = \"log.txt\")"
    ),
    default_drake_source
  )
  options(drake_abind_opt = NULL)
  expect_null(getOption("drake_abind_opt"))
  r_make(r_args = list(show = FALSE))
  expect_equal(sort(as.integer(readd(x))), sort(c(1L, 2L)))
  options(drake_abind_opt = NULL)
})

test_with_dir("configuring a background callr process", {
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
      "drake_config(my_plan, verbose = 1L)"
    ),
    default_drake_source
  )
  expect_false(file.exists("stdout.log"))
  px <- r_make(r_fn = callr::r_bg, r_args = list(stdout = "stdout.log"))
  while (px$is_alive()) {
    Sys.sleep(1e-2)
  }
  expect_true(file.exists("stdout.log"))
  expect_true(is.data.frame(readd(small)))
  expect_equal(r_outdated(r_args = list(show = FALSE)), character(0))
  px$kill()
})

test_with_dir("callr RStudio addins", {
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
      "drake_config(my_plan, verbose = FALSE)"
    ),
    default_drake_source
  )
  r_args <- list(show = FALSE)
  expect_true(length(rs_addin_r_outdated(r_args, .print = FALSE)) > 1)
  rs_addin_r_make(r_args)
  expect_equal(rs_addin_r_outdated(r_args, .print = FALSE), character(0))
  skip_if_not_installed("lubridate")
  skip_if_not_installed("visNetwork")
  graph <- rs_addin_r_vis_drake_graph(r_args, .print = FALSE)
  expect_true(inherits(graph, "visNetwork"))
})

test_with_dir("errors keep their informative messages (#969)", {
  skip_on_cran()
  skip_if_not_installed("callr")
  code <- c(
    "library(drake)",
    "my_plan <- drake_plan(",
    "  x = stop('7a3f077af6c8e425')",
    ")",
    "drake_config(my_plan)"
  )
  writeLines(code, "_drake.R")
  expect_error(
    r_make(r_args = list(show = FALSE)),
    regexp = "7a3f077af6c8e425",
    class = "callr_error"
  )
})

test_with_dir("drake_script() can generate an example script", {
  drake_script()
  expect_true(length(readLines("_drake.R")) > 0L)
})

test_with_dir("drake_script() can generate a single symbol", {
  drake_script(x)
  expect_equal(readLines("_drake.R"), "x")
})

test_with_dir("drake_script() can generate multiple lines", {
  drake_script({
    x
    y
  })
  expect_equal(readLines("_drake.R"), c("x", "y"))
})
