context("extract-imports")

test_with_dir("extract_imports() works", {
  scenario <- get_testing_scenario()
  envir <- eval(parse(text = scenario$envir))
  envir$f <- function(x){
    g(x)
  }
  envir$g <- function(x){
    hmac(x)
  }
  plan <- drake_plan(
    x = f(1),
    y = digest::digest(x)) # double-scoped functions stop the nesting.
  config <- drake_config(plan, envir = envir)
  n_nodes <- length(igraph::V(config$graph)$name)
  expect_true(n_nodes < 10)
  extract_imports(digest, envir = envir)
  config <- drake_config(plan, envir = envir)
  n_nodes_new <- length(igraph::V(config$graph)$name)
  expect_true(n_nodes_new > n_nodes)
})
