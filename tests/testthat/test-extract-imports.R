drake_context("extract imports")

test_with_dir("extract_imports() works", {
  scenario <- get_testing_scenario()
  envir <- eval(parse(text = scenario$envir))
  evalq(
    f <- function(x){
      g(x)
    },
    envir = envir
  )
  evalq(
    g <- function(x){
      digest(x)
    },
    envir = envir
  )
  plan <- drake_plan(
    x = f(1),
    y = digest::digest(x)) # double-scoped functions stop the nesting.
  config <- drake_config(plan, envir = envir)
  n_nodes <- length(igraph::V(config$graph)$name)
  expect_true(n_nodes < 10)
  envir <- extract_imports(digest, envir = envir)
  config <- drake_config(plan, envir = envir)
  n_nodes_new <- length(igraph::V(config$graph)$name)
  expect_true(n_nodes_new > n_nodes)
  make_with_config(config)
  expect_is(readd(x), "character")
})
