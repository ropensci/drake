drake_context("namespaced")

test_with_dir("function_dependencies() works on :: and :::", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_false("g" %in% ls())
  crazy <- function(x, y) {
    z <- g(x) + y
    k <- "local"
    j <- TRUE
    h <- function(x) {
      digest::digest(x)
    }
    digest::digest(stats::rnorm(runif(stats::rpois(100))))
    doesnt:::exist
    outer:::outer(inner::inner(triple:::triple(x) + sqrt(y)))
  }
  ns <- sort(
    c(
      "digest::digest",
      "doesnt:::exist",
      "inner::inner",
      "outer:::outer",
      "stats::rnorm",
      "stats::rpois",
      "triple:::triple"
    )
  )
  expect_equal(sort(find_namespaced_functions(crazy)), ns)
  expect_equal(
    unname(sort(unlist(code_dependencies(crazy)))),
    sort(c(ns, "g", "runif", "sqrt", "local"))
  )
  command <- "digest::digest(stats::rnorm(runif(stats::rpois(100))))"
  d <- sort(deps_code(command))
  expect_equal(
    d,
    sort(
      c(
        "digest::digest",
        "runif",
        "stats::rnorm",
        "stats::rpois"
      )
    )
  )
})

test_with_dir("namespaced drake_plan works", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  scenarios <- get_testing_scenario()
  envir <- dbug()$envir
  rm(list = ls(envir), envir = envir)
  envir$f <- function(x) {
    x <- nchar(digest::digest(sqrt(x)))
    base:::c(x, 1)
  }
  x <- drake_plan(a = base::list(f(1)))
  config <- make(
    x,
    envir = envir,
    jobs = scenarios$jobs,
    parallelism = scenarios$parallelism,
    verbose = FALSE,
    session_info = FALSE
  )
  fromcache <- readd("base::list", character_only = TRUE)
  expect_equal(fromcache(1, "a"), list(1, "a"))
  fromcache2 <- readd("base:::c", character_only = TRUE)
  expect_equal(fromcache2(1, 2), c(1, 2))
  ns <- sort(c("base:::c", "base::list", "digest::digest"))
  expect_true(all(cached(list = ns)))
  expect_true(all(ns %in% imported()))
  expect_equal(
    outdated(config),
    character(0)
  )
})
