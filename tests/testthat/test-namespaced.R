context("namespaced")

test_with_dir("function_dependencies() works on :: and :::", {
  expect_false("g" %in% ls())
  crazy <- function(x, y) {
    z <- g(x) + y
    k <- "local"
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
    function_dependencies(crazy),
    list(
      functions = sort(c(ns, "g", "runif", "sqrt")),
      variables = character(0)
    )
  )
  command <- "digest::digest(stats::rnorm(runif(stats::rpois(100))))"
  d <- sort(deps(command))
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

test_with_dir("namespaced workflow works", {
  dclean()
  opts <- test_opt()
  envir <- dbug()$envir
  rm(list = ls(envir), envir = envir)
  envir$f <- function(x) {
    x <- nchar(digest::digest(sqrt(x)))
    base:::c(x, 1)
  }
  x <- plan(a = base::list(f(1)))
  make(
    x,
    envir = envir,
    jobs = opts$jobs,
    parallelism = opts$parallelism,
    verbose = FALSE
  )
  fromcache <- readd("base::list", character_only = TRUE)
  expect_equal(fromcache(1, "a"), list(1, "a"))
  fromcache2 <- readd("base:::c", character_only = TRUE)
  expect_equal(fromcache2(1, 2), c(1, 2))
  ns <- sort(c("base:::c", "base::list", "digest::digest"))
  expect_true(all(cached(list = ns)))
  expect_true(all(ns %in% imported()))
  expect_equal(
    outdated(x, envir = envir, verbose = FALSE),
    character(0)
  )
  dclean()
})
