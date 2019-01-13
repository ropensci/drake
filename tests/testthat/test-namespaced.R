drake_context("namespaced")

test_with_dir("function_dependencies() works on :: and :::", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_false("g" %in% ls())
  crazy <- function(x, y) {
    z <- g(x) + y
    k <- "local"
    j <- TRUE
    h <- function(x) {
      pkgx::pkgx(x)
    }
    pkgx::pkgx(mypkg1::myfun3(myfun1(mypkg1::myfun2(100))))
    doesnt:::exist
    outer:::outer(inner::inner(triple:::triple(x) + sqrt(y)))
  }
  ns <- sort(
    c(
      "pkgx::pkgx",
      "doesnt:::exist",
      "inner::inner",
      "outer:::outer",
      "mypkg1::myfun3",
      "mypkg1::myfun2",
      "triple:::triple"
    )
  )
  cd <- analyze_code(crazy)
  cd <- decode_deps_list(cd)
  expect_equal(sort(cd$namespaced), ns)
  cd <- analyze_code(crazy)
  cd <- decode_deps_list(cd)
  expect_equal(
    unname(sort(unlist(cd))),
    sort(c(ns, "g", "myfun1", "sqrt", "local"))
  )
  command <- "pkgx::pkgx(mypkg1::myfun3(myfun1(mypkg1::myfun2(100))))"
  d <- sort(clean_dependency_list(deps_code(command)))
  expect_equal(
    d,
    sort(
      c(
        "pkgx::pkgx",
        "myfun1",
        "mypkg1::myfun3",
        "mypkg1::myfun2"
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
    x <- base::nchar(sqrt(x))
    base:::c(x, 1)
  }
  x <- drake_plan(a = base::list(f(1)))
  make(
    x,
    envir = envir,
    jobs = scenarios$jobs,
    parallelism = scenarios$parallelism,
    verbose = FALSE,
    session_info = FALSE
  )
  config <- drake_config(
    x,
    envir = envir,
    jobs = scenarios$jobs,
    parallelism = scenarios$parallelism,
    verbose = FALSE,
    session_info = FALSE
  )
  fromcache <- readd("base::list", character_only = TRUE)
  expect_true(is.character(fromcache))
  fromcache2 <- readd("base:::c", character_only = TRUE)
  expect_true(is.character(fromcache2))
  ns <- sort(c("base:::c", "base::list", "base::nchar"))
  expect_true(all(cached(list = ns)))
  expect_true(all(ns %in% setdiff(cached(),
                                  cached(no_imported_objects = TRUE))))
  expect_equal(
    outdated(config),
    character(0)
  )
})
