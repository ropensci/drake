drake_context("dependencies")

test_with_dir("unparsable commands are handled correctly", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- "bluh$"
  expect_false(is_parsable(x))
  expect_error(deps_code(x))
})

test_with_dir("magrittr dot is ignored", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_equal(
    sort(clean_dependency_list(deps_code("sqrt(x + y + .)"))),
    sort(c("sqrt", "x", "y"))
  )
  expect_equal(
    sort(clean_dependency_list(
      deps_code("dplyr::filter(complete.cases(.))"))),
    sort(c("complete.cases", "dplyr::filter"))
  )
})

test_with_dir("file_out() and knitr_in(): commands vs imports", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  cmd <- "file_in(\"x\"); file_out(\"y\"); knitr_in(\"report.Rmd\")"
  f <- function(){
    file_in("x")
    file_out("y")
    knitr_in("report.Rmd")
  }
  file.create("x")
  file.create("y")
  path <- system.file(
    file.path("rmarkdown", "mtcars.Rmd"),
    package = "drake",
    mustWork = TRUE
  )
  file.copy(
    from = path, to = file.path(getwd(), "report.Rmd"), overwrite = TRUE)
  x <- command_dependencies(cmd)
  x0 <- list(
    file_in = "\"x\"", file_out = "\"y\"", loadd = "large",
    readd = c("small", "coef_regression2_small"),
    knitr_in = "\"report.Rmd\"")
  expect_equal(length(x), length(x0))
  for (i in names(x)){
    expect_equal(sort(x[[i]]), sort(x0[[i]]) )
  }
  y <- import_dependencies(f)
  y0 <- list(
    file_in = "\"x\"",
    knitr_in = "\"report.Rmd\"",
    loadd = "large",
    readd = c("small", "coef_regression2_small")
  )
  expect_equal(length(y), length(y0))
  for (i in names(y)){
    expect_equal(sort(y[[i]]), sort(y0[[i]]) )
  }
  expect_equal(
    sort(clean_dependency_list(deps_code(f))), sort(unname(unlist(y))))
  expect_equal(
    sort(clean_dependency_list(deps_code(cmd))),
    sort(
      c("coef_regression2_small", "large",
        "\"report.Rmd\"", "small", "\"x\"", "\"y\""
      )
    )
  )
})

test_with_dir(
  "deps_code() correctly reports dependencies of functions and commands", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_equal(length(deps_code("")), 0)
  expect_equal(length(command_dependencies(NA)), 0)
  expect_equal(length(command_dependencies(NULL)), 0)
  expect_equal(length(command_dependencies(character(0))), 0)
  expect_equal(clean_dependency_list(deps_code(base::c)), character(0))
  expect_equal(clean_dependency_list(deps_code(base::list)), character(0))
  expect_equal(clean_dependency_list(deps_code(NA)), character(0))
  f <- function(x, y) {
    out <- x + y + g(x)
    saveRDS(out, "out.rds")
  }
  expect_false(is_vectorized(f))
  expect_false(is_vectorized("char"))
  expect_equal(
    sort(clean_dependency_list(deps_code(f))), sort(c("g", "saveRDS")))
  my_plan <- drake_plan(
    x = 1 + some_object,
    my_target = x + readRDS(file_in("tracked_input_file.rds")),
    return_value = f(x, y, g(z + w)),
    botched = read.csv(file_in(nothing)),
    meta = read.table(file_in("file_in")),
    strings_in_dots = "literals"
  )
  expect_equal(
    clean_dependency_list(deps_code(my_plan$command[1])), "some_object")
  expect_equal(sort(
    clean_dependency_list(deps_code(my_plan$command[2]))),
    sort(c("\"tracked_input_file.rds\"", "readRDS", "x")))
  expect_equal(sort(
    clean_dependency_list(deps_code(my_plan$command[3]))), sort(c("f", "g", "w",
    "x", "y", "z")))
  expect_equal(sort(
    clean_dependency_list(deps_code(my_plan$command[4]))), sort(c("read.csv")))
  expect_equal(
    sort(clean_dependency_list(deps_code(my_plan$command[5]))),
    sort(c("read.table", "\"file_in\"")))
})

test_with_dir("tracked() works", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  config <- dbug()
  x <- sort(tracked(config))
  y <- sort(c("\"intermediatefile.rds\"", "drake_target_1",
    "yourinput", "nextone",
    "combined", "myinput", "final", "j", "i", "h", "g", "f",
    "c", "b", "a", "saveRDS", "\"input.rds\"", "readRDS"))
  expect_equal(x, y)
})

test_with_dir("missing files via check_plan()", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  config <- dbug()
  expect_silent(check_plan(config$plan, envir = config$envir))
  expect_silent(tmp <- missing_input_files(config))
  unlink("input.rds", force = TRUE)
  expect_warning(
    tmp <- capture.output(check_plan(config$plan, envir = config$envir)))
  expect_warning(tmp <- missing_input_files(config))
})

test_with_dir("Vectorized nested functions work", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  e <- new.env(parent = globalenv())
  eval(parse(text = "f <- Vectorize(function(x) g(x), \"x\")"),
       envir = e)
  eval(parse(text = "g <- function(x) x + y"), envir = e)
  e$y <- 7
  config <- dbug()
  config$envir <- e
  config$plan <- drake_plan(a = f(1:10))
  config$targets <- "a"
  expect_equal(clean_dependency_list(deps_code(e$f)), "g")
  expect_equal(clean_dependency_list(deps_code(e$g)), "y")

  config <- testrun(config)
  if ("a" %in% ls(config$envir)){
    rm(a, envir = config$envir)
  }
  expect_equal(readd(a), 8:17)
  k <- readd(f)
  expect_equal(k(2:5), 9:12)
  expect_equal(character(0), outdated(config))
  config$envir$y <- 8
  expect_equal("a", outdated(config))

  # Target "a" should react.
  config <- testrun(config)
  expect_equal(character(0), outdated(config))
  expect_equal(readd(a), 9:18)

  # Change a vectorized function and see target "a" react.
  eval(parse(text = "f <- Vectorize(function(x){g(x) + 3}, \"x\")"),
       envir = e)
  config <- testrun(config)
  expect_equal(justbuilt(config), "a")
  expect_equal(readd(a), 12:21)
})

test_with_dir("deps_targets()", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  load_mtcars_example()
  config <- drake_config(my_plan, cache = storr::storr_environment())
  expect_equal(
    sort(deps_targets("report", config = config)),
    sort(
      c(
        "coef_regression2_small", "knit", "large",
        "small", file_store("report.Rmd")
      )
    )
  )
  expect_equal(
    sort(deps_targets("regression1_small", config = config)),
    sort(c("reg1", "small"))
  )
  expect_equal(
    sort(deps_targets(c("small", "large"), config = config, reverse = TRUE)),
    sort(c(
      "regression1_large", "regression1_small",
      "regression2_large", "regression2_small",
      "report"
    ))
  )
  config <- dbug()
  deps <- sort(deps_targets(config$targets, config))
  truth <- sort(c(
    "combined", "saveRDS", "f", "g", "myinput", file_store("input.rds"),
    "nextone", "yourinput", "readRDS", "drake_target_1"
  ))
  expect_equal(deps, truth)
})

test_with_dir("dependency profile", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  b <- 1
  config <- make(drake_plan(a = b), session_info = FALSE)
  expect_error(
    dependency_profile(target = missing, config = config),
    regexp = "no recorded metadata"
  )
  expect_false(any(dependency_profile(target = a, config = config)$changed))
  b <- 2
  expect_false(any(dependency_profile(target = a, config = config)$changed))
  config$skip_targets <- TRUE
  make(config = config)
  dp <- dependency_profile(target = a, config = config)
  expect_true(
    all(as.logical(dp[dp$name == c("_depend_trigger", "b"), "changed"]))
  )
  expect_equal(sum(dp$changed), 2)
  config$plan$command <- "b + c"
  dp <- dependency_profile(target = a, config = config)
  expect_true(as.logical(dp[dp$name == "_command_trigger", "changed"]))
  expect_equal(sum(dp$changed), 3)
  config <- drake_config(config$plan)
  dp <- dependency_profile(target = a, config = config)
  expect_true("c" %in% dp$name)
  expect_true(dp$changed[dp$name == "c"])
  expect_true(is.na(dp$old_hash[dp$name == "c"]))
  expect_true(is.na(dp$new_hash[dp$name == "c"]))
  c <- 2
  make(config = config)
  dp <- dependency_profile(target = a, config = config)
  expect_true("c" %in% dp$name)
  expect_false(dp$changed[dp$name == "c"])
  expect_false(is.na(dp$old_hash[dp$name == "c"]))
  expect_false(is.na(dp$new_hash[dp$name == "c"]))
})

test_with_dir("self-referential commands and imports", {
  f <- function(x, ...){
    dplyr::select(x, f)
  }
  x <- data.frame(f = 123)
  plan <- drake_plan(y = f(x, y))
  cache <- storr::storr_environment()
  o <- make(plan, cache = cache, session_info = FALSE)
  expect_equal(justbuilt(o), "y")
  log1 <- drake_cache_log(cache = cache)
  o <- make(plan, cache = cache, session_info = FALSE)
  expect_true(nobuild(o))
  log2 <- drake_cache_log(cache = cache)
  expect_equal(log1, log2)
})
