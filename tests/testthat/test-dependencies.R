drake_context("dependencies")

test_with_dir("unparsable commands are handled correctly", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- "bluh$"
  expect_error(deps_code(x))
})

test_with_dir("dot symbol is illegal", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_equal(
    sort(clean_dependency_list(deps_code("sqrt(x + y + .)"))),
    sort(c("sqrt", "x", "y"))
  )
  expect_equal(
    sort(clean_dependency_list(
      deps_code("subset(complete.cases(.))"))),
    sort(c("complete.cases", "subset"))
  )
  plan <- drake_plan(
    x = 1,
    y = 2,
    a = sqrt(x + y + .),
    b = subset(complete.cases(.))
  )
  e <- environment()
  expect_false(exists(".", envir = e))
  config <- drake_config(plan)
  plan <- drake_plan(
    . = 1,
    y = 2,
    a = sqrt(x + y),
    b = subset(complete.cases(.))
  )
  expect_error(drake_config(plan), "cannot be target names")
})

test_with_dir("file_out() and knitr_in(): commands vs imports", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  cmd <- "file_in(\"x\"); file_out(\"y\"); knitr_in(\"report.Rmd\")"
  f <- function() {
    file_in("x")
    file_out("y")
    knitr_in("report.Rmd")
  }
  file.create("x")
  file.create("y")
  path <- system.file(
    file.path("examples", "mtcars", "report.Rmd"),
    package = "drake",
    mustWork = TRUE
  )
  file.copy(
    from = path, to = file.path(getwd(), "report.Rmd"), overwrite = TRUE)
  x <- command_dependencies(cmd)
  x <- decode_deps_list(x)
  x0 <- list(
    file_in = "x", file_out = "y", loadd = "large",
    readd = c("small", "coef_regression2_small"),
    knitr_in = "report.Rmd")
  expect_equal(length(x), length(x0))
  for (i in names(x)) {
    expect_equal(sort(x[[i]]), sort(x0[[i]]))
  }
  y <- import_dependencies(f)
  y <- decode_deps_list(y)
  y0 <- list(
    file_in = "x",
    knitr_in = "report.Rmd",
    loadd = "large",
    readd = c("small", "coef_regression2_small")
  )
  expect_equal(length(y), length(y0))
  for (i in names(y)) {
    expect_equal(sort(y[[i]]), sort(y0[[i]]))
  }
  expect_equal(
    sort(clean_dependency_list(deps_code(f))), sort(unname(unlist(y))))
  expect_equal(
    sort(clean_dependency_list(deps_code(cmd))),
    sort(
      c("coef_regression2_small", "large",
        "report.Rmd", "small", "x", "y"
      )
    )
  )
})

test_with_dir("deps_code() and deps_target()", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_equal(length(deps_code("")), 0)
  expect_equal(length(command_dependencies(NA)), 0)
  expect_equal(length(command_dependencies(NULL)), 0)
  expect_equal(length(command_dependencies(character(0))), 0)
  expect_equal(clean_dependency_list(deps_code(base::c)), character(0))
  expect_equal(clean_dependency_list(deps_code(base::list)), character(0))
  f <- function(x, y) {
    out <- x + y + g(x)
    saveRDS(out, "out.rds")
  }
  expect_false(is_vectorized(f))
  expect_false(is_vectorized("char"))
  expect_equal(
    sort(clean_dependency_list(deps_code(f))),
    sort(c("g", "saveRDS"))
  )
  my_plan <- drake_plan(
    x = 1 + some_object,
    my_target = x + readRDS(file_in("tracked_input_file.rds")),
    return_value = f(x, y, g(z + w)),
    botched = read.csv(file_in(nothing)),
    meta = read.table(file_in("file_in"))
  )
  config <- drake_config(
    my_plan,
    session_info = FALSE,
    cache = storr::storr_environment()
  )
  expect_equal(
    clean_dependency_list(deps_code(my_plan$command[1])), "some_object")
  expect_equal(sort(
    clean_dependency_list(deps_code(my_plan$command[2]))),
    sort(c("tracked_input_file.rds", "x", "readRDS")))
  expect_equal(sort(
    clean_dependency_list(deps_code(my_plan$command[3]))),
    sort(c("f", "g", "w", "x", "y", "z"))
  )
  expect_equal(sort(
    clean_dependency_list(
      deps_code(my_plan$command[4]))),
    sort(c("read.csv"))
  )
  expect_equal(
    sort(clean_dependency_list(deps_code(my_plan$command[5]))),
    sort(c("read.table", "file_in")))
  expect_true(!length(deps_target(x, config)))
  expect_equal(sort(
    clean_dependency_list(deps_target(my_target, config))),
    sort(c("tracked_input_file.rds", "x")))
  expect_equal(sort(
    clean_dependency_list(deps_target(return_value, config))),
    sort(c("f", "x")))
  expect_equal(sort(
    clean_dependency_list(deps_target(botched, config))),
    character(0))
  expect_equal(sort(
    clean_dependency_list(deps_target(meta, config))),
    sort("file_in"))
})

test_with_dir("tracked() works", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  config <- dbug()
  x <- sort(tracked(config))
  y <- sort(c(display_keys(encode_path("intermediatefile.rds")),
              "drake_target_1",
    "yourinput", "nextone",
    "combined", "myinput", "final", "j", "i", "h", "g", "f",
    "c", "b", "a",  display_keys(encode_path("input.rds"))))
  expect_equal(x, y)
})

test_with_dir("missing input files", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  config <- dbug()
  expect_silent(check_plan(config$plan, envir = config$envir))
  expect_silent(tmp <- missing_input_files(config))
  unlink("input.rds", force = TRUE)
  expect_warning(tmp <- missing_input_files(config))
  expect_silent(tmp <- config_checks(config))
  expect_warning(runtime_checks(config), regexp = "missing")
  config$skip_safety_checks <- TRUE
  expect_silent(tmp <- runtime_checks(config))
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
  expect_equal(clean_dependency_list(deps_code(e$f)), sort(c("g")))
  expect_equal(clean_dependency_list(deps_code(e$g)), sort(c("y")))

  testrun(config)
  config <- testconfig(config)
  if ("a" %in% ls(config$envir)) {
    rm(a, envir = config$envir)
  }
  expect_equal(readd(a), 8:17)
  k <- readd(f)
  expect_equal(k(2:5), 9:12)
  expect_equal(character(0), outdated(config))
  config$envir$y <- 8
  expect_equal("a", outdated(config))

  # Target "a" should react.
  testrun(config)
  config <- testconfig(config)
  expect_equal(character(0), outdated(config))
  expect_equal(readd(a), 9:18)

  # Change a vectorized function and see target "a" react.
  eval(parse(text = "f <- Vectorize(function(x) {g(x) + 3}, \"x\")"),
       envir = e)
  testrun(config)
  config <- testconfig(config)
  expect_equal(justbuilt(config), "a")
  expect_equal(readd(a), 12:21)
})

test_with_dir("deps_target()", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  load_mtcars_example()
  config <- drake_config(my_plan, cache = storr::storr_environment())
  d1 <- lapply(deps_target(report, config = config), sort)
  d2 <- list(
    knitr_in = "report.Rmd",
    file_out = "report.md",
    readd = sort(c("coef_regression2_small", "small")),
    loadd = "large"
  )
  expect_equal(length(d1), length(d2))
  for (n in names(d2)) {
    expect_equal(d1[[n]], d2[[n]])
  }
  d <- deps_target(regression1_small, config = config)
  expect_equal(length(d), 2)
  expect_equal(sort(d$globals), sort(c("reg1", "small")))
  expect_equal(d$memory, "small")
})

test_with_dir("self-referential commands and imports", {
  f <- function(x, ...) {
    x <- f
  }
  x <- data.frame(f = 123)
  plan <- drake_plan(y = f(x, y))
  cache <- storr::storr_environment()
  make(plan, cache = cache, session_info = FALSE)
  o <- drake_config(plan, cache = cache, session_info = FALSE)
  expect_equal(justbuilt(o), "y")
  log1 <- drake_cache_log(cache = cache)
  make(plan, cache = cache, session_info = FALSE)
  o <- drake_config(plan, cache = cache, session_info = FALSE)
  expect_true(nobuild(o))
  log2 <- drake_cache_log(cache = cache)
  expect_equal(log1, log2)
})

test_with_dir("._drake_envir and drake_envir() are not dependencies", {
  deps1 <- deps_code(quote(drake_envir()))$globals
  deps2 <- deps_code(quote(rm(x, envir = ._drake_envir)))$globals
  expect_false("drake_envir" %in% deps1)
  expect_false("drake_envir" %in% deps2)
  expect_false("._drake_envir" %in% deps1)
  expect_false("._drake_envir" %in% deps2)
})

test_with_dir("ignore() suppresses updates", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  cache <- storr::storr_environment()
  envir <- new.env(parent = globalenv())
  envir$arg <- 4

  # Without ignore()
  make(
    plan = drake_plan(x = sqrt(arg)),
    envir = envir,
    cache = cache
  )
  con <- drake_config(
    plan = drake_plan(x = sqrt(arg)),
    envir = envir,
    cache = cache
  )
  expect_equal(justbuilt(con), "x")
  con$envir$arg <- con$envir$arg + 1
  make(config = con)
  expect_equal(justbuilt(con), "x")

  # With ignore()
  make(
    plan = drake_plan(x = sqrt( ignore(arg) + 123)), # nolint
    envir = envir,
    cache = cache
  )
  con <- drake_config(
    plan = drake_plan(x = sqrt( ignore(arg) + 123)), # nolint
    envir = envir,
    cache = cache
  )
  expect_equal(justbuilt(con), "x")
  con$envir$arg <- con$envir$arg + 1
  con$cache$clear(namespace = "progress")
  make(config = con)
  expect_equal(justbuilt(con), character(0))

  con$envir$arg2 <- con$envir$arg + 1234
  con$plan <- drake_plan(x = sqrt( ignore  (arg2 ) + 123)) # nolint
  con$cache$clear(namespace = "progress")
  make(config = con)
  expect_equal(justbuilt(con), character(0))
})

test_with_dir("ignore() works on its own", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_equal(ignore(), NULL)
  expect_equal(ignore(1234), 1234)
  expect_identical(ignore_ignore(digest::digest), digest::digest)
})

test_with_dir("Can standardize commands", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_true(is.character(standardize_command("")))
  expect_identical(
    standardize_command("f(x +2) + 2"),
    standardize_command("f(x + 2) + 2")
  )
  expect_identical(
    standardize_command(parse(text = "f(x +2) + 2")),
    standardize_command("f(x + 2) + 2")
  )
  expect_identical(
    standardize_command(quote(f(x + 2) + 2)),
    standardize_command("f(x + 2) + 2")
  )
  expect_false(
    identical(
      standardize_command("f(x + 2) + 2"),
      standardize_command("f(x + 1 - 1) + 2")
    )
  )
  expect_identical(
    standardize_command("b->a"),
    standardize_command("a <- b")
  )
  expect_identical(
    standardize_command("y=sqrt(x=1)"),
    standardize_command("y = sqrt(x = 1)")
  )
  expect_identical(
    standardize_command("abcdefg = hijklmnop <- qrstuvwxyz\n\n"),
    standardize_command("abcdefg = hijklmnop <- qrstuvwxyz")
  )
  a <- standardize_command("z = {f('#') # comment
    x = 5

    y <-
      'test'
    z <- 4

    x2 <- 'test2'
  }")
  b <- standardize_command("z = {f('#') # comment X
  x = 5

  y <- 'test'
  z <- 4
  'test2' -> x2
  }")
  c <- standardize_command("z = {f('#') # comment X
  x = 5

  y <- 'test3'
  z <- 4
  'test2' -> x2
  }")
  expect_identical(a, b)
  expect_false(identical(b, c))
})

test_with_dir("standardized commands with ignore()", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_equal(
    standardize_command("f(sqrt( ignore(fun(arg) + 7) + 123))"),
    standardize_command("f(sqrt(ignore() + 123))")
  )
  expect_equal(
    standardize_command("f(sqrt( ignore  (fun(arg) + 7) + 123) )"),
    standardize_command("f(sqrt(ignore() + 123))")
  )

  expect_equal(
    standardize_command(" f (sqrt( drake::ignore(fun(arg) + 7) + 123 ))"),
    standardize_command("f(sqrt(ignore() + 123))")
  )
  expect_equal(
    standardize_command("\tf(sqrt( drake ::: ignore  (fun(arg) + 7) + 123))"),
    standardize_command("f(sqrt(ignore() + 123))")
  )
  expect_equal(
    standardize_command("function(x){(sqrt( ignore(fun(arg) + 7) + 123))}"),
    standardize_command("function(x) {\n    (sqrt(ignore() + 123))\n}")
  )
  expect_equal(
    standardize_command("f(sqrt( ignore(fun(arg) + 7) + 123)); g(ignore(i))"),
    standardize_command("f(sqrt( ignore() + 123)); g(ignore())")
  )
  f <- function(x) {
    (sqrt( ignore(fun(arg) + 7) + 123)) # nolint
  }
  b <- body(ignore_ignore(f))
  for (a in names(attributes(b))) {
    attr(b, a) <- NULL
  }
  expect_equal(b, quote({  (sqrt(ignore() + 123)) })) # nolint
})

test_with_dir("Can standardize commands from expr or lang", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- parse(text = c("f(x +2) + 2", "!!y"))
  y <- standardize_command(x[[1]])
  x <- parse(text = "f(x +2) + 2")
  z <- standardize_command(x)
  w <- standardize_command(x[[1]])
  s <- "f(x + 2) + 2"
  debug_char0 <-
    expect_equal(y, s)
  expect_equal(z, s)
  expect_equal(w, s)
})

test_with_dir("ignore() in imported functions", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  f <- function(x) {
    (sqrt( ignore(sqrt(x) + 7) + 123)) # nolint
  }
  plan <- drake_plan(x = f(1))
  cache <- storr::storr_environment()
  make(plan, cache = cache)
  config <- drake_config(plan, cache = cache)
  expect_equal(justbuilt(config), "x")
  expect_equal(readd(f, cache = cache), f)
  expect_equal(
    readd(f, cache = cache, namespace = "kernels")[3],
    "    (sqrt(ignore() + 123))"
  )
  f <- function(x) {
    (sqrt( ignore(sqrt(x) + 8) + 123)) # nolint
  }
  make(plan, cache = cache)
  config <- drake_config(plan, cache = cache)
  expect_equal(justbuilt(config), character(0))
  f <- function(x) {
    (sqrt( ignore(sqrt(x) + 8) + 124)) # nolint
  }
  make(plan, cache = cache)
  config <- drake_config(plan, cache = cache)
  expect_equal(justbuilt(config), "x")
})

test_with_dir("deps_code() on a knitr file", {
  skip_on_cran()
  load_mtcars_example()
  expect_true(is.list(deps_code(file_store("report.Rmd"))))
})
