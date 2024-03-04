drake_context("analysis")

# nolint start
test_with_dir("busy function", {
  f <- function(a = 1, b = k(i), nineteen, string_args = c("sa1", "sa2")) {
    for (iter in 1:10) {
      got_for <- got_for + iter
    }
    while (iter2 %in% 1:10) {
      got_while <- got_while + iter2
    }
    assign("iter3", val1)
    delayedAssign(x = "iter4", value = val2)
    x <- g(a + b) + iter + iter2 + iter3 + iter4
    g(a - b) -> y
    z <- g(a * b)
    local({
      xyz1 <- 5
    })
    stringvar <- "string1"
    stringlist <- list(c("string2", "string3"))
    h <- function() {
      xyz2 <- 6
    }
    abc <- xyz1 + xyz2
    f2 <- "local"
    lm(f1 ~ f2 + f3)
    file_in("x", "y")
    drake::file_in(c("w", "z"))
    base::c(got, basevar)
    quote(quoted)
    Quote(quoted2)
    expression(quoted3)
  }
  out <- drake_deps(f)
  out <- select_nonempty(decode_deps_list(out))
  expect_equal(sort(out$file_in), sort(c("w", "x", "y", "z")))
  str <- sort(
    c("iter3", "iter4", "local", paste0("string", 1:3), "sa1", "sa2")
  )
  expect_equal(sort(out$strings), str)
  expect_equal(out$namespaced, "base::c")
  exp <- sort(c(
    "assign", "basevar", "c", "delayedAssign", "expression", "for",
    "f1", "f3", "g", "got", "got_for", "got_while",
    "i", "iter2", "k",  "list", "lm", "local", "Quote", "quote",
    "val1", "val2", "while", "xyz1", "xyz2"
  ))
  expect_equal(sort(out$globals), exp)
  str <- sort(c(str, "w", "x", "y", "z"))
  expect_equal(sort(analyze_strings(f)), str)
})
# nolint end

test_with_dir("equals analysis", {
  for (text in c("z = g(a * b)", "function(x) {z = g(a * b)}")) {
    expr <- parse(text = text)
    out <- drake_deps(expr)
    expect_equal(sort(out$globals), sort(c("a", "b", "g")))
  }
})

# https://github.com/cran/codetools/blob/main/tests/tests.R # nolint
test_with_dir("local variable tests from the codetools package", {
  find_locals <- function(expr) {
    if (!is.function(expr) && !is.language(expr)) {
      return(list())
    }
    results <- new_drake_deps_ht()
    locals <- ht_new()
    walk_code(expr, results, locals, NULL)
    ht_list(locals)
  }
  expect_equal(find_locals(quote(x <- 1)), "x")
  expect_equal(find_locals(quote(x <- y <- 1)), c("x", "y"))
  expect_equal(find_locals(quote(local(x <- 1))), character(0))
  expect_equal(find_locals(quote(assign(x, 3))), character(0))
  expect_equal(find_locals(quote(delayedAssign(x, 3))), character(0))
  expect_equal(find_locals(quote(assign("x", 3))), "x")
  expect_equal(find_locals(quote(assign("x", 3, 4))), character(0))
})

test_with_dir("same tests with global variables", {
  code <- quote(x <- 1)
  expect_equal(as.character(drake_deps(code)$globals), character(0))
  code <- quote(x <- y <- 1)
  expect_equal(as.character(drake_deps(code)$globals), character(0))
  code <- quote(local(x <- 1))
  expect_equal(drake_deps(code)$globals, "local")
  code <- quote(assign(x, 3))
  out <- sort(drake_deps(code)$globals)
  expect_equal(out, sort(c("assign", "x")))
  code <- quote({
    assign(x, 3)
    x <- 1
  })
  out <- sort(drake_deps(code)$globals)
  expect_equal(out, sort(c("assign", "x")))
  code <- quote({
    x <- 1
    assign(x, 3)
  })
  expect_equal(drake_deps(code)$globals, "assign")
  code <- quote(assign("x", 3))
  out <- sort(drake_deps(code)$globals)
  expect_equal(out, "assign")
  code <- quote(assign("x", 3, 4))
  out <- sort(drake_deps(code)$globals)
  expect_equal(out, "assign")
})

test_with_dir("solitary codetools globals tests", {
  code <- quote({
    local <- 1
    local(x <- 1)
  })
  out <- as.character(drake_deps(code)$globals)
  expect_equal(out, character(0))
  out <- drake_deps(quote(local(x <- 1, e)))$globals
  expect_equal(sort(out), sort(c("local", "e")))
  f <- function() {
    if (g()) {
      x
    } else {
      y
    }
  }
  out <- drake_deps(f)$globals
  expect_equal(sort(out), sort(c("if", "g", "x", "y")))
  f <- function() {
    if (FALSE) {
      x
    }
  }
  out <- drake_deps(f)$globals
  expect_equal(sort(out), sort(c("if", "x")))

  f <- function(x) {z <- 1; x + y + z} # nolint
  expect_equal(sort(drake_deps(f)$globals), "y")
  expect_equal(drake_deps(function() Quote(x))$globals, "Quote")
  f <- function(f, x, y) {
    local <- f
    local(x <- y)
    x
  }
  expect_equivalent(drake_deps(f), new_drake_deps())
  f <- function() {
    x <- 1; y <- 2 # nolint
  }
  out <- as.character(drake_deps(f)$globals)
  expect_equal(out, character(0))
  f <- function(u = x <- 1) {
    y <- 2
  }
  expect_equal(as.character(drake_deps(f)$globals), character(0))
})

# https://github.com/cran/codetools/blob/9bac1daaf19a36bd03a2cd7d67041893032e7a04/R/codetools.R#L302-L365 # nolint
# https://cran.r-project.org/doc/manuals/R-lang.html#Subset-assignment
test_with_dir("replacement functions", {
  code <- quote(f(x) <- 1)
  out <- sort(drake_deps(code)$globals)
  expect_equal(out, sort(c("f<-", "x")))

  code <- quote({
    f(x) <- 1
    x <- 5
  })
  out <- sort(drake_deps(code)$globals)
  expect_equal(out, sort(c("f<-", "x")))

  code <- quote({
    x <- 5
    f(x) <- 1
  })
  out <- drake_deps(code)$globals
  expect_equal(out, "f<-")

  code <- quote(f(g(h(k(x)))) <- seven)
  out <- sort(as.character(drake_deps(code)$globals))
  exp <- sort(c("f<-", "g", "g<-", "h", "h<-", "k", "k<-", "x", "seven"))
  expect_equal(out, exp)

  code <- quote(f(g(h(x, w), y(a)), z(u, v)) <- 1)
  out <- sort(as.character(drake_deps(code)$globals))
  exp <- sort(
    c("f<-", "g", "g<-", "h", "h<-", "a", "u", "v", "w", "x", "y", "z")
  )
  expect_equal(out, exp)

  code <- quote({
    x <- 5
    f(g(h(x, w), y(a)), z(u, v)) <- 1
  })
  out <- sort(as.character(drake_deps(code)$globals))
  exp <- sort(
    c("f<-", "g", "g<-", "h", "h<-", "a", "u", "v", "w", "y", "z")
  )
  expect_equal(out, exp)

  code <- quote({
    f(g(h(x, w), y(a)), z(u, v)) <- 1
    x <- 5
  })
  out <- sort(as.character(drake_deps(code)$globals))
  exp <- sort(
    c("f<-", "g", "g<-", "h", "h<-", "a", "u", "v", "w", "x", "y", "z")
  )
  expect_equal(out, exp)

  code <- quote(f(base::g(pkg:::h(x, w), y(a)), z(u, v)) <- 1)
  out <- drake_deps(code)
  out <- select_nonempty(decode_deps_list(out))
  expect_equal(
    sort(out$globals),
    sort(c("f<-", "a", "u", "v", "x", "w", "y", "z"))
  )
  expect_equal(
    sort(out$namespaced),
    sort(c("pkg:::h", "base::g", "base::`g<-`", "pkg:::`h<-`"))
  )
})

test_with_dir("code analysis error handling", {
  e <- quote(a <- 1)
  expect_error(
    make_assignment_fn(e),
    regexp = "bad function in complex assignments"
  )

  f <- function(a, b) {
    invisible()
  }
  expect_error(get_assigned_var(formals(f)), regexp = "missing assignment")

  e <- quote(x <- 1)
  e <- list(e[1], e[1])
  expect_error(get_assigned_var(e), regexp = "unfinished code")

  e <- quote(x <- 1)
  e[[2]] <- quote(x <- 1)
  e[[2]][[2]] <- formals(f)[[1]]
  expect_error(get_assigned_var(e), regexp = "missing variable")

  e <- list(1, 2)
  expect_error(get_assigned_var(e), regexp = "not a symbol")
})

test_with_dir("character vectors inside language objects", {
  y <- c("a", "b")
  plan <- drake::drake_plan(
    out = data.frame(x = 1:2, y = !!y)
  )
  expect_silent(
    drake_config(plan, cache = storr::storr_environment(), session_info = FALSE)
  )
  expect_equal(
    sort(drake_deps(plan$command[[1]])$strings),
    sort(c("a", "b"))
  )
})

test_with_dir("dollar sign (#938)", {
  expect_equal(drake_deps(quote(x$y))$globals, "x")
  f <- function(target, cache) {
    exists <- cache$exists(key = target) && (
      imported <- diagnose(
        target = target,
        character_only = TRUE,
        cache = cache
      )$imported %||%
        FALSE
    )
  }
  out <- sort(drake_deps(f)$globals)
  out <- out[out == make.names(out, unique = FALSE)]
  exp <- "diagnose"
  expect_equal(out, exp)
})

test_with_dir("user-defined S3 (#959)", {
  skip_on_cran()
  dostuff <- function(x) {
    do.stuff.class2 <- 40 # nolint
    if (1 == 1) {
      UseMethod("do.stuff")
    } else {
      sqrt(5)
    }
  }
  # nolint start
  do.stuff.class1 <-
    do.stuffclass1 <-
    do.stuff_class1 <-
    do_stuff.class1 <-
    do_stuff_class1 <-
    dostuff.class1 <-
    do.stuff.class2 <-
    do.stuff.class3 <-
    function(x) {
      invisible()
    }
  # nolint end
  plan <- drake_plan(x = {
    y <- list(123)
    class(y) <- "class1"
    dostuff(y)
  })
  config <- drake_config(
    plan,
    history = FALSE,
    cache = storr::storr_environment()
  )
  out <- sort(deps_target_impl(dostuff, config)$name)
  exp <- sort(c("do.stuff.class1", "do.stuff.class3"))
  expect_equal(out, exp)
  dostuff <- function(x) {
    do.stuff.class2 <- 40 # nolint
    if (1 == 1) {
      UseMethod(object = x, generic = "do.stuff")
    } else {
      sqrt(5)
    }
  }
  config <- drake_config(
    plan,
    history = FALSE,
    cache = storr::storr_environment()
  )
  out <- sort(deps_target_impl(dostuff, config)$name)
  exp <- sort(c("do.stuff.class1", "do.stuff.class3"))
  make_impl(config = config)
  expect_equal(justbuilt(config), "x")
  make_impl(config = config)
  expect_equal(justbuilt(config), character(0))
  do.stuff.class1 <- function(...) { # nolint
    message(123)
    invisible()
  }
  make_impl(config = config)
  expect_equal(justbuilt(config), "x")
  dostuff <- function(...) {
    do.stuff.class2 <- 40 # nolint
    if (a == 1) {
      dont_use_method("do.stuff")
    } else {
      sqrt(5)
    }
  }
  config <- drake_config(
    plan,
    history = FALSE,
    cache = storr::storr_environment()
  )
  expect_equal(deps_target_impl(dostuff, config)$name, character(0))
  dostuff <- function(x) {
    UseMethod(paste0("do", ".", "stuff"))
  }
  config <- drake_config(
    plan,
    history = FALSE,
    cache = storr::storr_environment()
  )
  expect_equal(deps_target_impl(dostuff, config)$name, character(0))
})

test_with_dir("unparsable commands are handled correctly", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  x <- "bluh$"
  expect_error(deps_code(x))
})

test_with_dir("bad target names", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  expect_equal(
    sort(deps_code("sqrt(x + y + .)")$name),
    sort(c("sqrt", "x", "y"))
  )
  expect_equal(
    sort(deps_code("subset(complete.cases(.))")$name),
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
    .gitignore = 1,
    y = 2,
    a = sqrt(x + y),
    b = subset(complete.cases(.gitignore))
  )
  expect_error(drake_config(plan), "cannot be target names")
})

test_with_dir("file_in() and file_out() and knitr_in(): commands vs imports", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("knitr")
  # nolint start
  cmd <- quote({
    file_in("x"); file_out("y"); knitr_in("report.Rmd")
  })
  # nolint end
  f <- function() {
    file_in("x")
  }
  file.create("x")
  file.create("y")
  path <- system.file(
    file.path("rmarkdown", "examples", "mtcars", "report.Rmd"),
    package = "drake",
    mustWork = TRUE
  )
  file.copy(
    from = path,
    to = file.path(getwd(), "report.Rmd"),
    overwrite = TRUE
  )
  x <- cds_command_dependencies(cmd)
  x <- select_nonempty(decode_deps_list(x))
  x0 <- list(
    file_in = "x", file_out = "y", loadd = "large",
    readd = c("small", "coef_regression2_small"),
    knitr_in = "report.Rmd"
  )
  expect_equal(length(x), length(x0))
  for (i in names(x)) {
    expect_equal(sort(x[[i]]), sort(x0[[i]]))
  }
  y <- cds_import_dependencies(f)
  y <- select_nonempty(decode_deps_list(y))
  y0 <- list(
    file_in = "x"
  )
  expect_equal(length(y), length(y0))
  for (i in names(y)) {
    expect_equal(sort(y[[i]]), sort(y0[[i]]))
  }
  expect_equal(
    sort(deps_code(f)$name), sort(unname(unlist(y))))
  expect_equal(
    sort(deps_code(cmd)$name),
    sort(
      c("coef_regression2_small", "large",
        "report.Rmd", "small", "x", "y"
      )
    )
  )
})

test_with_dir("deps_code() and deps_target_impl()", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  expect_equal(nrow(deps_code("")), 0)
  expect_equal(length(select_nonempty(cds_command_dependencies(NA))), 0)
  expect_equal(length(select_nonempty(cds_command_dependencies(NULL))), 0)
  expect_equal(
    length(select_nonempty(cds_command_dependencies(character(0)))),
    0
  )
  expect_equal(deps_code(base::c)$name, character(0))
  expect_equal(deps_code(base::list)$name, character(0))
  f <- function(x, y) {
    out <- x + y + g(x)
    saveRDS(out, "out.rds")
  }
  expect_false(is_vectorized(f))
  expect_false(is_vectorized("char"))
  expect_equal(
    sort(deps_code(f)$name),
    sort(c("g", "saveRDS"))
  )
  my_plan <- drake_plan(
    x = 1 + some_object,
    my_target = x + readRDS(file_in("tracked_input_file.rds")),
    return_value = f(x, y, g(z + w)),
    botched = read.csv(file_in(nothing)),
    meta = read.table(file_in("file_in"))
  )
  expect_warning(
    config <- drake_config(
      my_plan,
      session_info = FALSE,
      cache = storr::storr_environment()
    ),
    regexp = "must be literal strings"
  )
  expect_equal(deps_code(my_plan$command[[1]])$name, "some_object")
  expect_equal(
    sort(deps_code(my_plan$command[[2]])$name),
    sort(c("tracked_input_file.rds", "x", "readRDS")))
  expect_equal(
    sort(deps_code(my_plan$command[[3]])$name),
    sort(c("f", "g", "w", "x", "y", "z"))
  )
  expect_warning(
    expect_equal(
      sort(deps_code(my_plan$command[[4]])$name),
      sort(c("read.csv"))
    ),
    regexp = "must be literal strings"
  )
  expect_equal(
    sort(deps_code(my_plan$command[[5]])$name),
    sort(c("read.table", "file_in"))
  )
  expect_true(!nrow(deps_target_impl(x, config)))
  expect_equal(
    sort(deps_target_impl(my_target, config)$name),
    sort(c("tracked_input_file.rds", "x")))
  expect_equal(
    sort(deps_target_impl(return_value, config)$name),
    sort(c("f", "x"))
  )
  expect_equal(
    sort(deps_target_impl(botched, config)$name),
    character(0)
  )
  expect_equal(
    sort(deps_target_impl(meta, config)$name),
    sort("file_in"))
})

test_with_dir("tracked() works", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  config <- dbug()
  x <- sort(tracked(config))
  y <- sort(c(
    redisplay_keys(reencode_path("intermediatefile.rds")),
    "drake_target_1",
    "yourinput", "nextone",
    "combined", "myinput", "final", "j", "i", "h", "g", "f",
    "c", "b", "a",  redisplay_keys(reencode_path("input.rds"))
  ))
  expect_equal(x, y)
})

test_with_dir("missing input files", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  config <- dbug()
  expect_silent(tmp <- missing_input_files(config))
  unlink("input.rds", force = TRUE)
  expect_warning(tmp <- missing_input_files(config))
  expect_silent(tmp <- config_checks(config))
  expect_warning(runtime_checks(config), regexp = "missing")
  config$settings$skip_safety_checks <- TRUE
  expect_silent(tmp <- runtime_checks(config))
})

test_with_dir("Vectorized nested functions work", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  e <- new.env(parent = globalenv())
  eval(parse(text = "f <- Vectorize(function(x) g(x), \"x\")"), envir = e)
  eval(parse(text = "g <- function(x) x + y"), envir = e)
  e$y <- 7
  config <- dbug()
  config$envir <- e
  config$plan <- drake_plan(a = f(1:10))
  config$targets <- "a"
  expect_equal(deps_code(e$f)$name, "g")
  expect_equal(deps_code(e$g)$name, "y")

  testrun(config)
  config <- testconfig(config)
  if ("a" %in% ls(config$envir)) {
    rm(a, envir = config$envir)
  }
  expect_equal(readd(a), 8:17)
  k <- readd(f)
  expect_true(is.character(k))
  expect_equal(character(0), outdated_impl(config))
  config$envir$y <- 8
  expect_equal("a", outdated_impl(config))

  # Target "a" should react.
  testrun(config)
  config <- testconfig(config)
  expect_equal(character(0), outdated_impl(config))
  expect_equal(readd(a), 9:18)

  # Change a vectorized function and see target "a" react.
  eval(
    parse(text = "f <- Vectorize(function(x) {g(x) + 3}, \"x\")"),
    envir = e
  )
  testrun(config)
  config <- testconfig(config)
  expect_equal(justbuilt(config), "a")
  expect_equal(readd(a), 12:21)
})

test_with_dir("deps_target_impl()", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("knitr")
  load_mtcars_example()
  config <- drake_config(my_plan, cache = storr::storr_environment())
  d1 <- deps_target_impl(report, config = config)
  d1 <- as.data.frame(d1[order(d1$name), ])
  d2 <- data.frame(
    name = c(
      "coef_regression2_small",
      "knitr::knit",
      "large",
      "report.md",
      "report.Rmd",
      "small"
    ),
    type = c("readd", "namespaced", "loadd", "file_out", "knitr_in", "readd"),
    stringsAsFactors = FALSE
  )
  d2 <- d2[order(d2$name), ]
  d1$hash <- NULL
  expect_equivalent(d1, d2)
  d <- deps_target_impl(regression1_small, config = config)
  expect_equal(sort(d$name), sort(c("reg1", "small")))
  expect_equal(d$type, rep("globals", 2))
})

test_with_dir("self-referential commands and imports", {
  skip_on_cran()
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

test_with_dir("ignore() suppresses updates", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
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
  make_impl(config = con)
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
  make_impl(config = con)
  expect_equal(justbuilt(con), character(0))

  con$envir$arg2 <- con$envir$arg + 1234
  con$plan <- drake_plan(x = sqrt( ignore  (arg2 ) + 123)) # nolint
  con$cache$clear(namespace = "progress")
  make_impl(config = con)
  expect_equal(justbuilt(con), character(0))
})

test_with_dir("ignore() works on its own", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  expect_equal(ignore(), NULL)
  expect_equal(ignore(1234), 1234)
  expect_identical(ignore_ignore(digest::digest), digest::digest)
})

test_with_dir("Standardized commands have no attributes", {
  expect_null(attributes(cds_standardize_command("")))
  expect_null(attributes(
    cds_standardize_command("f(x) + y + function(abc) {}"))
  )
  expect_null(attributes(cds_standardize_command(quote(NULL))))
  expect_null(attributes(cds_standardize_command(digest::digest)))
  expect_null(attributes(cds_standardize_command(body(digest::digest))))
})

test_with_dir("Can standardize commands", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  expect_true(is.character(cds_standardize_command(parse(text = ""))))
  expect_identical(
    cds_standardize_command(parse(text = "f(x +2) + 2")),
    cds_standardize_command(parse(text = "f(x + 2) + 2"))
  )
  expect_identical(
    cds_standardize_command(quote(f(x + 2) + 2)),
    cds_standardize_command(parse(text = "f(x + 2) + 2"))
  )
  expect_false(
    identical(
      cds_standardize_command(parse(text = "f(x + 2) + 2")),
      cds_standardize_command(parse(text = "f(x + 1 - 1) + 2"))
    )
  )
  expect_identical(
    cds_standardize_command(parse(text = "b->a")),
    cds_standardize_command(parse(text = "a <- b"))
  )
  expect_identical(
    cds_standardize_command(parse(text = "y=sqrt(x=1)")),
    cds_standardize_command(parse(text = "y = sqrt(x = 1)"))
  )
  expect_identical(
    cds_standardize_command(
      parse(text = "abcdefg = hijklmnop <- qrstuvwxyz\n\n")
    ),
    cds_standardize_command(parse(text = "abcdefg = hijklmnop <- qrstuvwxyz"))
  )
  a <- cds_standardize_command(parse(text = "z = {f('#') # comment
    x = 5

    y <-
      'test'
    z <- 4

    x2 <- 'test2'
  }"))
  b <- cds_standardize_command(parse(text = "z = {f('#') # comment X
  x = 5

  y <- 'test'
  z <- 4
  'test2' -> x2
  }"))
  c <- cds_standardize_command(parse(text = "z = {f('#') # comment X
  x = 5

  y <- 'test3'
  z <- 4
  'test2' -> x2
  }"))
  expect_identical(a, b)
  expect_false(identical(b, c))
})

test_with_dir("standardized commands with ignore()", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  expect_equal(
    cds_standardize_command(
      parse(text = "f(     sqrt( ignore(fun(arg) + 7) + 123))")
    ),
    cds_standardize_command(parse(text = "f(sqrt(ignore() + 123))"))
  )
  expect_equal(
    cds_standardize_command(
      parse(text = "f(sqrt( ignore  (fun(arg) + 7)+123) ) # noooop")
    ),
    cds_standardize_command(parse(text = "f(sqrt(ignore() + 123))"))
  )
  expect_equal(
    cds_standardize_command(
      parse(text = " f (sqrt( drake::ignore(fun(arg) + 7) + 123 ))")
    ),
    cds_standardize_command(parse(text = "f(sqrt(ignore() + 123))"))
  )
  expect_equal(
    cds_standardize_command(
      parse(text = "\tf(sqrt( drake ::: ignore  (fun(arg) + 7) + 123))")
    ),
    cds_standardize_command(parse(text = "f(sqrt(ignore() + 123))"))
  )
  expect_equal(
    cds_standardize_command(
      parse(text = "function(x) {(sqrt( ignore(fun(arg) + 7) + 123))}")
    ),
    cds_standardize_command(
      parse(text = "function(x) {\n    (sqrt(ignore() + 123))\n}")
    )
  )
  expect_equal(
    cds_standardize_command(
      parse(text = "f(sqrt( ignore(fun(arg) + 7) + 123)); g(ignore(i))")
    ),
    cds_standardize_command(
      parse(text = "f(sqrt( ignore() + 123)); g(ignore())")
    )
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
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  x <- parse(text = "f(x +2) + 2")
  y <- cds_standardize_command(x)
  z <- cds_standardize_command(x)
  w <- cds_standardize_command(x[[1]])
  s <- safe_deparse(quote(f(x + 2) + 2))
  expect_equal(y, s)
  expect_equal(z, s)
  expect_equal(w, s)
})

test_with_dir("ignore() in imported functions", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  f <- function(x) {
    (sqrt( ignore(sqrt(x) + 7) + 123)) # nolint
  }
  plan <- drake_plan(x = f(1))
  cache <- storr::storr_environment()
  make(plan, cache = cache)
  # Because ignore() affects standardization:
  expect_true(is.character(readd(f, cache = cache)))
  config <- drake_config(plan, cache = cache)
  expect_equal(justbuilt(config), "x")

  str <- readd(f, cache = cache)
  expect_false(any(grepl("sqrt(x)", str, fixed = TRUE)))
  expect_true(any(grepl("(sqrt(ignore() + 123))", str, fixed = TRUE)))
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

test_with_dir("ignore() inside special functions", {
  plan <- drake_plan(
    a = 1,
    b1 = readd(a),
    b2 = readd("a"),
    b3 = ignore(readd(a)),
    b4 = readd(ignore(a)),
    b5 = readd(ignore("a")),
    c1 = loadd(a),
    c2 = loadd("a"),
    c3 = ignore(loadd(a)),
    c4 = loadd(ignore(a)),
    c5 = loadd(ignore("a")),
    d1 = file_in("a"),
    d2 = file_in("a"),
    d3 = ignore(file_in("a")),
    d4 = file_in(ignore("a")),
    d5 = file_in(ignore("a")),
    e1 = file_out("a"),
    e2 = file_out("a"),
    e3 = ignore(file_out("a")),
    e4 = file_out(ignore("a")),
    e5 = file_out(ignore("a")),
    f1 = knitr_in("a"),
    f2 = knitr_in("a"),
    f3 = ignore(knitr_in("a")),
    f4 = knitr_in(ignore("a")),
    f5 = knitr_in(ignore("a"))
  )
  suppressWarnings(config <- drake_config(plan))
  for (x in letters[2:6]) {
    for (y in 1:5) {
      target <- paste0(x, y)
      deps <- deps_target_impl(target, config, character_only = TRUE)$name
      if (y < 3) {
        expect_equal(deps, "a")
      } else {
        expect_equal(deps, character(0))
      }
    }
  }
})

test_with_dir("no_deps() in a command", {
  skip_on_cran()
  plan <- drake_plan(y = sqrt(no_deps(x)))
  cache <- storr::storr_environment()
  config <- drake_config(plan, cache = cache)
  x <- 4
  make(plan, cache = cache)
  expect_equal(justbuilt(config), "y")
  x <- 5
  make(plan, cache = cache)
  expect_equal(justbuilt(config), character(0))
  plan <- drake_plan(y = sqrt(no_deps(x + 1)))
  make(plan, cache = cache)
  expect_equal(justbuilt(config), "y")
})

test_with_dir("no_deps() in a function", {
  skip_on_cran()
  y <- 1
  f <- function(x) {
    no_deps({
      out <- sqrt(x + y) + 1
      out
    })
  }
  plan <- drake_plan(z = f(2))
  cache <- storr::storr_environment()
  config <- drake_config(plan, cache = cache)
  make(plan, cache = cache)
  expect_equal(justbuilt(config), "z")
  y <- 2
  make(plan, cache = cache)
  expect_equal(justbuilt(config), character(0))
  f <- function(x) {
    no_deps({
      out <- sqrt(x + y) + 2
      out
    })
  }
  make(plan, cache = cache)
  expect_equal(justbuilt(config), "z")
})

test_with_dir("function_dependencies() works on :: and :::", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
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
  cd <- drake_deps(crazy)
  cd <- select_nonempty(decode_deps_list(cd))
  expect_equal(sort(cd$namespaced), ns)
  cd <- drake_deps(crazy)
  cd <- select_nonempty(decode_deps_list(cd))
  expect_equal(
    unname(sort(unlist(cd))),
    sort(c(ns, "g", "myfun1", "sqrt", "local"))
  )
  command <- "pkgx::pkgx(mypkg1::myfun3(myfun1(mypkg1::myfun2(100))))"
  expect_equal(
    sort(deps_code(command)$name),
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
  skip_on_cran() # CRAN gets essential tests only (check time limits).
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
    verbose = 0L,
    session_info = FALSE
  )
  config <- drake_config(
    x,
    envir = envir,
    jobs = scenarios$jobs,
    parallelism = scenarios$parallelism,
    verbose = 0L,
    session_info = FALSE
  )
  fromcache <- readd("base::list", character_only = TRUE)
  expect_true(is.character(fromcache))
  fromcache2 <- readd("base:::c", character_only = TRUE)
  expect_true(is.character(fromcache2))
  ns <- sort(c("base:::c", "base::list", "base::nchar"))
  expect_true(all(ns %in% cached(targets_only = FALSE)))
  expect_true(all(ns %in% setdiff(cached(targets_only = FALSE),
                                  cached(targets_only = TRUE))))
  expect_equal(
    outdated_impl(config),
    character(0)
  )
})

test_with_dir("standardizing Rcpp functions (#806)", {
  skip_on_cran()
  if (FALSE) {
    # Takes too long.
    skip_if_not_installed("Rcpp")
    f <- Rcpp::cppFunction(
      "int add(int x, int y, int z) {
        int sum = x + y + z;
        return sum;
      }"
    )
  }
  str <- "function (x, y, z) \n.Call(<pointer: 0x116937930>, x, y, z)"
  x <- standardize_deparsed_function(str)
  expect_true(grepl("function", x))
  expect_true(grepl("Call", x))
  expect_false(grepl("pointer: 0x", x))
  expect_true(grepl("pointer: 0x", str))
})

test_with_dir("utils for code analysis fns", {
  expect_equal(pair_text("x", c("y", "z")), c("xy", "xz"))
})

test_with_dir("handle @ (#1130)", {
  expect_equal(deps_code(quote(x@y))$name, "x")
})

test_with_dir("handle calls in analyze_assign() (#1119)", {
  test <- function(input) {
    assign(paste0(input, x, "var"), 1)
  }
  expect_silent(out <- deps_code(test))
  expect_equal(nrow(out), 3)
  expect_equal(sort(out$name), sort(c("assign", "paste0", "x")))
})

test_with_dir("$<-() and @<-() (#1144)", {
  f <- function() {
    x$y <- 1
    x@y <- 1
  }
  expect_equal(deps_code(f)$name, "x")
  f <- function() {
    g(x)$y <- 1
  }
  expect_equal(sort(deps_code(f)$name), sort(c("g", "x")))
})

test_with_dir("nonliteral file_in() (#1229)", {
  expect_silent(
    x <- deps_code(quote(file_in(c("file1", "file2"))))
  )
  expect_warning(
    x <- deps_code(quote(file_in(paste("file1", "file2")))),
    regexp = "must be literal strings"
  )
})

test_with_dir("no file_out() or knitr_in() in imported fns (#1229)", {
  expect_error(
    deps_code(function(x) file_out("abc")),
    regexp = "file_out"
  )
  expect_error(
    suppressWarnings(deps_code(function(x) knitr_in("abc"))),
    regexp = "knitr_in"
  )
})

test_with_dir("ignore deps of drake_plan() calls inside functions (#1237)", {
  f <- function() {
    y <- x + 1
    drake_plan(
      report = rmarkdown::render(
        input = knitr_in("report.Rmd"),
        output_file = file_out("report.md"),
        output_dir = ".",
        quiet = TRUE
      )
    )
  }
  expect_true(grepl("file_out", standardize_imported_function(f)))
  expect_true(grepl("knitr_in", standardize_imported_function(f)))
  expect_silent(out <- deps_code(f))
  expect_equal(out$name, "x")
})

test_with_dir("error analyzing malformed code (#1371)", {
  expect_error(make(drake_plan(bar = scale_y_log10() + mod <- list())))
})
