drake_context("other features")

test_with_dir("debug_command()", {
  skip_on_cran()
  txt <- "    f(x + 2) + 2"
  txt2 <- "drake::debug_and_run(function() {\n    f(x + 2) + 2\n})"
  x <- parse(text = txt)[[1]]
  out1 <- debug_command(x)
  out2 <- debug_command(txt)
  txt3 <- wide_deparse(out1)
  expect_equal(out2, txt2)
  expect_equal(out2, txt3)
})

test_with_dir("build_target() does not need to access cache", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  config <- drake_config(drake_plan(x = 1), lock_envir = FALSE)
  meta <- drake_meta(target = "x", config = config)
  config$cache <- NULL
  build <- build_target(target = "x", meta = meta, config = config)
  expect_equal(1, build$value)
  expect_error(
    drake_build(target = "x", config = config),
    regexp = "cannot find drake cache"
  )
})

test_with_dir("cache log files, gc, and make()", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- drake_plan(a = 1)
  make(x, session_info = FALSE, garbage_collection = TRUE)
  expect_false(file.exists("drake_cache.log"))
  make(x, session_info = FALSE)
  expect_false(file.exists("drake_cache.log"))
  make(x, session_info = FALSE, cache_log_file = TRUE)
  expect_true(file.exists("drake_cache.log"))
  make(x, session_info = FALSE, cache_log_file = "my.log")
  expect_true(file.exists("my.log"))
})

test_with_dir("drake_build works as expected", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  pl <- drake_plan(a = 1, b = a)
  con <- drake_config(
    plan = pl,
    session_info = FALSE,
    envir = e,
    lock_envir = TRUE
  )

  # can run before any make()
  o <- drake_build(
    target = "a", character_only = TRUE, config = con)
  x <- cached()
  expect_equal(x, "a")
  make(pl, envir = e)
  o <- drake_config(pl, envir = e)
  expect_equal(justbuilt(o), "b")

  # Replacing deps in environment
  con$eval$a <- 2
  o <- drake_build(b, config = con)
  expect_equal(o, 2)
  expect_equal(con$eval$a, 2)
  expect_equal(readd(a), 1)
  o <- drake_build(b, config = con, replace = FALSE)
  expect_equal(con$eval$a, 2)
  expect_equal(readd(a), 1)
  con$eval$a <- 3
  o <- drake_build(b, config = con, replace = TRUE)
  expect_equal(con$eval$a, 1)
  expect_equal(o, 1)

  # `replace` in loadd()
  e$b <- 1
  expect_equal(e$b, 1)
  e$b <- 5
  loadd(b, envir = e, replace = FALSE)
  expect_equal(e$b, 5)
  loadd(b, envir = e, replace = TRUE)
  expect_equal(e$b, 1)
  e$b <- 5
  loadd(b, envir = e)
  expect_equal(e$b, 1)
})

test_with_dir("colors and shapes", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_message(drake_palette())
  expect_is(color_of("target"), "character")
  expect_is(color_of("import"), "character")
  expect_is(color_of("not found"), "character")
  expect_is(color_of("not found"), "character")
  expect_equal(color_of("bluhlaksjdf"), color_of("other"))
  expect_is(shape_of("object"), "character")
  expect_is(shape_of("file"), "character")
  expect_is(shape_of("not found"), "character")
  expect_equal(shape_of("bluhlaksjdf"), shape_of("other"))
})

test_with_dir("shapes", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_is(shape_of("target"), "character")
  expect_is(shape_of("import"), "character")
  expect_is(shape_of("not found"), "character")
  expect_is(shape_of("object"), "character")
  expect_is(color_of("file"), "character")
  expect_is(color_of("not found"), "character")
  expect_equal(color_of("bluhlaksjdf"), color_of("other"))
})

test_with_dir("make() with skip_targets", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_silent(make(drake_plan(x = 1), skip_targets = TRUE,
    verbose = FALSE, session_info = FALSE))
  expect_false(cached(x))
})

test_with_dir("in_progress() works and errors are handled correctly", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_equal(in_progress(), character(0))
  bad_plan <- drake_plan(x = function_doesnt_exist())
  expect_error(
    make(bad_plan, verbose = TRUE, session_info = FALSE))
  expect_equal(failed(), "x")
  expect_equal(in_progress(), character(0))
  expect_is(e <- diagnose(x)$error, "error")
  expect_true(
    grepl(
      pattern = "function_doesnt_exist",
      x = e$message,
      fixed = TRUE
    )
  )
  expect_error(diagnose("notfound"))
  expect_true(inherits(diagnose(x)$error, "error"))
  y <- "x"
  expect_true(inherits(diagnose(y, character_only = TRUE)$error, "error"))
})

test_with_dir("warnings and messages are caught", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_equal(in_progress(), character(0))
  f <- function(x) {
    warning("my first warn")
    message("my first mess")
    warning("my second warn")
    message("my second mess")
    123
  }
  bad_plan <- drake_plan(x = f(), y = x)
  expect_warning(make(bad_plan, verbose = TRUE, session_info = FALSE))
  x <- diagnose(x)
  expect_true(grepl("my first warn", x$warnings[1], fixed = TRUE))
  expect_true(grepl("my second warn", x$warnings[2], fixed = TRUE))
  expect_true(grepl("my first mess", x$messages[1], fixed = TRUE))
  expect_true(grepl("my second mess", x$messages[2], fixed = TRUE))
})

test_with_dir("missed() works with in-memory deps", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  # May have been loaded in a globalenv() testing scenario
  remove_these <- intersect(ls(envir = globalenv()), c("f", "g"))
  rm(list = remove_these, envir = globalenv())
  o <- dbug()
  expect_equal(character(0), missed(o))
  rm(list = c("f", "g"), envir = o$envir)
  expect_equal(sort(c("f", "g")), sort(missed(o)))
})

test_with_dir("missed() works with files", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  o <- dbug()
  expect_equal(character(0), missed(o))
  unlink("input.rds")
  expect_equal(display_keys(encode_path("input.rds")), missed(o))
})

test_with_dir(".onLoad() warns correctly and .onAttach() works", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  f <- ".RData"
  expect_false(file.exists(f))
  expect_silent(drake:::.onLoad())
  save.image()
  expect_true(file.exists(f))
  expect_warning(drake:::.onLoad())
  unlink(f, force = TRUE)
  set.seed(0)
  expect_true(is.character(drake_tip()))
  expect_silent(suppressPackageStartupMessages(drake:::.onAttach()))
})

test_with_dir("config_checks() via check_plan() and make()", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  config <- dbug()
  y <- data.frame(x = 1, y = 2)
  suppressWarnings(expect_error(check_plan_(y, envir = config$envir)))
  suppressWarnings(
    expect_error(
      make(y, envir = config$envir, session_info = FALSE, verbose = FALSE)))
  y <- data.frame(target = character(0), command = character(0))
  expect_error(suppressWarnings(check_plan_(y, envir = config$envir)))
  suppressWarnings(
    expect_error(
      make(y, envir = config$envir,
           session_info = FALSE, verbose = FALSE)))
  suppressWarnings(expect_error(
    check_plan_(config$plan, targets = character(0), envir = config$envir)))
  suppressWarnings(expect_error(
    make(
      config$plan,
      targets = character(0),
      envir = config$envir,
      session_info = FALSE,
      verbose = FALSE
    )
  ))
})

test_with_dir("targets can be partially specified", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  config <- dbug()
  config$targets <- "drake_target_1"
  testrun(config)
  expect_true(file.exists("intermediatefile.rds"))
  expect_error(readd(final, search = FALSE))
  config$targets <- "final"
  testrun(config)
  expect_true(is.numeric(readd(final, search = FALSE)))
  pl <- drake_plan(x = 1, y = 2)
  expect_error(check_plan_(pl, "lskjdf", verbose = FALSE))
  expect_warning(check_plan_(pl, c("lskdjf", "x"), verbose = FALSE))
  expect_silent(check_plan_(pl, verbose = FALSE))
})

test_with_dir("file_store quotes properly", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_equal(file_store("x"), encode_path("x"))
})

test_with_dir("misc utils", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_equal(pair_text("x", c("y", "z")), c("xy", "xz"))
  config <- list(plan = data.frame(x = 1, y = 2))
  expect_error(config_checks(config), regexp = "columns")
  expect_error(targets_from_dots(123, NULL), regexp = "must contain names")
})

test_with_dir("make(..., skip_imports = TRUE) works", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  con <- dbug()
  verbose <- max(con$jobs) < 2
  suppressMessages({
    make(
      con$plan, parallelism = con$parallelism,
      envir = con$envir, jobs = con$jobs, verbose = verbose,
      skip_imports = TRUE,
      session_info = FALSE
    )
    con <- drake_config(
      con$plan, parallelism = con$parallelism,
      envir = con$envir, jobs = con$jobs, verbose = verbose,
      skip_imports = TRUE,
      session_info = FALSE
    )
  })
  expect_equal(
    sort(cached()),
    sort(display_keys(
      c(encode_path("intermediatefile.rds"), con$plan$target)
    ))
  )

  # If the imports are already cached, the targets built with
  # skip_imports = TRUE should be up to date.
  make(con$plan, verbose = FALSE, envir = con$envir, session_info = FALSE)
  clean(list = con$plan$target, verbose = FALSE)
  suppressMessages({
    make(
      con$plan, parallelism = con$parallelism,
      envir = con$envir, jobs = con$jobs, verbose = verbose,
      skip_imports = TRUE, session_info = FALSE
    )
    con <- drake_config(
      con$plan, parallelism = con$parallelism,
      envir = con$envir, jobs = con$jobs, verbose = verbose,
      skip_imports = TRUE, session_info = FALSE
    )
  })
  out <- outdated(con)
  expect_equal(out, character(0))
})

test_with_dir("assert_pkg", {
  skip_on_cran()
  expect_error(assert_pkg("_$$$blabla"), regexp = "not installed")
  expect_error(
    assert_pkg("digest", version = "9999.9999.9999"),
    regexp = "must be version 9999.9999.9999 or greater"
  )
  expect_error(
    assert_pkg(
      "CodeDepends",
      version = "9999.9999.9999",
      install = "BiocManager::install"
    ),
    regexp = "with BiocManager::install"
  )
})

test_with_dir("packages are loaded in prework", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("abind")

  original <- getOption("test_drake_option_12345")
  options(test_drake_option_12345 = "unset")
  expect_equal(getOption("test_drake_option_12345"), "unset")
  config <- dbug()
  try(detach("package:abind", unload = TRUE), silent = TRUE) # nolint
  expect_error(abind(1))

  # Load packages with the 'packages' argument
  config$packages <- "abind"
  config$prework <- "options(test_drake_option_12345 = 'set')"
  config$plan <- drake_plan(
    x = getOption("test_drake_option_12345"),
    y = c(deparse(body(abind)), x)
  )
  config$targets <- config$plan$target
  expect_false(any(c("x", "y") %in% config$cache$list()))
  testrun(config)
  expect_true(all(c("x", "y") %in% config$cache$list()))
  expect_equal(readd(x, search = FALSE), "set")
  expect_true(length(readd(y, search = FALSE)) > 0)
  options(test_drake_option_12345 = original)
  clean(search = FALSE)

  # load packages the usual way
  options(test_drake_option_12345 = "unset")
  expect_equal(getOption("test_drake_option_12345"), "unset")
  try(detach("package:abind", unload = TRUE), silent = TRUE) # nolint
  expect_error(abind(1))
  library(abind) # nolint
  config$packages <- NULL
  expect_false(any(c("x", "y") %in% config$cache$list()))

  # drake may be loaded with devtools::load_all() but not
  # installed.
  scenario <- get_testing_scenario()
  suppressWarnings(
    make(
      plan = config$plan,
      targets = config$targets,
      envir = config$envir,
      verbose = FALSE,
      parallelism = scenario$parallelism,
      jobs = scenario$jobs,
      prework = config$prework,
      command = config$command,
      session_info = FALSE
    )
  )
  expect_true(all(c("x", "y") %in% config$cache$list()))
  expect_equal(readd(x, search = FALSE), "set")
  expect_true(length(readd(y, search = FALSE)) > 0)
  options(test_drake_option_12345 = original)
})

test_with_dir("parallelism can be a scheduler function", {
  plan <- drake_plan(x = file.create("x"))
  build_ <- function(target, config){
    tidy_expr <- eval(
      expr = config$layout[[target]]$command_build,
      envir = config$eval
    )
    eval(expr = tidy_expr, envir = config$eval)
  }
  loop_ <- function(config) {
    targets <- igraph::topo_sort(config$schedule)$name
    for (target in targets) {
      console_target(target = target, config = config)
      config$eval[[target]] <- build_(
        target = target,
        config = config
      )
    }
    invisible()
  }
  config <- drake_config(plan, parallelism = loop_)
  expect_warning(
    make(config = config),
    regexp = "Use at your own risk"
  )
  expect_true(file.exists("x"))
  expect_false(config$cache$exists("x"))
})
