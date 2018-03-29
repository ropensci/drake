drake_context("Makefile")

test_with_dir("recipe commands", {
  expect_message(Makefile_recipe())
  expect_message(Makefile_recipe(recipe_command = "R -e 'R_RECIPE' -q"))
  my_plan <- drake_plan(y = 1)
  expect_true(is.character(default_recipe_command()))
  expect_true(is.character(r_recipe_wildcard()))
  con1 <- drake_config(my_plan, command = default_Makefile_command(),
    parallelism = "Makefile", recipe_command = "some_command",
    verbose = FALSE
  )
  expect_equal(con1$recipe_command, "some_command")
  expect_true(con1$recipe_command != default_recipe_command())
  con2 <- drake_config(plan = my_plan, parallelism = "Makefile",
    recipe_command = "my_command", verbose = FALSE)
  expect_equal(con2$recipe_command, "my_command")
  expect_true(con2$recipe_command != default_recipe_command())
})

test_with_dir("no Makefile for make_imports()", {
  expect_equal(cached(), character(0))
  x <- drake_plan(a = ls())
  expect_false(file.exists("Makefile"))
  con <- drake_config(
    x,
    parallelism = "Makefile",
    verbose = FALSE
  )
  make_imports(con)
  expect_true(cached("ls", verbose = FALSE))
  expect_false(file.exists("Makefile"))
})

test_with_dir("prepend arg works", {
  config <- dbug()
  config$verbose <- FALSE
  config$prepend <- "# add"
  store_drake_config(config = config)
  run_Makefile(config, run = FALSE)
  lines <- readLines("Makefile")
  expect_true(grepl("# add", lines[1], fixed = TRUE))
})

test_with_dir("files inside directories can be timestamped", {
  plan <- drake_plan({
    dir.create("t1"); saveRDS(1, file_out("t1/t2"))
  })
  file <- plan$target[1]
  config <- drake_config(plan = plan, targets = plan$target[1],
    parallelism = "parLapply", verbose = FALSE,
    envir = new.env(), cache = NULL)
  path <- cache_path(config$cache)
  store_drake_config(config = config)
  run_Makefile(config, run = FALSE)
  prepare_distributed(config = config)
  expect_silent(mk(config$plan$target[1], cache_path = path))
  expect_true(file.exists("t1"))
  expect_true(file.exists(drake::drake_unquote(file)))
  unlink("t1", recursive = TRUE, force = TRUE)
  expect_false(file.exists("t1"))

  expect_silent(make(config$plan, verbose = FALSE, session_info = FALSE))
  expect_true(file.exists("t1"))
  expect_true(file.exists(drake::drake_unquote(file)))
  unlink("t1", recursive = TRUE, force = TRUE)
  expect_false(file.exists("t1"))
})

test_with_dir("basic Makefile stuff works", {
  config <- dbug()
  make(config$plan, targets = "combined", envir = config$envir,
    verbose = FALSE, session_info = FALSE)
  config$verbose <- FALSE
  cache_path <- cache_path(config$cache)
  initialize_session(config = config)
  set_attempt_flag(config = config)
  config$recipe_command <- "Rscript -e"
  store_drake_config(config = config)
  run_Makefile(config, run = FALSE, debug = TRUE)
  using_global <- identical(config$envir, globalenv())
  if (using_global) {
    expect_true(file.exists(globalenv_file(cache_path)))
  }
  expect_true(file.exists("Makefile"))
  lines <- paste(readLines("Makefile"), collapse = "\n")
  expect_true(grepl("Rscript -e 'drake::mk(", lines, fixed = TRUE))
  dir <- time_stamp_dir(cache_path)
  stamps <- unname(sort(list.files(dir, full.names = FALSE)))
  stamps2 <- unname(sort(
    time_stamp(
     c(
        "combined",
        "final",
        "myinput",
        "nextone",
        "yourinput"
      ),
      config = config
    )
  ))
  expect_equal(stamps, stamps2)

  targ <- "\"intermediatefile.rds\""
  expect_false(file.exists(drake::drake_unquote(targ)))
  config$cache$del(key = targ, namespace = "progress")
  mk(targ, cache_path = cache_path)
  expect_equal(unname(progress(list = targ)), "finished")
  expect_true(file.exists(drake::drake_unquote(targ)))
  config$cache$del(key = targ, namespace = "progress")
  mk(targ, cache_path = cache_path) # Verify behavior when target is current
  expect_equal(unname(progress(list = targ)), "not built or imported")

  store_drake_config(config = config)
  run_Makefile(config, run = FALSE)
  expect_false(file.exists(globalenv_file(cache_path)))
})

test_with_dir("Makefile stuff in globalenv()", {
  targ <- "drake_TESTGLOBAL_target"
  drake_TESTGLOBAL_plan <- data.frame(target = targ, command = 1)
  drake_TESTGLOBAL_config <- make(
    drake_TESTGLOBAL_plan,
    envir = globalenv(),
    verbose = FALSE, session_info = FALSE
  )
  store_drake_config(drake_TESTGLOBAL_config)
  run_Makefile(drake_TESTGLOBAL_config, run = FALSE, debug = TRUE)
  clean(list = targ)
  drake_TESTGLOBAL_config$cache$del(key = targ, namespace = "progress")
  expect_equal(unname(progress(list = targ)), "not built or imported")
  mk("drake_TESTGLOBAL_target", cache_path = default_cache_path())
  expect_equal(unname(progress(list = targ)), "finished")
  drake_TESTGLOBAL_config$cache$del(key = targ, namespace = "progress")
  mk("drake_TESTGLOBAL_target", cache_path = default_cache_path())
  expect_equal(unname(progress(list = targ)), "not built or imported")
  loaded <- ls(envir = globalenv())
  rm(list =
    intersect(loaded,
      c(
        "drake_TESTGLOBAL_target",
        "drake_TESTGLOBAL_plan",
        "drake_TESTGLOBAL_config"
      )
    ),
    envir = globalenv()
  )
})

test_with_dir("packages are loaded in prework", {
  skip_if_not_installed("abind")
  skip_if_not_installed("MASS")

  original <- getOption("test_drake_option_12345")
  options(test_drake_option_12345 = "unset")
  expect_equal(getOption("test_drake_option_12345"), "unset")
  config <- dbug()
  if (R.utils::isPackageLoaded("abind")){
    # Suppress goodpractice::gp(): legitimate need for detach(). # nolint
    eval(parse(text = "detach('package:abind', unload = TRUE)"))
  }
  if (R.utils::isPackageLoaded("MASS")){
    # Suppress goodpractice::gp(): legitimate need for detach(). # nolint
    eval(parse(text = "detach('package:MASS', unload = TRUE)"))
  }
  expect_error(abind(1))
  expect_error(deparse(body(lda)))

  # Load packages with the 'packages' argument
  config$packages <- c("abind", "MASS")
  config$prework <- "options(test_drake_option_12345 = 'set')"
  config$plan <- drake_plan(
    x = getOption("test_drake_option_12345"),
    y = c(deparse(body(abind)), deparse(body(lda)), x),
    strings_in_dots = "literals"
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
  if (R.utils::isPackageLoaded("abind")){
    # Suppress goodpractice::gp(): legitimate need for detach()
    eval(parse(text = "detach('package:abind', unload = TRUE)"))
  }
  if (R.utils::isPackageLoaded("MASS")){
    # Suppress goodpractice::gp(): legitimate need for detach()
    eval(parse(text = "detach('package:MASS', unload = TRUE)"))
  }
  expect_error(abind(1))
  expect_error(deparse(body(lda)))
  library(abind)
  library(MASS)
  config$packages <- NULL
  expect_false(any(c("x", "y") %in% config$cache$list()))

  # drake may be loaded with devtools::load_all() but not
  # installed.
  scenario <- get_testing_scenario()
  suppressWarnings(make(plan = config$plan, targets = config$targets,
    envir = config$envir, verbose = FALSE, parallelism = scenario$parallelism,
    jobs = scenario$jobs, prework = config$prework, prepend = config$prepend,
    command = config$command, session_info = FALSE
  ))
  expect_true(all(c("x", "y") %in% config$cache$list()))
  expect_equal(readd(x, search = FALSE), "set")
  expect_true(length(readd(y, search = FALSE)) > 0)
  options(test_drake_option_12345 = original)
})
