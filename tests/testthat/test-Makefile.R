drake_context("Makefile")

test_with_dir("recipe commands", {
  expect_output(Makefile_recipe())
  expect_output(Makefile_recipe(recipe_command = "R -e 'R_RECIPE' -q"))
  my_plan <- workflow(y = 1)
  expect_true(is.character(default_recipe_command()))
  expect_true(is.character(r_recipe_wildcard()))
  con1 <- make(my_plan, command = default_Makefile_command(),
    parallelism = "Makefile", recipe_command = "some_command",
    verbose = FALSE, imports_only = TRUE
  )
  expect_equal(con1$recipe_command, "some_command")
  expect_true(con1$recipe_command != default_recipe_command())
  con2 <- config(plan = my_plan, parallelism = "Makefile",
    recipe_command = "my_command", verbose = FALSE)
  expect_equal(con2$recipe_command, "my_command")
  expect_true(con2$recipe_command != default_recipe_command())
})

test_with_dir("no Makefile if imports_only is TRUE", {
  expect_equal(cached(), character(0))
  x <- workflow(a = ls())
  expect_false(file.exists("Makefile"))
  make(
    x,
    parallelism = "Makefile",
    imports_only = TRUE,
    verbose = FALSE
  )
  expect_true(cached("ls"))
  expect_false(file.exists("Makefile"))
})

test_with_dir("prepend arg works", {
  config <- dbug()
  config$verbose <- FALSE
  config$prepend <- "# add"
  run_Makefile(config, run = FALSE)
  lines <- readLines("Makefile")
  expect_true(grepl("# add", lines[1]))
})

test_with_dir("files inside directories can be timestamped", {
  plan <- workflow(
    list = c(
      `'t1/t2'` = "dir.create(\"t1\"); saveRDS(1, file.path(\"t1\", \"t2\"))"
    )
  )
  plan$target[1] <- file <- eply::quotes(file.path("t1",
    "t2"), single = TRUE)
  config <- build_config(plan = plan, targets = plan$target[1],
    parallelism = "parLapply", verbose = FALSE, packages = character(0),
    prework = character(0), prepend = character(0), command = character(0),
    args = character(0), recipe_command = default_recipe_command(),
    envir = new.env(), jobs = 1,
    cache = NULL, clear_progress = FALSE,
    timeout = Inf, cpu = Inf, elapsed = Inf,
    hook = function(code){
      force(code)
    },
    retries = 0, imports_only = FALSE)
  path <- cache_path(config$cache)
  run_Makefile(config, run = FALSE)
  expect_silent(mk(config$plan$target[1], cache_path = path))
  expect_true(file.exists("t1"))
  expect_true(file.exists(eply::unquote(file)))
  unlink("t1", recursive = TRUE, force = TRUE)
  expect_false(file.exists("t1"))

  expect_silent(make(config$plan, verbose = FALSE))
  expect_true(file.exists("t1"))
  expect_true(file.exists(eply::unquote(file)))
  unlink("t1", recursive = TRUE, force = TRUE)
  expect_false(file.exists("t1"))
})

test_with_dir("basic Makefile stuff works", {
  config <- dbug()
  make(config$plan, targets = "combined", envir = config$envir,
    verbose = FALSE)
  config$verbose <- FALSE
  cache_path <- cache_path(config$cache)
  run_Makefile(config, run = FALSE, debug = TRUE)
  using_global <- identical(config$envir, globalenv())
  if (using_global) {
    expect_true(file.exists(globalenv_file(cache_path)))
  }
  expect_true(file.exists("Makefile"))
  dir <- time_stamp_dir(cache_path)
  stamps <- unname(sort(list.files(dir, full.names = FALSE)))
  stamps2 <- unname(sort(
    time_stamp(
     c(
        "combined",
        "myinput",
        "nextone",
        "yourinput"
      ),
      config = config
    )
  ))
  expect_equal(stamps, stamps2)

  targ <- "'intermediatefile.rds'"
  expect_false(file.exists(eply::unquote(targ)))
  config$cache$del(key = targ, namespace = "progress")
  mk(targ, cache_path = cache_path)
  expect_equal(unname(progress(list = targ)), "finished")
  expect_true(file.exists(eply::unquote(targ)))
  config$cache$del(key = targ, namespace = "progress")
  mk(targ, cache_path = cache_path) # Verify behavior when target is current
  expect_equal(unname(progress(list = targ)), "not built or imported")

  run_Makefile(config, run = FALSE)
  expect_false(file.exists(globalenv_file(cache_path)))
})

test_with_dir("Makefile stuff in globalenv()", {
  targ <- "drake_TESTGLOBAL_target"
  drake_TESTGLOBAL_plan <- data.frame(target = targ, command = 1)
  drake_TESTGLOBAL_config <- make(
    drake_TESTGLOBAL_plan,
    envir = globalenv(),
    verbose = FALSE
  )
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
  pkgs <- rownames(utils::installed.packages())
  need <- c("abind", "MASS")
  for (pkg in need){
    if (!(pkg %in% pkgs)){
      skip(paste("Package", pkg, "is not installed."))
    }
  }

  original <- getOption("test_drake_option_12345")
  options(test_drake_option_12345 = "unset")
  expect_equal(getOption("test_drake_option_12345"), "unset")
  config <- dbug()
  if (R.utils::isPackageLoaded("abind"))
  detach("package:abind")
  if (R.utils::isPackageLoaded("MASS"))
  detach("package:MASS")
  expect_error(abind(1))
  expect_error(deparse(body(lda)))

  # Load packages with the 'packages' argument
  config$packages <- c("abind", "MASS")
  config$prework <- "options(test_drake_option_12345 = 'set')"
  config$plan <- workflow(
    x = getOption("test_drake_option_12345"),
    y = c(abind("option"), deparse(body(lda)), x),
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
  if (R.utils::isPackageLoaded("abind"))
  detach("package:abind")
  if (R.utils::isPackageLoaded("MASS"))
  detach("package:MASS")
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
    command = config$command
  ))
  expect_true(all(c("x", "y") %in% config$cache$list()))
  expect_equal(readd(x, search = FALSE), "set")
  expect_true(length(readd(y, search = FALSE)) > 0)
  options(test_drake_option_12345 = original)
})
