# library(testthat); library(devtools); load_all()
context("Makefile")

test_that("prepend arg works", {
  dclean()
  config = dbug()
  config$verbose = FALSE
  config$prepend = "# add"
  run_Makefile(config, run = FALSE)
  lines = readLines("Makefile")
  expect_true(grepl("# add", lines[1]))
  dclean()
})

test_that("files inside directories can be timestamped", {
  dclean()
  plan = plan(list = c("'t1/t2'" = 
    'dir.create("t1"); saveRDS(1, file.path("t1", "t2"))'))
  plan$target[1] = file = eply::quotes(file.path("t1", "t2"), 
    single = TRUE)
  config = build_config(plan = plan, targets = plan$target[1],
    parallelism = "parLapply", verbose = FALSE, packages = character(0),
    prework = character(0), prepend = character(0),
    command = character(0), args = character(0), envir = new.env(),
    jobs = 1)
  run_Makefile(config, run = FALSE)
  expect_silent(mk(config$plan$target[1]))
  expect_true(file.exists("t1"))
  expect_true(file.exists(eply::unquote(file)))
  unlink("t1", recursive = TRUE)
  expect_false(file.exists("t1"))
  dclean()
  expect_silent(make(config$plan, verbose = FALSE))
  expect_true(file.exists("t1"))
  expect_true(file.exists(eply::unquote(file)))
  unlink("t1", recursive = TRUE)
  expect_false(file.exists("t1"))
  dclean()
})

test_that("basic Makefile stuff works", {
  dclean()
  config = dbug()
  make(config$plan, targets = "combined", 
    envir = config$envir, verbose = FALSE)
  config$verbose = FALSE
  run_Makefile(config, run = FALSE, debug = TRUE)
  using_global = identical(config$envir, globalenv())
  if(using_global) expect_true(file.exists(globalenvpath))
  expect_true(file.exists("Makefile"))
  stamps = sort(list.files(file.path(time_stamp_dir), full.names = TRUE))
  stamps2 = sort(time_stamp(c("combined", "myinput", "nextone", 
    "yourinput")))
  expect_equal(stamps, stamps2)
  expect_false(file.exists("intermediatefile.rds"))
  mk("'intermediatefile.rds'")
  expect_true(file.exists("intermediatefile.rds"))
  run_Makefile(config, run = FALSE)
  expect_false(file.exists(globalenvpath))
  dclean()
  expect_false(file.exists("Makefile"))
})

test_that("packages are loaded in prework", {
  dclean()
  original = getOption("testdrake")
  options(testdrake = "unset")
  expect_equal(getOption("testdrake"), "unset")
  config = dbug()
  if(R.utils::isPackageLoaded("abind"))
    detach("package:abind")
  if(R.utils::isPackageLoaded("MASS"))
    detach("package:MASS")
  expect_error(abind(1))
  expect_error(deparse(body(lda)))
  
  # Load packages with the 'packages' argument
  config$packages = c("abind", "MASS")
  config$prework = "options(testdrake = 'set')"
  config$plan = plan(x = getOption("testdrake"),
    y = c(abind("option"), deparse(body(lda)), x), 
    strings_in_dots = "literals")
  config$targets = config$plan$target
  expect_false(any(c("x", "y") %in% config$cache$list()))
  testrun(config)
  expect_true(all(c("x", "y") %in% config$cache$list()))
  expect_equal(readd(x, search = FALSE), "set")
  expect_true(length(readd(y, search = FALSE)) > 0)
  options(testdrake = original)
  clean(search = FALSE)
  
  # load packages the usual way
  options(testdrake = "unset")
  expect_equal(getOption("testdrake"), "unset")
  if(R.utils::isPackageLoaded("abind"))
    detach("package:abind")
  if(R.utils::isPackageLoaded("MASS"))
    detach("package:MASS")
  expect_error(abind(1))
  expect_error(deparse(body(lda)))
  library(abind)
  library(MASS)
  config$packages = NULL
  expect_false(any(c("x", "y") %in% config$cache$list()))
  opts = test_opt()
  suppressWarnings( # drake may be loaded but not installed.
    make(plan = config$plan, targets = config$targets,
      envir = config$envir, verbose = FALSE,
      parallelism = opt$parallelism, jobs = opt$jobs,
      prework = config$prework,
      prepend = config$prepend, command = config$command)
  )
  expect_true(all(c("x", "y") %in% config$cache$list()))
  expect_equal(readd(x, search = FALSE), "set")
  expect_true(length(readd(y, search = FALSE)) > 0)
  options(testdrake = original)
  dclean()
})
