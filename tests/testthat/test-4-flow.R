drake_context("flow")

test_with_dir("scratch build with custom filesystem cache.", {
  config <- dbug()
  unlink(default_cache_path(), recursive = TRUE)
  path <- "my_cache"
  config$cache <- cache <- new_cache(
    path = path,
    hash_algorithm = "murmur32"
  )
  expect_error(drake_get_session_info(cache = cache))
  expect_true(nrow(drake_progress(cache = cache)) == 0)
  expect_equal(config$cache$list(), character(0))

  testrun(config)

  expect_true(is.numeric(readd(final, cache = cache)))
  expect_true(length(config$cache$list()) > 2)
  expect_false(any(c("f", "final") %in% ls()))
  cache <- drake_cache(path = path)
  expect_equal(cache$hash_algorithm, "murmur32")

  # changed nothing
  testrun(config)
  nobuild(config)

  cache <- storr::storr_rds(path = path)

  # take this opportunity to test clean() and prune()
  all <- sort(c(reencode_path("input.rds"),
    reencode_path("intermediatefile.rds"), "drake_target_1", "a",
    "b", "c", "combined", "f", "final", "g", "h", "i", "j",
    "myinput", "nextone", "yourinput"))
  expect_equal(config$cache$list(), all)
  expect_true(file.exists("intermediatefile.rds"))
  expect_true(file.exists("input.rds"))
  expect_false(file.exists(default_cache_path()))
  expect_true(file.exists(path))

  # clean specific targets
  clean(b, c, list = c("drake_target_1", "nextone"),
    cache = cache)
  expect_true(file.exists("intermediatefile.rds"))
  expect_true(file.exists("input.rds"))
  expect_equal(
    sort(config$cache$list()),
    sort(setdiff(
      all,
      c("b", "c", "drake_target_1", "nextone")
    ))
  )

  # clean does not remove imported files
  expect_true(file.exists("input.rds"))
  expect_true(reencode_path("input.rds") %in%
    config$cache$list())
  clean(list = reencode_path("input.rds"), cache = cache)
  expect_true(file.exists("input.rds"))
  expect_false(reencode_path("input.rds") %in%
    config$cache$list())

  # clean removes imported functions and cleans up 'functions'
  # namespace
  expect_true("f" %in% cached(targets_only = FALSE, cache = cache))
  expect_true("f" %in% config$cache$list())
  clean(f, cache = cache)
  expect_false("f" %in% config$cache$list())

  clean(destroy = FALSE, cache = cache)
  expect_equal(config$cache$list(), character(0))
  expect_true(file.exists("intermediatefile.rds"))
  expect_true(file.exists("input.rds"))
  expect_false(file.exists(default_cache_path()))
  expect_true(file.exists(path))

  testrun(config)
  clean(destroy = FALSE, cache = cache, garbage_collection = TRUE)
  expect_equal(config$cache$list(), character(0))
  expect_true(file.exists("intermediatefile.rds"))
  expect_true(file.exists("input.rds"))
  expect_false(file.exists(default_cache_path()))
  expect_true(file.exists(path))

  clean(destroy = TRUE, cache = cache)
  expect_false(file.exists(path))
})

test_with_dir("clean in full build.", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  config <- dbug()
  make_impl(config)
  expect_true("final" %in% config$cache$list())
  clean(final)
  expect_false("final" %in% config$cache$list())
  clean()
  expect_equal(config$cache$list(), character(0))
  expect_true(file.exists(default_cache_path()))
  clean(destroy = TRUE)
  expect_false(file.exists(default_cache_path()))
})

test_with_dir("make() with skip_targets", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  expect_silent(make(drake_plan(x = 1), skip_targets = TRUE,
    verbose = 0L, session_info = FALSE))
  expect_false("x" %in% cached())
})

test_with_dir("make(..., skip_imports = TRUE) works", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  con <- dbug()
  plan <- dbug_plan()
  make(
    plan, parallelism = con$settings$parallelism,
    envir = con$envir, jobs = con$settings$jobs,
    skip_imports = TRUE,
    session_info = FALSE
  )
  con <- drake_config(
    plan, parallelism = con$settings$parallelism,
    envir = con$envir, jobs = con$settings$jobs,
    skip_imports = TRUE,
    session_info = FALSE
  )
  expect_equal(
    sort(cached(targets_only = FALSE)),
    sort(redisplay_keys(
      c(reencode_path("intermediatefile.rds"), plan$target)
    ))
  )

  # If the imports are already cached, the targets built with
  # skip_imports = TRUE should be up to date.
  make(plan, envir = con$envir, session_info = FALSE)
  clean(list = plan$target)
  make(
    plan, parallelism = con$settings$parallelism,
    envir = con$envir, jobs = con$settings$jobs,
    skip_imports = TRUE, session_info = FALSE
  )
  con <- drake_config(
    plan, parallelism = con$settings$parallelism,
    envir = con$envir, jobs = con$settings$jobs,
    skip_imports = TRUE, session_info = FALSE
  )
  out <- outdated_impl(con)
  expect_equal(out, character(0))
})

test_with_dir("skip everything", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  f <- function(x) {
    x
  }
  pl <- drake_plan(a = f(0))
  make(
    pl,
    session_info = FALSE,
    skip_targets = TRUE,
    skip_imports = TRUE,
    verbose = 1L
  )
  con <- drake_config(
    pl,
    session_info = FALSE,
    skip_targets = TRUE,
    skip_imports = TRUE
  )
  expect_equal(justbuilt(con), character(0))
})

test_with_dir("can keep going", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  parallelism <- scenario$parallelism
  jobs <- scenario$jobs
  e$fail <- function(...) {
    stop("oops")
  }
  e$succeed <- function(...) {
    invisible()
  }
  plan <- drake_plan(
    a1 = fail(),
    a2 = succeed(),
    a3 = succeed(),
    a4 = fail(),
    b1 = fail(a1),
    b2 = succeed(a2),
    b3 = succeed(a3),
    b4 = succeed(a4)
  )
  # warnings depend on the parallelism
  suppressWarnings(
    make(
      plan,
      keep_going = TRUE,
      parallelism = parallelism,
      verbose = 0L,
      jobs = jobs,
      envir = e,
      session_info = FALSE
    )
  )
  expect_equal(sort(cached(targets_only = FALSE)),
               sort(c("a2", "a3", "b2", "b3", "b4", "fail", "succeed")))
  expect_equal(sort(drake_failed()), sort(c("a1", "a4", "b1")))
})

test_with_dir("failed targets do not become up to date", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  fail <- FALSE
  plan <- drake_plan(
    d = 3,
    a = {
      if (fail) {
        stop("my failure message")
      } else {
        d
      }
    },
    b = 5,
    c = list(a, b)
  )
  make(plan)
  con <- drake_config(plan)
  expect_equal(sort(justbuilt(con)), sort(letters[1:4]))
  fail <- TRUE
  expect_error(make(plan))
  expect_error(make(plan))
  meta <- diagnose(a)
  expect_true(grepl("my failure message", meta$error$message, fixed = TRUE))
  con <- drake_config(plan)
  expect_equal(sort(outdated_impl(con)), sort(c("a", "c")))
})

test_with_dir("true targets can be functions", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  generator <- function() {
    return(
      function(x) {
        x + 1
      }
    )
  }
  plan <- drake_plan(myfunction = generator(), output = myfunction(1))
  make(plan, verbose = 0L, session_info = FALSE)
  config <- drake_config(plan, verbose = 0L, session_info = FALSE)
  expect_equal(readd(output), 2)
  expect_true(is.function(config$cache$get("myfunction")))
  myfunction <- readd(myfunction)
  expect_equal(myfunction(4), 5)
})

test_with_dir("targets can be partially specified", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  config <- dbug()
  config$targets <- "drake_target_1"
  testrun(config)
  expect_true(file.exists("intermediatefile.rds"))
  expect_error(
    suppressWarnings( # https://github.com/richfitz/storr/issues/105 # nolint
      readd(final)
    )
  )
  config$targets <- "final"
  testrun(config)
  expect_true(is.numeric(readd(final)))
})
