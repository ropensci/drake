drake_context("cache")

test_with_dir("clean() removes the correct files", {
  skip_on_cran()
  skip_if_not_installed("knitr")
  cache <- storr::storr_environment()
  writeLines("123", "a.txt")
  writeLines("123", "b.txt")
  dir.create("abc")
  writeLines("123", "abc/c.txt")
  plan <- drake_plan(
    a = file_in("a.txt"),
    b = knitr_in("b.txt"),
    d = writeLines("123", file_out("d.rds")),
    x = file_in("abc"),
    y = {
      dir.create(file_out("xyz"))
      writeLines("123", "xyz/e.txt")
    }
  )
  make(
    plan,
    cache = cache,
    session_info = FALSE
  )
  clean(cache = cache)
  expect_true(file.exists("a.txt"))
  expect_true(file.exists("b.txt"))
  expect_true(file.exists("d.rds"))
  expect_true(dir.exists("abc"))
  expect_true(dir.exists("xyz"))
  expect_true(file.exists("abc/c.txt"))
  expect_true(file.exists("xyz/e.txt"))
  make(
    plan,
    cache = cache,
    session_info = FALSE,
    recover = TRUE
  )
  clean(cache = cache, garbage_collection = TRUE)
  expect_true(file.exists("a.txt"))
  expect_true(file.exists("b.txt"))
  expect_true(file.exists("d.rds"))
  expect_true(dir.exists("abc"))
  expect_true(dir.exists("xyz"))
  expect_true(file.exists("abc/c.txt"))
  expect_true(file.exists("xyz/e.txt"))
})

test_with_dir("drake_version", {
  skip_on_cran()
  cache <- storr::storr_environment()
  expect_equal(
    drake_cache_version(cache),
    as.character(utils::packageVersion("drake"))
  )
  make(drake_plan(x = 1), session_info = FALSE)
  con <- drake_config(drake_plan(x = 1), session_info = FALSE)
  expect_true(is.character(drake_cache_version(con$cache)))
  clean()
  make(drake_plan(x = 1), session_info = TRUE)
  con <- drake_config(drake_plan(x = 1), session_info = TRUE)
  expect_true(is.character(drake_cache_version(con$cache)))
  con$cache$clear(namespace = "session")
  expect_equal(
    drake_cache_version(con$cache),
    as.character(utils::packageVersion("drake"))
  )
})

test_with_dir("dependency profile", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("knitr")
  b <- 1
  plan <- drake_plan(a = b)
  make(plan, session_info = FALSE)
  config <- drake_config(plan, session_info = FALSE)
  expect_error(
    deps_profile_impl(target = missing, config = config),
    regexp = "no recorded metadata"
  )
  expect_false(any(deps_profile_impl(target = a, config = config)$changed))
  b <- 2
  expect_false(any(deps_profile_impl(target = a, config = config)$changed))
  config$settings$skip_targets <- TRUE
  make_impl(config = config)
  dp <- deps_profile_impl(target = a, config = config)
  expect_true(as.logical(dp[dp$name == "depend", "changed"]))
  expect_equal(sum(dp$changed), 1)
  plan$command <- "b + c"
  config$spec <- create_drake_spec(
    plan = plan,
    envir = config$envir,
    cache = config$cache,
    logger = config$logger
  )
  dp <- deps_profile_impl(target = a, config = config)
  expect_true(as.logical(dp[dp$name == "command", "changed"]))
  expect_equal(sum(dp$changed), 2)
  load_mtcars_example()
  config <- drake_config(
    my_plan,
    cache = storr::storr_environment(),
    skip_targets = TRUE,
    session_info = FALSE
  )
  make_impl(config = config)
  out <- deps_profile_impl(
    file_store("report.Rmd"),
    character_only = TRUE,
    config
  )
  expect_equal(nrow(out), 5L)
})

test_with_dir("deps_profile_impl() on imports (#1134)", {
  skip_on_cran()
  f <- function(x) {
    x
  }
  plan <- drake_plan(y = f(1))
  make(plan)
  config <- drake_config(plan)
  out <- deps_profile_impl(target = f, config = config)
  expect_equal(sort(out$name), sort(c("depend", "file_in")))
})

test_with_dir("Missing cache", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  s <- storr::storr_rds("s")
  unlink(s$path, recursive = TRUE)
  expect_equal(cached(), character(0))
  expect_equal(cached_planned(), character(0))
  expect_equal(cached_unplanned(), character(0))
})

test_with_dir("Cache namespaces", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  y <- target_namespaces_()
  z <- cleaned_namespaces_()
  expect_true(all(z %in% y))
  expect_false(all(y %in% z))
})

test_with_dir("clean() works if there is no cache already", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  clean(list = "no_cache")
  expect_false(file.exists(default_cache_path()))
})

test_with_dir("can exclude bad targets from loadd()", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  plan <- drake_plan(a = TRUE)
  make(plan)
  e <- new.env(parent = emptyenv())
  loadd(a, b, lazy = FALSE, envir = e)
  expect_true(exists("a", envir = e, inherits = FALSE))
  expect_equal(e$a, TRUE)
  expect_false(exists("b", envir = e, inherits = FALSE))
})

test_with_dir("bad/corrupt caches, no progress, no seed", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  expect_null(drake_fetch_rds("sldkfjlke"))
  expect_warning(new_cache(type = "nope"))
  x <- drake_plan(a = 1)
  make(x, verbose = 0L, session_info = FALSE, log_progress = FALSE)
  expect_equal(drake_cache()$list(namespace = "progress"), character(0))
  clean()
  make(x, verbose = 0L, session_info = FALSE, log_progress = TRUE)
  expect_equal(drake_cache()$list(namespace = "progress"), "a")
  path <- file.path(default_cache_path(), "config")
  expect_true(file.exists(path))
  unlink(path, recursive = TRUE)
  expect_false(file.exists(path))
  expect_error(
    read_drake_seed(cache = storr::storr_environment()),
    regexp = "random seed not found"
  )
})

test_with_dir("non-existent caches", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  expect_equal(0, nrow(drake_cache_log()))
  expect_equal(find_cache(), NULL)
  expect_error(loadd(list = "nothing"))
  expect_error(tmp <- read_drake_seed())
  expect_error(tmp <- drake_get_session_info())
  expect_error(tmp <- drake_set_session_info())
  dummy <- new_cache()
})

test_with_dir("drake_gc() and mangled keys", {
  skip_on_cran()
  cache <- storr::storr_rds(tempfile(), mangle_key = TRUE)
  cache$set("a", 1)
  expect_silent(tmp <- drake_gc(cache = cache))
})

test_with_dir("try to rescue non-existent stuff", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  expect_null(rescue_cache())
  cache <- storr_rds("dummy_cache")
  expect_silent(rescue_del(key = "no_key", cache = cache, namespace = "none"))
})

test_with_dir("drake_cache() can search", {
  skip_on_cran()
  dir.create(file.path("w"))
  dir.create(file.path("w", "x"))
  dir.create(file.path("w", "x", "y"))
  dir.create(file.path("w", "x", "y", "z"))
  tmp <- storr::storr_rds(file.path("w", "x", ".drake"), mangle_key = TRUE)
  cache <- with_dir(
    file.path("w", "x", "y", "z"),
    drake_cache()
  )
  expect_true(inherits(cache, "refclass_decorated_storr"))
  cache <- drake_cache(file.path("w", "x", ".drake"))
  expect_true(inherits(cache, "refclass_decorated_storr"))
  cache <- with_dir(
    file.path("w", "x", ".drake", "keys"),
    drake_cache()
  )
  expect_true(inherits(cache, "refclass_decorated_storr"))
  cache <- with_dir(
    file.path("w", "x"),
    drake_cache()
  )
  expect_true(inherits(cache, "refclass_decorated_storr"))
})

test_with_dir("neighboring caches", {
  skip_on_cran()
  cache <- new_cache(".test")
  test_plan <- drake_plan(
    dot_test = 1L
  )
  make(test_plan, cache = cache)
  default_plan <- drake_plan(
    dot_drake = 2L
  )
  make(default_plan)
  expect_equal(cached(cache = drake_cache(".test")), "dot_test")
  expect_equal(cached(), "dot_drake")
  expect_equal(cached(cache = drake_cache(".drake")), "dot_drake")
})

test_with_dir("cache functions work from various working directories", {
  skip_if_not_installed("lubridate")
  # May have been loaded in a globalenv() testing scenario # nolint
  remove_these <- intersect(ls(envir = globalenv()), c("h", "j"))
  rm(list = remove_these, envir = globalenv())

  cache_dir <- basename(default_cache_path())
  scratch <- file.path(getwd(), "scratch")
  if (!file.exists(scratch)) {
    dir.create(scratch) # Will move up a level later.
  }
  with_dir(scratch, {
    expect_equal(nrow(build_times()), 0)
    expect_equal(nrow(drake_progress()), 0)
    expect_false(any("in progress" %in% drake_progress()))
    expect_error(readd())
    config <- dbug()
    using_global <- identical(config$envir, globalenv())
    if (using_global) {
      envir <- globalenv()
    } else {
      envir <- environment()
    }

    config$settings$session_info <- TRUE
    testrun(config)

    # drake_cache_log() # nolint
    all_hashes <- drake_cache_log()
    some_hashes <- drake_cache_log(targets_only = TRUE)
    expect_equal(ncol(all_hashes), ncol(some_hashes))
    n_a <- nrow(all_hashes)
    n_s <- nrow(some_hashes)
    expect_true(n_a > n_s && n_s > 0)

    # drake_gc() should not remove any important targets/imports.
    x <- cached()
    expect_true(length(x) > 0)
    drake_gc()
    y <- cached()
    expect_equal(sort(x), sort(y))
    expect_equal(outdated_impl(config), character(0))

    # targets and imports
    imports <- sort(
      c(
        config$cache$encode_path("input.rds"),
        "a", "b", "c", "f", "g",
        "h", "i", "j"
      )
    )
    builds <- sort(config$plan$target)
    out_files <- config$cache$encode_path("intermediatefile.rds")
    all <- sort(c(builds, imports, out_files))

    # build_times
    x <- config$cache
    bt <- build_times()
    expect_equal(
      sort(redisplay_keys(x$list(namespace = "meta"))),
      sort(cached(targets_only = FALSE))
    )
    expect_equal(
      sort(config$plan$target),
      sort(cached(targets_only = TRUE))
    )
    expect_equal(sort(bt$target), sort(builds))
    expect_length(bt, 4) # 4 columns
    n1 <- nrow(bt)

    # find stuff in current directory session, progress
    expect_equal(read_drake_seed(), config$settings$seed)
    expect_true(is.list(drake_get_session_info()))
    expect_true(all(drake_progress()$progress == "done"))
    expect_false(any("running" %in% drake_progress()))
    expect_equal(sort(drake_progress()$target), sort(config$plan$target))
    exp <- weak_tibble(target = "final", progress = "done")
    expect_equal(drake_progress(final), exp)
    expect_equal(drake_progress(list = "final"), exp)

    # cached, diagnose, rescue
    expect_true(length(diagnose()) > length(config$plan$target))
    expect_error(diagnose("xyz", cache = config$cache), regexp = "metadata")
    expect_equal(
      sort(cached(targets_only = TRUE)),
      sort(builds)
    )

    # find your project
    expect_equal(find_cache(), file.path(getwd(), cache_dir))
    expect_true(is.numeric(readd(a)))

    # load and read stuff
    list <- intersect(
      setdiff(cached(targets_only = FALSE), cached(targets_only = TRUE)),
      ls(envir = envir)
    )
    rm(list = list, envir = envir)
    expect_error(h(1))
    expect_true(is.numeric(readd(final)))

    loadd(yourinput, nextone, jobs = 2, envir = envir)
    expect_true(is.numeric(envir[["yourinput"]]))
    expect_true(is.numeric(envir[["nextone"]]))
    rm(yourinput, nextone, envir = envir)

    # test loadd loadd() everything
    e <- new.env()
    loadd(envir = e)
    expect_equal(sort(config$plan$target), sort(ls(envir = e)))

    # search from a different directory
    if (!file.exists("searchfrom")) {
      dir.create("searchfrom")
      dir.create(file.path("searchfrom", "here"))
    }
  })
  s <- file.path(scratch, "searchfrom", "here")
  with_dir(s, {
    # progress, session
    expect_true(is.list(drake_get_session_info()))
    prog <- drake_progress()
    expect_equal(sort(prog$target), sort(config$plan$target))
    expect_true(all(prog$progress == "done"))
    prog2 <- drake_progress(nothing)
    expect_equal(prog, prog2)

    # cached and diagnose
    expect_equal(sort(diagnose()), sort(config$cache$list()))
    expect_equal(
      sort(cached(targets_only = TRUE)),
      sort(config$plan$target)
    )
    expect_equal(
      length(cached(targets_only = FALSE)),
      length(config$cache$list())
    )

    # find your project
    expect_equal(find_cache(), file.path(scratch, cache_dir))

    # load and read stuff
    expect_true(is.numeric(readd(a)))

    loadd(yourinput, nextone, jobs = 2, envir = envir)
    expect_true(is.numeric(envir[["yourinput"]]))
    expect_true(is.numeric(envir[["nextone"]]))
    rm(yourinput, nextone, envir = envir)

    # load dependencies
    e <- new.env()
    deps <- c("nextone", "yourinput")
    expect_false(any(deps %in% ls(envir = e)))
    loadd(combined, deps = TRUE, config = config, envir = e)
    expect_true(all(deps %in% ls(envir = e)))

    # clean
    expect_true(all(config$plan$target %in% cached()))
    clean(final, garbage_collection = TRUE)
    targs <- setdiff(config$plan$target, "final")
    expect_true(all(targs %in% cached()))
    drake_gc()

    # Test purging
    prog <- drake_progress()
    expect_true("final" %in% prog$target)

    clean(final, garbage_collection = TRUE, purge = TRUE)
    prog <- drake_progress()
    expect_false("final" %in% prog$target)

    # progress is erased with cache rescue
    rescue_cache(targets = "final")
    expect_true(nrow(drake_progress()) > 0L)
    rescue_cache(garbage_collection = FALSE)
    expect_equal(nrow(drake_progress()), 0L)
    rescue_cache(garbage_collection = TRUE)

    # More cleaning checks
    clean(garbage_collection = FALSE)
    expect_equal(cached(), character(0))
    x <- file.path(scratch, cache_dir)
    expect_true(file.exists(x))

    clean(destroy = TRUE)
    expect_false(file.exists(x))
    expect_silent(drake_gc()) # Cache does not exist
  })
})

test_with_dir("memo_expr() works without a cache", {
  skip_on_cran()
  x <- "x"
  expect_equal(memo_expr(x, cache = NULL), x)
})

test_with_dir("run make() from subdir", {
  skip_on_cran()
  old <- Sys.getenv("drake_warn_subdir")
  Sys.setenv(drake_warn_subdir = "")
  on.exit(Sys.setenv(drake_warn_subdir = old))
  plan <- drake_plan(x = 1)
  x <- new_cache()
  y <- new_cache("not_.drake")
  dir.create("subdir")
  with_dir("subdir", {
    expect_warning(make(plan), regexp = "subdirectory")
    expect_warning(make(plan), regexp = "subdirectory")
    expect_warning(make(plan, cache = y), regexp = "subdirectory")
    new_cache(".drake")
    make(plan)
    make(plan, cache = storr::storr_environment())
  })
})

test_with_dir("loadd() does not load imports", {
  skip_on_cran()
  f <- function(x) {
    x + 1
  }
  plan <- drake_plan(y = f(1))
  cache <- storr::storr_environment()
  make(plan, cache = cache, session_info = FALSE)
  e <- new.env(parent = emptyenv())
  rm(f)
  loadd(envir = e, cache = cache)
  expect_equal(ls(e), "y")
  expect_message(
    loadd(f, envir = e, cache = cache, verbose = 1L),
    regexp = "No targets to load"
  )
})

test_with_dir("selection and filtering in progress", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  plan <- drake_plan(x_a = TRUE, y_b = x_a, x_c = stop(y_b))
  expect_error(make(plan))
  out <- drake_progress(x_a, y_b, x_c, d)
  exp <- weak_tibble(
    target = c("x_a", "y_b", "x_c"),
    progress = c("done", "done", "failed")
  )
  expect_equivalent(out, exp)
  exp1 <- weak_tibble(
    target = c("x_a", "x_c", "y_b"),
    progress = c("done", "failed", "done")
  )
  exp2 <- weak_tibble(
    target = c("x_a", "y_b"),
    progress = c("done", "done")
  )
  exp3 <- weak_tibble(
    target = "x_c",
    progress = "failed"
  )
  exp4 <- weak_tibble(
    target = c("x_a", "x_c"),
    progress = c("done", "failed")
  )
  expect_equivalent(drake_progress(progress = c("done", "failed")), exp1)
  expect_equivalent(drake_progress(progress = "done"), exp2)
  expect_equivalent(drake_progress(progress = "failed"), exp3)
  expect_error(drake_progress(progress = "stuck"), "should be one of")
  skip_if_not_installed("tidyselect")
  expect_equivalent(drake_progress(tidyselect::starts_with("x_")), exp4)
  cache <- drake_cache()
  expect_equal(cache$get_progress("12345"), "none")
})

test_with_dir("make() writes a cache log file", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  plan <- drake_plan(a = TRUE, b = TRUE)
  expect_false(file.exists("log.txt"))
  make(plan, cache_log_file = "log.txt")
  expect_true(file.exists("log.txt"))

  # Check structure of cache
  log1 <- read.csv("log.txt", header = TRUE, stringsAsFactors = FALSE)
  expect_equal(log1$type, c("target", "target"))
  expect_equal(log1$name, c("a", "b"))

  # Change plan so cache has to change.
  plan <- drake_plan(a = TRUE, b = FALSE)
  make(plan, cache_log_file = "log.txt")
  log2 <- read.csv("log.txt", header = TRUE, stringsAsFactors = FALSE)

  expect_equal(log1$hash[1], log2$hash[1])

  # Changed parts of cache are different.
  expect_false(log1$hash[2] == log2$hash[2])
})

test_with_dir("loadd(x, deps = TRUE) when x is not cached", {
  skip_on_cran()
  plan <- drake_plan(x = "abc", y = x + 1)
  expect_error(make(plan, session_info = FALSE))
  config <- drake_config(plan, session_info = FALSE)
  e <- new.env(parent = emptyenv())
  expect_equal(ls(e), character(0))
  loadd(y, envir = e, config = config, deps = TRUE)
  expect_equal(ls(e), "x")
  expect_equal(e$x, "abc")
  expect_message(
    loadd(y, envir = e, config = config, deps = TRUE, tidyselect = TRUE),
    regexp = "Disabled"
  )
})

test_with_dir("clean: garbage_collection and destroy", {
  skip_on_cran()
  plan <- drake_plan(x = file.create(file_out("abc")))
  make(plan)
  expect_true(file.exists(".drake"))
  expect_true(file.exists("abc"))
  clean(garbage_collection = TRUE, destroy = TRUE)
  expect_false(file.exists(".drake"))
  expect_true(file.exists("abc"))
})

test_with_dir("fancy cache features, bad paths", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  saveRDS(1, file = "exists")
  suppressWarnings(expect_error(x <- new_cache("exists")))
})

test_with_dir("Pick the hash", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  x <- new_cache("new", hash_algorithm = "murmur32")
  expect_true(file.exists("new"))
  y <- storr::storr_rds(path = "new")
  expect_true(file.exists("new"))
  expect_equal(x$hash_algorithm, "murmur32")
  y <- storr::storr_rds(path = "new")
  z <- drake_cache("new")
  expect_true(file.exists("new"))
  expect_equal(z$hash_algorithm, "murmur32")
})

test_with_dir("totally off the default cache", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  saveRDS("stuff", file = "some_file")
  con <- dbug()
  unlink(default_cache_path(), recursive = TRUE)
  con$plan <- data.frame(target = "a", command = "file_in(\"some_file\")")
  con$targets <- con$plan$target
  con$cache <- new_cache(
    path = "my_new_cache",
    hash_algorithm = "murmur32"
  )
  make(
    con$plan,
    cache = con$cache,
    verbose = 0L,
    parallelism = get_testing_scenario()$parallelism,
    jobs = get_testing_scenario()$jobs,
    session_info = FALSE
  )
  expect_false(file.exists(default_cache_path()))
})

test_with_dir("use two differnt file system caches", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  saveRDS("stuff", file = "some_file")
  targ <- "DRAKE_TEST_target"
  my_plan <- data.frame(
    target = targ,
    command = "my_function(file_in(\"some_file\"))"
  )
  scenario <- get_testing_scenario()
  parallelism <- scenario$parallelism
  jobs <- scenario$jobs
  envir <- eval(parse(text = scenario$envir))
  if (targ %in% ls(envir)) {
    rm(list = targ, envir = envir)
  }
  envir$my_function <- function(x) {
    x
  }
  cache <- new_cache(path = "cache1", hash_algorithm = "murmur32")
  make(
    my_plan,
    cache = cache,
    envir = envir,
    verbose = 0L,
    parallelism = parallelism,
    jobs = jobs,
    session_info = FALSE
  )
  con <- drake_config(
    my_plan,
    cache = cache,
    envir = envir,
    verbose = 0L,
    parallelism = parallelism,
    jobs = jobs,
    session_info = FALSE
  )

  o1 <- outdated_impl(con)

  expect_equal(o1, character(0))
  expect_equal(
    cache$hash_algorithm,
    "murmur32"
  )

  cache2 <- new_cache(
    path = "my_new_cache",
    hash_algorithm = "crc32"
  )
  con2 <- con
  con2$cache <- cache2
  o2 <- outdated_impl(con2)
  make(
    my_plan,
    cache = cache2,
    envir = envir,
    verbose = 0L,
    parallelism = parallelism,
    jobs = jobs,
    session_info = FALSE
  )
  con2 <- drake_config(
    my_plan,
    cache = cache2,
    envir = envir,
    verbose = 0L,
    parallelism = parallelism,
    jobs = jobs,
    session_info = FALSE
  )
  o3 <- outdated_impl(con2)
  expect_equal(o2, targ)
  expect_equal(o3, character(0))
  expect_equal(
    cache2$hash_algorithm,
    "crc32"
  )
  expect_false(file.exists(".drake"))
  expect_true(file.exists("cache1"))
  expect_true(file.exists("my_new_cache"))
  expect_true(grepl("my_new_cache", con2$cache$path, fixed = TRUE))
  expect_true(grepl("my_new_cache", cache2$path, fixed = TRUE))
})

test_with_dir("storr_environment is usable", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  x <- decorate_storr(storr_environment(hash_algorithm = "murmur32"))
  expect_false(file.exists(default_cache_path()))
  expect_equal(x$hash_algorithm, "murmur32")
  expect_error(drake_get_session_info(cache = x))
  pln <- drake_plan(y = 1)
  make(pln, cache = x, verbose = 0L, session_info = FALSE)
  config <- drake_config(
    pln, cache = x, verbose = 0L, session_info = FALSE)
  expect_equal(cached(cache = x), "y")
  cached_data <- file.path(default_cache_path(), "data")
  expect_false(file.exists(cached_data))
  expect_equal(outdated_impl(config), character(0))
  expect_false(file.exists(cached_data))
})

test_with_dir("arbitrary storr in-memory cache", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("lubridate")
  expect_false(file.exists(default_cache_path()))
  parallelism <- "loop"
  jobs <- 1
  envir <- eval(parse(text = get_testing_scenario()$envir))
  cache <- storr::storr_environment(hash_algorithm = "murmur32")
  load_mtcars_example(envir = envir)
  my_plan <- envir$my_plan
  my_plan <- my_plan[my_plan$target != "report", ]
  make(
    my_plan,
    envir = envir,
    cache = cache,
    parallelism = parallelism,
    jobs = jobs,
    verbose = 0L,
    session_info = TRUE
  )
  con <- drake_config(
    my_plan,
    envir = envir,
    cache = cache,
    parallelism = parallelism,
    jobs = jobs,
    verbose = 0L,
    session_info = TRUE
  )
  envir$reg2 <- function(d) {
    d$x3 <- d$x ^ 3
    lm(y ~ x3, data = d)
  }
  cached_data <- file.path(default_cache_path(), "data")
  expect_false(file.exists(cached_data))
  expect_equal(con$cache$hash_algorithm, "murmur32")

  targets <- my_plan$target
  expect_true(all(targets %in% cached(cache = cache)))
  expect_false(file.exists(cached_data))

  expect_true(is.list(drake_get_session_info(cache = cache)))
  expect_false(file.exists(cached_data))

  imp <- setdiff(
    cached(cache = cache, targets_only = FALSE),
    cached(cache = cache, targets_only = TRUE)
  )
  expect_true(length(imp) > 0)
  expect_false(file.exists(cached_data))

  expect_true(length(cached(cache = cache)) > 0)
  expect_false(file.exists(cached_data))

  expect_true(nrow(build_times(cache = cache)) > 0)
  expect_false(file.exists(cached_data))

  o1 <- outdated_impl(con)
  expect_equal(length(o1), 6)
  expect_false(file.exists(cached_data))

  p1 <- drake_progress(verbose = 0L)
  unlink(default_cache_path(), recursive = TRUE)
  p2 <- drake_progress(cache = cache, verbose = 0L)
  expect_true(nrow(p2) > nrow(p1))
  expect_false(file.exists(cached_data))

  expect_error(readd(small, verbose = 0L))
  expect_true(is.data.frame(readd(small, cache = cache, verbose = 0L)))
  expect_false(file.exists(cached_data))

  expect_error(loadd(large, verbose = 0L))
  expect_silent(loadd(large, cache = cache, verbose = 0L))
  expect_true(nrow(large) > 0)
  rm(large)
  expect_false(file.exists(cached_data))
})

test_with_dir("clean a nonexistent cache", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  clean(list = "no_cache")
  expect_false(file.exists(default_cache_path()))
})

test_with_dir("make() from inside the cache", {
  skip_on_cran()
  cache <- storr::storr_rds(getwd())
  plan <- drake_plan(x = 1)
  expect_error(
    make(plan, cache = cache),
    regexp = "from inside the cache"
  )
})

test_with_dir("cache log files, gc, and make()", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  x <- drake_plan(a = 1)
  make(x, session_info = FALSE, garbage_collection = TRUE)
  expect_false(file.exists("drake_cache.csv"))
  make(x, session_info = FALSE)
  expect_false(file.exists("drake_cache.csv"))
  make(x, session_info = FALSE, cache_log_file = TRUE)
  expect_true(file.exists("drake_cache.csv"))
  make(x, session_info = FALSE, cache_log_file = "my.log")
  expect_true(file.exists("my.log"))
})

test_with_dir("try_build() does not need to access cache", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  config <- drake_config(drake_plan(x = 1))
  meta <- drake_meta_(target = "x", config = config)
  config$cache <- config$settings$cache_log_file <- NULL
  build <- try_build(target = "x", meta = meta, config = config)
  expect_equal(1, build$value)
  expect_error(drake_build_impl(target = "x", config = config))
})

test_with_dir("drake_running()", {
  skip_on_cran()
  plan <- drake_plan(a = 1)
  cache <- storr::storr_environment()
  make(plan, session_info = FALSE, cache = cache)
  expect_equal(drake_running(cache = cache), character(0))
  config <- drake_config(plan, cache = cache, log_progress = TRUE)
  config$running_make <- TRUE
  set_progress(
    target = "a",
    value = "running",
    config = config
  )
  expect_equal(drake_running(cache = cache), "a")
})

test_with_dir("need a storr for a decorated storr", {
  skip_on_cran()
  expect_error(decorate_storr(123), regexp = "not a storr")
})

test_with_dir("dir_create()", {
  skip_on_cran()
  x <- tempfile()
  dir_create(x)
  expect_true(dir.exists(x))
  x <- tempfile()
  file.create(x)
  expect_error(dir_create(x), regexp = "cannot create directory")
})

test_with_dir("which_clean() (#1014)", {
  skip_on_cran()
  cache <- storr::storr_environment()
  expect_equal(which_clean(cache = cache), character(0))
  plan <- drake_plan(x = 1, y = 2, z = 3)
  cache <- storr::storr_environment()
  make(plan, cache = cache, session_info = FALSE, history = FALSE)
  expect_equal(sort(c("x", "y", "z")), sort(cached(cache = cache)))
  expect_equal(sort(which_clean(x, y, cache = cache)), sort(c("x", "y")))
  clean(x, y, cache = cache)       # Invalidates targets x and y.
  expect_equal(cached(cache = cache), "z")
})

test_with_dir("ignore storrs (#1071)", {
  skip_on_cran()
  cache <- new_cache(tempfile())
  cache$set("x", "val")
  plan <- drake_plan(x = c(cache$get("x"), "target"))
  make(plan)
  expect_equal(readd(x), c("val", "target"))
  expect_equal(cache$get("x"), "val")
  expect_equal(readd(cache), "storr")
})

test_with_dir("cache locking (#1081)", {
  skip_on_cran()
  skip_if_not_installed("visNetwork")
  plan <- drake_plan(x = 1)
  config <- drake_config(plan)
  config$cache$lock()
  expect_error(make(plan), regexp = "locked")
  expect_error(outdated_impl(config), regexp = "locked")
  expect_error(recoverable_impl(config), regexp = "locked")
  expect_error(vis_drake_graph_impl(config), regexp = "locked")
  expect_error(drake_gc(), regexp = "locked")
  expect_error(clean(), regexp = "locked")
  expect_error(rescue_cache(), regexp = "locked")
  outdated_impl(config, make_imports = FALSE)
  recoverable_impl(config, make_imports = FALSE)
  g <- vis_drake_graph_impl(config, make_imports = FALSE)
  config$cache$unlock()
  outdated_impl(config)
  recoverable_impl(config)
  g <- vis_drake_graph_impl(config)
  config$cache$lock()
  expect_error(config$cache$lock(), regexp = "locked")
  replicate(4, config$cache$unlock())
  rescue_cache()
  drake_gc()
  clean()
})

test_with_dir("suppress cache locking (#1081)", {
  skip_on_cran()
  skip_if_not_installed("visNetwork")
  plan <- drake_plan(x = 1)
  config <- drake_config(plan, lock_cache = FALSE)
  config$cache$lock()
  make(plan, lock_cache = FALSE)
  expect_equal(justbuilt(config), "x")
  expect_equal(outdated_impl(config), character(0))
  expect_equal(recoverable_impl(config), character(0))
})

test_with_dir("drake_done() (#1205)", {
  skip_on_cran()
  plan <- drake_plan(x = 1, y = x)
  make(plan)
  expect_equal(sort(drake_done()), sort(c("x", "y")))
})

test_with_dir("drake_cancelled() (#1205)", {
  skip_on_cran()
  plan <- drake_plan(x = 1, y = cancel_if(x > 0))
  make(plan)
  expect_equal(drake_cancelled(), "y")
})

test_with_dir("cached_(un)planned() with custom cache location (#1268)", {
  skip_on_cran()
  path_cache <- tempfile()
  test_cache <- new_cache(path_cache)
  plan <- drake_plan(w = 1)
  make(plan, cache = test_cache)
  expect_equal(cached_planned(plan, cache = test_cache), "w")
  expect_equal(cached_unplanned(plan, cache = test_cache), character(0))
  plan <- drake_plan(x = 2)
  expect_equal(cached_planned(plan, cache = test_cache), character(0))
  expect_equal(cached_unplanned(plan, cache = test_cache), "w")
  make(plan, cache = test_cache)
  expect_equal(cached_planned(plan, cache = test_cache), "x")
  expect_equal(cached_unplanned(plan, cache = test_cache), "w")
  expect_equal(sort(cached(cache = test_cache)), sort(c("w", "x")))
  clean(list = cached_unplanned(plan, cache = test_cache), cache = test_cache)
  expect_equal(cached(cache = test_cache), "x")
})
