drake_context("cache")

test_with_dir("clean() removes the correct files", {
  skip_if_not_installed("knitr")
  cache <- storr::storr_environment()
  writeLines("123", "a.txt")
  writeLines("123", "b.txt")
  plan <- drake_plan(
    a = file_in("a.txt"),
    b = knitr_in("b.txt"),
    d = writeLines("123", file_out("d.rds"))
  )
  config <- drake_config(plan, session_info = FALSE, skip_targets = TRUE)
  make(config = config)
  clean()
  expect_true(file.exists("a.txt"))
  expect_true(file.exists("b.txt"))
  expect_false(file.exists("d.txt"))
})

test_with_dir("drake_version", {
  cache <- storr::storr_environment()
  expect_equal(
    get_cache_version(cache),
    as.character(utils::packageVersion("drake"))
  )
  make(drake_plan(x = 1), session_info = FALSE)
  con <- drake_config(drake_plan(x = 1), session_info = FALSE)
  expect_true(is.character(get_cache_version(con$cache)))
  clean()
  make(drake_plan(x = 1), session_info = TRUE)
  con <- drake_config(drake_plan(x = 1), session_info = TRUE)
  expect_true(is.character(get_cache_version(con$cache)))
  con$cache$clear(namespace = "session")
  expect_equal(
    get_cache_version(con$cache),
    as.character(utils::packageVersion("drake"))
  )
})

test_with_dir("dependency profile", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("knitr")
  b <- 1
  make(drake_plan(a = b), session_info = FALSE)
  config <- drake_config(drake_plan(a = b), session_info = FALSE)
  expect_error(
    deps_profile(target = missing, config = config),
    regexp = "no recorded metadata"
  )
  expect_false(any(deps_profile(target = a, config = config)$changed))
  b <- 2
  expect_false(any(deps_profile(target = a, config = config)$changed))
  config$skip_targets <- TRUE
  make(config = config)
  dp <- deps_profile(target = a, config = config)
  expect_true(as.logical(dp[dp$hash == "depend", "changed"]))
  expect_equal(sum(dp$changed), 1)
  config$plan$command <- "b + c"
  config$layout <- create_drake_layout(
    plan = config$plan,
    envir = config$envir,
    cache = config$cache
  )$layout
  dp <- deps_profile(target = a, config = config)
  expect_true(as.logical(dp[dp$hash == "command", "changed"]))
  expect_equal(sum(dp$changed), 2)
  load_mtcars_example()
  config <- drake_config(
    my_plan,
    cache = storr::storr_environment(),
    skip_targets = TRUE,
    session_info = FALSE
  )
  make(config = config)
  out <- deps_profile(
    file_store("report.Rmd"),
    character_only = TRUE,
    config
  )
  expect_equal(nrow(out), 4)
})

test_with_dir("Missing cache", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  s <- storr::storr_rds("s")
  unlink(s$driver$path, recursive = TRUE)
  expect_error(assert_cache(s), regexp = "drake cache missing")
  expect_equal(cached(), character(0))
})

test_with_dir("broken or incomplete cache", {
  skip_on_cran()
  make(drake_plan(x = 1), session_info = FALSE)
  unlink(file.path(".drake", "config"), recursive = TRUE)
  expect_error(
    suppressWarnings(make(drake_plan(x = 1), session_info = FALSE)),
    regexp = "failed to get the storr"
  )
})

test_with_dir("Cache namespaces", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  y <- target_namespaces_()
  z <- cleaned_namespaces_()
  expect_true(all(z %in% y))
  expect_false(all(y %in% z))
})

test_with_dir("safe_get", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  con <- list(cache = storr::storr_environment())
  expect_true(is.na(safe_get(key = "x", namespace = "y", config = con)))
})

test_with_dir("clean() works if there is no cache already", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  clean(list = "no_cache")
  expect_false(file.exists(default_cache_path()))
})

test_with_dir("can exclude bad targets from loadd()", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  plan <- drake_plan(a = TRUE)
  make(plan)
  e <- new.env(parent = emptyenv())
  loadd(a, b, lazy = FALSE, envir = e)
  expect_true(exists("a", envir = e, inherits = FALSE))
  expect_equal(e$a, TRUE)
  expect_false(exists("b", envir = e, inherits = FALSE))
})

test_with_dir("bad/corrupt caches, no progress, no seed", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_null(drake_fetch_rds("sldkfjlke"))
  expect_warning(new_cache(type = "nope"))
  x <- drake_plan(a = 1)
  make(x, verbose = FALSE, session_info = FALSE, log_progress = FALSE)
  expect_equal(get_cache()$list(namespace = "progress"), character(0))
  clean()
  make(x, verbose = FALSE, session_info = FALSE, log_progress = TRUE)
  expect_equal(get_cache()$list(namespace = "progress"), "a")
  path <- file.path(default_cache_path(), "config", "hash_algorithm")
  expect_true(file.exists(path))
  unlink(path)
  expect_false(file.exists(path))
  expect_warning(expect_error(
    make(x, verbose = FALSE, session_info = FALSE)))
  expect_error(
    read_drake_seed(cache = storr::storr_environment()),
    regexp = "random seed not found"
  )
})

test_with_dir("non-existent caches", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_equal(0, nrow(drake_cache_log()))
  expect_equal(find_cache(), NULL)
  expect_error(loadd(list = "nothing", search = FALSE))
  expect_error(tmp <- read_drake_seed(search = FALSE))
  expect_error(tmp <- drake_get_session_info(search = FALSE))
  expect_error(tmp <- drake_set_session_info(search = FALSE))
  dummy <- new_cache()
})

test_with_dir("drake_gc() and mangled keys", {
  cache <- storr::storr_rds(tempfile(), mangle_key = TRUE)
  cache$set("a", 1)
  expect_silent(tmp <- drake_gc(cache = cache))
})

test_with_dir("try to rescue non-existent stuff", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_null(rescue_cache())
  cache <- storr_rds("dummy_cache")
  expect_silent(rescue_del(key = "no_key", cache = cache, namespace = "none"))
})

test_with_dir("subspaces", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  lst <- list_subspace(
    subspace = "y", namespace = "x", cache = NULL, jobs = 1)
  expect_equal(lst, character(0))
  x <- storr::storr_rds("test")
  lst <- list_subspace(
    subspace = "y", namespace = "x", cache = x, jobs = 1)
  expect_equal(lst, character(0))
  set_in_subspaces(
    key = "a",
    values = 1,
    namespace = "x",
    subspaces = "y",
    cache = x
  )
  set_in_subspaces(
    key = "b",
    values = 2,
    namespace = "x",
    subspaces = "y",
    cache = x
  )
  lst <- list_subspace(
    subspace = "y", namespace = "x", cache = x, jobs = 1)
  expect_equal(sort(lst), c("a", "b"))
})

test_with_dir("get_cache() can search", {
  dir.create(file.path("w"))
  dir.create(file.path("w", "x"))
  dir.create(file.path("w", "x", "y"))
  dir.create(file.path("w", "x", "y", "z"))
  tmp <- storr::storr_rds(file.path("w", "x", ".drake"), mangle_key = TRUE)
  cache <- get_cache(file.path("w", "x", "y", "z"))
  expect_true(inherits(cache, "storr"))
  cache <- get_cache(file.path("w", "x", ".drake"))
  expect_true(inherits(cache, "storr"))
  cache <- get_cache(file.path("w", "x", ".drake", "keys"))
  expect_true(inherits(cache, "storr"))
  cache <- get_cache(file.path("w", "x"), search = FALSE)
  expect_true(inherits(cache, "storr"))
  expect_null(get_cache(file.path("w", "x", "y", "z"), search = FALSE))
})

test_with_dir("cache functions work from various working directories", {
  skip_if_not_installed("lubridate")
  # May have been loaded in a globalenv() testing scenario # nolint
  remove_these <- intersect(ls(envir = globalenv()), c("h", "j"))
  rm(list = remove_these, envir = globalenv())

  cache_dir <- basename(default_cache_path())
  first_wd <- getwd()
  scratch <- file.path(first_wd, "scratch")
  if (!file.exists(scratch)) {
    dir.create(scratch) # Will move up a level later.
  }
  setwd(scratch) # nolint
  owd <- getwd()
  expect_equal(nrow(build_times(search = FALSE)), 0)
  expect_equal(nrow(progress(search = FALSE)), 0)
  expect_false(any("in progress" %in% progress(search = FALSE)))
  expect_error(readd(search = FALSE))
  config <- dbug()
  using_global <- identical(config$envir, globalenv())
  if (using_global) {
    envir <- globalenv()
  } else {
    envir <- environment()
  }

  config$session_info <- TRUE
  testrun(config)

  # drake_cache_log() # nolint
  all_hashes <- drake_cache_log()
  some_hashes <- drake_cache_log(targets_only = TRUE)
  expect_equal(ncol(all_hashes), ncol(some_hashes))
  n_a <- nrow(all_hashes)
  n_s <- nrow(some_hashes)
  expect_true(n_a > n_s && n_s > 0)
  expect_false(file.exists("log.txt"))
  drake_cache_log_file(file = "log.txt")
  expect_true(file.exists("log.txt"))

  # drake_gc() should not remove any important targets/imports.
  x <- cached()
  expect_true(length(x) > 0)
  drake_gc()
  y <- cached()
  expect_equal(sort(x), sort(y))
  expect_equal(outdated(config), character(0))

  # targets and imports
  imports <- sort(c(encode_path("input.rds"),
                    "a", "b", "c", "f", "g",
                    "h", "i", "j"))
  builds <- sort(config$plan$target)
  out_files <- encode_path("intermediatefile.rds")
  all <- sort(c(builds, imports, out_files))

  # build_times
  x <- config$cache
  bt <- build_times(search = FALSE)
  expect_equal(
    sort(display_keys(x$list(namespace = "meta"))),
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
  expect_equal(read_drake_seed(search = FALSE), config$seed)
  expect_true(is.list(drake_get_session_info(search = FALSE)))
  expect_true(all(progress(search = FALSE)$progress == "done"))
  expect_false(any("running" %in% progress(search = FALSE)))
  expect_equal(sort(progress(search = FALSE)$target), sort(config$plan$target))
  exp <- weak_tibble(target = "final", progress = "done")
  expect_equal(progress(final, search = FALSE), exp)
  expect_equal(progress(list = "final", search = FALSE), exp)

  # cached, diagnose, rescue
  expect_true(length(diagnose(search = FALSE)) > length(config$plan$target))
  expect_error(diagnose("xyz", cache = config$cache), regexp = "diagnostic")
  expect_equal(
    sort(cached(search = FALSE, targets_only = TRUE)),
    sort(builds)
  )
  expect_true(
    inherits(rescue_cache(search = FALSE, targets = "final"), "storr"))
  expect_true(inherits(
    rescue_cache(search = FALSE, garbage_collection = FALSE), "storr"))
  expect_true(inherits(
    rescue_cache(search = FALSE, garbage_collection = TRUE), "storr"))

  # find your project
  expect_equal(find_cache(), file.path(getwd(), cache_dir))
  expect_true(is.numeric(readd(a, search = FALSE)))

  # load and read stuff
  list <- intersect(
    setdiff(cached(targets_only = FALSE), cached(targets_only = TRUE)),
    ls(envir = envir)
  )
  rm(list = list, envir = envir)
  expect_error(h(1))
  expect_true(is.numeric(readd(final, search = FALSE)))

  loadd(yourinput, nextone, jobs = 2, search = FALSE, envir = envir)
  expect_true(is.numeric(envir[["yourinput"]]))
  expect_true(is.numeric(envir[["nextone"]]))
  rm(yourinput, nextone, envir = envir)

  # test loadd loadd() everything
  e <- new.env()
  loadd(search = FALSE, envir = e)
  expect_equal(sort(config$plan$target), sort(ls(envir = e)))

  # search from a different directory
  if (!file.exists("searchfrom")) {
    dir.create("searchfrom")
    dir.create(file.path("searchfrom", "here"))
  }
  setwd("..") # nolint
  expect_equal(getwd(), first_wd)
  s <- normalizePath(file.path(scratch, "searchfrom", "here"))

  # progress, session
  expect_true(is.list(drake_get_session_info(search = TRUE, path = s)))
  prog <- progress(search = TRUE, path = s)
  expect_equal(sort(prog$target), sort(config$plan$target))
  expect_true(all(prog$progress == "done"))
  prog <- progress(nothing, search = TRUE, path = s)
  expect_equal(prog, weak_tibble(target = "nothing", progress = "none"))

  # cached and diagnose
  expect_equal(diagnose(search = TRUE), character(0))
  expect_equal(
    sort(cached(targets_only = TRUE, path = s, search = TRUE)),
    sort(config$plan$target)
  )
  expect_equal(
    length(cached(targets_only = FALSE, path = s, search = TRUE)),
    length(config$cache$list())
  )
  expect_true(inherits(rescue_cache(path = s, search = TRUE), "storr"))

  # find your project
  expect_equal(find_cache(path = s),
               file.path(scratch, cache_dir))

  # load and read stuff
  expect_true(is.numeric(readd(a, path = s, search = TRUE)))

  loadd(yourinput, nextone, jobs = 2, search = TRUE, path = s, envir = envir)
  expect_true(is.numeric(envir[["yourinput"]]))
  expect_true(is.numeric(envir[["nextone"]]))
  rm(yourinput, nextone, envir = envir)

  # load dependencies
  e <- new.env()
  deps <- c("nextone", "yourinput")
  expect_false(any(deps %in% ls(envir = e)))
  loadd(
    combined, deps = TRUE, config = config,
    path = s, search = TRUE, envir = e
  )
  expect_true(all(deps %in% ls(envir = e)))

  # clean using search = TRUE or FALSE
  expect_true(all(config$plan$target %in% cached(path = s, search = TRUE)))
  clean(final, path = s, search = TRUE, jobs = 2,
        garbage_collection = TRUE)
  targs <- setdiff(config$plan$target, "final")
  expect_true(all(targs %in% cached(path = s, search = TRUE)))
  drake_gc(path = s, search = TRUE)

  # Test purging
  prog <- progress(search = TRUE, path = s)
  expect_true("final" %in% prog$target)

  clean(final, path = s, search = TRUE, jobs = 2,
        garbage_collection = TRUE, purge = TRUE)
  prog <- progress(search = TRUE, path = s)
  expect_false("final" %in% prog$target)

  # More cleaning checks
  clean(path = s, search = TRUE, garbage_collection = FALSE)
  expect_equal(cached(path = s, search = TRUE), character(0))
  where <- file.path(scratch, cache_dir)
  expect_true(file.exists(where))
  clean(path = s, search = FALSE, destroy = TRUE)
  expect_true(file.exists(where))
  clean(path = s, search = TRUE, destroy = TRUE)
  expect_false(file.exists(where))
  expect_silent(drake_gc()) # Cache does not exist

  setwd(scratch) # nolint
  unlink("searchfrom", recursive = TRUE, force = TRUE)
})

test_with_dir("memo_expr() works without a cache", {
  x <- "x"
  expect_equal(memo_expr(x, cache = NULL), x)
})

test_with_dir("master caching, environment caches and parallelism", {
  skip_on_cran()
  skip_if_not_installed("future")
  load_mtcars_example()
  future::plan(future::multiprocess)
  cache <- storr::storr_environment() # not thread-safe
  make(
    my_plan,
    cache = cache,
    caching = "master",
    parallelism = "future",
    jobs = 2
  )
  config <- drake_config(
    my_plan,
    cache = cache,
    caching = "master",
    parallelism = "future",
    jobs = 2
  )
  expect_true("report" %in% justbuilt(config))
})

test_with_dir("run make() from subdir", {
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
    make(plan, cache = y)
    new_cache(".drake")
    make(plan)
    make(plan, cache = storr::storr_environment())
  })
})

test_with_dir("loadd() does not load imports", {
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
    loadd(f, envir = e, cache = cache, verbose = TRUE),
    regexp = "No targets to load"
  )
})

test_with_dir("can filter progress", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  plan <- drake_plan(a = TRUE, b = TRUE, c = stop())
  expect_error(make(plan))

  out <- progress(a, b, c, d)
  exp <- weak_tibble(
    target = c("a", "b", "c", "d"),
    progress = c("done", "done", "failed", "none")
  )
  expect_equivalent(out, exp)

  exp1 <- weak_tibble(
    target = c("a", "b", "c"),
    progress = c("done", "done", "failed")
  )
  exp2 <- weak_tibble(
    target = c("a", "b"),
    progress = c("done", "done")
  )
  exp3 <- weak_tibble(
    target = "c",
    progress = "failed"
  )

  expect_equivalent(progress(progress = c("done", "failed")), exp1)
  expect_equivalent(progress(progress = "done"), exp2)
  expect_equivalent(progress(progress = "failed"), exp3)

  expect_error(
    progress(progress = "stuck"),
    "should be one of")
})
