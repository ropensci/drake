drake_context("cache")

test_with_dir("clean() removes the correct files", {
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

test_with_dir("empty read_drake_plan()", {
  expect_equal(
    read_drake_plan(cache = storr::storr_environment()),
    drake_plan()
  )
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
  b <- 1
  make(drake_plan(a = b), session_info = FALSE)
  config <- drake_config(drake_plan(a = b), session_info = FALSE)
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
  expect_true(as.logical(dp[dp$hash == "depend", "changed"]))
  expect_equal(sum(dp$changed), 1)
  config$plan$command <- "b + c"
  config$layout <- create_drake_layout(
    plan = config$plan,
    envir = config$envir,
    cache = config$cache
  )$layout
  dp <- dependency_profile(target = a, config = config)
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
  out <- dependency_profile(
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
})

test_with_dir("broken cache", {
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
  x <- cache_namespaces()
  y <- target_namespaces()
  z <- cleaned_namespaces()
  expect_true(all(y %in% x))
  expect_true(all(z %in% y))
  expect_false(all(x %in% y))
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
  loadd(a, b, lazy = FALSE)
  expect_equal(a, TRUE)
  expect_error(b)
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
  expect_equal(find_project(), NULL)
  expect_error(loadd(list = "nothing", search = FALSE))
  expect_error(tmp <- read_drake_config(search = FALSE))
  expect_error(tmp <- read_drake_plan(search = FALSE))
  expect_error(tmp <- read_drake_graph(search = FALSE))
  expect_error(tmp <- read_drake_seed(search = FALSE))
  expect_error(tmp <- drake_get_session_info(search = FALSE))
  expect_error(tmp <- drake_set_session_info(search = FALSE))
  dummy <- new_cache()
  expect_silent(read_drake_graph(cache = dummy))
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

test_with_dir("cache functions work", {
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
  expect_equal(character(0), cached(search = FALSE), imported(search = FALSE),
    built(search = FALSE))
  expect_equal(nrow(build_times(search = FALSE)), 0)
  expect_equal(progress(search = FALSE), character(0))
  expect_equal(in_progress(search = FALSE), character(0))
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
    sort(cached())
  )
  expect_equal(sort(bt$target), sort(builds))
  expect_length(bt, 4) # 4 columns
  n1 <- nrow(bt)

  # find stuff in current directory session, progress
  expect_true(is.list(drake_get_session_info(search = FALSE)))
  expect_true(all(progress(search = FALSE) == "finished"))
  expect_equal(in_progress(search = FALSE), character(0))
  expect_equal(sort(names(progress(search = FALSE))), all)
  expect_equal(
    sort(names(progress(search = FALSE, no_imported_objects = TRUE))),
    sort(c(encode_path("input.rds"), out_files, builds)))
  expect_equal(progress(bla, f, list = c("h", "final"), search = FALSE),
    c(bla = "not built or imported", f = "finished", h = "finished",
      final = "finished"))

  # config
  newconfig <- read_drake_config(search = FALSE)
  expect_true(is.list(newconfig) & length(newconfig) > 1)
  expect_equal(config$plan, newconfig$plan)
  expect_equal(config$seed, newconfig$seed)
  expect_equal(read_drake_plan(search = FALSE), config$plan)
  expect_equal(read_drake_seed(search = FALSE), config$seed)
  expect_true(inherits(read_drake_graph(search = FALSE), "igraph"))

  # imported , built, cached, diagnose, rescue
  expect_true(length(diagnose(search = FALSE)) > length(config$plan$target))
  expect_equal(
    sort(imported(files_only = FALSE, search = FALSE)),
    sort(display_keys(imports))
  )
  expect_equal(
    imported(files_only = TRUE, search = FALSE),
    display_keys(encode_path("input.rds"))
  )
  expect_equal(
    sort(built(search = FALSE)),
    sort(display_keys(c(config$plan$target, out_files)))
  )
  twopiece <- sort(c(built(search = FALSE), imported(search = FALSE,
    files_only = FALSE)))
  expect_equal(
    sort(cached(search = FALSE)),
    sort(display_keys(all)),
    sort(display_keys(twopiece))
  )
  expect_equal(
    sort(cached(search = FALSE, no_imported_objects = TRUE)),
    sort(display_keys(c(encode_path("input.rds"), out_files, builds)))
  )
  expect_true(
    is_cached(
      targets = encode_path("input.rds"),
      no_imported_objects = TRUE,
      cache = config$cache, jobs = 1,
      namespace = config$cache$default_namespace
    )
  )
  expect_true(all(cached(search = FALSE, list = all)))
  expect_equal(
    length(cached(search = FALSE, i, list = imported(files_only = FALSE))),
    length(imported(files_only = FALSE)))
  expect_equal(sort(cached(i, bla, list = c("final", "run"),
    search = FALSE)), sort(c(i = TRUE, bla = FALSE, final = TRUE,
    run = FALSE)))
  expect_true(
    inherits(rescue_cache(search = FALSE, targets = "final"), "storr"))
  expect_true(inherits(
    rescue_cache(search = FALSE, garbage_collection = FALSE), "storr"))
  expect_true(inherits(
    rescue_cache(search = FALSE, garbage_collection = TRUE), "storr"))
  expect_equal(
    sort(cached(search = FALSE)),
    sort(display_keys(all)),
    sort(display_keys(twopiece))
  )

  # find your project
  expect_equal(find_project(), getwd())
  expect_equal(find_cache(), file.path(getwd(), cache_dir))
  expect_true(is.numeric(readd(a, search = FALSE)))

  # load and read stuff
  list <- intersect(c(imported(), built()), ls(envir = envir))
  rm(list = list, envir = envir)
  expect_error(h(1))
  expect_true(is.numeric(readd(final, search = FALSE)))
  expect_error(loadd(yourinput, myinput, search = FALSE, imported_only = TRUE))
  loadd(h, i, j, c, jobs = 2, search = FALSE, envir = envir)
  expect_true(is.numeric(h(1)))
  rm(h, i, j, c, envir = envir)
  expect_error(h(1))

  # test loadd imported_only and loadd() everything safe
  e <- new.env()
  loadd(imported_only = TRUE, envir = e)
  should_have_loaded <- setdiff(
    imported(search = FALSE),
    c("readRDS", "saveRDS")
  )
  expect_true(all(should_have_loaded %in% display_keys(ls(envir = e))))
  e <- new.env()
  loadd(search = FALSE, envir = e)
  should_have_loaded <- setdiff(
    config$cache$list(),
    c("readRDS", "saveRDS")
  )
  expect_true(all(should_have_loaded %in% ls(envir = e)))

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
  expect_equal(sort(names(progress(search = TRUE, path = s))),
    sort(all))
  expect_equal(
    sort(
      names(
        progress(no_imported_objects = TRUE, search = TRUE, path = s)
      )
    ),
    sort(c(encode_path("input.rds"), out_files, builds))
  )
  expect_equal(sort(progress(search = TRUE, path = s, bla, f,
    list = c("h", "final"))), sort(c(bla = "not built or imported",
    f = "finished", h = "finished", final = "finished")))
  expect_equal(in_progress(search = TRUE, path = s), character(0))

  # imported, built, cached, diagnose
  expect_equal(diagnose(search = TRUE), character(0))
  expect_equal(
    sort(imported(files_only = FALSE, search = TRUE, path = s)),
    sort(display_keys(imports))
  )
  expect_equal(
    imported(files_only = TRUE, search = TRUE, path = s),
    display_keys(encode_path("input.rds"))
  )
  expect_equal(
    sort(built(search = TRUE, path = s)),
    sort(display_keys(c(config$plan$target, out_files)))
  )
  twopiece <- sort(c(
    built(path = s, search = TRUE),
    imported(files_only = FALSE, path = s, search = TRUE))
  )
  expect_equal(
    sort(cached(path = s, search = TRUE)),
    sort(display_keys(all)),
    sort(display_keys(twopiece))
  )
  expect_equal(
    sort(cached(no_imported_objects = TRUE, path = s, search = T)),
    sort(display_keys(c(encode_path("input.rds"), out_files, builds)))
  )
  expect_true(all(cached(list = all, path = s, search = TRUE)))
  expect_true(inherits(rescue_cache(path = s, search = TRUE), "storr"))
  expect_true(all(cached(list = all, path = s, search = TRUE)))

  # find your project
  expect_equal(find_project(path = s), file.path(scratch))
  expect_equal(find_cache(path = s),
    file.path(scratch, cache_dir))

  # config
  newconfig <- read_drake_config(search = TRUE, path = s)
  expect_true(is.list(newconfig) & length(newconfig) > 1)
  expect_equal(read_drake_plan(search = TRUE, path = s), config$plan)
  expect_equal(class(read_drake_graph(search = TRUE, path = s)),
    "igraph")

  # load and read stuff
  expect_true(is.numeric(readd(a, path = s, search = TRUE)))
  expect_error(h(1))
  expect_error(j(1))
  loadd(h, i, j, c, path = s, search = TRUE, envir = envir)
  expect_true(is.numeric(h(1)))
  rm(h, i, j, c, envir = envir)
  expect_error(h(1))

  # load dependencies
  e <- new.env()
  deps <- c("nextone", "yourinput")
  expect_false(any(deps %in% ls(envir = e)))
  loadd(combined, deps = TRUE, path = s, search = TRUE, envir = e)
  expect_true(all(deps %in% ls(envir = e)))

  # Read the graph
  pdf(NULL)
  tmp <- dbug()
  tmp <- read_drake_graph(search = TRUE, path = s)
  tmp <- capture.output(dev.off())
  unlink("Rplots.pdf", force = TRUE)

  setwd(scratch) # nolint
  pdf(NULL)
  tmp <- read_drake_graph(search = FALSE)
  tmp <- capture.output(dev.off())
  unlink("Rplots.pdf", force = TRUE)
  pdf(NULL)
  tmp <- capture.output(dev.off())
  unlink("Rplots.pdf", force = TRUE)
  setwd("..") # nolint

  # clean using search = TRUE or FALSE
  expect_true(all(display_keys(all) %in% cached(path = s, search = T)))
  clean(final, path = s, search = TRUE, jobs = 2,
    garbage_collection = TRUE)
  expect_true(all(
    sort(display_keys(setdiff(all, "final"))) %in%
      cached(path = s, search = T)))
  drake_gc(path = s, search = T)

  # Test purging
  prog <- progress(search = TRUE, path = s)
  expect_true("final" %in% names(prog))

  clean(final, path = s, search = TRUE, jobs = 2,
    garbage_collection = TRUE, purge = TRUE)
  prog <- progress(search = TRUE, path = s)
  expect_false("final" %in% names(prog))

  # More cleaning checks
  clean(path = s, search = TRUE, garbage_collection = FALSE)
  expect_equal(cached(path = s, search = T), character(0))
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
