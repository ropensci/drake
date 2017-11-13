drake_context("cache")

test_with_dir("clean() works if there is no cache already", {
  clean(list = "no_cache")
  expect_false(file.exists(default_cache_path()))
})

test_with_dir("corrupt cache", {
  x <- workplan(a = 1)
  make(x, verbose = FALSE)
  path <- file.path(default_cache_path(), "config", "hash_algorithm")
  expect_true(file.exists(path))
  unlink(path)
  expect_false(file.exists(path))
  expect_warning(expect_error(make(x, verbose = FALSE)))
})

test_with_dir("try to find a non-existent project", {
  expect_equal(find_cache(), NULL)
  expect_equal(find_project(), NULL)
  expect_error(loadd(list = "nothing", search = FALSE))
  expect_error(tmp <- read_drake_config(search = FALSE))
  expect_error(tmp <- drake_session(search = FALSE))
})

test_with_dir("try to rescue non-existent stuff", {
  expect_null(rescue_cache())
  cache <- storr_rds("dummy_cache")
  expect_silent(rescue_del(key = "no_key", cache = cache, namespace = "none"))
})

test_with_dir("cache functions work", {
  # May have been loaded in a globalenv() testing scenario
  remove_these <- intersect(ls(envir = globalenv()), c("h", "j"))
  rm(list = remove_these, envir = globalenv())

  cache_dir <- basename(default_cache_path())
  first_wd <- getwd()
  scratch <- file.path(first_wd, "scratch")
  if (!file.exists(scratch)){
    dir.create(scratch) # Will move up a level later.
  }
  setwd(scratch)
  owd <- getwd()
  expect_equal(character(0), cached(search = FALSE), imported(search = FALSE),
    built(search = FALSE))
  expect_equal(nrow(build_times(search = FALSE)), 0)
  expect_equal(progress(search = FALSE), character(0))
  expect_equal(in_progress(search = FALSE), character(0))
  expect_error(readd(search = FALSE))
  config <- dbug()
  using_global <- identical(config$envir, globalenv())
  if (using_global){
    envir <- globalenv()
  } else {
    envir <- environment()
  }

  testrun(config)

  # drake_gc() should not remove any important targets/imports.
  x <- cached()
  expect_true(length(x) > 0)
  drake_gc()
  y <- cached()
  expect_equal(sort(x), sort(y))

  # targets
  all <- sort(c("'input.rds'",
    "'intermediatefile.rds'", "a",
    "b", "c", "combined", "f", "final", "g", "h", "i", "j",
    "myinput", "nextone", "readRDS", "saveRDS", "yourinput"))
  imports <- sort(c("'input.rds'",
    "a", "b", "c", "f", "g",
    "h", "i", "j", "readRDS", "saveRDS"))
  builds <- setdiff(all, imports)

  # build_times
  x <- config$cache
  bt <- build_times(search = FALSE, targets_only = FALSE)
  expect_equal(sort(x$list(namespace = "build_times")), sort(cached()))
  expect_equal(sort(bt$item), all)
  expect_length(bt, 5) # 5 columns
  n1 <- nrow(bt)
  n2 <- nrow(build_times(search = FALSE, targets_only = TRUE))
  expect_true(n1 > n2 & n2 > 0)

  # find stuff in current directory session, progress
  expect_true(is.list(drake_session(search = FALSE)))
  expect_true(all(progress(search = FALSE) == "finished"))
  expect_equal(in_progress(search = FALSE), character(0))
  expect_warning(tmp <- progress(imported_files_only = TRUE))
  expect_equal(sort(names(progress(search = FALSE))), all)
  expect_equal(
    sort(names(progress(search = FALSE, no_imported_objects = TRUE))),
    sort(c("'input.rds'", builds)))
  expect_equal(progress(bla, f, list = c("h", "final"), search = FALSE),
    c(bla = "not built or imported", f = "finished", h = "finished",
      final = "finished"))

  # config
  newconfig <- read_drake_config(search = FALSE)
  expect_equal(newconfig$short_hash_algo, default_short_hash_algo())
  expect_equal(newconfig$long_hash_algo, default_long_hash_algo())
  expect_true(is.list(newconfig) & length(newconfig) > 1)
  expect_equal(read_plan(search = FALSE), config$plan)
  expect_equal(class(read_drake_graph(search = FALSE)), "igraph")

  # imported , built, cached, diagnose, rescue
  expect_equal(diagnose(search = FALSE), character(0))
  expect_equal(imported(files_only = FALSE, search = FALSE),
    imports)
  expect_equal(imported(files_only = TRUE, search = FALSE),
    "'input.rds'")
  expect_equal(sort(built(search = FALSE)), sort(config$plan$target))
  twopiece <- sort(c(built(search = FALSE), imported(search = FALSE,
    files_only = FALSE)))
  expect_equal(sort(cached(search = FALSE)), sort(all), twopiece)
  expect_equal(sort(cached(search = FALSE, no_imported_objects = TRUE)),
    sort(c("'input.rds'", builds)))
  expect_true(all(cached(search = FALSE, list = all)))
  expect_equal(
    length(cached(search = FALSE, i, list = imported(files_only = FALSE))),
    length(imported(files_only = FALSE)))
  expect_equal(sort(cached(i, bla, list = c("final", "run"),
    search = FALSE)), sort(c(i = TRUE, bla = FALSE, final = TRUE,
    run = FALSE)))
  expect_true(inherits(rescue_cache(search = FALSE), "storr"))
  expect_equal(sort(cached(search = FALSE)), sort(all), twopiece)

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

  # test loadd imported_only and loadd() everything
  loadd(imported_only = TRUE)
  expect_true(all(imported(search = FALSE) %in% ls()))
  loadd(search = FALSE)
  expect_true(all(config$cache$list() %in% ls()))
  rm(list = intersect(all, ls()))

  # search from a different directory
  if (!file.exists("searchfrom")) {
    dir.create("searchfrom")
    dir.create(file.path("searchfrom", "here"))
  }
  setwd("..")
  expect_equal(getwd(), first_wd)
  s <- normalizePath(file.path(scratch, "searchfrom", "here"))

  # progress, session
  expect_true(is.list(drake_session(search = TRUE, path = s)))
  expect_equal(sort(names(progress(search = TRUE, path = s))),
    sort(all))
  expect_equal(sort(names(progress(no_imported_objects = TRUE,
    search = TRUE, path = s))), sort(c("'input.rds'", builds)))
  expect_equal(sort(progress(search = TRUE, path = s, bla, f,
    list = c("h", "final"))), sort(c(bla = "not built or imported",
    f = "finished", h = "finished", final = "finished")))
  expect_equal(in_progress(search = TRUE, path = s), character(0))

  # imported, built, cached, diagnose
  expect_equal(diagnose(search = TRUE), character(0))
  expect_equal(sort(imported(files_only = FALSE, search = TRUE,
    path = s)), sort(imports))
  expect_equal(imported(files_only = TRUE, search = TRUE, path = s),
    "'input.rds'")
  expect_equal(sort(built(search = TRUE, path = s)),
    sort(config$plan$target))
  twopiece <- sort(c(built(path = s, search = TRUE),
    imported(files_only = FALSE,
    path = s, search = TRUE)))
  expect_equal(sort(cached(path = s, search = TRUE)), sort(all),
    twopiece)
  expect_equal(sort(cached(no_imported_objects = TRUE, path = s,
    search = T)), sort(c("'input.rds'", builds)))
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
  expect_equal(read_plan(search = TRUE, path = s), config$plan)
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

  # Read the graph
  pdf(NULL)
  tmp <- dbug()
  tmp <- read_drake_graph(search = TRUE, path = s)
  tmp <- capture.output(dev.off())
  unlink("Rplots.pdf", force = TRUE)

  setwd(scratch)
  pdf(NULL)
  tmp <- read_drake_graph(search = FALSE)
  tmp <- capture.output(dev.off())
  unlink("Rplots.pdf", force = TRUE)
  pdf(NULL)
  null_graph()
  tmp <- capture.output(dev.off())
  unlink("Rplots.pdf", force = TRUE)
  setwd("..")

  # clean using search = TRUE or FALSE
  expect_true(all(all %in% cached(path = s, search = T)))
  clean(final, path = s, search = TRUE, jobs = 2,
    garbage_collection = TRUE)
  expect_true(all(setdiff(all, "final") %in% cached(path = s,
    search = T)))
  drake_gc(path = s, search = T)
  clean(path = s, search = TRUE, garbage_collection = FALSE)
  expect_equal(cached(path = s, search = T), character(0))
  where <- file.path(scratch, cache_dir)
  expect_true(file.exists(where))
  clean(path = s, search = FALSE, destroy = TRUE)
  expect_true(file.exists(where))
  clean(path = s, search = TRUE, destroy = TRUE)
  expect_false(file.exists(where))
  expect_silent(drake_gc()) # Cache does not exist

  setwd(scratch)
  unlink("searchfrom", recursive = TRUE, force = TRUE)
})
