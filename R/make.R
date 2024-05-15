#' @title Run your project (build the outdated targets).
#' `r lifecycle::badge("stable")`
#' @description This is the central, most important function
#' of the drake package. It runs all the steps of your
#' workflow in the correct order, skipping any work
#' that is already up to date. Because of how `make()`
#' tracks global functions and objects as dependencies of targets,
#' please restart your R session so the pipeline runs
#' in a clean reproducible environment.
#' @section Interactive mode:
#' In interactive sessions, consider [r_make()], [r_outdated()], etc.
#' rather than [make()], [outdated()], etc. The `r_*()` `drake` functions
#' are more reproducible when the session is interactive.
#' If you do run `make()` interactively, please restart your R session
#' beforehand so your functions and global objects get loaded into
#' a clean reproducible environment. This prevents targets
#' from getting invalidated unexpectedly.
#'
#' A serious drake workflow should be consistent and reliable,
#' ideally with the help of a main R script.
#' This script should begin in a fresh R session,
#' load your packages and functions in a dependable manner,
#' and then run `make()`. Example:
#' `https://github.com/wlandau/drake-examples/tree/main/gsp`.
#' Batch mode, especially within a container, is particularly helpful.
#'
#' Interactive R sessions are still useful,
#' but they easily grow stale.
#' Targets can falsely invalidate if you accidentally change
#' a function or data object in your environment.
#'
#' @section Self-invalidation:
#' It is possible to construct a workflow that tries to invalidate itself.
#' Example:
#' ```r
#' plan <- drake_plan(
#'   x = {
#'     data(mtcars)
#'     mtcars$mpg
#'   },
#'   y = mean(x)
#' )
#' ```
#' Here, because `data()` loads `mtcars` into the global environment,
#' the very act of building `x` changes the dependencies of `x`.
#' In other words, without safeguards, `x` would not be up to date at
#' the end of `make(plan)`.
#' Please try to avoid workflows that modify the global environment.
#' Functions such as `data()` belong in your setup scripts
#' prior to `make()`, not in any functions or commands that get called
#' during `make()` itself.
#'
#' For each target that is still problematic  (e.g.
#' `https://github.com/rstudio/gt/issues/297`)
#' you can safely run the command in its own special `callr::r()` process.
#' Example: `https://github.com/rstudio/gt/issues/297#issuecomment-497778735`. # nolint
#' @section Cache locking:
#' When `make()` runs, it locks the cache so other processes cannot modify it.
#' Same goes for [outdated()], [vis_drake_graph()], and similar functions
#' when `make_imports = TRUE`. This is a safety measure to prevent simultaneous
#' processes from corrupting the cache. If you get an error saying that the
#' cache is locked, either set `make_imports = FALSE` or manually force
#' unlock it with `drake_cache()$unlock()`.
#'
#' @seealso
#'   [drake_plan()],
#'   [drake_config()],
#'   [vis_drake_graph()],
#'   [outdated()]
#' @export
#' @return nothing
#' @inheritParams drake_config
#' @param config Deprecated.
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' config <- drake_config(my_plan)
#' outdated(my_plan) # Which targets need to be (re)built?
#' make(my_plan) # Build what needs to be built.
#' outdated(my_plan) # Everything is up to date.
#' # Change one of your imported function dependencies.
#' reg2 = function(d) {
#'   d$x3 = d$x^3
#'   lm(y ~ x3, data = d)
#' }
#' outdated(my_plan) # Some targets depend on reg2().
#' make(my_plan) # Rebuild just the outdated targets.
#' outdated(my_plan) # Everything is up to date again.
#' if (requireNamespace("visNetwork", quietly = TRUE)) {
#' vis_drake_graph(my_plan) # See how they fit in an interactive graph.
#' make(my_plan, cache_log_file = TRUE) # Write a CSV log file this time.
#' vis_drake_graph(my_plan) # The colors changed in the graph.
#' # Run targets in parallel:
#' # options(clustermq.scheduler = "multicore") # nolint
#' # make(my_plan, parallelism = "clustermq", jobs = 2) # nolint
#' }
#' clean() # Start from scratch next time around.
#' }
#' # Dynamic branching
#' # Get the mean mpg for each cyl in the mtcars dataset.
#' plan <- drake_plan(
#'   raw = mtcars,
#'   group_index = raw$cyl,
#'   munged = target(raw[, c("mpg", "cyl")], dynamic = map(raw)),
#'   mean_mpg_by_cyl = target(
#'     data.frame(mpg = mean(munged$mpg), cyl = munged$cyl[1]),
#'     dynamic = group(munged, .by = group_index)
#'   )
#' )
#' make(plan)
#' readd(mean_mpg_by_cyl)
#' })
#' }
make <- function(
  plan,
  targets = NULL,
  envir = parent.frame(),
  verbose = 1L,
  hook = NULL,
  cache = drake::drake_cache(),
  fetch_cache = NULL,
  parallelism = "loop",
  jobs = 1L,
  jobs_preprocess = 1L,
  packages = rev(.packages()),
  lib_loc = NULL,
  prework = character(0),
  prepend = NULL,
  command = NULL,
  args = NULL,
  recipe_command = NULL,
  log_progress = TRUE,
  skip_targets = FALSE,
  timeout = NULL,
  cpu = Inf,
  elapsed = Inf,
  retries = 0,
  force = FALSE,
  graph = NULL,
  trigger = drake::trigger(),
  skip_imports = FALSE,
  skip_safety_checks = FALSE,
  config = NULL,
  lazy_load = "eager",
  session_info = NULL,
  cache_log_file = NULL,
  seed = NULL,
  caching = "main",
  keep_going = FALSE,
  session = NULL,
  pruning_strategy = NULL,
  makefile_path = NULL,
  console_log_file = NULL,
  ensure_workers = NULL,
  garbage_collection = FALSE,
  template = list(),
  sleep = function(i) 0.01,
  hasty_build = NULL,
  memory_strategy = "speed",
  layout = NULL,
  spec = NULL,
  lock_envir = NULL,
  history = TRUE,
  recover = FALSE,
  recoverable = TRUE,
  curl_handles = list(),
  max_expand = NULL,
  log_build_times = TRUE,
  format = NULL,
  lock_cache = TRUE,
  log_make = NULL,
  log_worker = FALSE
) {
  force(envir)
  deprecate_arg(config, "config")
  config <- config %|||% drake_config(
    plan = plan,
    targets = targets,
    envir = envir,
    verbose = verbose,
    hook = hook,
    cache = cache,
    fetch_cache = fetch_cache,
    parallelism = parallelism,
    jobs = jobs,
    jobs_preprocess = jobs_preprocess,
    packages = packages,
    lib_loc = lib_loc,
    prework = prework,
    prepend = prepend,
    command = command,
    args = args,
    recipe_command = recipe_command,
    log_progress = log_progress,
    skip_targets = skip_targets,
    timeout = timeout,
    cpu = cpu,
    elapsed = elapsed,
    retries = retries,
    force = force,
    graph = graph,
    trigger = trigger,
    skip_imports = skip_imports,
    skip_safety_checks = skip_safety_checks,
    lazy_load = lazy_load,
    session_info = session_info,
    cache_log_file = cache_log_file,
    seed = seed,
    caching = caching,
    keep_going = keep_going,
    session = session,
    pruning_strategy = pruning_strategy,
    makefile_path = makefile_path,
    console_log_file = console_log_file,
    ensure_workers = ensure_workers,
    garbage_collection = garbage_collection,
    template = template,
    sleep = sleep,
    hasty_build = hasty_build,
    memory_strategy = memory_strategy,
    layout = layout,
    spec = spec,
    lock_envir = lock_envir,
    history = history,
    recover = recover,
    recoverable = recoverable,
    curl_handles = curl_handles,
    max_expand = max_expand,
    log_build_times = log_build_times,
    format = format,
    lock_cache = lock_cache,
    log_make = log_make,
    log_worker = log_worker
  )
  make_impl(config)
}

#' @title Internal function with a drake_config() argument
#' @export
#' @keywords internal
#' @description Not a user-side function.
#' @param config a [drake_config()] object.
make_impl <- function(config) {
  config$logger$disk("begin make()")
  on.exit(config$logger$disk("end make()"), add = TRUE)
  runtime_checks(config = config)
  if (config$settings$lock_cache) {
    config$cache$lock()
    on.exit(config$cache$unlock(), add = TRUE)
  }
  config <- prep_config_for_make(config)
  if (config$settings$log_progress) {
    config$cache$clear(namespace = "progress")
  }
  drake_set_session_info(
    cache = config$cache,
    full = config$settings$session_info
  )
  do_prework(config = config, verbose_packages = config$logger$verbose)
  if (!config$settings$skip_imports) {
    process_imports(config)
  }
  config$envir_graph <- new.env(parent = emptyenv())
  config$envir_graph$graph <- outdated_subgraph(config)
  r_make_message(force = FALSE)
  if (!config$settings$skip_targets) {
    process_targets(config)
  }
  drake_cache_log_file_(
    file = config$settings$cache_log_file,
    cache = config$cache,
    jobs = config$settings$jobs_preprocess
  )
  clear_make_memory(config)
  invisible()
}

prep_config_for_make <- function(config) {
  config$running_make <- TRUE
  config <- init_config_tmp(config)
}

init_config_tmp <- function(config) {
  config$ht_dynamic <- ht_new()
  config$ht_dynamic_size <- ht_new()
  config$ht_is_subtarget <- ht_new()
  config$ht_subtarget_parents <- ht_new()
  config$ht_target_exists <- ht_target_exists(config)
  config$envir_loaded <- new.env(hash = FALSE, parent = emptyenv())
  config$cache$reset_memo_hash()
  config$meta <- new.env(hash = TRUE, parent = emptyenv())
  config$meta_old <- new.env(hash = TRUE, parent = emptyenv())
  seed <- config$settings$seed
  config$cache$set(key = "seed", value = seed, namespace = "session")
  config
}

process_targets <- function(config) {
  run_backend(config)
  config$logger$terminate_progress()
  invisible()
}

run_backend <- function(config) {
  order <- igraph::gorder(config$envir_graph$graph)
  if (order) {
    config$logger$set_progress_total(order)
    config$logger$progress(0L)
    drake_backend(config)
  } else {
    config$logger$up_to_date()
  }
}

drake_backend <- function(config) {
  if (identical(config$settings$parallelism, "loop")) {
    drake_backend_loop(config)
  } else if (identical(config$settings$parallelism, "clustermq")) {
    drake_backend_clustermq(config)
  } else if (identical(config$settings$parallelism, "future")) {
    drake_backend_future(config)
  } else {
    drake_backend_bad(config)
  }
}

drake_backend_bad <- function(config) {
  if (!is.character(config$settings$parallelism)) {
    warn0("Custom drake parallel backends are deprecated. Using \"loop\".")
  }
  warn0("Illegal drake backend. Running without parallelism.")
  drake_backend_loop(config)
}

outdated_subgraph <- function(config) {
  outdated <- outdated_impl(config, do_prework = FALSE, make_imports = FALSE)
  config$logger$disk("isolate oudated targets")
  igraph::induced_subgraph(graph = config$graph, vids = outdated)
}

drake_set_session_info <- function(
  path = NULL,
  search = NULL,
  cache = drake::drake_cache(path = path),
  verbose = NULL,
  full = TRUE
) {
  deprecate_verbose(verbose)
  if (is.null(cache)) {
    stop("No drake::make() session detected.")
  }
  if (full) {
    cache$set(
      key = "sessionInfo",
      value = utils::sessionInfo(),
      namespace = "session"
    )
  }
  cache$set(
    key = "drake_version",
    value = as.character(utils::packageVersion("drake")),
    namespace = "session"
  )
  invisible()
}

#' @title Do the prework in the `prework`
#'   argument to [make()].
#' `r lifecycle::badge("stable")`
#' @export
#' @keywords internal
#' @description For internal use only.
#' The only reason this function is exported
#' is to set up parallel socket (PSOCK) clusters
#' without too much fuss.
#' @return Inivisibly returns `NULL`.
#' @param config A configured workflow from [drake_config()].
#' @param verbose_packages logical, whether to print
#'   package startup messages
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' # Create a main internal configuration list with prework.
#' con <- drake_config(my_plan, prework = c("library(knitr)", "x <- 1"))
#' # Do the prework. Usually done at the beginning of `make()`,
#' # and for distributed computing backends like "future_lapply",
#' # right before each target is built.
#' do_prework(config = con, verbose_packages = TRUE)
#' # The `eval` element is the environment where the prework
#' # and the commands in your workflow plan data frame are executed.
#' identical(con$eval$x, 1) # Should be TRUE.
#' }
#' })
#' }
do_prework <- function(config, verbose_packages) {
  for (package in base::union(c("methods", "drake"), config$packages)) {
    expr <- as.call(c(
      quote(require),
      package = package,
      lib.loc = as.call(c(quote(c), config$settings$lib_loc)),
      quietly = TRUE,
      character.only = TRUE
    ))
    if (verbose_packages) {
      expr <- as.call(c(quote(suppressPackageStartupMessages), expr))
    }
    eval(expr, envir = config$envir_targets)
  }
  if (is.character(config$prework)) {
    config$prework <- parse(text = config$prework)
  }
  if (is.language(config$prework)) {
    eval(config$prework, envir = config$envir_targets)
  } else if (is.list(config$prework)) {
    lapply(config$prework, eval, envir = config$envir_targets)
  } else if (length(config$prework)) {
    stop0("prework must be an expression or a list of expressions.")
  }
  invisible()
}

clear_make_memory <- function(config) {
  config$cache$reset_memo_hash()
  envirs <- c(
    "envir_graph",
    "envir_targets",
    "envir_dynamic",
    "envir_subtargets",
    "envir_loaded",
    "ht_dynamic",
    "ht_dynamic_size",
    "ht_is_subtarget",
    "ht_subtarget_parents",
    "ht_target_exists",
    "meta",
    "meta_old"
  )
  for (key in envirs) {
    remove(
      list = as.character(names(config[[key]])),
      envir = config[[key]]
    )
  }
  config$cache$flush_cache()
  if (config$settings$garbage_collection) {
    gc()
  }
}

# Generate a flat csv log file to represent the state of the cache.
drake_cache_log_file_ <- function(
  file = "drake_cache.csv",
  path = NULL,
  search = NULL,
  cache = drake::drake_cache(path = path),
  verbose = 1L,
  jobs = 1L,
  targets_only = FALSE
) {
  deprecate_search(search)
  if (!length(file) || identical(file, FALSE)) {
    return(invisible())
  } else if (identical(file, TRUE)) {
    file <- formals(drake_cache_log_file_)$file
  }
  out <- drake_cache_log(
    path = path,
    cache = cache,
    verbose = verbose,
    jobs = jobs,
    targets_only = targets_only
  )
  out <- as.data.frame(out)
  # Suppress partial arg match warnings.
  suppressWarnings(
    write.table(
      x = out,
      file = file,
      quote = FALSE,
      row.names = FALSE,
      sep = ","
    )
  )
}

runtime_checks <- function(config) {
  assert_config(config)
  if (identical(config$settings$skip_safety_checks, TRUE)) {
    return(invisible())
  }
  missing_input_files(config = config)
  subdirectory_warning(config = config)
  assert_outside_cache(config = config)
}

check_formats <- function(formats) {
  if (length(formats)) {
    formats <- unique(formats[!is.na(formats)])
  }
  lapply(formats, assert_format)
}

assert_format <- function(format) {
  class(format) <- format
  assert_format_impl(format)
}

assert_format_impl <- function(format) {
  UseMethod("assert_format_impl")
}

#' @export
assert_format_impl.fst <- function(format) {
  assert_pkg("fst")
}

#' @export
assert_format_impl.fst_tbl <- function(format) {
  assert_pkg("fst")
  assert_pkg("tibble")
}

#' @export
assert_format_impl.fst_dt <- function(format) {
  assert_pkg("fst")
  assert_pkg("data.table")
}

#' @export
assert_format_impl.diskframe <- function(format) {
  assert_pkg("disk.frame")
}

#' @export
assert_format_impl.keras <- function(format) {
  assert_pkg("keras") # nocov
}

#' @export
assert_format_impl.qs <- function(format) {
  assert_pkg("qs")
}

#' @export
assert_format_impl.rds <- function(format) {
  stopifnot(getRversion() >= "3.5.0")
}

#' @export
assert_format_impl.file <- function(format) {
}

#' @export
assert_format_impl.default <- function(format) {
  stop0(
    "illegal format ", format, ". Read ",
    "https://docs.ropensci.org/drake/reference/drake_plan.html#formats"
  )
}

missing_input_files <- function(config) {
  files <- parallel_filter(
    all_imports(config),
    f = is_encoded_path,
    jobs = config$settings$jobs_preprocess
  )
  files <- config$cache$decode_path(x = files)
  missing_files <- files[!file_dep_exists(files)]
  if (length(missing_files)) {
    warn0(
      "missing file_in() files:\n",
      multiline_message(missing_files)
    )
  }
  invisible()
}

file_dep_exists <- function(x) {
  file.exists(x) | is_url(x)
}

subdirectory_warning <- function(config) {
  if (identical(Sys.getenv("drake_warn_subdir"), "false")) {
    return()
  }
  dir <- normalizePath(dirname(config$cache$path), mustWork = FALSE)
  wd <- normalizePath(getwd(), mustWork = FALSE)
  if (!length(dir) || wd == dir || is.na(pmatch(dir, wd))) {
    return()
  }
  warn0(
    "Do not run make() from a subdirectory of your project.\n",
    "  running make() from: ", wd, "\n",
    "  drake project root:  ", dir, "\n",
    "  cache directory:     ", config$cache$path
  )
}

assert_outside_cache <- function(config) {
  work_dir <- normalizePath(getwd(), mustWork = FALSE)
  cache_dir <- normalizePath(config$cache$path, mustWork = FALSE)
  if (identical(work_dir, cache_dir)) {
    stop0(
      "cannot run make() from inside the cache: ", cache_dir,
      ". The cache path must be different from your working directory."
    )
  }
}

r_make_message <- function(force) {
  r_make_message <- getOption("drake_r_make_message") %||%
    .pkg_envir[["r_make_message"]] %|||%
    TRUE
  on.exit(
    assign(
      x = "r_make_message",
      value = FALSE,
      envir = .pkg_envir,
      inherits = FALSE
    )
  )
  if (force || (r_make_message && sample.int(n = 10, size = 1) < 1.5)) {
    cli_msg("Consider drake::r_make() to improve robustness.")
  }
}
