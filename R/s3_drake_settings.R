#' @title `drake_settings` helper
#' @keywords internal
#' @description List of class `drake_settings`.
#' @return A `drake_settings` object.
#' @examples
#' if (FALSE) { # stronger than roxygen dontrun
#' drake_settings()
#' }
drake_settings <- function(
  cache_log_file = NULL,
  curl_handles = list(),
  garbage_collection = FALSE,
  jobs = 1L,
  jobs_preprocess = 1L,
  keep_going = TRUE,
  lazy_load = "eager",
  lib_loc = character(0),
  lock_cache = TRUE,
  lock_envir = TRUE,
  log_build_times = TRUE,
  log_progress = TRUE,
  memory_strategy = "speed",
  parallelism = "loop",
  recover = TRUE,
  recoverable = TRUE,
  seed = 0L,
  session_info = TRUE,
  skip_imports = FALSE,
  skip_safety_checks = FALSE,
  skip_targets = FALSE,
  sleep = function(i) 0.01,
  template = list(),
  log_worker = FALSE
) {
  sleep <- `environment<-`(sleep, new.env(parent = globalenv()))
  new_drake_settings(
    cache_log_file = cache_log_file,
    curl_handles = curl_handles,
    garbage_collection = garbage_collection,
    jobs = jobs,
    jobs_preprocess = jobs_preprocess,
    keep_going = keep_going,
    lazy_load = lazy_load,
    lib_loc = lib_loc,
    lock_cache = lock_cache,
    lock_envir = lock_envir,
    log_build_times = log_build_times,
    log_progress = log_progress,
    memory_strategy = memory_strategy,
    parallelism = parallelism,
    recover = recover,
    recoverable = recoverable,
    seed = seed,
    session_info = session_info,
    skip_imports = skip_imports,
    skip_safety_checks = skip_safety_checks,
    skip_targets = skip_targets,
    sleep = sleep,
    template = template,
    log_worker = log_worker
  )
}

#' @title `drake_settings` constructor
#' @keywords internal
#' @description List of class `drake_settings`.
#' @return A `drake_settings` object.
#' @inheritParams drake_config
#' @examples
#' if (FALSE) { # stronger than roxygen dontrun
#' new_drake_settings()
#' }
new_drake_settings <- function(
  cache_log_file = NULL,
  curl_handles = NULL,
  garbage_collection = NULL,
  jobs = NULL,
  jobs_preprocess = NULL,
  keep_going = NULL,
  lazy_load = NULL,
  lib_loc = NULL,
  lock_envir = NULL,
  lock_cache = NULL,
  log_build_times = NULL,
  log_progress = NULL,
  memory_strategy = NULL,
  parallelism = NULL,
  recover = NULL,
  recoverable = NULL,
  seed = NULL,
  session_info = NULL,
  skip_imports = NULL,
  skip_safety_checks = NULL,
  skip_targets = NULL,
  sleep = NULL,
  template = NULL,
  log_worker = NULL
) {
  out <- list(
    cache_log_file = cache_log_file,
    curl_handles = curl_handles,
    garbage_collection = garbage_collection,
    jobs = jobs,
    jobs_preprocess = jobs_preprocess,
    keep_going = keep_going,
    lazy_load = lazy_load,
    lib_loc = lib_loc,
    lock_cache = lock_cache,
    lock_envir = lock_envir,
    log_build_times = log_build_times,
    log_progress = log_progress,
    memory_strategy = memory_strategy,
    parallelism = parallelism,
    recover = recover,
    recoverable = recoverable,
    seed = seed,
    session_info = session_info,
    skip_imports = skip_imports,
    skip_safety_checks = skip_safety_checks,
    skip_targets = skip_targets,
    sleep = sleep,
    template = template,
    log_worker = log_worker
  )
  class(out) <- c("drake_settings", "drake")
  out
}

validate_drake_settings <- function(x) {
  stopifnot(inherits(x, "drake_settings"))
  stopifnot(inherits(x, "drake"))
  out_fields <- names(x)
  exp_fields <- names(formals(new_drake_settings))
  stopifnot(identical(sort(out_fields), sort(exp_fields)))
  val_drake_settings_lengths(x)
  val_drake_settings_types(x)
  val_drake_settings_values(x)
}

val_drake_settings_lengths <- function(x) {
  stopifnot(length(x$garbage_collection) == 1L)
  stopifnot(length(x$jobs) == 1L)
  stopifnot(length(x$jobs_preprocess) == 1L)
  stopifnot(length(x$keep_going) == 1L)
  stopifnot(length(x$lazy_load) == 1L)
  stopifnot(length(x$lock_cache) == 1L)
  stopifnot(length(x$lock_envir) == 1L)
  stopifnot(length(x$log_build_times) == 1L)
  stopifnot(length(x$log_progress) == 1L)
  stopifnot(length(x$memory_strategy) == 1L)
  stopifnot(length(x$parallelism) == 1L)
  stopifnot(length(x$recover) == 1L)
  stopifnot(length(x$recoverable) == 1L)
  stopifnot(length(x$seed) == 1L)
  stopifnot(length(x$session_info) == 1L)
  stopifnot(length(x$skip_imports) == 1L)
  stopifnot(length(x$skip_safety_checks) == 1L)
  stopifnot(length(x$skip_targets) == 1L)
  stopifnot(length(x$log_worker) == 1L)
}

val_drake_settings_types <- function(x) {
  stopifnot(is.list(x$curl_handles))
  stopifnot(is.logical(x$garbage_collection))
  stopifnot(is.numeric(x$jobs))
  stopifnot(is.numeric(x$jobs_preprocess))
  stopifnot(is.logical(x$keep_going))
  stopifnot(is.character(x$lazy_load))
  stopifnot(is.character(x$lib_loc))
  stopifnot(is.logical(x$lock_cache))
  stopifnot(is.logical(x$lock_envir))
  stopifnot(is.logical(x$log_build_times))
  stopifnot(is.logical(x$log_progress))
  stopifnot(is.character(x$memory_strategy))
  stopifnot(is.character(x$parallelism))
  stopifnot(is.logical(x$recover))
  stopifnot(is.logical(x$recoverable))
  stopifnot(is.numeric(x$seed))
  stopifnot(is.logical(x$session_info))
  stopifnot(is.logical(x$skip_imports))
  stopifnot(is.logical(x$skip_safety_checks))
  stopifnot(is.logical(x$skip_targets))
  stopifnot(is.function(x$sleep))
  stopifnot(is.list(x$template))
  stopifnot(is.logical(x$log_worker))
}

val_drake_settings_values <- function(x) {
  stopifnot(x$jobs >= 1L)
  stopifnot(x$jobs_preprocess >= 1L)
  vals <- c("eager", "promise", "bind")
  stopifnot(x$lazy_load %in% vals)
  vals <- memory_strategies()
  stopifnot(x$memory_strategy %in% vals)
  vals <- c("loop", "clustermq", "future")
  stopifnot(x$parallelism %in% vals)
}

#' @export
print.drake_settings <- function(x, ...) {
  cat("drake_settings\n")
  str0(x)
}
