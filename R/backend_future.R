drake_backend_future <- function(config) {
  assert_pkg("future")
  config$queue <- priority_queue(config)
  config$workers <- initialize_workers(config)
  config$sleeps <- new.env(parent = emptyenv())
  config$sleeps$count <- 1L
  config$ft_config <- hpc_config(config)
  ids <- as.character(seq_along(config$workers))
  while (work_remains(config)) {
    ft_scan_workers(ids, config)
  }
}

ft_scan_workers <- function(ids, config) {
  for (id in ids) {
    ft_scan_worker(id, config)
  }
}

ft_scan_worker <- function(id, config) {
  if (is_idle(config$workers[[id]])) {
    config$workers[[id]] <- conclude_worker(config$workers[[id]], config)
    target <- config$queue$pop0()
    ft_check_target(target, id, config)
  }
}

ft_check_target <- function(target, id, config) {
  if (length(target)) {
    ft_do_target(target, id, config)
  } else {
    ft_no_target(config) # nocov
  }
}

ft_no_target <- function(config) {
  Sys.sleep(config$settings$sleep(max(0L, config$sleeps$count)))
  config$sleeps$count <- config$sleeps$count + 1L
}

ft_do_target <- function(target, id, config) {
  config$sleeps$count <- 1L
  running <- running_targets(config = config)
  protect <- c(running, config$queue$list())
  ft_build_target(target, id, running, protect, config)
}

ft_build_target <- function(target, id, running, protect, config) {
  if (no_hpc(target, config)) {
    future_local_build(target, protect, config)
  } else {
    config$workers[[id]] <- ft_decide_worker(target, protect, config)
  }
}

future_local_build <- function(target, protect, config) {
  config$logger$disk("local target", target = target)
  local_build(target, config, downstream = protect)
  decrease_revdep_keys(config$queue, target, config)
  config$logger$progress()
}

initialize_workers <- function(config) {
  out <- new.env(parent = emptyenv())
  ids <- as.character(seq_len(config$settings$jobs))
  for (id in ids) {
    out[[id]] <- empty_worker(target = NA_character_)
  }
  out
}

ft_decide_worker <- function(target, protect, config) {
  meta <- drake_meta_(target, config)
  if (handle_triggers(target, meta, config)) {
    config$logger$progress()
    return(empty_worker(target))
  }
  ft_launch_worker(target, meta, protect, config)
}

ft_launch_worker <- function(target, meta, protect, config) {
  caching <- hpc_caching(target, config)
  if (identical(caching, "main")) {
    manage_memory(target = target, config = config, downstream = protect)
  }
  DRAKE_GLOBALS__ <- NULL # Avoid name conflicts with other globals.
  spec <- hpc_spec(target, config)
  globals <- future_globals(
    target = target,
    meta = meta,
    config = config$ft_config,
    spec = spec,
    config_tmp = get_hpc_config_tmp(config),
    protect = protect
  )
  announce_build(target = target, config = config)
  config$logger$disk("build on an hpc worker", target = target)
  structure(
    future::future(
      expr = drake::future_build(
        target = DRAKE_GLOBALS__$target,
        meta = DRAKE_GLOBALS__$meta,
        config = DRAKE_GLOBALS__$config,
        spec = DRAKE_GLOBALS__$spec,
        config_tmp = DRAKE_GLOBALS__$config_tmp,
        protect = DRAKE_GLOBALS__$protect
      ),
      globals = globals,
      label = target,
      resources = as.list(spec[[target]]$resources),
      seed = TRUE
    ),
    target = target
  )
}

future_globals <- function(
    target,
    meta,
    config,
    spec,
    config_tmp,
    protect
) {
  globals <- list(
    DRAKE_GLOBALS__ = list(
      target = target,
      meta = meta,
      config = config,
      spec = spec,
      config_tmp = config_tmp,
      protect = protect
    )
  )
  if (identical(config$envir, globalenv())) {
    # nocov start
    # Unit tests should not modify global env
    if (exists("DRAKE_GLOBALS__", config$envir)) {
      warn0(
        "Do not define an object named `DRAKE_GLOBALS__` ",
        "in the global environment"
      )
    }
    globals <- c(globals, as.list(config$envir, all.names = TRUE))
    # nocov end
  }
  globals
}

#' @title Task passed to individual futures in the `"future"` backend
#' `r lifecycle::badge("stable")`
#' @description For internal use only. Only exported to make available
#' to futures.
#' @keywords internal
#' @export
#' @return Either the target value or a list of build results.
#' @param target Name of the target.
#' @param meta A list of metadata.
#' @param config A [drake_config()] list.
#' @param config_tmp Internal, parts of `config` that the workers need.
#' @param protect Names of targets that still need their
#' dependencies available in memory.
future_build <- function(
    target,
    meta,
    config,
    spec,
    config_tmp,
    protect
) {
  config$spec <- spec
  config <- restore_hpc_config_tmp(config_tmp, config)
  caching <- hpc_caching(target, config)
  if (identical(caching, "worker")) {
    manage_memory(target = target, config = config, downstream = protect)
  }
  do_prework(config = config, verbose_packages = FALSE)
  build <- try_build(target = target, meta = meta, config = config)
  if (identical(caching, "main")) {
    build$checksum <- get_outfile_checksum(target, build$value, config)
    build <- classify_build(build, config)
    build <- serialize_build(build)
    return(build)
  }
  conclude_build(build = build, config = config)
  checksum <- get_checksum(target, build$value, config)
  value <- hpc_worker_build_value(target, build$value, config)
  list(target = target, value = value, checksum = checksum)
}

running_targets <- function(config) {
  unlist(eapply(config$workers, running_worker))
}

running_worker <- function(worker) {
  if (is_idle(worker)) {
    NULL
  } else {
    attr(worker, "target") # nocov
  }
}

# Need to check if the worker quit in error early somehow.
# Maybe the job scheduler failed.
# This should be the responsibility of the `future` package
# or something lower level.
is_idle <- function(worker) {
  is_empty_worker(worker) ||
    is_concluded_worker(worker) ||
    future::resolved(worker)
}

is_empty_worker <- function(worker) {
  !inherits(worker, "Future")
}

conclude_worker <- function(worker, config) {
  ft_decrease_revdep_keys(worker, config)
  out <- concluded_worker()
  if (is_empty_worker(worker)) {
    return(out)
  }
  build <- resolve_worker_value(worker, config)
  caching <- hpc_caching(build$target, config)
  if (identical(caching, "worker")) {
    wait_checksum(
      target = build$target,
      value = build$value,
      checksum = build$checksum,
      config = config
    )
    return(out)
  } else {
    build <- unserialize_build(build)
  }
  wait_outfile_checksum(
    target = build$target,
    value = build$value,
    checksum = build$checksum,
    config = config
  )
  conclude_build(build = build, config = config)
  config$logger$progress()
  out
}

concluded_worker <- function() {
  empty_worker(target = NULL)
}

empty_worker <- function(target) {
  structure(NA_character_, target = target)
}

ft_decrease_revdep_keys <- function(worker, config) {
  target <- attr(worker, "target")
  if (!length(target) || safe_is_na(target) || !is.character(target)) {
    return()
  }
  decrease_revdep_keys(config$queue, target, config)
}

# Also caches error information if available.
# I know, it has a return value AND a side effect,
# but it's hard to think of another clean way
# to handle crashes.
resolve_worker_value <- function(worker, config) {
  tryCatch(
    # Check if the worker crashed.
    future::value(worker),
    error = function(e) {
      e$message <- "future worker terminated before target could complete."
      meta <- list(error = e)
      target <- attr(worker, "target")
      caching <- hpc_caching(target, config)
      if (caching == "worker") {
        # Need to store the error if the worker crashed.
        handle_build_exceptions(
          target = target,
          meta = meta,
          config = config
        )
      }
      # For `caching = "main"`, we need to conclude the build
      # and store the value and metadata.
      list(
        target = target,
        value = e,
        meta = meta
      )
    }
  )
}

work_remains <- function(config) {
  !config$queue$empty() ||
    !all_concluded(config)
}

all_concluded <- function(config) {
  for (id in names(config$workers)) {
    if (!is_concluded_worker(config$workers[[id]])) {
      return(FALSE)
    }
  }
  TRUE
}

is_concluded_worker <- function(worker) {
  is.null(attr(worker, "target"))
}
