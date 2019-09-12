#' @title Manage the in-memory dependencies of a target.
#' \lifecycle{stable}
#' @description Load/unload a target's dependencies.
#'   Not a user-side function.
#' @export
#' @keywords internal
#' @return Nothing.
#' @param target Character, name of the target.
#' @param config [drake_config()] list.
#' @param downstream Optional, character vector of any targets
#'   assumed to be downstream.
#' @param jobs Number of jobs for local parallel computing
manage_memory <- function(target, config, downstream = NULL, jobs = 1) {
  stopifnot(length(target) == 1L)
  class(target) <- config$layout[[target]]$memory_strategy %||NA%
    config$memory_strategy
  manage_deps(
    target = target,
    config = config,
    downstream = downstream,
    jobs = jobs
  )
}

manage_deps <- function(target, config, downstream, jobs) {
  UseMethod("manage_deps")
}

manage_deps.speed <- function(target, config, downstream, jobs) {
  if (identical(config$garbage_collection, TRUE)) {
    gc()
  }
  already_loaded <- setdiff(names(config$eval), drake_markers)
  target_deps <- deps_memory(targets = target, config = config)
  target_deps <- setdiff(target_deps, target)
  target_deps <- setdiff(target_deps, already_loaded)
  try_load(targets = target_deps, config = config, jobs = jobs)
}

manage_deps.autoclean <- function(target, config, downstream, jobs) {
  already_loaded <- setdiff(names(config$eval), drake_markers)
  target_deps <- deps_memory(targets = target, config = config)
  discard_these <- setdiff(x = already_loaded, y = target_deps)
  if (length(discard_these)) {
    config$logger$minor("unload", discard_these, target = target)
    rm(list = discard_these, envir = config$eval)
  }
  if (identical(config$garbage_collection, TRUE)) {
    gc()
  }
  target_deps <- setdiff(target_deps, target)
  target_deps <- setdiff(target_deps, already_loaded)
  try_load(targets = target_deps, config = config, jobs = jobs)
}

manage_deps.preclean <- manage_deps.autoclean

manage_deps.lookahead <- function(target, config, downstream, jobs) {
  downstream <- downstream %||% downstream_nodes(config$graph, target)
  downstream_deps <- deps_memory(targets = downstream, config = config)
  already_loaded <- setdiff(names(config$eval), drake_markers)
  target_deps <- deps_memory(targets = target, config = config)
  keep_these <- c(target_deps, downstream_deps)
  discard_these <- setdiff(x = already_loaded, y = keep_these)
  if (length(discard_these)) {
    config$logger$minor("unload", discard_these, target = target)
    rm(list = discard_these, envir = config$eval)
  }
  if (identical(config$garbage_collection, TRUE)) {
    gc()
  }
  target_deps <- setdiff(target_deps, target)
  target_deps <- setdiff(target_deps, already_loaded)
  try_load(targets = target_deps, config = config, jobs = jobs)
}

manage_deps.unload <- function(target, config, downstream, jobs) {
  discard_these <- setdiff(names(config$eval), drake_markers)
  if (length(discard_these)) {
    config$logger$minor("unload", discard_these, target = target)
    rm(list = discard_these, envir = config$eval)
  }
  if (identical(config$garbage_collection, TRUE)) {
    gc()
  }
}

manage_deps.none <- function(target, config, downstream, jobs) {
  if (identical(config$garbage_collection, TRUE)) {
    gc()
  }
  return()
}

deps_memory <- function(targets, config) {
  out <- lapply(
    X = targets,
    FUN = function(target) {
      config$layout[[target]]$deps_build$memory
    }
  )
  as.character(unlist(out))
}

try_load <- function(targets, config, jobs = 1) {
  if (length(targets)) {
    if (config$lazy_load == "eager") {
      config$logger$minor("load", targets)
    }
    lapply(
      X = targets,
      FUN = try_load_target,
      config = config
    )
  }
  invisible()
}

try_load_target <- function(target, config) {
  try(
    load_target(
      target = target,
      namespace = config$cache$default_namespace,
      envir = config$eval,
      cache = config$cache,
      verbose = FALSE,
      lazy = config$lazy_load
    )
  )
}
