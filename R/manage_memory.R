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
  if (identical(config$garbage_collection, TRUE)) {
    gc()
  }
  invisible()
}

manage_deps <- function(target, config, downstream, jobs) {
  UseMethod("manage_deps")
}

manage_deps.speed <- function(target, config, downstream, jobs) {
  already_loaded <- setdiff(names(config$envir_targets), drake_markers)
  target_deps <- deps_memory(targets = target, config = config)
  target_deps <- setdiff(target_deps, target)
  target_deps <- setdiff(target_deps, already_loaded)
  try_load(targets = target_deps, config = config, jobs = jobs)
  load_subtarget_deps(target, config)
}

manage_deps.autoclean <- function(target, config, downstream, jobs) {
  already_loaded <- setdiff(names(config$envir_targets), drake_markers)
  target_deps <- deps_memory(targets = target, config = config)
  discard_these <- setdiff(x = already_loaded, y = target_deps)
  if (length(discard_these)) {
    config$logger$minor("unload", discard_these, target = target)
    rm(list = discard_these, envir = config$envir_targets)
  }
  clear_envir(target = target, envir = config$envir_subtargets, config = config)
  target_deps <- setdiff(target_deps, target)
  target_deps <- setdiff(target_deps, already_loaded)
  try_load(targets = target_deps, config = config, jobs = jobs)
  load_subtarget_deps(target, config)
}

manage_deps.preclean <- manage_deps.autoclean

manage_deps.lookahead <- function(target, config, downstream, jobs) {
  downstream <- downstream %||% downstream_nodes(config$graph, target)
  downstream_deps <- deps_memory(targets = downstream, config = config)
  already_loaded <- setdiff(names(config$envir_targets), drake_markers)
  target_deps <- deps_memory(targets = target, config = config)
  keep_these <- c(target_deps, downstream_deps)
  discard_these <- setdiff(x = already_loaded, y = keep_these)
  if (length(discard_these)) {
    config$logger$minor("unload", discard_these, target = target)
    rm(list = discard_these, envir = config$envir_targets)
  }
  target_deps <- setdiff(target_deps, target)
  target_deps <- setdiff(target_deps, already_loaded)
  try_load(targets = target_deps, config = config, jobs = jobs)
  load_subtarget_deps(target, config)
}

manage_deps.unload <- function(target, config, downstream, jobs) {
  for (name in c("envir_targets", "envir_subtargets")) {
    clear_envir(target = target, envir = config[[name]], config = config)
  }
}

manage_deps.none <- function(target, config, downstream, jobs) {
  return()
}

clear_envir <- function(target, envir, config) {
  discard_these <- setdiff(names(envir), drake_markers)
  if (length(discard_these)) {
    config$logger$minor("unload", discard_these, target = target)
    rm(list = discard_these, envir = envir)
  }
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
      envir = config$envir_targets,
      cache = config$cache,
      verbose = FALSE,
      lazy = config$lazy_load
    )
  )
}

load_subtarget_deps <- function(subtarget, config) {
  if (!is_subtarget(subtarget, config)) {
    return()
  }
  parent <- config$layout[[subtarget]]$subtarget_parent
  index <- config$layout[[subtarget]]$subtarget_index
  deps <- subtarget_deps(parent, index, config)
  lapply(
    names(deps),
    load_subtarget_dep,
    deps = deps,
    config = config
  )
}

load_subtarget_dep <- function(dep, deps, config) {
  index <- unlist(deps[[dep]])
  if (is_dynamic(dep, config)) {
    load_dynamic_subtarget(dep, index, config)
  } else {
    load_static_subtarget(dep, index, config)
  }
}

load_dynamic_subtarget <- function(target, index, config) {
  subtarget <- subtarget_name(target, index)
  value <- get(subtarget, envir = config$envir_targets, inherits = FALSE)
  assign(
    x = target,
    value = value,
    envir = config$envir_subtargets,
    inherits = FALSE
  )
}

load_static_subtarget <- function(target, index, config) {
  value <- get(target, envir = config$envir_targets, inherits = FALSE)
  value <- dynamic_subvalue(value, index)
  assign(
    x = target,
    value = value,
    envir = config$envir_subtargets,
    inherits = FALSE
  )
}
