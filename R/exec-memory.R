assign_to_envir <- function(target, value, config) {
  memory_strategy <- config$layout[[target]]$memory_strategy %||NA%
    config$memory_strategy
  if (memory_strategy %in% c("unload", "none")) {
    return()
  }
  if (
    identical(config$lazy_load, "eager") &&
    !is_encoded_path(target) &&
    !is_imported(target, config)
  ) {
    assign(x = target, value = value, envir = config$eval)
  }
  invisible()
}

#' @title Manage the in-memory dependencies of a target.
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
#' @examples
#' # Users should use make().
manage_memory <- function(target, config, downstream = NULL, jobs = 1) {
  stopifnot(length(target) == 1L)
  if (identical(config$garbage_collection, TRUE)) {
    gc()
  }
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
  already_loaded <- setdiff(names(config$eval), drake_markers)
  target_deps <- deps_memory(targets = target, config = config)
  target_deps <- setdiff(target_deps, target)
  target_deps <- setdiff(target_deps, already_loaded)
  try_load(targets = target_deps, config = config, jobs = jobs)
}

manage_deps.memory <- function(target, config, downstream, jobs) {
  already_loaded <- setdiff(names(config$eval), drake_markers)
  target_deps <- deps_memory(targets = target, config = config)
  discard_these <- setdiff(x = already_loaded, y = target_deps)
  if (length(discard_these)) {
    log_msg("unload", discard_these, config = config)
    rm(list = discard_these, envir = config$eval)
  }
  target_deps <- setdiff(target_deps, target)
  target_deps <- setdiff(target_deps, already_loaded)
  try_load(targets = target_deps, config = config, jobs = jobs)
}

manage_deps.lookahead <- function(target, config, downstream, jobs) {
  downstream <- downstream %||% downstream_nodes(config$graph, target)
  downstream_deps <- deps_memory(targets = downstream, config = config)
  already_loaded <- setdiff(names(config$eval), drake_markers)
  target_deps <- deps_memory(targets = target, config = config)
  keep_these <- c(target_deps, downstream_deps)
  discard_these <- setdiff(x = already_loaded, y = keep_these)
  if (length(discard_these)) {
    log_msg("unload", discard_these, config = config)
    rm(list = discard_these, envir = config$eval)
  }
  target_deps <- setdiff(target_deps, target)
  target_deps <- setdiff(target_deps, already_loaded)
  try_load(targets = target_deps, config = config, jobs = jobs)
}

manage_deps.unload <- function(target, config, downstream, jobs) {
  discard_these <- setdiff(names(config$eval), drake_markers)
  if (length(discard_these)) {
    log_msg("unload", discard_these, config = config)
    rm(list = discard_these, envir = config$eval)
  }
}

manage_deps.none <- function(...) {
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
      log_msg("load", targets, config = config)
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

get_import_from_memory <- function(target, config) {
  if (is_encoded_path(target)) {
    return(NA_character_)
  }
  if (is_encoded_namespaced(target)) {
    target <- decode_namespaced(target, config)
  }
  if (exists(x = target, envir = config$envir, inherits = FALSE)) {
    return(get(x = target, envir = config$envir, inherits = FALSE))
  }
  parsed <- parse(text = target)
  parsed <- as.call(parsed)
  parsed <- as.list(parsed)
  lang <- parsed[[1]]
  is_namespaced <- length(lang) > 1
  if (is_namespaced) {
    stopifnot(safe_deparse(lang[[1]]) %in% c("::", ":::"))
    pkg <- safe_deparse(lang[[2]])
    fun <- safe_deparse(lang[[3]])
    tryCatch(get(fun, envir = getNamespace(pkg)), error = error_na)
  } else {
    NA_character_
  }
}

missing_import <- function(x, config) {
  if (is_encoded_path(x)) {
    return(!file_dep_exists(decode_path(x, config)))
  }
  identical(get_import_from_memory(x, config = config), NA_character_)
}
