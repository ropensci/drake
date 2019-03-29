assign_to_envir <- function(target, value, config) {
  if (
    identical(config$lazy_load, "eager") &&
    !is_encoded_path(target) &&
    !is_imported(target, config)
  ) {
    assign(x = target, value = value, envir = config$eval)
  }
  invisible()
}

#' @title Manage in-memory targets
#' @description Load targets that you need to build the targets
#'   and unload the ones you will never need again in the
#'   current runthrough of the pipeline. This function should
#'   not be used directly by users. Only exported for
#'   internal reasons.
#' @export
#' @keywords internal
#' @return Nothing.
#' @param targets Character vector of targets.
#' @param config [drake_config()] list.
#' @param downstream Optional, character vector of any targets
#'   assumed to be downstream.
#' @param jobs Number of jobs for local parallel computing
#' @examples
#' # Users should use make().
manage_memory <- function(targets, config, downstream = NULL, jobs = 1) {
  if (identical(config$garbage_collection, TRUE)) {
    gc()
  }
  if (identical(config$memory_strategy, "lookahead")) {
    downstream <- downstream %||% downstream_nodes(config$graph, targets)
    downstream_deps <- deps_memory(targets = downstream, config = config)
  } else {
    downstream <- downstream_deps <- NULL
  }
  already_loaded <- setdiff(names(config$eval), drake_envir_marker)
  target_deps <- deps_memory(targets = targets, config = config)
  if (!identical(config$memory_strategy, "speed")) {
    keep_these <- c(target_deps, downstream_deps)
    discard_these <- setdiff(x = already_loaded, y = keep_these)
    if (length(discard_these)) {
      log_msg("unload", discard_these, config = config)
      rm(list = discard_these, envir = config$eval)
    }
  }
  targets <- setdiff(target_deps, targets)
  targets <- setdiff(targets, already_loaded)
  try_load(targets = targets, config = config, jobs = jobs)
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
    return(!file.exists(decode_path(x, config)))
  }
  identical(get_import_from_memory(x, config = config), NA_character_)
}
