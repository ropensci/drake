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
  already_loaded <- config$envir_loaded$targets
  target_deps <- deps_memory(targets = target, config = config)
  target_deps <- setdiff(target_deps, target)
  target_deps <- setdiff(target_deps, already_loaded)
  try_load(targets = target_deps, config = config, jobs = jobs)
  load_subtarget_subdeps(target, config)
}

manage_deps.autoclean <- function(target, config, downstream, jobs) {
  already_loaded <- config$envir_loaded$targets
  target_deps <- deps_memory(targets = target, config = config)
  discard_these <- setdiff(x = already_loaded, y = target_deps)
  discard_targets(discard_these, target, config)
  clear_envir_subtargets(target = target, config = config)
  target_deps <- setdiff(target_deps, target)
  target_deps <- setdiff(target_deps, already_loaded)
  try_load(targets = target_deps, config = config, jobs = jobs)
  load_subtarget_subdeps(target, config)
}

manage_deps.preclean <- manage_deps.autoclean

manage_deps.lookahead <- function(target, config, downstream, jobs) {
  downstream <- downstream %||% downstream_nodes(
    config$envir_graph$graph,
    target
  )
  downstream_deps <- deps_memory(targets = downstream, config = config)
  already_loaded <- config$envir_loaded$targets
  target_deps <- deps_memory(targets = target, config = config)
  keep_these <- c(target_deps, downstream_deps)
  discard_these <- setdiff(x = already_loaded, y = keep_these)
  discard_targets(discard_these, target, config)
  target_deps <- setdiff(target_deps, target)
  target_deps <- setdiff(target_deps, already_loaded)
  try_load(targets = target_deps, config = config, jobs = jobs)
  load_subtarget_subdeps(target, config)
}

discard_targets <- function(discard_these, target, config) {
  if (!length(discard_these)) {
    return()
  }
  config$logger$minor("unload", discard_these, target = target)
  rm(list = discard_these, envir = config$envir_targets)
  config$envir_loaded$targets <- setdiff(
    config$envir_loaded$targets,
    discard_these
  )
}

manage_deps.unload <- function(target, config, downstream, jobs) {
  clear_envir_subtargets(target = target, config = config)
  clear_envir_targets(target = target, config = config)
}

manage_deps.none <- function(target, config, downstream, jobs) {
  return()
}

clear_envir_subtargets <- function(target, config) {
  config$logger$minor("clear subtarget envir", target = target)
  rm(list = config$envir_loaded$subtargets, envir = config$envir_subtargets)
  config$envir_loaded$subtargets <- character(0)
  config$envir_subtargets[[drake_envir_marker]] <- TRUE
}

clear_envir_targets <- function(target, config) {
  config$logger$minor("clear target envir", target = target)
  rm(list = config$envir_loaded$targets, envir = config$envir_targets)
  config$envir_loaded$targets <- character(0)
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
  try({
    load_target(
      target = target,
      namespace = config$cache$default_namespace,
      envir = config$envir_targets,
      cache = config$cache,
      verbose = FALSE,
      lazy = config$lazy_load
    )
    config$envir_loaded$targets <- c(config$envir_loaded$targets, target)
  })
}

load_target <- function(target, cache, namespace, envir, verbose, lazy) {
  class(target) <- lazy
  load_target_impl(target, cache, namespace, envir, verbose)
}

load_target_impl <- function(target, cache, namespace, envir, verbose) {
  UseMethod("load_target_impl")
}

load_target_impl.eager <- function(target, cache, namespace, envir, verbose) {
  value <- cache$get(
    key = target,
    namespace = namespace,
    use_cache = FALSE
  )
  assign(x = target, value = value, envir = envir)
  local <- environment()
  rm(value, envir = local)
  invisible()
}

load_target_impl.promise <- function(target, cache, namespace, envir, verbose) {
  eval_env <- environment()
  delayedAssign(
    x = target,
    value = cache$get(
      key = target,
      namespace = namespace,
      use_cache = FALSE
    ),
    eval.env = eval_env,
    assign.env = envir
  )
}

load_target_impl.bind <- function(target, cache, namespace, envir, verbose) {
  assert_pkg("bindr")
  # Allow active bindings to overwrite existing variables.
  if (exists(x = target, envir = envir, inherits = FALSE)) {
    message(
      "Replacing already-loaded variable ", target,
      " with an active binding."
    )
    remove(list = target, envir = envir)
  }
  bindr::populate_env(
    env = envir,
    names = as.character(target),
    fun = function(key, cache, namespace) {
      if (!length(namespace)) {
        # Now impractical to cover because loadd() checks the namespace,
        # but good to have around anyway.
        namespace <- cache$default_namespace # nocov
      }
      cache$get(
        key = as.character(key),
        namespace = as.character(namespace),
        use_cache = FALSE
      )
    },
    cache = cache,
    namespace = namespace
  )
}

load_subtarget_subdeps <- function(subtarget, config) {
  if (!is_subtarget(subtarget, config)) {
    return()
  }
  parent <- config$layout[[subtarget]]$subtarget_parent
  index <- config$layout[[subtarget]]$subtarget_index
  deps <- subtarget_deps(parent, index, config)
  load_by_as_subdep(parent, index, config)
  dep_names <- names(deps)
  lapply(
    dep_names,
    load_subtarget_subdep,
    subtarget = subtarget,
    deps = deps,
    config = config
  )
  config$envir_loaded$subtargets <- unique(
    c(config$envir_loaded$subtargets, dep_names)
  )
}

load_by_as_subdep <- function(parent, index, config) {
  dynamic <- config$layout[[parent]]$dynamic
  if (no_by(dynamic)) {
    return()
  }
  by_key <- which_by(dynamic)
  by_value <- get_dynamic_by(by_key, config)
  by_value <- unique(by_value)[[index]]
  assign(
    x = by_key,
    value = by_value,
    envir = config$envir_subtargets,
    inherits = FALSE
  )
  config$envir_loaded$subtargets <- unique(
    c(config$envir_loaded$subtargets, by_key)
  )
}

load_subtarget_subdep <- function(subtarget, dep, deps, config) {
  index <- unlist(deps[[dep]])
  if (is_dynamic(dep, config)) {
    load_dynamic_subdep(subtarget, dep, index, config)
  } else {
    load_static_subdep(dep, index, config)
  }
}

load_dynamic_subdep <- function(subtarget, dep, index, config) {
  parent <- config$layout[[subtarget]]$subtarget_parent
  dynamic <- config$layout[[parent]]$dynamic
  load_dynamic_subdep_impl(dynamic, parent, dep, index, config)
}

load_dynamic_subdep_impl <- function(dynamic, parent, dep, index, config) {
  UseMethod("load_dynamic_subdep_impl")
}

load_dynamic_subdep_impl.group <- function( # nolint
  dynamic,
  parent,
  dep,
  index,
  config
) {
  subdeps <- config$cache$get(dep, namespace = "meta")$subtargets[index]
  value <- config$cache$mget(subdeps, use_cache = FALSE)
  assign(
    x = dep,
    value = value,
    envir = config$envir_subtargets,
    inherits = FALSE
  )
}

load_dynamic_subdep_impl.default <- function( # nolint
  dynamic,
  parent,
  dep,
  index,
  config
) {
  subdep <- config$cache$get(dep, namespace = "meta")$subtargets[[index]]
  value <- config$cache$get(subdep, use_cache = FALSE)
  assign(
    x = dep,
    value = value,
    envir = config$envir_subtargets,
    inherits = FALSE
  )
}

load_static_subdep <- function(dep, index, config) {
  value <- get(dep, envir = config$envir_targets, inherits = FALSE)
  value <- dynamic_subvalue(value, index)
  assign(
    x = dep,
    value = value,
    envir = config$envir_subtargets,
    inherits = FALSE
  )
}
