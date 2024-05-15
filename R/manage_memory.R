#' @title Manage the in-memory dependencies of a target.
#' `r lifecycle::badge("stable")`
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
  memory_strategy <- config$spec[[target]]$memory_strategy
  if (is.null(memory_strategy) || is.na(memory_strategy)) {
    memory_strategy <- config$settings$memory_strategy
  }
  class(target) <- memory_strategy
  if (!is_subtarget(target, config)) {
    clear_envir_subtargets(target = target, config = config)
  }
  manage_deps(
    target = target,
    config = config,
    downstream = downstream,
    jobs = jobs
  )
  sync_envir_dynamic(target, config)
  if (config$settings$garbage_collection) {
    gc()
  }
  invisible()
}

manage_deps <- function(target, config, downstream, jobs) {
  UseMethod("manage_deps")
}

#' @export
manage_deps.speed <- function(target, config, downstream, jobs) {
  already_loaded <- config$envir_loaded$targets
  memory_deps <- deps_memory(targets = target, config = config)
  target_deps <- memory_deps
  target_deps <- setdiff(target_deps, target)
  target_deps <- setdiff(target_deps, already_loaded)
  try_load_deps(targets = target_deps, config = config, jobs = jobs)
  load_subtarget_subdeps(target, config)
}

#' @export
manage_deps.autoclean <- function(target, config, downstream, jobs) {
  already_loaded <- config$envir_loaded$targets
  memory_deps <- deps_memory(targets = target, config = config)
  target_deps <- memory_deps
  discard_these <- setdiff(x = already_loaded, y = target_deps)
  discard_targets(discard_these, target, config)
  target_deps <- setdiff(target_deps, target)
  target_deps <- setdiff(target_deps, already_loaded)
  try_load_deps(targets = target_deps, config = config, jobs = jobs)
  load_subtarget_subdeps(target, config)
}

#' @export
manage_deps.preclean <- manage_deps.autoclean

#' @export
manage_deps.lookahead <- function(target, config, downstream, jobs) {
  downstream <- downstream %||% downstream_nodes(
    config$envir_graph$graph,
    target
  )
  downstream_deps <- deps_memory(targets = downstream, config = config)
  already_loaded <- config$envir_loaded$targets
  memory_deps <- deps_memory(targets = target, config = config)
  target_deps <- memory_deps
  keep_these <- c(target_deps, downstream_deps)
  discard_these <- setdiff(x = already_loaded, y = keep_these)
  discard_targets(discard_these, target, config)
  target_deps <- setdiff(target_deps, target)
  target_deps <- setdiff(target_deps, already_loaded)
  try_load_deps(targets = target_deps, config = config, jobs = jobs)
  load_subtarget_subdeps(target, config)
}

#' @export
manage_deps.unload <- function(target, config, downstream, jobs) {
  clear_envir_targets(target = target, config = config)
}

#' @export
manage_deps.none <- function(target, config, downstream, jobs) {
  return()
}

discard_targets <- function(discard_these, target, config) {
  if (!length(discard_these)) {
    return()
  }
  config$logger$disk("unload", discard_these, target = target)
  rm(
    list = as.character(discard_these),
    envir = config$envir_targets,
    inherits = FALSE
  )
  config$envir_loaded$targets <- setdiff(
    config$envir_loaded$targets,
    discard_these
  )
  discard_dynamic(discard_these, config)
}

discard_dynamic <- function(discard_these, config) {
  index <- vlapply(
    discard_these,
    exists,
    envir = config$envir_dynamic,
    inherits = FALSE
  )
  whole_dynamic <- discard_these[index]
  rm(
    list = as.character(whole_dynamic),
    envir = config$envir_dynamic,
    inherits = FALSE
  )
  config$envir_loaded$dynamic <- setdiff(
    config$envir_loaded$dynamic,
    whole_dynamic
  )
}

clear_envir_subtargets <- function(target, config) {
  rm(
    list = as.character(config$envir_loaded$subtargets),
    envir = config$envir_subtargets
  )
  config$envir_loaded$subtargets <- character(0)
}

clear_envir_targets <- function(target, config) {
  config$logger$disk("clear target envir", target = target)
  rm(
    list = as.character(config$envir_loaded$targets),
    envir = config$envir_targets
  )
  rm(
    list = as.character(config$envir_loaded$dynamic),
    envir = config$envir_dynamic
  )
  config$envir_loaded$targets <- character(0)
  config$envir_loaded$dynamic <- character(0)
}

deps_memory <- function(targets, config) {
  out <- lapply(
    X = targets,
    FUN = function(target) {
      config$spec[[target]]$deps_build$memory
    }
  )
  as.character(unlist(out))
}

try_load_deps <- function(targets, config, jobs = 1) {
  if (!length(targets)) {
    return()
  }
  if (config$settings$lazy_load == "eager") {
    config$logger$disk("load", targets)
  }
  lapply(
    X = targets,
    FUN = try_load_dep,
    config = config
  )
  config$envir_loaded$targets <- c(config$envir_loaded$targets, targets)
  invisible()
}

try_load_dep <- function(target, config) {
  try(try_load_dep_impl(target, config))
}

try_load_dep_impl <- function(target, config) {
  load_target(
    target = target,
    namespace = config$cache$default_namespace,
    envir = config$envir_targets,
    cache = config$cache,
    verbose = FALSE,
    lazy = config$settings$lazy_load
  )
}

load_target <- function(target, cache, namespace, envir, verbose, lazy) {
  class(target) <- lazy
  load_target_impl(target, cache, namespace, envir, verbose)
}

load_target_impl <- function(target, cache, namespace, envir, verbose) {
  UseMethod("load_target_impl")
}

#' @export
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

#' @export
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

#' @export
load_target_impl.bind <- function(target, cache, namespace, envir, verbose) {
  assert_pkg("bindr")
  # Allow active bindings to overwrite existing variables.
  if (exists(x = target, envir = envir, inherits = FALSE)) {
    cli_msg("Replacing", target, "with an active binding.")
    remove(list = as.character(target), envir = envir)
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
  spec <- config$spec[[subtarget]]
  parent <- spec$subtarget_parent
  index <- spec$subtarget_index
  dynamic <- config$spec[[parent]]$dynamic
  deps <- subtarget_deps(dynamic, parent, index, config)
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
  dynamic <- config$spec[[parent]]$dynamic
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
  parent <- config$spec[[subtarget]]$subtarget_parent
  dynamic <- config$spec[[parent]]$dynamic
  load_dynamic_subdep_impl(dynamic, parent, dep, index, config)
}

load_dynamic_subdep_impl <- function(dynamic, parent, dep, index, config) {
  UseMethod("load_dynamic_subdep_impl")
}

#' @export
load_dynamic_subdep_impl.group <- function( # nolint
  dynamic,
  parent,
  dep,
  index,
  config
) {
  subdeps <- config$cache$get(
    dep,
    namespace = "meta",
    use_cache = FALSE
  )$subtargets[index]
  value <- lapply(subdeps, config$cache$get, use_cache = FALSE)
  value <- do.call(safe_vec_c, value)
  assign(
    x = dep,
    value = value,
    envir = config$envir_subtargets,
    inherits = FALSE
  )
}

#' @export
load_dynamic_subdep_impl.default <- function( # nolint
  dynamic,
  parent,
  dep,
  index,
  config
) {
  subdep <- config$cache$get(
    dep,
    namespace = "meta",
    use_cache = FALSE
  )$subtargets[[index]]
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
  assert_dynamic_grouping_var(dep, value)
  value <- dynamic_subvalue(value, index)
  assign(
    x = dep,
    value = value,
    envir = config$envir_subtargets,
    inherits = FALSE
  )
}

assert_dynamic_grouping_var <- function(dep, value) {
  if (!length(value)) {
    stop0("dynamic grouping variable ", dep, " needs more than 0 elements.")
  }
}

sync_envir_dynamic <- function(target, config) {
  dynamic_whole <- config$spec[[target]]$deps_dynamic_whole
  index <- !vlapply(
    dynamic_whole,
    exists,
    config$envir_dynamic,
    inherits = FALSE
  )
  lapply(dynamic_whole[index], sync_dynamic_whole, config = config)
}

sync_dynamic_whole <- function(target, config) {
  hashes <- get(target, envir = config$envir_targets, inherits = FALSE)
  value <- get_subtargets(hashes, target, config$cache, NULL, FALSE)
  assign(target, value, envir = config$envir_dynamic, inherits = FALSE)
}
