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
  if (identical(config$memory_strategy, "lookahead")) {
    downstream <- downstream %||% downstream_nodes(config$graph, targets)
    downstream_deps <- deps_memory(targets = downstream, config = config)
  } else {
    downstream <- downstream_deps <- NULL
  }
  already_loaded <- names(config$eval)
  target_deps <- deps_memory(targets = targets, config = config)
  if (!identical(config$memory_strategy, "speed")) {
    keep_these <- c(target_deps, downstream_deps)
    discard_these <- setdiff(x = config$plan$target, y = keep_these)
    discard_these <- intersect(discard_these, already_loaded)
    if (length(discard_these)) {
      console_many_targets(
        discard_these,
        pattern = "unload",
        config = config
      )
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
      console_many_targets(
        targets,
        pattern = "load",
        config = config
      )
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
    stopifnot(deparse(lang[[1]]) %in% c("::", ":::"))
    pkg <- deparse(lang[[2]])
    fun <- deparse(lang[[3]])
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

#' @title Get the environment where drake builds targets
#' @description Call this function inside the commands in your plan
#'   to get the environment where `drake` builds targets.
#'   That way, you can strategically remove targets from memory
#'   while [make()] is running. That way, you can limit the
#'   amount of computer memory you use.
#' @export
#' @seealso [from_plan()]
#' @return The environment where `drake` builds targets.
#' @examples
#' plan <- drake_plan(
#'   large_data_1 = sample.int(1e4),
#'   large_data_2 = sample.int(1e4),
#'   subset = c(large_data_1[seq_len(10)], large_data_2[seq_len(10)]),
#'   summary = {
#'     print(ls(envir = drake_envir()))
#'     # We don't need the large_data_* targets in memory anymore.
#'     rm(large_data_1, large_data_2, envir = drake_envir())
#'     print(ls(envir = drake_envir()))
#'     mean(subset)
#'   }
#' )
#' make(plan, cache = storr::storr_environment(), session_info = FALSE)
drake_envir <- function() {
  envir <- environment()
  for (i in seq_len(getOption("expressions"))) {
    if (exists(drake_plan_marker, envir = envir, inherits = FALSE)) {
      return(envir)
    }
    if (identical(envir, globalenv())) {
      break # nocov
    }
    envir <- parent.frame(n = i)
  }
  stop(
    "Could not find the environment where drake builds targets. ",
    "drake_envir() should only be called inside commands ",
    "in your workflow plan data frame.",
    call. = FALSE
  )
}

#' @title Get a target's plan info from inside a plan's command.
#' @description Call this function inside the commands in your plan
#'   to get an entry from any column in the plan. Changes to custom
#'   columns referred to this way
#'   (for example, `a` in `drake_plan(x = target(from_plan("a"), a = 123))`)
#'   do not invalidate targets, so be careful. Only use `from_plan()`
#'   to reference data that does not actually affect the output value
#'   of the target. For example, you might use `from_plan()` to set the
#'   number of parallel workers within a target:
#'   `drake_plan(x = target(mclapply(..., from_plan("cores"), cores = 4))`.
#'   Now, if you change the `cores` column of the plan, the parallelism will
#'   change, but the target `x` will stay up to date.
#' @export
#' @seealso [drake_envir()]
#' @return `plan[target, column]`, where `plan` is your workflow
#'   plan data frame, `target` is the target being built,
#'   and `column` is the name of the column of the plan you provide.
#' @param column Character, name of a column in your `drake` plan.
#' @examples
#' plan <- drake_plan(my_target = target(from_plan("a"), a = "a_value"))
#' plan
#'
#' cache <- storr::storr_environment()
#' make(plan, cache = cache, session_info = FALSE)
#' readd(my_target, cache = cache)
#'
#' # Why do we care?
#' # Because we can customize parallel computing within targets this way.
#' # Notice how we treat `mc.cores` for `mclapply()`
#' plan <- drake_plan(
#'   a = target(
#'     mclapply(1:8, my_function, mc.cores = from_plan("cores")),
#'     cores = 4
#'   ),
#'   b = target(
#'     mclapply(1:4, my_function, mc.cores = from_plan("cores")),
#'     cores = 2
#'   )
#' )
#'
#' plan
#'
#' # make() # nolint
#'
#' # Now, if you change the `cores` column of the plan, the parallelism will
#' # change, but the targets will stay up to date.
#' plan$cores <- c(1, 1)
#' plan
#'
#' # make(plan) # nolint
from_plan <- function(column) {
  envir <- drake_envir()
  plan <- get(drake_plan_marker, envir = envir)
  target <- get(drake_target_marker, envir = envir)
  plan[[column]][plan$target == target]
}

drake_plan_marker <- "._drake_plan"
drake_target_marker <- "._drake_target"
