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
#' @param targets character vector of targets
#' @param config [drake_config()] list
#' @param downstream optional, character vector of any targets
#'   assumed to be downstream.
#' @param jobs number of jobs for local parallel computing
#' @examples
#' # Users should use make().
manage_memory <- function(targets, config, downstream = NULL, jobs = 1) {
  if (identical(config$memory_strategy, "lookahead")) {
    if (is.null(downstream)) {
      downstream <- downstream_nodes(
        from = targets,
        graph = config$graph,
        jobs = jobs
      )
    }
    downstream_deps <- target_graph_dependencies(
      targets = downstream,
      config = config,
      jobs = jobs
    )
  } else {
    downstream <- downstream_deps <- NULL
  }
  already_loaded <- ls(envir = config$eval, all.names = TRUE)
  target_deps <- target_graph_dependencies(
    targets = targets,
    config = config,
    jobs = jobs
  )
  if (!identical(config$memory_strategy, "speed")) {
    keep_these <- c(target_deps, downstream_deps)
    discard_these <- setdiff(x = config$plan$target, y = keep_these)
    # TODO: remove for version 7.0.0
    discard_these <- parallel_filter(
      discard_these,
      f = not_encoded_path,
      jobs = jobs
    )
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
  safe_load(targets = targets, config = config, jobs = jobs)
}

safe_load <- function(targets, config, jobs = 1) {
  targets <- exclude_unloadable(
    targets = targets,
    config = config,
    jobs = jobs
  )
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
      FUN = load_target,
      namespace = config$cache$default_namespace,
      envir = config$eval,
      cache = config$cache,
      verbose = FALSE,
      lazy = config$lazy_load
    )
  }
  invisible()
}

ensure_loaded <- function(targets, config) {
  targets <- Filter(
    x = targets,
    f = function(target) {
      !exists(x = target, envir = config$eval, inherits = FALSE)
    }
  )
  safe_load(targets = targets, config = config)
}

get_import_from_memory <- function(target, envir) {
  target <- decode_namespaced(target)
  if (is_encoded_path(target)) {
    return(NULL)
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
    if (exists(x = target, envir = envir, inherits = FALSE)) {
      out <- get(x = target, envir = envir, inherits = FALSE)
    } else {
      NA_character_
    }
  }
}

exclude_unloadable <- function(targets, config, jobs = jobs) {
  unloadable <- parallel_filter(
    x = targets,
    f = function(target) {
      !config$cache$exists(key = target)
    },
    jobs = jobs
  )
  if (length(unloadable)) {
    warning(
      "unable to load required dependencies:\n",
      multiline_message(targets),
      call. = FALSE
    )
  }
  setdiff(targets, unloadable)
}

#' @title Get the environment where drake builds targets
#' @description Call this function inside the commands in your plan
#'   to get the environment where `drake` builds targets.
#'   That way, you can strategically remove targets from memory
#'   while [make()] is running. That way, you can limit the
#'   amount of computer memory you use.
#' @export
#' @seealso [make()], [drake_plan()], [target()]
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
#'   },
#'   strings_in_dots = "literals"
#' )
#' make(plan, cache = storr::storr_environment(), session_info = FALSE)
drake_envir <- function() {
  envir <- environment()
  for (i in seq_len(getOption("expressions"))) {
    if (identical(envir[[drake_envir_marker]], TRUE)) {
      return(envir)
    }
    if (identical(envir, globalenv())) {
      break # nocov
    }
    envir <- parent.frame(n = i)
  }
  warning(
    "Could not find the environment where drake builds targets. ",
    "drake_envir() should only be called inside commands ",
    "in your workflow plan data frame.",
    call. = FALSE
  )
}

missing_import <- function(x, config) {
  if (is_encoded_path(x)) {
    return(!file.exists(decoded_path(x, config)))
  }
  x <- decode_namespaced(x)
  identical(get_import_from_memory(x, envir = config$envir), NA_character_)
}
