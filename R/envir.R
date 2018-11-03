assign_to_envir <- function(target, value, config){
  if (
    identical(config$lazy_load, "eager") &&
    !is_file(target) &&
    target %in% config$plan$target
  ){
    assign(x = target, value = value, envir = config$envir)
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
#' @return nothing
#' @param targets character vector of targets
#' @param config [drake_config()] list
#' @param downstream optional, character vector of any targets
#'   assumed to be downstream.
#' @param jobs number of jobs for local parallel computing
#' @examples
#' # Users should use make().
manage_memory <- function(targets, config, downstream = NULL, jobs = 1){
  if (identical(config$memory_strategy, "lookahead")){
    if (is.null(downstream)){
      downstream <- downstream_nodes(
        from = targets,
        graph = config$graph,
        jobs = jobs
      )
    }
    downstream_deps <- nonfile_target_dependencies(
      targets = downstream,
      config = config,
      jobs = jobs
    )
  } else {
    downstream <- downstream_deps <- NULL
  }
  already_loaded <- ls(envir = config$envir, all.names = TRUE) %>%
    intersect(y = config$plan$target)
  target_deps <- nonfile_target_dependencies(
    targets = targets,
    config = config,
    jobs = jobs
  )
  if (!identical(config$memory_strategy, "speed")){
    keep_these <- c(target_deps, downstream_deps)
    discard_these <- setdiff(x = config$plan$target, y = keep_these) %>%
      parallel_filter(f = is_not_file, jobs = jobs) %>%
      intersect(y = already_loaded)
    if (length(discard_these)){
      console_many_targets(
        discard_these,
        pattern = "unload",
        config = config
      )
      rm(list = discard_these, envir = config$envir)
    }
  }
  setdiff(target_deps, targets) %>%
    setdiff(y = already_loaded) %>%
    safe_load(config = config, jobs = jobs)
}

safe_load <- function(targets, config, jobs = 1){
  targets <- exclude_unloadable(
    targets = targets, config = config, jobs = jobs)
  if (length(targets)){
    if (config$lazy_load == "eager"){
      console_many_targets(
        targets,
        pattern = "load",
        config = config
      )
    }
    loadd(
      list = targets,
      envir = config$envir,
      cache = config$cache,
      verbose = FALSE,
      lazy = config$lazy_load
    )
  }
  invisible()
}

ensure_loaded <- function(targets, config){
  already_loaded <- ls(envir = config$envir, all.names = TRUE) %>%
    intersect(y = config$plan$target)
  setdiff(targets, already_loaded) %>%
    Filter(f = is_not_file) %>%
    safe_load(config = config)
}

flexible_get <- function(target, envir) {
  stopifnot(length(target) == 1)
  parsed <- parse(text = target) %>%
    as.call %>%
    as.list
  lang <- parsed[[1]]
  is_namespaced <- length(lang) > 1
  if (!is_namespaced){
    return(get(x = target, envir = envir, inherits = FALSE))
  }
  stopifnot(deparse(lang[[1]]) %in% c("::", ":::"))
  pkg <- deparse(lang[[2]])
  fun <- deparse(lang[[3]])
  get(fun, envir = getNamespace(pkg))
}

exclude_unloadable <- function(targets, config, jobs = jobs){
  unloadable <- parallel_filter(
    x = targets,
    f = function(target){
      !config$cache$exists(key = target)
    },
    jobs = jobs
  )
  if (length(unloadable)){
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
#' @return the environment where `drake` builds targets
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
drake_envir <- function(){
  envir <- environment()
  for (i in seq_len(getOption("expressions"))){
    if (identical(envir[[drake_envir_marker]], TRUE)){
      return(envir)
    }
    if (identical(envir, globalenv())){
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
