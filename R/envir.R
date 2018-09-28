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

#' @title Prune the evaluation environment
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
prune_envir <- function(targets, config, downstream = NULL, jobs = 1){
  if (identical(config$pruning_strategy, "lookahead")){
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
  if (!identical(config$pruning_strategy, "speed")){
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
