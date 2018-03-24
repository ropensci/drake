#' @title Predict the elapsed runtime of the next call to `make()`.
#' @description This function simply sums the elapsed build times
#' from [rate_limiting_times()], and this
#' feature is experimental.
#' To date, the accuracy/precision of the results
#' has not yet been confirmed by performance studies.
#' @details For the results to make sense, the previous build times
#' of all targets need to be available (automatically cached
#' by [make()]). Otherwise, `predict_runtime()`
#' will warn you and tell you which targets have missing times.
#' @export
#' @seealso [rate_limiting_times()],
#'   [build_times()]
#'   [make()]
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Get the code with drake_example("basic").
#' config <- make(my_plan) # Run the project, build the targets.
#' predict_runtime(config, digits = 4) # Everything is up to date.
#' predict_runtime(config, digits = 4, from_scratch = TRUE) # 1 job
#' # Assumes you clean() out your project and start from scratch with 2 jobs.
#' predict_runtime(config, future_jobs = 2, digits = 4, from_scratch = TRUE)
#' # Predict the runtime of just building targets
#' # "small" and "large".
#' predict_runtime(
#'   config,
#'   targets = c("small", "large"),
#'   from_scratch = TRUE,
#'   digits = 4
#' )
#' })
#' }
#' @return A `lubridate` `Duration` object
#'   with the predicted runtime of the next [make()].
#' @param config option internal runtime parameter list of
#'   \code{\link{make}(...)},
#'   produced by both [make()] and
#'   [drake_config()].
#' @param targets Character vector, names of targets.
#'   Predict the runtime of building these targets
#'   plus dependencies.
#'   Defaults to all targets.
#' @param from_scratch logical, whether to predict a
#'   [make()] build from scratch or to
#'   take into account the fact that some targets may be
#'   already up to date and therefore skipped.
#' @param targets_only logical, whether to factor in
#'   just the targets into the calculations or use the
#'   build times for everything, including the imports
#' @param future_jobs hypothetical number of jobs
#'   assumed for the predicted runtime.
#'   assuming this number of jobs.
#' @param digits number of digits for rounding the time
predict_runtime <- function(
  config = drake::read_drake_config(),
  targets = NULL,
  future_jobs = 1,
  from_scratch = FALSE,
  targets_only = FALSE,
  digits = 3
){
  eval(parse(text = "require(methods, quietly = TRUE)")) # needed for lubridate
  times <- rate_limiting_times(
    config = config,
    targets = targets,
    from_scratch = from_scratch,
    targets_only = targets_only,
    future_jobs = future_jobs,
    digits = Inf
  )
  sum(times$elapsed) %>%
    round(digits = digits) %>%
    to_build_duration
}

#' @title Return a data frame of elapsed build times of
#'   the rate-limiting targets of a drake project.
#'
#' @description This function produces a conservative
#' estimate for [predict_runtime()]
#' for when parallel computing is used in [make()].
#' This feature is experimental.
#' The accuracy, precision, and utility of these supposedly
#' rate-limiting times has not been confirmed by rigorous
#' performance studies.
#'
#' @export
#'
#' @details The `stage` column of the returned data frame
#' is an index that denotes a parallelizable stage.
#' Within each stage during [make()],
#' the targets are divided among the available jobs.
#' For `rate_limiting_times()`,
#' we assume the targets are divided evenly among the jobs
#' and one job gets all the slowest targets.
#' The build times of this hypothetical pessimistic job
#' are returned for each stage. \cr
#'
#' By default `from_scratch` is `FALSE`. That way,
#' `rate_limiting_times()` takes into account that some
#' targets are already up to date, meaning their elapsed
#' build times will be instant during the next [make()].
#'
#' For the results to make sense, the previous build times
#' of all targets need to be available (automatically cached
#' by [make()]). Otherwise, `rate_limiting_times()`
#' will warn you and tell you which targets have missing times.
#'
#' @seealso [predict_runtime()],
#'   [build_times()]
#'   [make()]
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Get the code with drake_example("basic").
#' config <- make(my_plan) # Run the project, build the targets.
#' rate_limiting_times(config) # Everything is up to date.
#' # Assume everything runs from scratch with 1 job.
#' rate_limiting_times(config, from_scratch  = TRUE, digits = 4)
#' # With 2 jobs, some of the targets are not rate-limiting.
#' rate_limiting_times(
#'   config,
#'   future_jobs = 2,
#'   from_scratch = TRUE,
#'   digits = 4
#' )
#' # Find the rate-limiting times of just building targets
#' # "small" and "large".
#' rate_limiting_times(
#'   config,
#'   targets = c("small", "large"),
#'   from_scratch = TRUE,
#'   digits = 4
#' )
#' })
#' }
#'
#' @return A data frame of times of the worst-case scenario
#'   rate-limiting targets in each parallelizable stage.
#'
#' @param config option internal runtime parameter list of
#'   \code{\link{make}(...)},
#'   produced by both [make()] and
#'   [drake_config()].
#'
#' @param targets Character vector, names of targets.
#'   Find the rate-limiting times for building these targets
#'   plus dependencies.
#'   Defaults to all targets.
#'
#' @param from_scratch logical, whether to assume
#'   next hypothetical call to [make()]
#'   is a build from scratch (after [clean()]).
#'
#' @param targets_only logical, whether to factor in just the
#'   targets or use times from everything, including the imports.
#'
#' @param future_jobs hypothetical number of jobs
#'   assumed for the predicted runtime.
#'   assuming this number of jobs.
#'
#' @param digits number of digits for rounding the times.
rate_limiting_times <- function(
  config = drake::read_drake_config(),
  targets = NULL,
  from_scratch = FALSE,
  targets_only = FALSE,
  future_jobs = 1,
  digits = 3
){
  if (!is.null(targets)){
    config$targets <- targets
    config$graph <- get_neighborhood(
      graph = config$graph,
      from = config$targets,
      mode = "in",
      order = numeric(0)
    )
  }
  times <- build_times(
    cache = config$cache,
    digits = Inf,
    targets_only = FALSE,
    verbose = config$verbose
  ) %>%
    as.data.frame
  keys <- V(config$graph)$name
  import_keys <- V(imports_graph(config$graph))$name
  items <- intersect(keys, times$item)
  not_timed <- setdiff(keys, items)
  warn_not_timed(not_timed)
  if (!nrow(times)){
    return(cbind(times, stage = numeric(0)))
  }
  rownames(times) <- times$item
  times <- times[intersect(items, keys), ]
  if (targets_only){
    times <- times[times$type == "target", ]
  }
  if (!nrow(times)){
    return(cbind(times, stage = numeric(0)))
  }
  if (from_scratch){
    config$trigger <- "always"
  }
  times <- append_stages_to_times(x = times, config = config)
  out <- ddply(
    times,
    "stage",
    rate_limiting_at_stage,
    future_jobs = future_jobs
  ) %>%
    round_times(digits = digits) %>%
    unname_rows
  out <- out[!is.na(out$item), ]
  tryCatch(
    as_tibble(out),
    error = error_tibble_times
  )
}

append_stages_to_times <- function(x, config){
  nodes <- parallel_stages(config = config)
  rownames(nodes) <- nodes$item
  items <- intersect(nodes$item, x$item)
  nodes <- nodes[items, ]
  x <- x[items, ]
  x$stage <- nodes$stage
  x
}

rate_limiting_at_stage <- function(stage, future_jobs){
  stage <- stage[order(stage$elapsed, decreasing = TRUE), ]
  targets_per_job <- ceiling(nrow(stage) / future_jobs) %>%
    min(nrow(stage))
  stage[seq_len(targets_per_job), ]
}

warn_not_timed <- function(not_timed){
  if (!length(not_timed)){
    return()
  }
  warning(
    "Some targets have never been built ",
    "and their times are ignored:\n",
    multiline_message(not_timed),
    call. = FALSE
  )
}

unname_rows <- function(x){
  rownames(x) <- NULL
  x
}
