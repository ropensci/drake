#' @title Predict the runtime of the next call to `make()`.
#' @description Give a rough estimate of the next run of `make()`
#' using a longest-path graph algorithm. This is only a rough guess.
#' Untimed targets are assumed to have a runtime of 0, so be warned.
#' @export
#' @seealso [build_times()], [make()]
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
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
#' @param future_jobs deprecated
#' @param digits number of digits for rounding the time
#' @param type character scalar: `"elapsed"`, `"user"`, or `"system"`
predict_runtime <- function(
  config = drake::read_drake_config(),
  targets = NULL,
  from_scratch = FALSE,
  targets_only = FALSE,
  future_jobs = NULL,
  digits = 3,
  type = c("elapsed", "user", "system")
){
  if (!is.null(future_jobs)){
    warning(
      "The future_jobs argument is deprecated. ",
      "Because staged parallelism was removed in drake ",
      "and replaced with a much better parallel algorithm, ",
      "there was no point in keeping it.",
      call. = FALSE
    )
  }
  times <- build_times(
    config = config,
    targets = targets,
    from_scratch = from_scratch,
    targets_only = targets_only
  )
  type <- match.arg(type)
  times <- times[times$item %in% V(graph)$name, ]
  times$time <- as.numeric(times[[type]])
  untimed <- setdiff(V(config$graph)$name, times$item)
  if (targets_only){
    untimed <- setdiff(untimed, times$item[times$type == "import"])
  }
  if (length(untimed)){
    warning(
      "Untimed targets not factored into runtime prediction:\n",
      multiline_message(untimed),
      call. = FALSE
    )
  }
  graph <- igraph::set_vertex_attr(graph, name = "weight", value = 0) %>%
    igraph::set_vertex_attr(
      name = "weight",
      index = times$item,
      value = -times$time
    )
  path <- shortest_path(graph)
  sum(times[path, "time"])
}

shortest_path = function(graph){
  

}
