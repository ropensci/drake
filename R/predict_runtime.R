#' @title Predict the elapsed runtime of the next call to `make()`.
#' @description Take the past recorded runtimes times from
#'   [build_times()] and use them to predict how the targets
#'   will be distributed among the available workers in the
#'   next [make()]. Then, predict the overall runtime to be the
#'   runtime of the slowest (busiest) workers. See Details for some
#'   caveats.
#' @details The prediction is only a rough approximation. It assumes
#'   that the overhead of initializing [make()] and any workers is
#'   negligible. Use the `default_time` and `known_times` arguments
#'   to adjust the assumptions as needed.
#' @export
#' @seealso [build_times()], [make()]
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' config <- make(my_plan) # Run the project, build the targets.
#' predict_runtime(config, from_scratch = TRUE) # 1 job
#' # Assumes you clean() out your project and start from scratch with 2 jobs.
#' predict_runtime(config, jobs = 4, from_scratch = TRUE)
#' # Predict the runtime of just building targets
#' # "small" and "large".
#' predict_runtime(
#'   config,
#'   targets = c("small", "large"),
#'   from_scratch = TRUE
#' )
#' })
#' }
#' @return Simulated runtimes. This is a single `lubridate` duration
#'   (seconds) if `jobs` is 1 and a vector if `jobs` is any larger.
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
#' @param jobs the `jobs` argument of your next planned
#'   `make()`. How many targets to do you plan
#'   to have running simultaneously?
#' @param future_jobs deprecated
#' @param digits deprecated
#' @param known_times a named numeric vector with targets/imports
#'   as names and values as hypothetical runtimes in seconds.
#'   Use this argument to overwrite any of the existing build times
#'   or the `default_time`.
#' @param default_time number of seconds to assume for any
#'   target or import with no recorded runtime (from [build_times()])
#'   or anything in `known_times`.
predict_runtime <- function(
  config = drake::read_drake_config(),
  targets = NULL,
  from_scratch = FALSE,
  targets_only = FALSE,
  future_jobs = NULL,
  digits = NULL,
  jobs = 1,
  known_times = numeric(0),
  default_time = 0
){
  if (!is.null(future_jobs) || !is.null(digits)){
    warning(
      "The `future_jobs` and `digits` arguments ",
      "of predict_runtime() are deprecated.",
      call. = FALSE
    )
  }
  times <- build_times(
    config = config,
    targets_only = targets_only
  )
  if (targets_only){
    config$graph <- targets_graph(config)
  }
  times <- times[times$item %in% V(config$graph)$name, ]
  untimed <- setdiff(V(config$graph)$name, times$item)
  if (length(untimed)){
    warning(
      "Some targets were never actually timed, ",
      "And no hypothetical time was specified in `known_times`",
      "Assuming a runtime of ",
      default_time, " for these targets:\n",
      multiline_message(untimed),
      call. = FALSE
    )
  }
  keep_known_times <- intersect(names(known_times), V(config$graph)$name)
  known_times <- known_times[keep_known_times]
  config$graph <- igraph::set_vertex_attr(
    config$graph,
    name = "time",
    value = default_time
  ) %>%
    igraph::set_vertex_attr(
      name = "time",
      index = times$item,
      value = times$elapsed
    ) %>%
    igraph::set_vertex_attr(
      name = "time",
      index = names(known_times),
      value = known_times
    )
  if (!from_scratch){
    skip <- setdiff(config$plan$target, outdated(config))
    config$graph <- igraph::set_vertex_attr(
      config$graph,
      name = "time",
      index = skip,
      value = 0
    )
  }
  if (!is.null(targets)){
    config$graph <- prune_drake_graph(config$graph, to = targets)
  }
  balance_load(config = config, jobs = jobs)
}

balance_load <- function(config, jobs){
  config$schedule <- config$graph
  queue <- new_target_queue(config)
  worker_targets <- lapply(seq_len(jobs), function(id){
    character(0)
  })
  worker_times <- rep(0, jobs)
  while (!queue$empty()){
    target <- queue$pop()
    i <- which.min(worker_times)
    worker_targets[[i]] <- c(worker_targets[[i]], target)
    worker_times[i] <- worker_times[i] +
      V(config$graph)$time[V(config$graph)$name == target]
    revdeps <- dependencies(
      targets = target,
      config = config,
      reverse = TRUE
    ) %>%
      intersect(y = queue$list(what = "names"))
    queue$decrease_key(names = revdeps)
  }
  dseconds(max(worker_times))
}
