#' @title Predict the elapsed runtime of the next call to `make()`
#'   for non-staged parallel backends.
#' @description Take the past recorded runtimes times from
#'   [build_times()] and use them to predict how the targets
#'   will be distributed among the available workers in the
#'   next [make()]. Then, predict the overall runtime to be the
#'   runtime of the slowest (busiest) workers.
#'   Predictions only include the time it takes to run the targets,
#'   not overhead/preprocessing from `drake` itself.
#' @export
#' @inheritParams predict_load_balancing
#' @seealso [predict_load_balancing()], [build_times()], [make()]
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' config <- drake_config(my_plan)
#' known_times <- rep(7200, nrow(my_plan))
#' names(known_times) <- my_plan$target
#' known_times
#' # Predict the runtime
#' predict_runtime(
#'   config,
#'   jobs = 7,
#'   from_scratch = TRUE,
#'   known_times = known_times
#' )
#' predict_runtime(
#'   config,
#'   jobs = 8,
#'   from_scratch = TRUE,
#'   known_times = known_times
#' )
#' balance <- predict_load_balancing(
#'   config,
#'   jobs = 7,
#'   from_scratch = TRUE,
#'   known_times = known_times
#' )
#' balance
#' })
#' }
predict_runtime <- function(
  config = drake::read_drake_config(),
  targets = NULL,
  from_scratch = FALSE,
  targets_only = NULL,
  jobs = 1,
  known_times = numeric(0),
  default_time = 0,
  warn = TRUE
) {
  predict_load_balancing(
    config = config,
    targets = targets,
    from_scratch = from_scratch,
    targets_only = targets_only,
    jobs = jobs,
    known_times = known_times,
    default_time = default_time,
    warn = warn
  )$time
}

#' @title Predict the load balancing of the next call to `make()`
#'   for non-staged parallel backends.
#' @description Take the past recorded runtimes times from
#'   [build_times()] and use them to predict how the targets
#'   will be distributed among the available workers in the
#'   next [make()].
#'   Predictions only include the time it takes to run the targets,
#'   not overhead/preprocessing from `drake` itself.
#' @export
#' @seealso [predict_runtime()], [build_times()], [make()]
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' config <- drake_config(my_plan)
#' known_times <- rep(7200, nrow(my_plan))
#' names(known_times) <- my_plan$target
#' known_times
#' # Predict the runtime
#' predict_runtime(
#'   config = config,
#'   jobs = 7,
#'   from_scratch = TRUE,
#'   known_times = known_times
#' )
#' predict_runtime(
#'   config,
#'   jobs = 8,
#'   from_scratch = TRUE,
#'   known_times = known_times
#' )
#' balance <- predict_load_balancing(
#'   config,
#'   jobs = 7,
#'   from_scratch = TRUE,
#'   known_times = known_times
#' )
#' balance
#' })
#' }
#' @return A list with (1) the total runtime and (2) a list
#'   of the names of the targets assigned to each worker.
#'   For each worker, targets are listed in the order they are assigned.
#' @param config option internal runtime parameter list of
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
#' @param targets_only deprecated
#' @param jobs the `jobs` argument of your next planned
#'   `make()`. How many targets to do you plan
#'   to have running simultaneously?
#' @param known_times a named numeric vector with targets/imports
#'   as names and values as hypothetical runtimes in seconds.
#'   Use this argument to overwrite any of the existing build times
#'   or the `default_time`.
#' @param default_time number of seconds to assume for any
#'   target or import with no recorded runtime (from [build_times()])
#'   or anything in `known_times`.
#' @param warn logical, whether to warn the user about
#'   any targets with no available runtime, either in
#'   `known_times` or [build_times()]. The times for these
#'   targets default to `default_time`.
predict_load_balancing <- function(
  config = drake::read_drake_config(),
  targets = NULL,
  from_scratch = FALSE,
  targets_only = NULL,
  jobs = 1,
  known_times = numeric(0),
  default_time = 0,
  warn = TRUE
) {
  deprecate_targets_only(targets_only) # 2019-01-03 # nolint
  config$schedule <- targets_graph(config)
  assumptions <- timing_assumptions(
    config = config,
    targets = targets,
    from_scratch = from_scratch,
    jobs = jobs,
    known_times = known_times,
    default_time = default_time,
    warn = warn
  )
  queue <- new_priority_queue(config, jobs = 1)
  running <- data.frame(
    target = character(0),
    time = numeric(0),
    worker = integer(0),
    stringsAsFactors = FALSE
  )
  time <- 0
  workers <- replicate(jobs, character(0))
  while (!queue$empty() || nrow(running)) {
    while (length(queue$peek0()) && nrow(running) < jobs) {
      new_target <- queue$pop0()
      running <- rbind(running, data.frame(
        target = new_target,
        time = assumptions[new_target],
        worker = min(which(!(seq_len(jobs) %in% running$worker))),
        stringsAsFactors = FALSE
      ))
    }
    running <- running[order(running$time), ]
    time <- time + running$time[1]
    running$time <- running$time - running$time[1]
    workers[[running$worker[1]]] <- c(
      workers[[running$worker[1]]],
      running$target[1]
    )
    decrease_revdep_keys(
      queue = queue,
      target = running$target[1],
      config = config
    )
    running <- running[-1, ]
  }
  list(time = lubridate::dseconds(time), workers = workers)
}

timing_assumptions <- function(
  config,
  targets,
  from_scratch,
  jobs,
  known_times,
  default_time,
  warn
) {
  assert_pkg("lubridate")
  if (!from_scratch) {
    outdated <- outdated(config)
  }
  times <- build_times(cache = config$cache)
  if (!is.null(targets)) {
    config$schedule <- prune_drake_graph(config$schedule, to = targets)
  }
  times <- times[times$target %in% V(config$schedule)$name, ]
  untimed <- setdiff(V(config$schedule)$name, times$target)
  untimed <- setdiff(untimed, names(known_times))
  if (length(untimed)) {
    warning(
      "Some targets were never actually timed, ",
      "And no hypothetical time was specified in `known_times`. ",
      "Assuming a runtime of ",
      default_time, " for these targets:\n",
      multiline_message(untimed),
      call. = FALSE
    )
  }
  keep_known_times <- intersect(names(known_times), V(config$schedule)$name)
  known_times <- known_times[keep_known_times]
  names <- igraph::V(config$schedule)$name
  assumptions <- rep(default_time, length(names))
  names(assumptions) <- names
  assumptions[times$target] <- times$elapsed
  assumptions[names(known_times)] <- known_times
  if (!from_scratch) {
    skip <- setdiff(config$plan$target, outdated)
    assumptions[skip] <- 0
  }
  assumptions
}
