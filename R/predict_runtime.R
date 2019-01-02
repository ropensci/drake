#' @title Predict the elapsed runtime of the next call to `make()`
#'   for non-staged parallel backends.
#' @description Take the past recorded runtimes times from
#'   [build_times()] and use them to predict how the targets
#'   will be distributed among the available workers in the
#'   next [make()]. Then, predict the overall runtime to be the
#'   runtime of the slowest (busiest) workers. See Details for some
#'   caveats.
#' @details The prediction is only a rough approximation.
#'   The algorithm that emulates the workers is not perfect,
#'   and it may turn out to perform poorly in some edge cases.
#'   It also assumes you are using one of the backends with persistent workers
#'   (`"mclapply"`, `"parLapply"`, or `"future_lapply"`),
#'   though the transient worker backends `"future"` and `"Makefile"`
#'   should be similar. The prediction does not apply
#'   to staged parallelism backends such as
#'   `make(parallelism = "mclapply_staged")` or
#'   `make(parallelism = "parLapply_staged")`.
#'   The function also assumes
#'   that the overhead of initializing [make()] and any workers is
#'   negligible. Use the `default_time` and `known_times` arguments
#'   to adjust the assumptions as needed.
#' @export
#' @inheritParams predict_load_balancing
#' @seealso [predict_load_balancing()], [build_times()], [make()]
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' config <- make(my_plan) # Run the project, build the targets.
#' known_times <- c(5, rep(7200, nrow(my_plan) - 1))
#' names(known_times) <- c(file_store("report.md"), my_plan$target[-1])
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
#'   known_times = known_times,
#'   targets_only = TRUE
#' )
#' balance
#' })
#' }
predict_runtime <- function(
  config = drake::read_drake_config(),
  targets = NULL,
  from_scratch = FALSE,
  targets_only = FALSE,
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
#' @details The prediction is only a rough approximation.
#'   The algorithm that emulates the workers is not perfect,
#'   and it may turn out to perform poorly in some edge cases.
#'   It assumes you are using one of the backends with persistent workers
#'   (`"mclapply"`, `"parLapply"`, or `"future_lapply"`),
#'   though the transient worker backends `"future"` and `"Makefile"`
#'   should be similar. The prediction does not apply
#'   to staged parallelism backends such as
#'   `make(parallelism = "mclapply_staged")` or
#'   `make(parallelism = "parLapply_staged")`.
#'   The function also assumes
#'   that the overhead of initializing [make()] and any workers is
#'   negligible. Use the `default_time` and `known_times` arguments
#'   to adjust the assumptions as needed.
#' @export
#' @seealso [predict_runtime()], [build_times()], [make()]
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' config <- make(my_plan) # Run the project, build the targets.
#' known_times <- c(5, rep(7200, nrow(my_plan) - 1))
#' names(known_times) <- c(file_store("report.md"), my_plan$target[-1])
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
#'   known_times = known_times,
#'   targets_only = TRUE
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
#' @param targets_only logical, whether to factor in
#'   just the targets into the calculations or use the
#'   build times for everything, including the imports
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
  targets_only = FALSE,
  jobs = 1,
  known_times = numeric(0),
  default_time = 0,
  warn = TRUE
) {
  names(known_times) <- standardize_key(names(known_times))
  assumptions <- timing_assumptions(
    config = config,
    targets = targets,
    from_scratch = from_scratch,
    targets_only = targets_only,
    jobs = jobs,
    known_times = known_times,
    default_time = default_time,
    warn = warn
  )
  config$schedule <- igraph::induced_subgraph(
    config$graph,
    vids = names(assumptions)
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
  targets_only,
  jobs,
  known_times,
  default_time,
  warn
) {
  assert_pkg("lubridate")
  if (!from_scratch) {
    outdated <- outdated(config)
  }
  times <- build_times(
    cache = config$cache,
    targets_only = targets_only,
    pretty_keys = FALSE
  )
  if (targets_only) {
    config$graph <- targets_graph(config)
  }
  if (!is.null(targets)) {
    config$graph <- prune_drake_graph(config$graph, to = targets)
  }
  times <- times[times$item %in% V(config$graph)$name, ]
  untimed <- setdiff(V(config$graph)$name, times$item)
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
  keep_known_times <- intersect(names(known_times), V(config$graph)$name)
  known_times <- known_times[keep_known_times]
  names <- igraph::V(config$graph)$name
  assumptions <- rep(default_time, length(names))
  names(assumptions) <- names
  assumptions[times$item] <- times$elapsed
  assumptions[names(known_times)] <- known_times
  if (!from_scratch) {
    skip <- setdiff(config$plan$target, outdated)
    assumptions[skip] <- 0
  }
  assumptions
}
