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
#' @seealso [build_times()], [make()]
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' config <- make(my_plan) # Run the project, build the targets.
#' # The predictions use the cached build times of the targets,
#' # but if you expect your target runtimes
#' # to be different, you can specify them (in seconds).
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
#' # Why isn't 8 jobs any better?
#' # 8 would be a good guess: https://ropensci.github.io/drake/images/outdated.html # nolint
#' # It's because of load balancing.
#' # Below, each row is a persistent worker.
#' balance <- predict_load_balancing(
#'   config,
#'   jobs = 7,
#'   from_scratch = TRUE,
#'   known_times = known_times,
#'   targets_only = TRUE
#' )
#' balance
#' max(balance$time)
#' # Each worker gets 2 rate-limiting targets.
#' balance$time
#' # Even if you add another worker, there will be still be workers
#' # with two heavy targets.
#' })
#' }
predict_runtime <- function(
  config = drake::read_drake_config(),
  targets = NULL,
  from_scratch = FALSE,
  targets_only = FALSE,
  future_jobs = NULL,
  digits = NULL,
  jobs = 1,
  known_times = numeric(0),
  default_time = 0,
  warn = TRUE
){
  balance <- predict_load_balancing(
    config = config,
    targets = targets,
    from_scratch = from_scratch,
    targets_only = targets_only,
    future_jobs = future_jobs,
    digits = digits,
    jobs = jobs,
    known_times = known_times,
    default_time = default_time,
    warn = warn
  )
  balance$total_runtime
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
#' @seealso [build_times()], [make()]
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' config <- make(my_plan) # Run the project, build the targets.
#' # The predictions use the cached build times of the targets,
#' # but if you expect your target runtimes
#' # to be different, you can specify them (in seconds).
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
#' # Why isn't 8 jobs any better?
#' # 8 would be a good guess: https://ropensci.github.io/drake/images/outdated.html # nolint
#' # It's because of load balancing.
#' # Below, each row is a persistent worker.
#' balance <- predict_load_balancing(
#'   config,
#'   jobs = 7,
#'   from_scratch = TRUE,
#'   known_times = known_times,
#'   targets_only = TRUE
#' )
#' balance
#' max(balance$time)
#' # Each worker gets 2 rate-limiting targets.
#' balance$time
#' # Even if you add another worker, there will be still be workers
#' # with two heavy targets.
#' })
#' }
#' @return A list with (1) the total runtime and (2) a list
#'   of the names of the targets assigned to each worker.
#'   For each worker, targets are listed in the order they are assigned.
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
#' @param warn logical, whether to warn the user about
#'   any targets with no available runtime, either in
#'   `known_times` or [build_times()]. The times for these
#'   targets default to `default_time`.
predict_load_balancing <- function(
  config = drake::read_drake_config(),
  targets = NULL,
  from_scratch = FALSE,
  targets_only = FALSE,
  future_jobs = NULL,
  digits = NULL,
  jobs = 1,
  known_times = numeric(0),
  default_time = 0,
  warn = TRUE
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
  untimed <- setdiff(V(config$graph)$name, times$item) %>%
    setdiff(y = names(known_times))
  if (length(untimed)){
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

# Taken directly from mc_master()
# and modified to simulate the workers.
balance_load <- function(config, jobs){
  # Mostly the same setup for mc_master().
  config$cache <- storr::storr_environment()
  config$jobs <- jobs
  config$workers <- as.character(seq_len(config$jobs))
  config$schedule <- config$graph
  config$queue <- new_target_queue(config = config)
  mc_init_worker_cache(config)
  lapply(config$workers, mc_set_ready, config = config)
  targets <- lapply(config$workers, function(id){
    character(0)
  })
  total_runtime <- 0
  step_times <- rep(0, config$jobs)
  names(step_times) <- names(targets) <- config$workers
  while (mc_work_remains(config)){
    is_running <- vapply(
      X = config$workers,
      FUN = mc_is_running,
      FUN.VALUE = logical(1),
      config = config
    )
    # Simulate how the master process waits for the next worker to finish.
    if (any(is_running)){
      next_idle_worker <- names(which.min(step_times[is_running]))
      mc_set_idle(worker = next_idle_worker, config = config)
      step <- min(step_times[is_running])
      step_times[is_running] <- step_times[is_running] - step
      total_runtime <- total_runtime + step
    }
    # Mostly the same as the main loop of mc_master()
    for (worker in config$workers){
      if (mc_is_idle(worker = worker, config = config)){
        mc_conclude_target(worker = worker, config = config)
        step_times[worker] <- 0 # Added: make sure to reset the worker's time.
        if (!config$queue$size()){
          mc_set_done(worker = worker, config = config)
          next
        }
        target <- config$queue$pop0(what = "names")
        if (length(target)){
          # Added line: get the time that the worker will spend on the target.
          step_times[worker] <- igraph::vertex_attr(
            graph = config$graph,
            name = "time",
            index = target
          )
          # Added line: record that the target was assigned.
          targets[[worker]] <- c(targets[[worker]], target)
          mc_set_target(worker = worker, target = target, config = config)
          mc_set_running(worker = worker, config = config)
        }
      }
    }
  }
  eval(parse(text = "require(methods, quietly = TRUE)")) # needed for lubridate
  list(
    total_runtime = lubridate::dseconds(total_runtime),
    targets_per_worker = targets
  )
}
