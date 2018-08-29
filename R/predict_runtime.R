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
#' # 8 would be a good guess based on the layout of the workflow graph.
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
#' @seealso [predict_runtime()], [build_times()], [make()]
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
#' # 8 would be a good guess based on the layout of the workflow graph.
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
  assert_pkg("lubridate")
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
  config$cache <- configure_cache(
    cache = storr::storr_environment(),
    init_common_values = TRUE
  )
  config$jobs <- jobs
  config$schedule <- config$graph
  config$queue <- new_priority_queue(config = config)
  mc_init_worker_cache(config)
  workers <- config$cache$list("mc_ready_db")
  targets <- lapply(workers, function(worker){
    character(0)
  })
  total_runtime <- 0
  time_remaining <- rep(0, length(workers))
  old_targets <- rep("", length(workers))
  names(time_remaining) <- names(targets) <- names(old_targets) <- workers
  lapply(workers, mc_get_ready_queue, config = config)
  lapply(workers, mc_get_done_queue, config = config)
  while (TRUE){
    # Run one iteration of mc_master()
    config <- mc_refresh_queue_lists(config)
    mc_conclude_done_targets(config, wait_for_checksums = FALSE)
    mc_assign_ready_targets(config)
    # List the running targets and their runtimes
    current_targets <- vapply(
      config$mc_ready_queues,
      function(queue){
        messages <- queue$list(1)
        if (nrow(messages)){
          messages$title[1]
        } else {
          ""
        }
      },
      FUN.VALUE = character(1)
    )
    is_running <- nzchar(current_targets)
    if (!any(is_running)){
      break
    }
    times <- vapply(
      current_targets,
      function(target){
        if (identical(target, "")){
          0
        } else {
          igraph::vertex_attr(
            graph = config$graph,
            name = "time",
            index = target
          )
        }
      },
      FUN.VALUE = numeric(1)
    )
    # For the workers that just got new targets,
    # increment the time they will remain running.
    index <- current_targets != old_targets
    time_remaining[index] <- time_remaining[index] + times[index]
    old_targets <- current_targets
    # Move time forward until soonest target finishes.
    step <- min(time_remaining[is_running])
    worker <- names(which.min(time_remaining[is_running]))
    time_remaining[is_running] <- time_remaining[is_running] - step
    total_runtime <- total_runtime + step
    # Finish the target.
    ready_queue <- config$mc_ready_queues[[worker]]
    done_queue <- config$mc_done_queues[[worker]]
    msg <- ready_queue$pop(1)
    if (nrow(msg) > 0){
      target <- msg$title
      targets[[worker]] <- c(targets[[worker]], target)
      done_queue$push(title = target, message = "target")
    }
  }
  eval(parse(text = "require(methods, quietly = TRUE)")) # needed for lubridate
  list(
    total_runtime = lubridate::dseconds(total_runtime),
    targets = targets
  )
}
