#' @title Predict the elapsed runtime of the next call to `make()`
#'   for non-staged parallel backends.
#' `r lifecycle::badge("stable")`
#' @description Take the past recorded runtimes times from
#'   [build_times()] and use them to predict how the targets
#'   will be distributed among the available workers in the
#'   next [make()]. Then, predict the overall runtime to be the
#'   runtime of the slowest (busiest) workers.
#'   Predictions only include the time it takes to run the targets,
#'   not overhead/preprocessing from `drake` itself.
#' @export
#' @return Predicted total runtime of the next call to [make()].
#' @inheritParams predict_workers
#' @seealso [predict_workers()], [build_times()], [make()]
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' known_times <- rep(7200, nrow(my_plan))
#' names(known_times) <- my_plan$target
#' known_times
#' # Predict the runtime
#' if (requireNamespace("lubridate", quietly = TRUE)) {
#' predict_runtime(
#'   my_plan,
#'   jobs_predict = 7L,
#'   from_scratch = TRUE,
#'   known_times = known_times
#' )
#' predict_runtime(
#'   my_plan,
#'   jobs_predict = 8L,
#'   from_scratch = TRUE,
#'   known_times = known_times
#' )
#' balance <- predict_workers(
#'   my_plan,
#'   jobs_predict = 7L,
#'   from_scratch = TRUE,
#'   known_times = known_times
#' )
#' balance
#' }
#' }
#' })
#' }
predict_runtime <- function(
  ...,
  targets_predict = NULL,
  from_scratch = FALSE,
  targets_only = NULL,
  jobs_predict = 1L,
  known_times = numeric(0),
  default_time = 0,
  warn = TRUE,
  config = NULL
) {
}

#' @title Internal function with a drake_config() argument
#' @export
#' @keywords internal
#' @description Not a user-side function.
#' @param config A [drake_config()] object.
predict_runtime_impl <- function(
  config,
  targets_predict = NULL,
  from_scratch = FALSE,
  targets_only = NULL,
  jobs_predict = 1L,
  known_times = numeric(0),
  default_time = 0,
  warn = TRUE
) {
  worker_prediction_info(
    config = config,
    targets = targets_predict,
    from_scratch = from_scratch,
    targets_only = targets_only,
    jobs = jobs_predict,
    known_times = known_times,
    default_time = default_time,
    warn = warn
  )$time
}

body(predict_runtime) <- config_util_body(predict_runtime_impl)

#' @title Predict the load balancing of the next call to `make()`
#'   for non-staged parallel backends.
#' `r lifecycle::badge("stable")`
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
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' known_times <- rep(7200, nrow(my_plan))
#' names(known_times) <- my_plan$target
#' known_times
#' # Predict the runtime
#' if (requireNamespace("lubridate", quietly = TRUE)) {
#' predict_runtime(
#'   my_plan,
#'   jobs_predict = 7L,
#'   from_scratch = TRUE,
#'   known_times = known_times
#' )
#' predict_runtime(
#'   my_plan,
#'   jobs_predict = 8L,
#'   from_scratch = TRUE,
#'   known_times = known_times
#' )
#' balance <- predict_workers(
#'   my_plan,
#'   jobs_predict = 7L,
#'   from_scratch = TRUE,
#'   known_times = known_times
#' )
#' balance
#' }
#' }
#' })
#' }
#' @return A data frame showing one likely arrangement
#'   of targets assigned to parallel workers.
#' @param ... Arguments to [make()], such as `plan` and `targets`.
#' @param config Deprecated.
#' @param targets_predict Character vector, names of targets
#'   to include in the total runtime and worker predictions.
#' @param from_scratch Logical, whether to predict a
#'   [make()] build from scratch or to
#'   take into account the fact that some targets may be
#'   already up to date and therefore skipped.
#' @param targets_only Deprecated.
#' @param jobs_predict The `jobs` argument of your next planned
#'   `make()`.
#' @param known_times A named numeric vector with targets/imports
#'   as names and values as hypothetical runtimes in seconds.
#'   Use this argument to overwrite any of the existing build times
#'   or the `default_time`.
#' @param default_time Number of seconds to assume for any
#'   target or import with no recorded runtime (from [build_times()])
#'   or anything in `known_times`.
#' @param warn Logical, whether to warn the user about
#'   any targets with no available runtime, either in
#'   `known_times` or [build_times()]. The times for these
#'   targets default to `default_time`.
predict_workers <- function(
  ...,
  targets_predict = NULL,
  from_scratch = FALSE,
  targets_only = NULL,
  jobs_predict = 1L,
  known_times = numeric(0),
  default_time = 0,
  warn = TRUE,
  config = NULL
) {
}

#' @title Internal function with a drake_config() argument
#' @export
#' @keywords internal
#' @description Not a user-side function.
#' @param config A [drake_config()] object.
predict_workers_impl <- function(
  config,
  targets_predict = NULL,
  from_scratch = FALSE,
  targets_only = NULL,
  jobs_predict = 1,
  known_times = numeric(0),
  default_time = 0,
  warn = TRUE
) {
  worker_prediction_info(
    config,
    targets = targets_predict,
    from_scratch = from_scratch,
    targets_only = targets_only,
    jobs = jobs_predict,
    known_times = known_times,
    default_time = default_time,
    warn = warn
  )$workers
}

body(predict_workers) <- config_util_body(predict_workers_impl)

worker_prediction_info <- function(
  config,
  targets = NULL,
  from_scratch = FALSE,
  targets_only = NULL,
  jobs = 1,
  known_times = numeric(0),
  default_time = 0,
  warn = TRUE
) {
  assert_config(config)
  config$logger$file <- NULL
  deprecate_targets_only(targets_only) # 2019-01-03 # nolint
  assumptions <- timing_assumptions(
    config = config,
    targets = targets,
    from_scratch = from_scratch,
    jobs = jobs,
    known_times = known_times,
    default_time = default_time,
    warn = warn
  )
  config$graph <- subset_graph(config$graph, all_targets(config))
  if (!is.null(targets)) {
    config$graph <- nbhd_graph(
      config$graph,
      vertices = targets,
      mode = "in",
      order = igraph::gorder(config$graph)
    )
  }
  config <- register_subtargets_predict(config)
  config$envir_graph <- new.env(parent = emptyenv())
  config$envir_graph$graph <- config$graph
  on.exit(config$envir_graph <- NULL)
  queue <- priority_queue(config, jobs = 1)
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
  workers <- lapply(seq_along(workers), function(index) {
    weak_tibble(target = workers[[index]], worker = index)
  })
  workers <- do.call(rbind, workers)
  list(time = lubridate::dseconds(time), workers = workers)
}

register_subtargets_predict <- function(config) {
  vertices <- igraph::V(config$graph)$name
  for (target in vertices) {
    should_register <- is_dynamic(target, config) &&
      !is_subtarget(target, config) &&
      config$cache$exists(target, namespace = "meta")
    if (should_register) {
      sub <- subtargets(target, character_only = TRUE, cache = config$cache)
      config <- register_subtargets_graph2(target, sub, config)
    }
  }
  config
}

register_subtargets_graph2 <- function(target, subtargets, config) {
  edgelist <- do.call(rbind, lapply(subtargets, c, target))
  deps <- drake_adjacent_vertices(config$graph, target, mode = "in")
  edgelist_deps <- expand.grid(deps, subtargets, stringsAsFactors = FALSE)
  edgelist <- rbind(edgelist, as.matrix(edgelist_deps))
  subgraph <- igraph::graph_from_edgelist(edgelist)
  subgraph <- igraph::set_vertex_attr(
    subgraph,
    name = "imported",
    value = FALSE
  )
  config$graph <- igraph::union(config$graph, subgraph)
  config
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
    outdated <- outdated_impl(config)
  }
  times <- build_times(cache = config$cache)
  vertices <- all_targets(config)
  for (target in vertices) {
    append_subtargets <- is_dynamic(target, config) &&
      !is_subtarget(target, config) &&
      config$cache$exists(target, "meta")
    if (append_subtargets) {
      sub <- subtargets(target, character_only = TRUE, cache = config$cache)
      vertices <- c(vertices, sub)
    }
  }
  times <- times[times$target %in% vertices, ]
  untimed <- setdiff(vertices, times$target)
  untimed <- setdiff(untimed, names(known_times))
  if (length(untimed)) {
    warn0(
      "No known_times set. Assuming ", default_time, " runtime for:\n",
      multiline_message(untimed)
    )
  }
  keep_known_times <- intersect(names(known_times), vertices)
  known_times <- known_times[keep_known_times]
  assumptions <- rep(default_time, length(vertices))
  names(assumptions) <- vertices
  assumptions[times$target] <- times$elapsed
  assumptions[names(known_times)] <- known_times
  if (!from_scratch) {
    skip <- setdiff(all_targets(config), outdated)
    assumptions[skip] <- 0
  }
  assumptions
}
