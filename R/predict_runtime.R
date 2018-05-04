#' @title Predict the runtime of the next call to `make()`.
#' @description Simulate the runtime of the next `make()`
#' by crudely emulating virtual parallel workers.
#' Basically, the workers repeatedly grab random available targets
#' and the observed runtimes are added up per worker. The runtime
#' is the largest of any of the worker times. Set the number of workers
#' with `jobs`. 
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
#' @param jobs_sims Number of jobs to use right now
#'   to speed up the runtime predictions.
#' @param future_jobs deprecated
#' @param digits number of digits for rounding the time
#' @param type character scalar: `"elapsed"`, `"user"`, or `"system"`
#' @param sims Number of times to simulate the runtime. Downgrades to 1
#'   if `jobs` is 1.
predict_runtime <- function(
  config = drake::read_drake_config(),
  targets = NULL,
  from_scratch = FALSE,
  targets_only = FALSE,
  future_jobs = NULL,
  digits = NULL,
  type = c("elapsed", "user", "system"),
  jobs = 1,
  jobs_sims = 1,
  sims = 1e3
){
  if (!is.null(future_jobs)){
    warning(
      "The `future_jobs` argument is deprecated ",
      "because the name is awkward. ",
      "Use `jobs` instead.",
      call. = FALSE
    )
  }
  times <- build_times(
    config = config,
    targets = targets,
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
  if (!from_scratch){
    times <- times[
      times$item %in% outdated(config) |
        times$type == "import",
    ]
  }
  if (jobs <= 1){
    return(sum(times$time))
  }
  times <- as.data.frame(times)
  rownames(times) <- times$item
  simulate_runtimes(
    graph = graph,
    times = times,
    jobs = jobs,
    sims = sims
  )
}

simulate_runtimes = function(graph, times, jobs, jobs_sims, sims){
  lightly_parallelize(
    X = seq_len(sims),
    FUN = simulate_runtime,
    jobs = jobs_sims,
    graph = graph,
    times = times,
    workers = jobs
  )
}

simulate_runtime <- function(sim, graph, times, workers){
  
  print(sim)
  
  totals <- rep(0, workers)
  while(length(V(graph))){
    leaves <- leaf_nodes(graph)
    leaf_times <- times[leaves, ]
    size <- min(nrow(leaf_times), workers)
    index <- sample.int(n = nrow(leaf_times), size = size)
    leaf_times <- leaf_times[index, ]
    increments <- leaf_times$time
    if (length(increments) < length(totals)){
      increments <- c(
        increments,
        rep(0, length(totals) - length(increments))
      )
    }
    totals <- totals + sample(increments)
    graph <- delete_vertices(graph, v = leaf_times$item)
    times <- times[setdiff(times$item, leaf_times$item), ]
  }
  max(totals)
}
