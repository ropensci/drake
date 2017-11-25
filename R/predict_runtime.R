#' @title Function predict_runtime
#' @description Predict the elapsed runtime of the next call to `make()`.
#' This function simply sums the elapsed build times.
#' from \code{\link{rate_limiting_times}()}.
#' @details For the results to make sense, the previous build times
#' of all targets need to be available (automatically cached
#' by \code{\link{make}()}). Otherwise, \code{predict_runtime()}
#' will warn you and tell you which targets have missing times.
#' @export
#' @seealso \code{\link{rate_limiting_times}},
#' \code{\link{build_times}}
#' \code{\link{make}}
#' @examples
#' \dontrun{
#' load_basic_example() # Load drake's canonical example.
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
#' }
#' @return A \code{lubridate} \code{Duration} object
#' with the predicted runtime of the next \code{\link{make}()}.
#' @param config option internal runtime parameter list of
#' \code{\link{make}(...)},
#' produced by both \code{\link{make}()} and
#' \code{\link{drake_config}()}.
#' @param targets Character vector, names of targets.
#' Predict the runtime of building these targets
#' plus dependencies.
#' Defaults to all targets.
#' @param from_scratch logical, whether to predict a
#' \code{\link{make}()} build from scratch or to
#' take into account the fact that some targets may be
#' already up to date and therefore skipped.
#' @param targets_only logical, whether to factor in
#' just the targets into the calculations or use the
#' build times for everything, including the imports
#' @param future_jobs hypothetical number of jobs
#' assumed for the predicted runtime.
#' assuming this number of jobs.
predict_runtime <- function(
  config,
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

#' @title Function rate_limiting_times
#' @description Return a data frame of elapsed build times of
#' the rate-limiting targets of a \code{\link{make}()} workplan.
#' @export
#'
#' @details The \code{stage} column of the returned data frame
#' is an index that denotes a parallelizable stage.
#' Within each stage during \code{\link{make}()},
#' the targets are divided among the available jobs.
#' For \code{rate_limiting_times()},
#' we assume the targets are divided evenly among the jobs
#' and one job gets all the slowest targets.
#' The build times of this hypothetical pessimistic job
#' are returned for each stage. \cr
#'
#' By default \code{from_scratch} is \code{FALSE}. That way,
#' \code{rate_limiting_times()} takes into account that some
#' targets are already up to date, meaning their elapsed
#' build times will be instant during the next \code{\link{make}()}.
#'
#' For the results to make sense, the previous build times
#' of all targets need to be available (automatically cached
#' by \code{\link{make}()}). Otherwise, \code{rate_limiting_times()}
#' will warn you and tell you which targets have missing times.
#'
#' @seealso \code{\link{predict_runtime}},
#' \code{\link{build_times}}
#' \code{\link{make}}
#'
#' @examples
#' \dontrun{
#' load_basic_example() # Load drake's canonical example.
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
#' }
#'
#' @return A data frame of times of the worst-case scenario
#' rate-limiting targets in each parallelizable stage.
#'
#' @param config option internal runtime parameter list of
#' \code{\link{make}(...)},
#' produced by both \code{\link{make}()} and
#' \code{\link{drake_config}()}.
#'
#' @param targets Character vector, names of targets.
#' Find the rate-limiting times for building these targets
#' plus dependencies.
#' Defaults to all targets.
#'
#' @param from_scratch logical, whether to assume
#' next hypothetical call to \code{\link{make}()}
#' is a build from scratch (after \code{\link{clean}()}).
#'
#' @param targets_only logical, whether to factor in just the
#' targets or use times from everything, including the imports.
#'
#' @param future_jobs hypothetical number of jobs
#' assumed for the predicted runtime.
#' assuming this number of jobs.
#'
#' @param digits number of digits for rounding the times.
rate_limiting_times <- function(
  config,
  targets = NULL,
  from_scratch = FALSE,
  targets_only = FALSE,
  future_jobs = 1,
  digits = 3
){
  if (!is.null(targets)){
    config$from <- config$targets <- targets
    config$mode <- "in"
    config$order <- numeric(0)
    config <- trim_graph(config)
  }
  times <- build_times(
    cache = config$cache,
    digits = Inf,
    targets_only = FALSE,
    verbose = TRUE
  )
  keys <- V(config$graph)$name
  import_keys <- setdiff(keys, config$plan$target)
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
  nodes <- parallel_stages(config = config)
  times <- times[nodes$item, ]
  times$stage <- nodes$stage
  out <- ddply(
    times,
    "stage",
    rate_limiting_at_stage,
    future_jobs = future_jobs
  ) %>%
    round_times(digits = digits) %>%
    unname_rows
  out[!is.na(out$item), ]
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
