#' @title Function predict_runtime
#' @description Predict the runtime of the next call to `make()`.
#' This function resolves the \code{\link{rate_limiting_targets}()},
#' optionally removes the up-to-date targets,
#' and then sums the remaining elapsed times.
#' @details For the results to make sense, the previous build times
#' of all targets need to be available (automatically cached
#' by \code{\link{make}()}). Otherwise, \code{predict_runtime()}
#' will warn you and tell you which targets have missing times.
#' @export
#' @seealso \code{\link{predict_runtime}},
#' \code{\link{build_times}}
#' \code{\link{make}}
#' @examples
#' \dontrun{
#' load_basic_example()
#' make(my_plan)
#' predict_runtime(my_plan) # 1 job
#' predict_runtime(my_plan, jobs = 2)
#' predict_runtime(
#'   my_plan,
#'   targets = c("small", "large"),
#'   jobs = 2
#' )
#' }
#' @param plan same as for \code{\link{make}}
#' @param from_scratch logical, whether to predict a
#' \code{\link{make}()} build from scratch or to
#' take into account the fact that some targets may be
#' already up to date and therefore skipped.
#' @param targets Targets to build in the workflow.
#' Timing information is
#' limited to these targets and their dependencies.
#' @param envir same as for \code{\link{make}}
#' @param verbose same as for \code{\link{make}}
#' @param cache optional drake cache. See code{\link{new_cache}()}.
#' The \code{cache} argument is ignored if a
#' non-null \code{config} argument is supplied.
#' @param jobs same as for \code{\link{make}}
#' @param config option internal runtime parameter list of
#' \code{\link{make}(...)},
#' produced with \code{\link{config}()}.
#' Computing this
#' in advance could save time if you plan multiple calls to
#' this function.
#' @param digits number of digits for rounding the times.
#' @param path file path to the folder contianing the cache.
#' Yes, this is the parent directory containing the cache,
#' not the cache itself.
#' @param search logical, whether to search back in the file system
#' for the cache.
predict_runtime <- function(
  plan,
  from_scratch = FALSE,
  targets = drake::possible_targets(plan),
  envir = parent.frame(),
  verbose = TRUE,
  cache = NULL,
  jobs = 1,
  packages = (.packages()),
  prework = character(0),
  config = NULL,
  digits = 0,
  path = getwd(),
  search = TRUE
){
  force(envir)
  if (is.null(config)){
    config <- config(
      plan = plan,
      targets = targets,
      envir = envir,
      verbose = verbose,
      cache = cache,
      parallelism = parallelism,
      jobs = jobs,
      packages = packages,
      prework = prework
    )
  }
  if (!is.null(cache)){
    config$cache <- configure_cache(cache)
  }
  times <- rate_limiting_times(
    plan = plan,
    jobs = config$jobs,
    config = config,
    digits = Inf
  )
  if (!from_scratch){
    keep <- outdated(plan = plan, config = config) %>%
      intersect(y = times$target)
    times <- times[times$target %in% keep, ]
  }
  sum(times$elapsed)
}
  
#' @title Function rate_limiting_times
#' @description Return a data frame of build times of
#' the rate-limiting targets of a \code{\link{make}()} workflow.
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
#' For the results to make sense, the previous build times
#' of all targets need to be available (automatically cached
#' by \code{\link{make}()}). Otherwise, \code{rate_limiting_times()}
#' will warn you and tell you which targets have missing times.
#' @export
#' @seealso \code{\link{predict_runtime}},
#' \code{\link{build_times}}
#' \code{\link{make}}
#' @examples
#' \dontrun{
#' load_basic_example()
#' make(my_plan)
#' rate_limiting_times(my_plan) # 1 job
#' rate_limiting_times(my_plan, jobs = 2)
#' rate_limiting_times(
#'   my_plan,
#'   targets = c("small", "large"),
#'   jobs = 2
#' )
#' }
#' @param plan same as for \code{\link{make}}
#' @param targets Targets to build in the workflow.
#' Timing information is
#' limited to these targets and their dependencies.
#' @param envir same as for \code{\link{make}}
#' @param verbose same as for \code{\link{make}}
#' @param cache optional drake cache. See code{\link{new_cache}()}.
#' The \code{cache} argument is ignored if a
#' non-null \code{config} argument is supplied.
#' @param jobs same as for \code{\link{make}}
#' @param config option internal runtime parameter list of
#' \code{\link{make}(...)},
#' produced with \code{\link{config}()}.
#' Computing this
#' in advance could save time if you plan multiple calls to
#' this function.
#' @param digits number of digits for rounding the times.
#' @param path file path to the folder contianing the cache.
#' Yes, this is the parent directory containing the cache,
#' not the cache itself.
#' @param search logical, whether to search back in the file system
#' for the cache.
rate_limiting_times <- function(
  plan,
  targets = drake::possible_targets(plan),
  envir = parent.frame(),
  verbose = TRUE,
  cache = NULL,
  jobs = 1,
  packages = (.packages()),
  prework = character(0),
  config = NULL,
  digits = 0,
  path = getwd(),
  search = TRUE
){
  force(envir)
  if (is.null(config)){
    config <- config(
      plan = plan,
      targets = targets,
      envir = envir,
      verbose = verbose,
      cache = cache,
      parallelism = parallelism,
      jobs = jobs,
      packages = packages,
      prework = prework
    )
  }
  if (!is.null(cache)){
    config$cache <- configure_cache(cache)
  }
  times <- build_times(
    path = path,
    search = search,
    digits = Inf,
    cache = config$cache
  )
  browser()
  not_timed <- setdiff(plan$target, times$target)
  warn_not_timed(not_timed)
  times <- times[intersect(plan$target, times$target), ]
  rownames(times) <- times$target
  times <- resolve_levels(nodes = times, graph = config$graph)
  colnames(times) <- gsub("^level$", "stage", times)
  ddply(times, "stage", rate_limiting_at_stage, jobs = jobs) %>%
    round_times(digits = digits) %>%
    unname_rows
}

rate_limiting_at_stage <- function(stage, jobs){
  stage <- stage[order(stage$elapsed, decreasing = TRUE), ]
  targets_per_job <- ceiling(nrow(x) / jobs) %>%
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
    multiline_message(not_timed)
  )
}

unname_rows <- function(x){
  rownames(x) <- NULL
  x
}
