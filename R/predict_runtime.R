#' @title Function predict_runtime
#' @description Predict the elapsed runtime of the next call to `make()`.
#' Only accounts for target build times, not imports or hashing.
#' This function simply sums the elapsed build times
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
#' load_basic_example()
#' make(my_plan)
#' predict_runtime(my_plan, digits = 4) # everything is up to date
#' predict_runtime(my_plan, digits = 4, from_scratch = TRUE) # 1 job
#' predict_runtime(my_plan, future_jobs = 2, digits = 4, from_scratch = TRUE)
#' predict_runtime(
#'   my_plan,
#'   targets = c("small", "large"),
#'   future_jobs = 2,
#'   digits = 4,
#'   from_scratch = TRUE
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
#' @param parallelism samea as for \code{\link{make}}. Used
#' to parallelize import objects.
#' @param jobs same as for \code{\link{make}}, just used to
#' process imports
#' @param future_jobs hypothetical number of jobs
#' assumed for the predicted runtime.
#' assuming this number of jobs.
#' @param packages same as for \code{\link{make}}
#' @param prework same as for \code{\link{make}}
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
  parallelism = drake::default_parallelism(),
  jobs = 1,
  future_jobs = jobs,
  packages = (.packages()),
  prework = character(0),
  config = NULL,
  digits = 0,
  path = getwd(),
  search = TRUE
){
  force(envir)
  eval(parse(text = "require(methods, quietly = TRUE)")) # needed for dseconds
  times <- rate_limiting_times(
    plan = plan,
    from_scratch = from_scratch,
    targets = targets,
    envir = envir,
    verbose = verbose,
    cache = cache,
    parallelism = parallelism,
    jobs = jobs,
    future_jobs = future_jobs,
    packages = packages,
    prework = prework,
    config = config,
    digits = Inf,
    path = path,
    search = search
  )
  sum(times$elapsed) %>%
    round(digits = digits) %>%
    dseconds()
}
  
#' @title Function rate_limiting_times
#' @description Return a data frame of elapsed build times of
#' the rate-limiting targets of a \code{\link{make}()} workflow.
#' Only build times are used.
#' Hashing times, etc. are not factored in.
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
#' @export
#' @seealso \code{\link{predict_runtime}},
#' \code{\link{build_times}}
#' \code{\link{make}}
#' @examples
#' \dontrun{
#' load_basic_example()
#' make(my_plan)
#' rate_limiting_times(my_plan) # everything is up to date
#' rate_limiting_times(my_plan, from_scratch  = TRUE, digits = 4) # 1 job
#' rate_limiting_times(
#'   my_plan,
#'   future_jobs = 2,
#'   from_scratch = TRUE,
#'   digits = 4
#' )
#' rate_limiting_times(
#'   my_plan,
#'   targets = c("small", "large"),
#'   future_jobs = 2,
#'   from_scratch = TRUE,
#'   digits = 4
#' )
#' }
#' @param plan same as for \code{\link{make}}
#' @param from_scratch logical, whether to assume
#' next hypothetical call to \code{\link{make}()}
#' is a build from scratch (after \code{\link{clean}()}).
#' @param targets Targets to build in the workflow.
#' Timing information is
#' limited to these targets and their dependencies.
#' @param envir same as for \code{\link{make}}. Supercedes
#' \code{config$envir}.
#' @param verbose same as for \code{\link{make}}
#' @param cache optional drake cache. See code{\link{new_cache}()}.
#' The \code{cache} argument is ignored if a
#' non-null \code{config} argument is supplied.
#' @param parallelism same as for \code{\link{make}}, and
#' also used to process imported objects/files.
#' @param jobs same as for \code{\link{make}}, just used to
#' process imports.
#' @param future_jobs hypothetical number of jobs
#' assumed for the predicted runtime.
#' assuming this number of jobs.
#' @param packages same as for \code{\link{make}}
#' @param prework same as for \code{\link{make}}
#' @param config option internal runtime parameter list of
#' \code{\link{make}(...)},
#' produced with \code{\link{config}()}.
#' \code{config$envir} is ignored.
#' Otherwise, computing this
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
  from_scratch = FALSE,
  targets = drake::possible_targets(plan),
  envir = parent.frame(),
  verbose = TRUE,
  cache = NULL,
  parallelism = drake::default_parallelism(),
  jobs = 1,
  future_jobs = jobs,
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
  targets <- intersect(V(config$graph)$name, plan$target)
  not_timed <- setdiff(targets, times$target)
  warn_not_timed(not_timed)
  if (!nrow(times)){
    return(cbind(times, stage = numeric(0)))
  }
  rownames(times) <- times$target
  times <- times[intersect(targets, times$target), ]
  if (!from_scratch){
    outdated <- outdated(plan = plan, envir = envir, config = config) %>%
      intersect(y = times$target)
    times <- times[outdated, ]
  }
  if (!nrow(times)){
    return(cbind(times, stage = numeric(0)))
  }
  keep_these <- setdiff(V(config$graph)$name, rownames(times))
  graph <- delete_vertices(config$graph, v = keep_these)
  times <- resolve_levels(nodes = times, graph = graph)
  colnames(times) <- gsub("^level$", "stage", colnames(times))
  ddply(times, "stage", rate_limiting_at_stage, future_jobs = future_jobs) %>%
    round_times(digits = digits) %>%
    unname_rows
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
    multiline_message(not_timed)
  )
}

unname_rows <- function(x){
  rownames(x) <- NULL
  x
}
