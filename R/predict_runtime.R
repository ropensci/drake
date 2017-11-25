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
#' make(my_plan) # Run the project, build the targets.
#' predict_runtime(my_plan, digits = 4) # Everything is up to date.
#' predict_runtime(my_plan, digits = 4, from_scratch = TRUE) # 1 job
#' # Assumes you clean() out your project and start from scratch with 2 jobs.
#' predict_runtime(my_plan, future_jobs = 2, digits = 4, from_scratch = TRUE)
#' # Just predict how long it will take to build
#' # the targets 'small' and 'large'.
#' predict_runtime(
#'   my_plan,
#'   targets = c("small", "large"),
#'   future_jobs = 2,
#'   digits = 4,
#'   from_scratch = TRUE
#' )
#' }
#' @return A \code{lubridate} \code{Duration} object
#' with the predicted runtime of the next \code{\link{make}()}.
#' @param plan same as for \code{\link{make}}
#' @param from_scratch logical, whether to predict a
#' \code{\link{make}()} build from scratch or to
#' take into account the fact that some targets may be
#' already up to date and therefore skipped.
#' @param targets_only logical, whether to factor in
#' just the targets into the calculations or use the
#' build times for everything, including the imports
#' @param targets Targets to build in the workplan.
#' Timing information is
#' limited to these targets and their dependencies.
#' @param envir same as for \code{\link{make}}
#' @param verbose same as for \code{\link{make}}
#' @param hook same as for \code{\link{make}}
#' @param cache optional drake cache. See code{\link{new_cache}()}.
#' The \code{cache} argument is ignored if a
#' non-null \code{config} argument is supplied.
#' @param parallelism same as for \code{\link{make}}. Used
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
#' Overrides all arguments except \code{from_scratch},
#' \code{targets_only}, and \code{digits}.
#' Computing \code{config}
#' in advance could save time if you plan multiple calls to
#' this function.
#' @param digits number of digits for rounding the times.
#' @param path file path to the folder containing the cache.
#' Yes, this is the parent directory containing the cache,
#' not the cache itself.
#' @param search logical, whether to search back in the file system
#' for the cache.
predict_runtime <- function(
  plan = workplan(),
  from_scratch = FALSE,
  targets_only = FALSE,
  targets = drake::possible_targets(plan),
  envir = parent.frame(),
  verbose = 1,
  hook = default_hook,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  parallelism = drake::default_parallelism(),
  jobs = 1,
  future_jobs = jobs,
  packages = rev(.packages()),
  prework = character(0),
  config = NULL,
  digits = 3,
  path = getwd(),
  search = TRUE
){
  force(envir)
  eval(parse(text = "require(methods, quietly = TRUE)")) # needed for lubridate
  times <- rate_limiting_times(
    plan = plan,
    from_scratch = from_scratch,
    targets_only = targets_only,
    targets = targets,
    envir = envir,
    verbose = verbose,
    hook = hook,
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
#' make(my_plan) # Run the project, build the targets.
#' rate_limiting_times(my_plan) # Everything is up to date.
#' # Assume everything runs from scratch with 1 job.
#' rate_limiting_times(my_plan, from_scratch  = TRUE, digits = 4)
#' # With 2 jobs, some of the targets are not rate-limiting.
#' rate_limiting_times(
#'   my_plan,
#'   future_jobs = 2,
#'   from_scratch = TRUE,
#'   digits = 4
#' )
#' # What if we just build the 'small' and 'large' targets,
#' # plus dependencies?
#' rate_limiting_times(
#'   my_plan,
#'   targets = c("small", "large"),
#'   future_jobs = 2,
#'   from_scratch = TRUE,
#'   digits = 4
#' )
#' }
#'
#' @return A data frame of times of the worst-case scenario
#' rate-limiting targets in each parallelizable stage.
#'
#' @param plan same as for \code{\link{make}}
#'
#' @param from_scratch logical, whether to assume
#' next hypothetical call to \code{\link{make}()}
#' is a build from scratch (after \code{\link{clean}()}).
#'
#' @param targets_only logical, whether to factor in just the
#' targets or use times from everything, including the imports.
#'
#' @param targets Targets to build in the workplan.
#' Timing information is
#' limited to these targets and their dependencies.
#'
#' @param envir same as for \code{\link{make}}. Supersedes
#' \code{config$envir}.
#'
#' @param verbose same as for \code{\link{make}}
#'
#' @param hook same as for \code{\link{make}}
#'
#' @param cache optional drake cache. See code{\link{new_cache}()}.
#' The \code{cache} argument is ignored if a
#' non-null \code{config} argument is supplied.
#'
#' @param parallelism same as for \code{\link{make}}, and
#' also used to process imported objects/files.
#'
#' @param jobs same as for \code{\link{make}}, just used to
#' process imports.
#'
#' @param future_jobs hypothetical number of jobs
#' assumed for the predicted runtime.
#' assuming this number of jobs.
#'
#' @param packages same as for \code{\link{make}}
#'
#' @param prework same as for \code{\link{make}}
#'
#' @param config option internal runtime parameter list of
#' \code{\link{make}(...)},
#' produced with \code{\link{config}()}.
#' Overrides all arguments except \code{from_scratch},
#' \code{targets_only}, and \code{digits}.
#' Computing \code{config}
#' in advance could save time if you plan multiple calls to
#' this function.
#'
#' @param digits number of digits for rounding the times.
#'
#' @param path file path to the folder containing the cache.
#' Yes, this is the parent directory containing the cache,
#' not the cache itself.
#'
#' @param search logical, whether to search back in the file system
#' for the cache.
rate_limiting_times <- function(
  plan = workplan(),
  from_scratch = FALSE,
  targets_only = FALSE,
  targets = drake::possible_targets(plan),
  envir = parent.frame(),
  verbose = 1,
  hook = default_hook,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  parallelism = drake::default_parallelism(),
  jobs = 1,
  future_jobs = jobs,
  packages = rev(.packages()),
  prework = character(0),
  config = NULL,
  digits = 3,
  path = getwd(),
  search = TRUE
){
  force(envir)
  if (is.null(config)){
    config <- drake_config(
      plan = plan,
      targets = targets,
      envir = envir,
      verbose = verbose,
      hook = hook,
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
    targets_only = FALSE,
    path = path,
    search = search,
    digits = Inf,
    cache = config$cache
  )
  keys <- V(config$graph)$name
  import_keys <- setdiff(keys, plan$target)
  items <- intersect(keys, times$item)
  not_timed <- setdiff(keys, items)
  warn_not_timed(not_timed)
  if (!nrow(times)){
    return(cbind(times, stage = numeric(0)))
  }
  rownames(times) <- times$item
  times <- times[intersect(items, keys), ]
  if (!from_scratch){
    outdated <- outdated(
      plan = plan,
      envir = envir,
      config = config
    ) %>%
      intersect(y = plan$target) %>%
      c(import_keys) %>%
      intersect(y = rownames(times)) %>%
      unique
    times <- times[outdated, ]
  }
  if (targets_only){
    times <- times[times$type == "target", ]
  }
  if (!nrow(times)){
    return(cbind(times, stage = numeric(0)))
  }
  nodes <- real_stages(config = config)
  times <- times[nodes$item, ]
  times$stage <- nodes$stage
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
    multiline_message(not_timed),
    call. = FALSE
  )
}

unname_rows <- function(x){
  rownames(x) <- NULL
  x
}
