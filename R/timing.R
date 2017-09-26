#' @title Function \code{build_times}
#' @description List all the build times.
#' This doesn't include the amount of time
#' spent loading and saving objects!
#' @seealso \code{\link{built}}
#' @export
#' @return data.frame of times from \code{\link{system.time}}
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#' @param search logical. If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
#' @param digits How many digits to round the times to.
#' @param cache optional drake cache. If supplied,
#' the \code{path} and \code{search} arguments are ignored.
#' @examples
#' \dontrun{
#' load_basic_example()
#' make(my_plan)
#' build_times()
#' }
build_times <- function(
  path = getwd(),
  search = TRUE,
  digits = 0,
  cache = NULL
  ){
  eval(parse(text = "require(methods, quietly = TRUE)"))  # needed for dseconds

  empty_times <- data.frame(
    target = character(0),
    user = duration(numeric(0)),
    system = duration(numeric(0)),
    elapsed = duration(numeric(0))
  )

  if (is.null(cache)){
    cache <- get_cache(path = path, search = search)
  }
  if (is.null(cache)){
    return(empty_times)
  }

  times <- cache$list(namespace = "build_times") %>% Map(f = function(target) {
    # Try to get times if they are saved
    time <- cache$get(key = target, namespace = "build_times")
    if (class(time) == "proc_time") {
      data.frame(
        target = target,
        user = time[["user.self"]] %>%
          round(digits) %>%
          dseconds,
        system = time[["sys.self"]] %>%
          round(digits) %>%
          dseconds,
        elapsed = time[["elapsed"]] %>%
          round(digits) %>%
          dseconds
      )
    }
  }) %>% # Filter out NULLs
    lapply(Filter, f = Negate(is.null)) %>% # Merge to data.frame
    Reduce(f = function(x, y) rbind(x, y))

  if (!length(times)){
    return(empty_times)
  }

  times$target <- times$target %>%
    as.character()
  return(times)
}

#' Predict runtime
#' 
#' This function uses \code{\link{build_times}} and your plan to predict the amount of time it will
#' take to \code{\link{make}}. 
#' 
#' @param plan same as for \code{\link{make}}
#' @param from_scratch Predict building only \code{\link{outdated}} targets, or the entire plan?
#' @param untimed_method What to do with targets that have never been built (and thus have no
#' timing data)? By default, the function uses the mean of other build_times in the same build
#' stage. This could be semi-accurate depending on your dependency graph. Override this by passing
#' in a function like \code{mean} that summarizes a vector. Or, just pass in a constant.
#' @param build_times An optional data.frame in the format of \code{\link{build_times}} to override the
#' internally calculated times for targets. Only the columns "target" and "elapsed" are required.
#' For targets not in this data.frame, \code{\link{build_times}} is used as a fallback, and then
#' \code{untimed_method}.
#' @param digits How many digits to round the times to.
#' @param verbose same as for \code{\link{make}}
#' @param config Optional internal runtime parameter list of
#' \code{\link{make}(...)},
#' produced with \code{\link{config}()}.
#' Computing this
#' in advance could save time if you plan multiple calls to
#' \code{predict_runtime()}.
#' @param ... Other arguments to pass to \code{\link{config}}
#' 
#' @importFrom lubridate duration as.duration duration
#' @importFrom stats na.omit
#' @importFrom utils tail
#' @import magrittr
#' @export
predict_runtime <- function(plan,
                            from_scratch = FALSE,
                            untimed_method = mean,
                            build_times = data.frame(target = character(0),
                                                     elapsed = duration(numeric(0)),
                                               stringsAsFactors = F),
                            digits = 0,
                            verbose = FALSE,
                            config = NULL,
                            ...){
  
  if (missing(config))
    config = config(plan = plan, verbose = verbose, ...)
  
  build_times_fresh = build_times()
  build_times_fresh = build_times_fresh[!(build_times_fresh$target %in% build_times$target), ]
  build_times = merge(build_times, build_times_fresh, all = TRUE)
  
  graph_remaining_targets = config$graph
  stage_i = 1
  dt = data.frame()
  
  if (from_scratch) {
    cat("If building from scratch:\n\n")
  } else {
    cat("If only building outdated targets:\n\n")
  }
  
  while(length(igraph::V(graph_remaining_targets))) {
    
    candidates = next_targets(graph_remaining_targets)
    targets = Filter(. %>% is_in(plan$target), candidates)
    
    # Filter out targets if they have already been built
    if (!from_scratch)
      targets = Filter(. %>% target_current(hashes(., config), config) %>% not, targets)
    
    if (length(targets)) {

      stage_dt = data.frame(
        parallel_stage = stage_i,
        target = targets
      ) %>% merge(build_times, by = "target", all.x = TRUE)
      
      # Deal with untimed targets
      assumed_times = stage_dt$elapsed
      if (is.function(untimed_method)) {
        if (length(stage_dt$elapsed %>% na.omit)) {
          untimed_filler = untimed_method(stage_dt$elapsed %>% na.omit) %>% duration
        } else {
          untimed_filler = duration(0)
        }
      } else {
        untimed_filler = duration(untimed_method)
      }
      assumed_times[is.na(assumed_times)] <- untimed_filler
      
      # Cumulative times without parallelization
      pick_up_from = tail(dt$cumtime_serial, 1)
      if (is.null(pick_up_from)) pick_up_from = duration(0)
      stage_dt$cumtime_serial = (pick_up_from + cumsum(assumed_times)) %>% round(digits)
      
      # Cumulative times with parallelization and max_useful_jobs
      pick_up_from = tail(dt$cumtime_parallel, 1)
      if (is.null(pick_up_from)) pick_up_from = duration(0)
      stage_dt$cumtime_parallel = (pick_up_from + max(assumed_times)) %>% round(digits) # Rounding is necessary because I'm
                                                                                    # working with an outdated 
                                                                                    # version of lubridate that breaks
                                                                                    # when you have non-integer Durations
                                                                                    # over 60sec. :^/
      
      # Output
      cat("Build stage", stage_i, "\n")
      cat(" ", targets %>% length, "targets,", is.na(stage_dt$elapsed) %>% sum, "untimed\n")
      cat("  Cumulative serial build time:", tail(stage_dt$cumtime_serial, 1) %>% format, "\n")
      cat("  Cumulative parallel build time:", tail(stage_dt$cumtime_parallel, 1) %>% format, "\n")
      
      dt = dt %>% rbind(stage_dt)
      stage_i = stage_i + 1
    }
    
    graph_remaining_targets = igraph::delete_vertices(graph_remaining_targets, v = candidates)
  }
  
  cat("\nView(predict_build_time(...)) for details\n")
  cat("(assuming max_useful_jobs when parallel)\n")
  cat("(not including hashing and storage time [yet])\n")
  
  invisible(dt)
}
