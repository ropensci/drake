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
#' @param cache storr cache for drake
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
  cache = drake::get_cache(path = path, search = search)
  ){
  eval(parse(text = "require(methods, quietly = TRUE)"))  # needed for dseconds

  empty_times <- data.frame(
    target = character(0),
    user = duration(numeric(0)),
    system = duration(numeric(0)),
    elapsed = duration(numeric(0))
  )

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

#' @importFrom lubridate duration as.duration
#' @export
predict_runtime <- function(plan, from_scratch = FALSE, config = NULL, ...){
  
  if (missing(config))
    config = config(plan = plan, ...)
  
  times = build_times()
  graph_remaining_targets = config$graph
  i = 1
  total_time = duration(0)
  
  while(length(V(graph_remaining_targets))) {
    
    candidates = next_targets(graph_remaining_targets)
    targets = Filter(x = candidates, . %>% is_in(plan$target))
    
    # Filter out targets if they have already been built
    if (!from_scratch)
      targets = Filter(x = targets, . %>% target_current(hashes(., config), config) %>% not)
    
    if (length(targets)) {
      cat("Build stage", i, "\n")
      cat("  Targets:", targets, "\n")
      
      untimed = setdiff(targets, times$target)
      time = times[times$target %in% targets,]$elapsed
      if (length(time)) {
        time = time %>% max %>% as.duration
        cat("  Est build time:", time %>% format, "\n")
        total_time = total_time + time
      }
      if (length(untimed)) {
        cat("  Untimed targets:", untimed, "\n")
      }
      
      i = i + 1
    }
    
    graph_remaining_targets = delete_vertices(graph_remaining_targets, v = candidates)
  }
  
  cat("\nTOTAL BUILD TIME:", total_time %>% format, "\n")
  cat(" ", setdiff(plan$target, times$target) %>% length, "untimed targets (never built)\n")
  cat("  (assuming max_useful_jobs)\n")
  cat("  (not including hashing and storage time [yet])\n")
  
  invisible(total_time)
}
