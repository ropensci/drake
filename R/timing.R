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
build_times <- function(path = getwd(), search = TRUE, digits = 0,
  cache = drake::get_cache(path = path, search = search)) {

  empty_times <- data.frame(
    target = character(0),
    user = numeric(0),
    system = numeric(0),
    elapsed = numeric(0)
  )

  if (is.null(cache)){
    return(empty_times)
  }

  eval(parse(text = "require(methods, quietly = TRUE)"))  # needed for dseconds
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
  times
}
