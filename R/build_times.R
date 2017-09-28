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
  if (is.null(cache)){
    cache <- get_cache(path = path, search = search)
  }
  if (is.null(cache)){
    return(empty_times())
  }
  lapply(
    cache$list(namespace = "build_times"),
    get_build_time,
    cache = cache,
    digits = digits
  ) %>%
    do.call(what = rbind) %>%
    rbind(empty_times()) %>%
    round_times(digits = digits)
}

get_build_time <- function(target, cache, digits) {
  time <- cache$get(key = target, namespace = "build_times")
  stopifnot(class(time) == "proc_time")
  out <- list(
    elapsed = time[["elapsed"]],
    user = time[["user.self"]],
    system = time[["sys.self"]]
  ) %>%
    lapply(FUN = dseconds)
  c(target = target, out) %>%
    as.data.frame(stringsAsFactors = FALSE)
}
  
empty_times <- function(){
  data.frame(
    target = character(0),
    elapsed = duration(numeric(0)),
    user = duration(numeric(0)),
    system = duration(numeric(0))
  )
}

round_times <- function(times, digits){
  cols <- setdiff(colnames(empty_times()), "target")
  for (col in cols){
    times[[col]] <- round(times[[col]], digits = digits)
  }
  times
}
