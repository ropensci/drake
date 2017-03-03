#' @title Function \code{clean}
#' @description Cleans up all work done by \code{\link{run}()}. 
#' You must be in your project's working directory.
#' WARNING: This deletes ALL work done with \code{\link{run}()}, which includes 
#' file targets as well as the entire drake cache. Only use \code{clean()}
#' if you're sure you won't lose anything important.
#' @seealso \code{\link{prune}}, \code{\link{run}}, 
#' @export
#' @param x If \code{x} is \code{NULL}, everything in the cache is removed,
#' and all drake-generated files are deleted. 
#' Otherwise, \code{x} is either a workflow plan data frame or a character vector
#' of targets. First suppose \code{action} is \code{"remove"}. If \code{x}  
#' is a character vector, the specified targets will be removed from the cache
#' (and the associated drake-generated external files will be deleted), 
#' and the dependencies will be left alone. 
#' If \code{x} is a workflow plan data frame,
#'  the targets in the \code{target} column
#' and their dependencies will be removed (including drake-generated 
#' external files). If \code{action} is \code{"keep"}, the complement of this 
#' scenario is true. Everything except targets \code{x} will be removed if 
#' \code{x} is a character vector, and everything except the targets and 
#' dependencies will be removed if \code{x} is a workflow plan data frame.
#' @param envir environment, same as in \code{\link{make}()}. If \code{x}
#' is a workflow plan data frame, this is needed in order to find the 
#' dependencies of the targets and remove them. Most of the time, this
#' will just be your current workspace, so you usually should not need
#' to set this manually.
#' @param action If \code{"remove"}, the targets and their dependencies will
#' be removed from the cache, and any associated external files will be removed.
#' If \code{action} is \code{"keep"}, the targets and their dependencies 
#' will be kept, but everything else will be removed from the cache.
#' @param destroy logical, whether to totally remove the drake cache. 
#' If \code{destroy} is \code{FALSE}, only the targets 
#' from \code{run}()
#' are removed. If \code{TRUE}, the whole cache is removed, including
#' session metadata, etc.
clean = function(x = NULL, envir = parent.frame(),
  action = c("remove", "keep"), destroy = FALSE){
  action = match.arg(action)
  if(is.null(x)) return(clean_everything(destroy = destroy))
  if(is.vector(x)){
    if(action == "remove") uncache(x, path = getwd(), search = TRUE)
    else if(action == "keep") uncache(setdiff(cached(), x), 
      path = getwd(), search = TRUE)
    return(invisible())
  }
  stopifnot(is.data.frame(x))
  force(envir)
  action = match.arg(action)
  config = config(plan = x, targets = x$target, envir = envir, jobs = 1,
    parallelism = parallelism_choices()[1], verbose = TRUE, packages = character(0),
    prework = character(0), prepend = character(0), command = character(0), 
    args = character(0))
  if(action == "remove") uncache(config$order, path = getwd(), search = FALSE)
  else setdiff(cached(), config$order) %>% uncache(path = getwd(), search = FALSE)
  invisible()
}

clean_everything = function(destroy){
  uncache(cached(), path = getwd(), search = FALSE)
  if(destroy) unlink(cachepath, recursive = TRUE)
  invisible()
}

uncache = Vectorize(function(target, path, search){
  if(is_file(target) & !is_imported(target = target, 
    path = path, search = search))
    unquote(target) %>% unlink(recursive = TRUE)
  cache = get_cache(path = path, search = search)
  for(space in c("objects", "depends", "filemtime"))
    if(target %in% cache$list(namespace = space))
      cache$del(target, namespace = space)
  invisible()
}, "target")

