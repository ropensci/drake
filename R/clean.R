#' @title Function \code{clean}
#' @description Cleans up all work done by \code{\link{make}()}.
#' @details You must be in your project's working directory
#' or a subdirectory of it.
#' \code{clean(search = TRUE)} searches upwards in your folder structure
#' for the drake cache and acts on the first one it sees. Use
#' \code{search == FALSE} to look within the current working
#' directory only.
#' WARNING: This deletes ALL work done with \code{\link{make}()},
#' which includes
#' file targets as well as the entire drake cache. Only use \code{clean()}
#' if you're sure you won't lose anything important.
#' @seealso \code{\link{prune}}, \code{\link{make}},
#' @export
#' @return Invisibly return \code{NULL}.
#'
#' @param ... targets to remove from the cache, as names (unquoted)
#' or character strings (quoted). Similar to \code{...} in
#' \code{\link{remove}(...)}.
#' The symbols must not match other (formal) arguments of \code{clean()},
#' such as \code{destroy}, \code{cache}, \code{path}, \code{search},
#' \code{verbose}, or \code{jobs}. If there are name conflicts,
#' use the \code{list} argument instead of \code{...}.
#'
#' @param list character vector naming targets to be removed from the
#' cache. Similar to the \code{list} argument of \code{\link{remove}()}.
#'
#' @param destroy logical, whether to totally remove the drake cache.
#' If \code{destroy} is \code{FALSE}, only the targets
#' from \code{make}()
#' are removed. If \code{TRUE}, the whole cache is removed, including
#' session metadata, etc.
#'
#' @param cache optional drake cache. See code{\link{new_cache}()}. If
#' If \code{cache} is supplied,
#' the \code{path} and \code{search} arguments are ignored.
#'
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#'
#' @param search logical. If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
#'
#' @param verbose whether to print console messages
#'
#' @param jobs Number of jobs for light parallelism
#' (disabled on Windows).
#'
#' @param force logical, whether to try to clean the cache
#' even though the project may not be back compatible with the
#' current version of drake.
#'
#' @examples
#' \dontrun{
#' load_basic_example() # Load drake's canonical example.
#' make(my_plan) # Run the project, build the targets.
#' # List objects in the cache, excluding R objects
#' # imported from your workspace.
#' cached(no_imported_objects = TRUE)
#' # Remove 'summ_regression1_large' and 'small' from the cache.
#' clean(summ_regression1_large, small)
#' # Those objects should be gone.
#' cached(no_imported_objects = TRUE)
#' # Rebuild the missing targets.
#' make(my_plan)
#' # Remove all the targets and imports.
#' # On non-Windows machines, parallelize over at most 2 jobs.
#' clean(jobs = 2)
#' # All the targets and imports are gone.
#' cached()
#' # Completely remove the entire cache (default: '.drake/' folder).
#' clean(destroy = TRUE)
#' }
clean <- function(
  ...,
  list = character(0),
  destroy = FALSE,
  path = getwd(),
  search = TRUE,
  cache = NULL,
  verbose = TRUE,
  jobs = 1,
  force = FALSE
){
  dots <- match.call(expand.dots = FALSE)$...
  targets <- targets_from_dots(dots, list)
  if (is.null(cache)){
    cache <- get_cache(
      path = path, search = search, verbose = verbose, force = force)
  }
  if (is.null(cache)){
    return(invisible())
  }
  rescue_cache(cache = cache)
  if (!length(targets)) {
    return(clean_everything(
      destroy = destroy,
      cache = cache,
      jobs = jobs
    ))
  }
  uncache(targets = targets, cache = cache, jobs = jobs)
  invisible()
}

clean_everything <- function(
  destroy,
  cache,
  jobs
){
  empty(cache = cache, jobs = jobs)
  if (destroy) {
    cache$destroy()
  }
}

empty <- function(cache, jobs){
  uncache(
    targets = cache$list(namespace = "readd"), cache = cache, jobs = jobs)
}

uncache <- function(targets, cache, jobs){
  if (is.null(cache)){
    return()
  }
  files <- Filter(x = targets, f = is_file)
  lightly_parallelize(
    X = targets,
    FUN = remove_file_target,
    jobs = jobs,
    cache = cache
  )
  namespaces <- cache_namespaces(default = cache$default_namespace)
  for (namespace in namespaces){
    lightly_parallelize(
      X = targets,
      FUN = uncache_single,
      jobs = jobs,
      cache = cache,
      namespace = namespace,
      list = cache$list(namespace = namespace)
    )
  }
  invisible()
}

remove_file_target <- function(target, cache){
  if (
    is_file(target) &&
    !is_imported(
      target = target,
      cache = cache
    )
  ){
    unquote(target) %>%
      unlink(recursive = TRUE, force = TRUE)
  }
}

uncache_single <- function(target, cache, namespace, list){
  if (target %in% list){
    cache$del(target, namespace = namespace)
  }
}
