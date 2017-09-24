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
#' @param ... targets to remove from the cache, as names (unquoted)
#' or character strings (quoted). Similar to \code{...} in
#' \code{\link{remove}(...)}.
#' @param list character vector naming targets to be removed from the
#' cache. Similar to the \code{list} argument of \code{\link{remove}()}.
#' @param destroy logical, whether to totally remove the drake cache.
#' If \code{destroy} is \code{FALSE}, only the targets
#' from \code{make}()
#' are removed. If \code{TRUE}, the whole cache is removed, including
#' session metadata, etc.
#' @param cache optional drake cache. See code{\link{new_cache}()}. If
#' If \code{cache} is supplied,
#' the \code{path} and \code{search} arguments are ignored.
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#' @param search logical. If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
#' @examples
#' \dontrun{
#' load_basic_example()
#' make(my_plan)
#' cached(no_imported_objects = TRUE)
#' clean(summ_regression1_large, small)
#' cached(no_imported_objects = TRUE)
#' make(my_plan)
#' clean()
#' clean(destroy = TRUE)
#' }
clean <- function(
  ...,
  list = character(0),
  destroy = FALSE,
  path = getwd(),
  search = TRUE,
  cache = NULL
){
  dots <- match.call(expand.dots = FALSE)$...
  targets <- targets_from_dots(dots, list)
  if (is.null(cache)){
    cache <- get_cache(path = path, search = search)
  }
  if (is.null(cache)){
    return(invisible())
  }
  if (!length(targets)) {
    return(clean_everything(
      destroy = destroy,
      cache = cache
    ))
  }
  uncache(targets, cache = cache)
  invisible()
}

clean_everything <- function(
  destroy,
  cache
){
  empty(cache)
  if (destroy) {
    cache$destroy()
  }
}

empty <- function(cache){
  uncache(target = cache$list(), cache = cache)
}

uncache <- Vectorize(function(target, cache){
  if (is.null(cache)){
    return()
  }
  if (
    is_file(target) &
      !is_imported(
        target = target,
        cache = cache
        )
    ){
    unquote(target) %>%
      unlink(recursive = TRUE, force = TRUE)
  }
  for (space in c("objects", "depends", "filemtime", "functions"))
    if (target %in% cache$list(namespace = space)){
      cache$del(target, namespace = space)
    }
    invisible()
},
"target")
