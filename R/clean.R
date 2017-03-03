#' @title Function \code{clean}
#' @description Cleans up all work done by \code{\link{make}()}. 
#' You must be in your project's working directory.
#' WARNING: This deletes ALL work done with \code{\link{make}()}, which includes 
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
clean = function(..., list = character(0), destroy = FALSE){
  dots = match.call(expand.dots = FALSE)$...
  targets = targets_from_dots(dots, list)
  if(!length(targets)) return(clean_everything(destroy = destroy))
  uncache(targets, path = getwd(), search = FALSE)
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
