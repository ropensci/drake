#' @title Function \code{find_cache}
#' @description Return the file path of the nearest drake
#' cache (searching upwards for directories containing a drake cache).
#' @seealso \code{\link{plan}}, \code{\link{make}},
#' @export
#' @return File path of the nearest drake cache or \code{NULL}
#' if no cache is found.
#' @param path starting path for search back for the cache.
#' Should be a subdirectory of the drake project.
#' @examples
#' \dontrun{
#' load_basic_example()
#' make(my_plan)
#' find_cache()
#' }
find_cache <- function(path = getwd()){
  while (!(cache_dir %in% list.files(path = path, all.files = TRUE))){
    path <- dirname(path)
    if (path == dirname(path)){
      return(NULL)
    }
  }
  path <- file.path(path, cache_dir)
  if (!file.exists(path)){
    return(NULL)
  }
  return(path)
}

#' @title Function \code{find_project}
#' @description Return the file path of the nearest drake
#' project (searching upwards for directories
#' containing a drake cache).
#' @export
#' @seealso \code{\link{plan}}, \code{\link{make}}
#' @return File path of the nearest drake project or \code{NULL}
#' if no drake project is found.
#' @param path starting path for search back for the project.
#' Should be a subdirectory of the drake project.
#' @examples
#' \dontrun{
#' load_basic_example()
#' make(my_plan)
#' find_project()
#' }
find_project = function(path = getwd()){
  cache = find_cache(path = path)
  if(is.null(cache)) return()
  dirname(cache)
}
