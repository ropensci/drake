#' @title Function \code{find_cache}
#' @description Return the file path of the nearest drake
#' cache (searching upwards for directories containing a drake cache).
#' Only works if the cache is a file system in a
#' hidden folder named \code{.drake}.
#' @seealso \code{\link{workflow}}, \code{\link{make}},
#' @export
#' @return File path of the nearest drake cache or \code{NULL}
#' if no cache is found.
#' @param path starting path for search back for the cache.
#' Should be a subdirectory of the drake project.
#' @param directory Name of the folder containing the cache.
#' @examples
#' \dontrun{
#' load_basic_example()
#' make(my_plan)
#' find_cache()
#' }
find_cache <- function(
  path = getwd(),
  directory = basename(drake::default_cache_path())
){
  while (!(directory %in% list.files(path = path, all.files = TRUE))){
    path <- dirname(path)
    # If we can search no higher...
    if (path == dirname(path)){
      return(NULL) # The cache does not exist
    }
  }
  file.path(path, directory)
}

#' @title Function \code{find_project}
#' @description Return the file path of the nearest drake
#' project (searching upwards for directories
#' containing a drake cache).
#' Only works if the cache is a file system in a folder named \code{.drake}.
#' @export
#' @seealso \code{\link{workflow}}, \code{\link{make}}
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
find_project <- function(path = getwd()){
  cache <- find_cache(path = path)
  if (is.null(cache)){
    return(NULL)
  }
  return(dirname(cache))
}
