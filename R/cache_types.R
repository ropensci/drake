#' @title Function cache_types
#' @description List the supported drake cache types.
#' @export
#' @seealso \code{\link{in_memory_cache_types}},
#' \code{\link{new_cache}}, \code{\link{get_cache}},
#' \code{\link{default_cache_type}}
#' @examples
#' cache_types()
cache_types <- function(){
  # The first is the default.
  c(
    "storr_rds",
    "storr_environment"
  )
}

#' @title Function in_memory_cache_types
#' @description List the supported drake in-memory cache types.
#' @export
#' @seealso \code{\link{cache_types}},
#' \code{\link{new_cache}}, \code{\link{get_cache}},
#' \code{\link{default_cache_type}}
#' @examples
#' in_memory_cache_types()
in_memory_cache_types <- function(){
  # The first is the default.
  c(
    "storr_environment"
  )
}

#' @title Function default_cache_type
#' @description Name the default type of drake cache.
#' @export
#' @seealso \code{\link{new_cache}}, \code{\link{get_cache}},
#' \code{\link{default_cache_type}}
#' @examples
#' default_cache_type()
default_cache_type <- function(){
  cache_types()[1]
}

#' @title experimental function type_of_cache
#' @export
#' @description Try to get the type of a drake
#' file system cache. Only works on known caches
#' with known file systems.
#' @details Experimental function for a possible
#' new feature in the future.
#' It will come in handy if/when multiple cache types
#' are supported.
#' @param path path to the cache
#' @examples
#' \dontrun{
#' load_basic_example() # Load drake's canonical example.
#' make(my_plan) # Run the project, build the targets.
#' # Get the type of the cache
#' # located in the (default) '.drake' folder.
#' type_of_cache(".drake")
#' }
type_of_cache <- function(path = drake::default_cache_path()){
  if (!file.exists(path)){
    return(NULL)
  }
  NULL
}
