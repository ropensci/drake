#' @title Function \code{cache_types}
#' @description List the supported drake cache types.
#' @export
#' @return Character vector naming the supported cache types.
#' @seealso \code{\link{in_memory_cache_types}},
#' \code{\link{new_cache}}, \code{\link{get_cache}},
#' \code{\link{default_cache_type}}
#' @examples
#' cache_types()
cache_types <- function(){
  # The first is the default.
  c(
    "storr_rds",
    in_memory_cache_types()
  )
}

#' @title Function \code{in_memory_cache_types}
#' @description List the supported drake in-memory cache types.
#' @export
#' @return Character vector naming the supported
#' in-memory cache types.
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

#' @title Function \code{default_cache_type}
#' @description Names the default type of drake cache.
#' @export
#' @return Default drake cache type.
#' @seealso \code{\link{new_cache}}, \code{\link{get_cache}},
#' \code{\link{default_cache_type}}
#' @examples
#' default_cache_type()
default_cache_type <- function(){
  cache_types()[1]
}

#' @title Experimental function \code{type_of_cache}
#' @export
#' @description Try to get the type of a drake
#' file system cache. Only works on known caches
#' with known file systems.
#' @details Experimental function for a possible
#' new future features.
#' @return Type of the cache at the specified path.
#' @param path Path to the file system cache.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Load drake's canonical example.
#' make(my_plan) # Run the project, build the targets.
#' # Get the type of the cache
#' # located in the (default) '.drake' folder.
#' type_of_cache(".drake") # NULL, signaling "storr_rds"
#' })
#' }
type_of_cache <- function(path = drake::default_cache_path()){
  if (!file.exists(path)){
    return(NULL)
  }
  # NULL means the same thing as the default "storr_rds".
  NULL
}
