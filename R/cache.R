#' @title Function cache_namespaces
#' @export
#' @seealso \code{\link{make}}
#' @return A character vector of storr namespaces used for drake.
#' @description List the important \code{storr} cache namespaces
#' that need to be inventoried periodically in a call to \code{\link{make}()}.
#' Ordinary users do not need to worry about this function.
#' It is just another window into \code{drake}'s internals.
#' @param default name of the default \code{storr} namespace
#' @examples
#' cache_namespaces()
cache_namespaces <- function(
  default = storr::storr_environment()$default_namespace
){
  c(
    default,
    "attempts",
    "commands",
    "depends",
    "imported",
    "kernels",
    "mtimes",
    "readd",
    "type"
  )
}

#' @title Function cache_path
#' @export
#' @description Returns the file path
#' where the cache is stored. Currently
#' only works with \code{storr} file system
#' caches.
#' @return File path where the cache is stored.
#' @param cache the cache whose file path
#' you want to know
#' @examples
#' \dontrun{
#' # Get/create a new drake/storr cache.
#' cache <- recover_cache()
#' # Show the file path of the cache.
#' cache_path(cache = cache)
#' # In-memory caches do not have file paths.
#' mem <- new_cache(type = "storr_environment") # or just storr_environment()
#' cache_path(cache = mem)
#' }
cache_path <- function(cache = NULL){
  if (is.null(cache)){
    NULL
  } else if ("storr" %in% class(cache)){
    cache$driver$path
  } else {
    NULL
  }
}

#' @title Function get_cache
#' @description Search for and return a drake file system cache.
#' @seealso \code{\link{this_cache}}, \code{\link{new_cache}},
#' \code{\link{recover_cache}}, \code{\link{config}}
#' @export
#' @return A drake/storr cache, if available. \code{NULL} otherwise.
#' @param path file path to the folder containing the cache.
#' Yes, this is the parent directory containing the cache,
#' not the cache itself, and it assumes the cache is in the
#' `.drake` folder. If you are looking for a different cache
#' with a known folder different from `.drake`, use
#' the \code{\link{this_cache}()} function.
#' @param search logical, whether to search back in the file system
#' for the cache.
#' @param verbose logical, whether to print the location of the cache
#' @param force logical, whether to load the cache
#' despite any back compatibility issues with the
#' running version of drake.
#' @examples
#' \dontrun{
#' # No cache is available.
#' get_cache() # NULL
#' load_basic_example() # Load drake's canonical example.
#' make(my_plan) # Run the project, build the targets.
#' x <- get_cache() # Now, there is a cache.
#' # List the objects readable from the cache with readd().
#' x$list(namespace = "readd")
#' }
get_cache <- function(
  path = getwd(),
  search = TRUE,
  verbose = TRUE,
  force = FALSE
){
  if (search){
    path <- find_cache(path = path)
  } else {
    path <- default_cache_path()
  }
  this_cache(path = path, force = force, verbose = verbose)
}

#' @title Function this_cache
#' @export
#' @description Get a known drake file system cache
#' at the exact specified file path
#' Do not use for in-memory caches such as
#' \code{storr_environment()}.
#' @return A drake/storr cache at the specified path, if it exists.
#' @param path file path of the cache
#' @param force logical, whether to load the cache
#' despite any back compatibility issues with the
#' running version of drake.
#' @param verbose, whether to print the file path of the cache.
#' @examples
#' \dontrun{
#' x <- this_cache() # The cache does not exist yet.
#' load_basic_example() # Load drake's canonical example.
#' make(my_plan) # Run the project, build the targets.
#' y <- this_cache() # Now, there is a cache.
#' z <- this_cache(".drake") # Same as above.
#' manual <- new_cache("manual_cache") # Make a new cache.
#' manual2 <- get_cache("manual_cache") # Get the new cache.
#' }
this_cache <- function(
  path = drake::default_cache_path(), force = FALSE, verbose = TRUE
){
  if (is.null(path) || !file.exists(path)){
    return(NULL)
  }
  console_cache(path = path, verbose = verbose)
  type <- type_of_cache(path)
  if (is.null(type)) {
    type <- default_cache_type()
  }
  cache_fetcher <- paste0("get_", type, "_cache")
  cache <- get(cache_fetcher, envir = getNamespace("drake"))(
    path = path
  )
  if (!force){
    assert_compatible_cache(cache = cache)
  }
  cache
}

#' @title Function new_cache
#' @description Make a new drake cache. Could be any
#' type of cache in \code{\link{cache_types}()}.
#' @export
#' @return A newly created drake cache as a storr object.
#' @seealso \code{\link{default_short_hash_algo}},
#' \code{\link{default_long_hash_algo}},
#' \code{\link{make}}, \code{\link{cache_types}},
#' \code{\link{in_memory_cache_types}}
#' @param path file path to the cache if the cache
#' is a file system cache.
#' @param verbose logical, whether to print out the path of the cache.
#' @param type character scalar, type of the drake cache.
#' Must be among the list of supported caches
#' in \code{\link{cache_types}()}.
#' @param short_hash_algo short hash algorithm for the cache.
#' See \code{\link{default_short_hash_algo}()} and
#' \code{\link{make}()}
#' @param long_hash_algo long hash algorithm for the cache.
#' See \code{\link{default_long_hash_algo}()} and
#' \code{\link{make}()}
#' @param ... other arguments to the cache constructor
#' @examples
#' \dontrun{
#' cache1 <- new_cache() # Creates a new hidden '.drake' folder.
#' cache2 <- new_cache(path = "not_hidden", short_hash_algo = "md5")
#' }
#' # Create a storr_environment() cache from a custom environment.
#' e <- new.env() # Create a new environment.
#' ls(e) # Emtpy.
#' y <- new_cache(type = "storr_environment", envir = e)
#' ls(e) # Storr populates the environment for use in an in-memory cache.
new_cache <- function(
  path = drake::default_cache_path(),
  verbose = TRUE,
  type = drake::default_cache_type(),
  short_hash_algo = drake::default_short_hash_algo(),
  long_hash_algo = drake::default_long_hash_algo(),
  ...
){
  if (type %in% in_memory_cache_types()){
    path <- NULL
  } else if (file.exists(path)){
    stop("Cannot create new cache at ", path, ". File already exists.")
  }
  type <- match.arg(type, choices = cache_types())
  cache_constructor <- paste0("new_", type, "_cache")
  cache <- get(cache_constructor, envir = getNamespace("drake"))(
    path = path,
    short_hash_algo = short_hash_algo,
    long_hash_algo = long_hash_algo,
    ...
  )
  configure_cache(
    cache = cache,
    short_hash_algo = short_hash_algo,
    long_hash_algo = long_hash_algo,
    clear_progress = FALSE,
    overwrite_hash_algos = FALSE
  )
  console_cache(path = cache_path(cache), verbose = verbose)
  cache
}

#' @title Function recover_cache
#' @export
#' @seealso \code{\link{new_cache}}, \code{\link{this_cache}},
#' \code{\link{get_cache}}
#' @description Load an existing drake files system cache if it exists
#' and create a new one otherwise.
#' Do not use for in-memory caches such as
#' \code{storr_environment()}.
#' For internal use only.
#' @return A drake/storr cache.
#' @param path file path of the cache
#' @param short_hash_algo short hash algorithm for the cache.
#' See \code{\link{default_short_hash_algo}()} and
#' \code{\link{make}()}
#' @param long_hash_algo long hash algorithm for the cache.
#' See \code{\link{default_long_hash_algo}()} and
#' \code{\link{make}()}
#' @param type character scalar, type of the drake cache.
#' Must be among the list of supported caches
#' in \code{\link{cache_types}()}.
#' @param force logical, whether to load the cache
#' despite any back compatibility issues with the
#' running version of drake.
#' @param verbose logical, whether to print the file path of the cache.
#' @examples
#' \dontrun{
#' load_basic_example() # Load drake's canonical example.
#' make(my_plan) # Run the project, build all the targets.
#' x <- recover_cache(".drake") # Recover the project's storr cache.
#' }
recover_cache <- function(
  path = drake::default_cache_path(),
  type = drake::default_cache_type(),
  short_hash_algo = drake::default_short_hash_algo(),
  long_hash_algo = drake::default_long_hash_algo(),
  force = FALSE,
  verbose = TRUE
){
  cache <- this_cache(path = path, force = force, verbose = verbose)
  if (is.null(cache)){
    cache <- new_cache(
      path = path,
      verbose = verbose,
      short_hash_algo = short_hash_algo,
      long_hash_algo = long_hash_algo,
      type = type
    )
  }
  cache
}

#' @title Function default_cache_path
#' @export
#' @description Return the default file path of the drake/storr cache.
#' @return Default file path of the drake/storr cache.
#' @examples
#' default_cache_path()
default_cache_path <- function(){
  file.path(getwd(), ".drake")
}

#' @title Function configure_cache
#' @export
#' @seealso \code{\link{default_short_hash_algo}},
#' \code{\link{default_long_hash_algo}}
#' @description configure a cache for drake. This is
#' to prepare the cache to be called from \code{\link{make}()}.
#' @return A drake/storr cache.
#'
#' @param cache cache to configure
#'
#' @param short_hash_algo short hash algorithm for drake.
#'
#' The short algorithm must be among \code{\link{available_hash_algos}{}},
#' which is just the collection of algorithms available to the `algo`
#' argument in \code{digest::digest()}.
#' See \code{?\link{default_short_hash_algo}} for more.
#'
#' @param long_hash_algo short hash algorithm for drake.
#' The long algorithm must be among \code{\link{available_hash_algos}{}},
#' which is just the collection of algorithms available to the `algo`
#' argument in \code{digest::digest()}.
#' See \code{?\link{default_long_hash_algo}} for more.
#'
#' @param clear_progress logical, whether to clear the recorded
#' build progress if this cache was used for previous calls to
#' \code{\link{make}()}
#'
#' @param overwrite_hash_algos logical, whether to try to overwrite
#' the hash algorithms in the cache with any user-specified ones.
#'
#' @param verbose whether to print console messages
#'
#' @examples
#' \dontrun{
#' load_basic_example() # Load drake's canonical example into the workspace.
#' config <- make(my_plan) # Run the project, build all the targets.
#' # Locate the drake/storr cache of the project
#' # inside the master internal configuration list.
#' cache <- config$cache
#' long_hash(cache) # Return the long hash algorithm used.
#' # Change the long hash algorithm of the cache.
#' cache <- configure_cache(
#'   cache = cache,
#'   long_hash_algo = "murmur32",
#'   overwrite_hash_algos = TRUE
#' )
#' long_hash(cache) # Show the new long hash algorithm.
#' make(my_plan) # Changing the long hash puts the targets out of date.
#' }
configure_cache <- function(
  cache = drake::get_cache(verbose = verbose),
  short_hash_algo = drake::default_short_hash_algo(cache = cache),
  long_hash_algo = drake::default_long_hash_algo(cache = cache),
  clear_progress = FALSE,
  overwrite_hash_algos = FALSE,
  verbose = TRUE
){
  short_hash_algo <- match.arg(short_hash_algo,
    choices = available_hash_algos())
  long_hash_algo <- match.arg(long_hash_algo,
    choices = available_hash_algos())
  if (clear_progress){
    cache$clear(namespace = "progress")
  }
  config_keys <- cache$list(namespace = "config")
  if (overwrite_hash_algos | !("short_hash_algo" %in% config_keys)){
    cache$set(
      key = "short_hash_algo",
      value = short_hash_algo,
      namespace = "config"
    )
  }
  if (overwrite_hash_algos | !("long_hash_algo" %in% config_keys)){
    cache$set(
      key = "long_hash_algo",
      value = long_hash_algo,
      namespace = "config"
    )
  }
  chosen_algo <- short_hash(cache)
  check_storr_short_hash(cache = cache, chosen_algo = chosen_algo)
  set_initial_drake_version(cache)
  cache
}

set_initial_drake_version <- function(cache){
  keys <- cache$list(namespace = "session")
  if ("initial_drake_version" %in% keys){
    return()
  } else if ("sessionInfo" %in% keys){
    last_session <- drake_session(cache = cache)
  } else {
    last_session <- sessionInfo()
  }
  version <- drake_version(last_session)
  cache$set(
    key = "initial_drake_version",
    value = version,
    namespace = "session"
  )
}

drake_version <- function(session_info = sessionInfo()){ # nolint
  session_info$otherPkgs$drake$Version # nolint
}

safe_get <- function(key, namespace, config){
  if (config$cache$exists(key = key, namespace = namespace)){
    config$cache$get(key = key, namespace = namespace)
  } else {
    NA
  }
}
