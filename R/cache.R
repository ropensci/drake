#' @title Function drake_namespaces
#' @export
#' @description List the default namespaces of a \code{storr}
#' cache for drake.
#' @param default name of the default namespace
#' @examples
#' cache_namespaces()
cache_namespaces <- function(default = storr_environment()$default_namespace){
  c(
    default,
    "commands",
    "depends",
    "depends_debug",
    "file_modification_times",
    "imported",
    "readd",
    "reproducibly_tracked",
    "target_attempts",
    "type"
  )
}

#' @title Function cache_path
#' @export
#' @description Returns the file path
#' where the cache is stored. Currently
#' only works with \code{storr} file system
#' caches.
#' @param cache the cache whose file path
#' you want to know
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
#' get_cache()
#' load_basic_example()
#' make(my_plan)
#' x <- get_cache()
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
  console_cache(path = path, verbose = verbose)
  this_cache(path = path, force = force)
}

#' @title Function this_cache
#' @export
#' @description Get a known drake file system cache
#' at the exact specified file path
#' Do not use for in-memory caches such as
#' \code{storr_environment()}.
#' @param path file path of the cache
#' @param force logical, whether to load the cache
#' despite any back compatibility issues with the
#' running version of drake.
#' @examples
#' \dontrun{
#' x <- this_cache() # Does not exist yet
#' load_basic_example()
#' make(my_plan)
#' y <- this_cache()
#' z <- this_cache(".drake") # same as above
#' manual <- new_cache("manual_cache")
#' manual2 <- get_cache("manual_cache") # same as above
#' }
this_cache <- function(
  path = drake::default_cache_path(), force = FALSE
){
  if (is.null(path) || !file.exists(path)){
    return(NULL)
  }
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
#' @seealso \code{\link{default_short_hash_algo}},
#' \code{\link{default_long_hash_algo}},
#' \code{\link{make}}, \code{\link{cache_types}},
#' \code{\link{in_memory_cache_types}}
#' @param path file path to the cache if the cache
#' is a file system cache.
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
#' e <- new.env()
#' ls(e)
#' y <- new_cache(type = "storr_environment", envir = e)
#' ls(e)
#' ls(e)
new_cache <- function(
  path = drake::default_cache_path(),
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
#' @examples
#' \dontrun{
#' load_basic_example()
#' make(my_plan)
#' x <- recover_cache(".drake")
#' }
recover_cache <- function(
  path = drake::default_cache_path(),
  type = drake::default_cache_type(),
  short_hash_algo = drake::default_short_hash_algo(),
  long_hash_algo = drake::default_long_hash_algo(),
  force = FALSE
){
  cache <- this_cache(path = path, force = force)
  if (is.null(cache)){
    cache <- new_cache(
      path = path,
      short_hash_algo = short_hash_algo,
      long_hash_algo = long_hash_algo,
      type = type
    )
  }
  cache
}


#' @title Function default_cache_path
#' @export
#' @description default drake cache path
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
#' load_basic_example()
#' config <- make(my_plan)
#' cache <- config$cache
#' long_hash(cache)
#' cache <- configure_cache(
#'   cache = cache,
#'   long_hash_algo = "murmur32",
#'   overwrite_hash_algos = TRUE
#' )
#' long_hash(cache) # long hash algorithm. See ?default_long_hash_algorithm.
#' make(my_plan) # Changing the long hash puts targets out of date.
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
    last_session <- session(cache = cache)
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
  ifelse(
    key %in% config$cache$list(namespace = namespace),
    config$cache$get(key = key, namespace = namespace),
    NA
  )
}
