#' @title Return the file path where the cache is stored,
#' if applicable.
#' @export
#' @description Currently only works with
#' \code{storr::storr_rds} file system caches.
#' @return File path where the cache is stored.
#' @param cache the cache whose file path
#' you want to know
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' clean(destroy = TRUE)
#' # Get/create a new drake/storr cache.
#' cache <- recover_cache()
#' # Show the file path of the cache.
#' cache_path(cache = cache)
#' # In-memory caches do not have file paths.
#' mem <- storr_environment()
#' cache_path(cache = mem)
#' })
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

#' @title Get the drake cache, optionally searching up the file system.
#' @description Only works if the cache
#' is in a folder called \code{.drake/}.
#' @seealso \code{\link{this_cache}}, \code{\link{new_cache}},
#' \code{\link{recover_cache}}, \code{\link{config}}
#' @export
#' @return A drake/storr cache in a folder called \code{.drake/},
#' if available. \code{NULL} otherwise.
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
#' @param fetch_cache character vector containing lines of code.
#' The purpose of this code is to fetch the \code{storr} cache
#' with a command like \code{storr_rds()} or \code{storr_dbi()},
#' but customized. This feature is experimental.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' clean(destroy = TRUE)
#' # No cache is available.
#' get_cache() # NULL
#' load_basic_example() # Get the code with drake_example("basic").
#' make(my_plan) # Run the project, build the targets.
#' x <- get_cache() # Now, there is a cache.
#' # List the objects readable from the cache with readd().
#' x$list() # Or x$list(namespace = x$default_namespace)
#' })
#' }
get_cache <- function(
  path = getwd(),
  search = TRUE,
  verbose = 1,
  force = FALSE,
  fetch_cache = NULL
){
  if (search){
    path <- find_cache(path = path)
  } else {
    path <- default_cache_path()
  }
  this_cache(
    path = path, force = force, verbose = verbose,
    fetch_cache = fetch_cache
  )
}

#' @title Get the cache at the exact file path specified.
#' @export
#' @description This function does not apply to
#' in-memory caches such as \code{storr_environment()}.
#' @return A drake/storr cache at the specified path, if it exists.
#' @param path file path of the cache
#' @param force logical, whether to load the cache
#' despite any back compatibility issues with the
#' running version of drake.
#' @param verbose, whether to print the file path of the cache.
#' @param fetch_cache character vector containing lines of code.
#' The purpose of this code is to fetch the \code{storr} cache
#' with a command like \code{storr_rds()} or \code{storr_dbi()},
#' but customized. This feature is experimental.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' clean(destroy = TRUE)
#' try(x <- this_cache(), silent = FALSE) # The cache does not exist yet.
#' load_basic_example() # Get the code with drake_example("basic").
#' make(my_plan) # Run the project, build the targets.
#' y <- this_cache() # Now, there is a cache.
#' z <- this_cache(".drake") # Same as above.
#' manual <- new_cache("manual_cache") # Make a new cache.
#' manual2 <- get_cache("manual_cache") # Get the new cache.
#' })
#' }
this_cache <- function(
  path = drake::default_cache_path(), force = FALSE, verbose = TRUE,
  fetch_cache = NULL
){
  usual_path_missing <- is.null(path) || !file.exists(path)
  if (usual_path_missing & is.null(fetch_cache)){
    return(NULL)
  }
  if (!is.null(path)){
    console_cache(path = path, verbose = verbose)
  }
  fetch_cache <- as.character(fetch_cache)
  if (length(fetch_cache) && nchar(fetch_cache)){
    cache <- eval(parse(text = functionize(fetch_cache)))
  } else {
    cache <- drake_fetch_rds(path)
  }
  configure_cache(
    cache = cache,
    long_hash_algo = "md5",
    overwrite_hash_algos = FALSE,
    log_progress = FALSE
  )
  if (!force){
    assert_compatible_cache(cache = cache)
  }
  cache
}

drake_fetch_rds <- function(path){
  if (!file.exists(path)) {
    return(NULL)
  }
  hash_algo_file <- file.path(path, "config", "hash_algorithm")
  hash_algo <- scan(hash_algo_file, quiet = TRUE, what = character())
  storr::storr_rds(
    path = path,
    mangle_key = TRUE,
    hash_algorithm = hash_algo
  )
}

#' @title  Make a new \code{drake} cache.
#' @description Uses the \code{storr_rds()} function
#' from the \code{storr} package.
#' @export
#' @return A newly created drake cache as a storr object.
#' @seealso \code{\link{default_short_hash_algo}},
#' \code{\link{default_long_hash_algo}},
#' \code{\link{make}}
#' @param path file path to the cache if the cache
#' is a file system cache.
#' @param verbose logical, whether to print out the path of the cache.
#' @param type deprecated argument. Once stood for cache type.
#' Use \code{storr} to customize your caches instead.
#' @param short_hash_algo short hash algorithm for the cache.
#' See \code{\link{default_short_hash_algo}()} and
#' \code{\link{make}()}
#' @param long_hash_algo long hash algorithm for the cache.
#' See \code{\link{default_long_hash_algo}()} and
#' \code{\link{make}()}
#' @param ... other arguments to the cache constructor
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine new_cache() side effects.", {
#' clean(destroy = TRUE) # Should not be necessary.
#' unlink("not_hidden", recursive = TRUE) # Should not be necessary.
#' cache1 <- new_cache() # Creates a new hidden '.drake' folder.
#' cache2 <- new_cache(path = "not_hidden", short_hash_algo = "md5")
#' clean(destroy = TRUE, cache = cache2)
#' })
#' }
new_cache <- function(
  path = drake::default_cache_path(),
  verbose = 1,
  type = NULL,
  short_hash_algo = drake::default_short_hash_algo(),
  long_hash_algo = drake::default_long_hash_algo(),
  ...
){
  if (!is.null(type)){
    warning(
      "The 'type' argument of new_cache() is deprecated. ",
      "Please see the storage vignette for the new cache interface."
    )
  }
  cache <- storr::storr_rds(
    path = path,
    mangle_key = TRUE,
    hash_algorithm = short_hash_algo
  )
  configure_cache(
    cache = cache,
    short_hash_algo = short_hash_algo,
    long_hash_algo = long_hash_algo,
    log_progress = FALSE,
    overwrite_hash_algos = FALSE
  )
  console_cache(path = cache_path(cache), verbose = verbose)
  cache
}

#' @title Load an existing drake files system cache if it exists
#' or create a new one otherwise.
#' @export
#' @seealso \code{\link{new_cache}}, \code{\link{this_cache}},
#' \code{\link{get_cache}}
#' @description
#' Does not work with
#' in-memory caches such as \code{storr_environment()}.
#' @return A drake/storr cache.
#' @param path file path of the cache
#' @param short_hash_algo short hash algorithm for the cache.
#' See \code{\link{default_short_hash_algo}()} and
#' \code{\link{make}()}
#' @param long_hash_algo long hash algorithm for the cache.
#' See \code{\link{default_long_hash_algo}()} and
#' \code{\link{make}()}
#' @param force logical, whether to load the cache
#' despite any back compatibility issues with the
#' running version of drake.
#' @param verbose logical, whether to print the file path of the cache.
#' @param fetch_cache character vector containing lines of code.
#' The purpose of this code is to fetch the \code{storr} cache
#' with a command like \code{storr_rds()} or \code{storr_dbi()},
#' but customized.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' clean(destroy = TRUE)
#' load_basic_example() # Get the code with drake_example("basic").
#' make(my_plan) # Run the project, build all the targets.
#' x <- recover_cache(".drake") # Recover the project's storr cache.
#' })
#' }
recover_cache <- function(
  path = drake::default_cache_path(),
  short_hash_algo = drake::default_short_hash_algo(),
  long_hash_algo = drake::default_long_hash_algo(),
  force = FALSE,
  verbose = TRUE,
  fetch_cache = NULL
){
  cache <- this_cache(
    path = path, force = force, verbose = verbose,
    fetch_cache = fetch_cache
  )
  if (is.null(cache)){
    cache <- new_cache(
      path = path,
      verbose = verbose,
      short_hash_algo = short_hash_algo,
      long_hash_algo = long_hash_algo,
      fetch_cache = fetch_cache
    )
  }
  cache
}

#' @title Return the default file path of the drake/storr cache.
#' @export
#' @description Applies to file system caches only.
#' @return Default file path of the drake/storr cache.
#' @examples
#' default_cache_path()
default_cache_path <- function(){
  file.path(getwd(), ".drake")
}

#' @title Configure the hash algorithms, etc. of a drake cache.
#' @export
#' @seealso \code{\link{default_short_hash_algo}},
#' \code{\link{default_long_hash_algo}}
#' @description The purpose of this function is
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
#' @param log_progress logical, whether to clear the recorded
#' build progress if this cache was used for previous calls to
#' \code{\link{make}()}
#'
#' @param overwrite_hash_algos logical, whether to try to overwrite
#' the hash algorithms in the cache with any user-specified ones.
#'
#' @param verbose whether to print console messages
#'
#' @param jobs number of jobs for parallel processing
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' clean(destroy = TRUE)
#' load_basic_example() # Get the code with drake_example("basic").
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
#' })
#' }
configure_cache <- function(
  cache = drake::get_cache(verbose = verbose),
  short_hash_algo = drake::default_short_hash_algo(cache = cache),
  long_hash_algo = drake::default_long_hash_algo(cache = cache),
  log_progress = FALSE,
  overwrite_hash_algos = FALSE,
  verbose = TRUE,
  jobs = 1
){
  short_hash_algo <- match.arg(short_hash_algo,
    choices = available_hash_algos())
  long_hash_algo <- match.arg(long_hash_algo,
    choices = available_hash_algos())
  if (log_progress){
    clear_progress(cache = cache, jobs = jobs)
  }
  short_exists <- cache$exists(key = "short_hash_algo", namespace = "config")
  long_exists <- cache$exists(key = "long_hash_algo", namespace = "config")
  if (overwrite_hash_algos | !short_exists){
    cache$set(
      key = "short_hash_algo",
      value = short_hash_algo,
      namespace = "config"
    )
  }
  if (overwrite_hash_algos | !long_exists){
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

clear_progress <- function(cache, jobs){
  lightly_parallelize(
    X = cache$list(),
    FUN = function(target){
      cache$del(key = target, namespace = "progress")
    },
    jobs = jobs
  )
  invisible()
}

set_initial_drake_version <- function(cache){
  if (cache$exists(
    key = "initial_drake_version",
    namespace = "session"
  )){
    return()
  } else if (cache$exists(
    key = "sessionInfo",
    namespace = "session"
  )){
    last_session <- drake_session(cache = cache)
  } else {
    last_session <- NULL
  }
  version <- drake_version(session_info = last_session)
  cache$set(
    key = "initial_drake_version",
    value = version,
    namespace = "session"
  )
}

drake_version <- function(session_info = NULL){ # nolint
  if (!length(session_info)){
    return(packageVersion("drake"))
  }
  all_pkgs <- c(
    session_info$otherPkgs, # nolint
    session_info$loadedOnly # nolint
  )
  all_pkgs$drake$Version # nolint
}

safe_get <- function(key, namespace, config){
  if (config$cache$exists(key = key, namespace = namespace)){
    config$cache$get(key = key, namespace = namespace)
  } else {
    NA
  }
}

kernel_exists <- function(target, config){
  config$cache$exists(key = target, namespace = "kernels")
}

target_exists <- kernel_exists
