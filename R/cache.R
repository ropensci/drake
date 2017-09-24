cache_dir <- ".drake"
globalenvpath <- file.path(cache_dir, "globalenv.RData")

#' @title Function get_cache
#' @description Search for and return a drake cache.
#' @seealso \code{\link{config}}
#' @export
#' @param path file path to the folder contianing the cache.
#' Yes, this is the parent directory containing the cache,
#' not the cache itself.
#' @param search logical, whether to search back in the file system
#' for the cache.
#' @examples
#' \dontrun{
#' get_cache()
#' load_basic_example()
#' make(my_plan)
#' get_cache()
#' }
get_cache <- function(
  path = getwd(),
  search = TRUE
){
  if (search){
    path <- find_cache(path = path)
  } else {
    path <- default_cache_path()
  }
  this_cache(path = path)
}

#' @title Function this_cache
#' @export
#' @description Get a specific drake cache
#' at the exact specified file path
#' @param path file path of the cache
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
  path = NULL
){
  if (is.null(path)){
    path <- file.path(getwd(), cache_dir)
  }
  if (!file.exists(path)){
    return(NULL)
  }
  type <- type_of_cache(path)
  if (is.null(type)) {
    type <- default_cache_type()
  }
  cache_fetcher <- paste0("get_", type, "_cache")
  get(cache_fetcher, envir = getNamespace("drake"))(
    path = path
  )
}

#' @title Function new_cache
#' @description Make a new drake cache.
#' @export
#' @seealso \code{\link{default_short_hash_algo}},
#' \code{\link{default_long_hash_algo}},
#' \code{\link{make}}, \code{\link{cache_types}}
#' @param path file path to the cache
#' @param type character scalar, type of the drake cache.
#' Must be among the list of supported caches
#' in \code{\link{cache_types}()}.
#' @param short_hash_algo short hash algorithm for the cache.
#' See \code{\link{default_short_hash_algo}()} and
#' \code{\link{make}()}
#' @param long_hash_algo long hash algorithm for the cache.
#' See \code{\link{default_long_hash_algo}()} and
#' \code{\link{make}()}
#' @examples
#' \dontrun{
#' new_cache() # Creates a new hidden '.drake' folder.
#' new_cache(path = "not_hidden", hash_algo = "md5")
#' }
new_cache <- function(
  path = NULL,
  type = drake::default_cache_type(),
  short_hash_algo = default_short_hash_algo(),
  long_hash_algo = default_long_hash_algo()
){
  if (is.null(path)){
    path <- file.path(path = getwd(), cache_dir)
  }
  if (file.exists(path)){
    stop("Cannot create new cache at ", path, ". File already exists.")
  }
  type <- match.arg(type, choices = cache_types())
  cache_constructor <- paste0("new_", type, "_cache")
  cache <- get(cache_constructor, envir = getNamespace("drake"))(
    path = path,
    short_hash_algo = short_hash_algo,
    long_hash_algo = long_hash_algo
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
#' @description Load an existing drake cache if it exists
#' and create a new one otherwise.
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
#' @examples
#' \dontrun{
#' load_basic_example()
#' make(my_plan)
#' x <- recover_cache(".drake")
#' }
recover_cache <- function(
  path = NULL,
  type = drake::default_cache_type(),
  short_hash_algo = default_short_hash_algo(),
  long_hash_algo = default_long_hash_algo()
){
  if (is.null(path)){
    path <- default_cache_path()
  }
  cache <- this_cache(path = path)
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
#' default_cache_path
default_cache_path <- function(){
  file.path(getwd(), cache_dir)
}

#' @title Function configure_cache
#' @export
#' @seealso \code{\link{default_short_hash_algo}},
#' \code{\link{default_long_hash_algo}}
#' @description configure a cache for drake. This is
#' to prepare the cache to be called from \code{\link{make}()}.
#' @param cache cache to configure
#' @param short_hash_algo short hash algorithm for drake.
#' The short algorithm must be among \code{\link{available_hash_algos}{}},
#' which is just the collection of algorithms available to the `algo`
#' argument in \code{digest::digest()}.
#' See \code{?\link{default_short_hash_algo}} for more.
#' @param long_hash_algo short hash algorithm for drake.
#' The long algorithm must be among \code{\link{available_hash_algos}{}},
#' which is just the collection of algorithms available to the `algo`
#' argument in \code{digest::digest()}.
#' See \code{?\link{default_long_hash_algo}} for more.
#' @param clear_progress logical, whether to clear the recorded
#' build progress if this cache was used for previous calls to
#' \code{\link{make}()}
#' @param overwrite_hash_algos logical, whether to try to overwrite
#' the hash algorithms in the cache with any user-specified ones.
#' @examples
#' \dontrun{
#' load_basic_example()
#' config <- make(my_plan, return_config = TRUE)
#' cache <- config$cache
#' long_hash(cache)
#' cache <- configure_cache(
#'   cache = cache,
#'   long_hash_algo = "murmur32",
#'   overwrite_hash_algos = TRUE
#' )
#' long_hash(cache)
#' }
configure_cache <- function(
  cache,
  short_hash_algo = NULL,
  long_hash_algo = NULL,
  clear_progress = FALSE,
  overwrite_hash_algos = FALSE
){
  if (is.null(short_hash_algo)){
    short_hash_algo <- short_hash(cache)
    if (is.null(short_hash_algo)){
      short_hash_algo <- default_short_hash_algo()
    }
  }
  if (is.null(long_hash_algo)){
    long_hash_algo <- long_hash(cache)
    if (is.null(long_hash_algo)){
      long_hash_algo <- defualt_long_hash_algo()
    }
  }
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
  cache
}

#' @title Function long_hash
#' @export
#' @seealso \code{\link{default_short_hash_algo}},
#' \code{\link{default_long_hash_algo}}
#' @description Get the long hash algorithm of a drake cache.
#' @details See \code{?\link{default_long_hash_algo}()}
#' @param cache drake cache
#' @examples
#' \dontrun{
#' load_basic_example()
#' config <- make(my_plan, return_config = TRUE)
#' cache <- config$cache
#' long_hash(cache)
#' }
long_hash <- function(cache){
  if (!("long_hash_algo" %in% cache$list(namespace = "config"))){
    return(NULL)
  }
  cache$get("long_hash_algo", namespace = "config")
}

#' @title Function short_hash
#' @export
#' @seealso \code{\link{default_short_hash_algo}},
#' \code{\link{default_long_hash_algo}}
#' @description Get the short hash algorithm of a drake cache.
#' @details See \code{?\link{default_long_hash_algo}()}
#' @param cache drake cache
#' @examples
#' \dontrun{
#' load_basic_example()
#' config <- make(my_plan, return_config = TRUE)
#' cache <- config$cache
#' short_hash(cache)
#' }
short_hash <- function(cache){
  if (!("short_hash_algo" %in% cache$list(namespace = "config"))){
    return(NULL)
  }
  chosen_algo <- cache$get("short_hash_algo", namespace = "config")
  check_storr_short_hash(cache = cache, chosen_algo = chosen_algo)
  cache$get("short_hash_algo", namespace = "config")
}

check_storr_short_hash <- function(cache, chosen_algo){
  if ("storr" %in% class(cache)){
    true_algo <- cache$driver$hash_algorithm
    if (true_algo != chosen_algo){
      warning(
        "The storr-based cache acutally uses ", true_algo,
        "for the short hash algorithm, but ", chosen_algo,
        "was also supplied. Reverting to ", true_algo, "."
      )
      cache$set(
        key = "short_hash_algo",
        value = true_algo,
        namespace = "config"
      )
    }
  }
}
