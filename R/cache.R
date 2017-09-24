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
  if(search){
    path <- find_cache(path = path, search = search)
  }
  this_cache(path = path)
}

#' @title Function this_cache
#' @export
#' @description Get a specific drake cache
#' at the exact specified file path
#' @param path file path to the cache
#' @param search logical, whether to search back in the file system
#' for the cache.
#' @param type character scalar, type of the drake cache.
#' Must be among the list of supported caches
#' in \code{\link{cache_types}()}.
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
    path = path, search = search
  )
}

#' @title Function new_cache
#' @description Make a new drake cache.
#' @export
#' @seealso \link{\code{default_short_hash_algorithm}},
#' \code{\link{make}}, \code{\link{cache_types}}
#' @param path file path to the cache
#' @param type character scalar, type of the drake cache.
#' Must be among the list of supported caches
#' in \code{\link{cache_types}()}.
#' @param short_hash_algo short hash algorithm for the cache.
#' See \link{\code{default_short_hash_algorithm}()} and
#' \code{\link{make}()}
#' @param long_hash_algo long hash algorithm for the cache.
#' See \link{\code{default_long_hash_algorithm}()} and
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
  type = match.arg(type, choices = cache_types())
  name <- paste0("new_", type, "_cache")
  cache <- get(name, envir = getNamespace("drake"))(path = path, search = search)
  cache$set(
    key = "short_hash_algo",
    value = short_hash_algo,
    namespace = "config"
  )
  cache$set(
    key = "long_hash_algo",
    value = long_hash_algo,
    namespace = "config"
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
#' See \link{\code{default_short_hash_algorithm}()} and
#' \code{\link{make}()}
#' @param long_hash_algo long hash algorithm for the cache.
#' See \link{\code{default_long_hash_algorithm}()} and
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
    path <- file.path(getwd(), cache_dir)
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

configure_cache <- function(cache, clear_progress){
  if (clear_progress){
    cache$clear(namespace = "progress")
  }
  config_keys <- cache$list(namespace = "config")
  if (!("short_hash_algo" %in% config_keys)){
    cache$set(
      key = "short_hash_algo",
      value = default_short_hash_algo(),
      namespace = "config"
    )
  }
  if (!("long_hash_algo" %in% config_keys)){
    cache$set(
      key = "long_hash_algo",
      value = default_long_hash_algo(),
      namespace = "config"
    )
  }
  cache
}
