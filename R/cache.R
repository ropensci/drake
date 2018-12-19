assert_cache <- function(cache) {
  cache_exists <- inherits(x = cache$driver, what = "driver_environment") ||
    file.exists(cache$driver$path)
  if (!cache_exists) {
    stop("drake cache missing.", call. = FALSE)
  }
}

#' @title Return the file path where the cache is stored,
#' if applicable.
#' @export
#' @description Currently only works with
#' [storr::storr_rds()] file system caches.
#' @return File path where the cache is stored.
#' @param cache the cache whose file path
#'   you want to know
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
cache_path <- function(cache = NULL) {
  if (is.null(cache)) {
    NULL
  } else if ("storr" %in% class(cache)) {
    cache$driver$path
  } else {
    NULL
  }
}

force_cache_path <- function(cache = NULL) {
  path <- cache_path(cache)
  if (is.null(path)) {
    path <- default_cache_path()
  }
  path
}

#' @title Get the default cache of a `drake` project.
#' @description Only works if the cache
#' is in a folder called `.drake/`. See the description of the
#' `path` argument for details.
#' @seealso [this_cache()], [new_cache()],
#'   [recover_cache()], [drake_config()]
#' @export
#' @return A drake/storr cache in a folder called `.drake/`,
#'   if available. `NULL` otherwise.
#' @inheritParams cached
#' @inheritParams drake_config
#' @param path If `search = FALSE`, `path` must be the root
#'   directory of a `drake` project (a folder containing a `.drake` cache).
#'   For example, if your cache is a folder called
#'   `/home/you/my_project/.drake`, then
#'   `get_cache(path = "/home/you/my_project", search = FALSE)`
#'   will return the cache.
#'   Otherwise, if `search = TRUE`, you can specify any
#'   subdirectory of the project. The following are equivalent and correct:
#'   - `get_cache(path = "/home/you/my_project", search = FALSE)`
#'   - `get_cache(path = "/home/you/my_project", search = TRUE)`
#'   - `get_cache(path = "/home/you/my_project/subdir/x", search = TRUE)`
#'   - `get_cache(path = "/home/you/my_project/.drake", search = TRUE)`
#'   - `get_cache(path = "/home/you/my_project/.drake/keys", search = TRUE)`
#' @param force deprecated
#' @param fetch_cache character vector containing lines of code.
#'   The purpose of this code is to fetch the `storr` cache
#'   with a command like `storr_rds()` or `storr_dbi()`,
#'   but customized. This feature is experimental.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' clean(destroy = TRUE)
#' # No cache is available.
#' get_cache() # NULL
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' x <- get_cache() # Now, there is a cache.
#' # List the objects readable from the cache with readd().
#' x$list() # Or x$list(namespace = x$default_namespace)
#' })
#' }
get_cache <- function(
  path = getwd(),
  search = TRUE,
  verbose = drake::default_verbose(),
  force = FALSE,
  fetch_cache = NULL,
  console_log_file = NULL
) {
  deprecate_force(force)
  deprecate_fetch_cache(fetch_cache)
  if (search) {
    path <- find_cache(path = path)
  } else {
    path <- default_cache_path(root = path)
  }
  this_cache(
    path = path,
    verbose = verbose,
    fetch_cache = fetch_cache,
    console_log_file = console_log_file
  )
}

#' @title Get the cache at the exact file path specified.
#' @export
#' @description This function does not apply to
#' in-memory caches such as `storr_environment()`.
#' @return A drake/storr cache at the specified path, if it exists.
#' @inheritParams cached
#' @inheritParams drake_config
#' @param path file path of the cache
#' @param force deprecated
#'   is compatible with your current version of drake.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' clean(destroy = TRUE)
#' try(x <- this_cache(), silent = FALSE) # The cache does not exist yet.
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' y <- this_cache() # Now, there is a cache.
#' z <- this_cache(".drake") # Same as above.
#' manual <- new_cache("manual_cache") # Make a new cache.
#' manual2 <- get_cache("manual_cache") # Get the new cache.
#' })
#' }
this_cache <- function(
  path = drake::default_cache_path(),
  force = FALSE,
  verbose = drake::default_verbose(),
  fetch_cache = NULL,
  console_log_file = NULL
) {
  deprecate_force(force)
  deprecate_fetch_cache(fetch_cache)
  usual_path_missing <- is.null(path) || !file.exists(path)
  if (usual_path_missing) {
    return(NULL)
  }
  if (!is.null(path)) {
    console_cache(
      config = list(
        cache_path = path,
        verbose = verbose,
        console_log_file = console_log_file
      )
    )
  }
  cache <- drake_try_fetch_rds(path = path)
  cache_vers_warn(cache = cache)
  cache
}

drake_try_fetch_rds <- function(path) {
  out <- try(drake_fetch_rds(path = path), silent = TRUE)
  if (!inherits(out, "try-error")) {
    return(out)
  }
  stop(
    "drake failed to get the storr::storr_rds() cache at ", path, ". ",
    "Something is wrong with the file system of the cache. ",
    "If you downloaded it from an online repository, are you sure ",
    "all the files were downloaded correctly? ",
    "If all else fails, remove the folder at ", path, "and try again.",
    call. = FALSE
  )
}

drake_fetch_rds <- function(path) {
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

#' @title  Make a new `drake` cache.
#' @description Uses the [storr_rds()] function
#' from the `storr` package.
#' @export
#' @return A newly created drake cache as a storr object.
#' @inheritParams cached
#' @inheritParams drake_config
#' @seealso [make()]
#' @param path file path to the cache if the cache
#'   is a file system cache.
#' @param type deprecated argument. Once stood for cache type.
#'   Use `storr` to customize your caches instead.
#' @param hash_algorithm name of a hash algorithm to use.
#'   See the `algo` argument of the `digest` package for your options.
#' @param short_hash_algo Deprecated on 2018-12-12. Use `hash_algorithm` instead
#' @param long_hash_algo Deprecated on 2018-12-12. Use `hash_algorithm` instead 
#' @param ... other arguments to the cache constructor
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine new_cache() side effects.", {
#' clean(destroy = TRUE) # Should not be necessary.
#' unlink("not_hidden", recursive = TRUE) # Should not be necessary.
#' cache1 <- new_cache() # Creates a new hidden '.drake' folder.
#' cache2 <- new_cache(path = "not_hidden", hash_algorithm = "md5")
#' clean(destroy = TRUE, cache = cache2)
#' })
#' }
new_cache <- function(
  path = drake::default_cache_path(),
  verbose = drake::default_verbose(),
  type = NULL,
  hash_algorithm = NULL,
  short_hash_algo = NULL,
  long_hash_algo = NULL,
  ...,
  console_log_file = NULL
) {
  hash_algorithm <- set_hash_algorithm(hash_algorithm)
  if (!is.null(type)) {
    warning(
      "The 'type' argument of new_cache() is deprecated. ",
      "Please see the storage guide in the manual for the new cache API:",
      "https://ropenscilabs.github.io/drake-manual/store.html"
    )
  }
  deprecate_hash_algo_args(short_hash_algo, long_hash_algo)
  cache <- storr::storr_rds(
    path = path,
    mangle_key = TRUE,
    hash_algorithm = hash_algorithm
  )
  writeLines(
    text = c("*", "!/.gitignore"),
    con = file.path(path, ".gitignore")
  )
  console_cache(
    config = list(
      cache_path = cache_path(cache),
      verbose = verbose,
      console_log_file = console_log_file
    )
  )
  cache
}

#' @title Load an existing drake files system cache if it exists
#'   or create a new one otherwise.
#' @export
#' @seealso [new_cache()], [this_cache()],
#'   [get_cache()]
#' @description
#' Does not work with
#' in-memory caches such as [storr_environment()].
#' @return A drake/storr cache.
#' @inheritParams cached
#' @inheritParams new_cache
#' @inheritParams this_cache
#' @inheritParams drake_config
#' @param path file path of the cache
#' @param force logical, whether to load the cache
#'   despite any back compatibility issues with the
#'   running version of drake.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' clean(destroy = TRUE)
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build all the targets.
#' x <- recover_cache(".drake") # Recover the project's storr cache.
#' })
#' }
recover_cache <- function(
  path = drake::default_cache_path(),
  hash_algorithm = NULL,
  short_hash_algo = NULL,
  long_hash_algo = NULL,
  force = FALSE,
  verbose = drake::default_verbose(),
  fetch_cache = NULL,
  console_log_file = NULL
) {
  deprecate_force(force)
  deprecate_fetch_cache(fetch_cache)
  deprecate_hash_algo_args(short_hash_algo, long_hash_algo)
  hash_algorithm <- set_hash_algorithm(hash_algorithm)
  cache <- this_cache(
    path = path,
    verbose = verbose,
    fetch_cache = fetch_cache,
    console_log_file = console_log_file
  )
  if (is.null(cache)) {
    cache <- new_cache(
      path = path,
      verbose = verbose,
      hash_algorithm = hash_algorithm,
      fetch_cache = fetch_cache,
      console_log_file = console_log_file
    )
  }
  init_common_values(cache)
  cache
}

#' @title Return the default file path of the drake/storr cache.
#' @export
#' @keywords internal
#' @description Applies to file system caches only.
#' @return Default file path of the drake/storr cache.
#' @param root character scalar, path to the root of the
#'   `drake` project with the cache.
#' @examples
#' \dontrun{
#' default_cache_path()
#' }
default_cache_path <- function(root = getwd()) {
  file.path(root, ".drake")
}

# Pre-set the values to avoid https://github.com/richfitz/storr/issues/80.
init_common_values <- function(cache) {
  set_initial_drake_version(cache)
  common_values <- list(TRUE, FALSE, "finished", "in progress", "failed")
  cache$mset(
    key = as.character(common_values),
    value = common_values,
    namespace = "common"
  )
}

clear_tmp_namespace <- function(cache, jobs, namespace) {
  lightly_parallelize(
    X = cache$list(),
    FUN = function(target) {
      cache$del(key = target, namespace = namespace)
    },
    jobs = jobs
  )
  cache$clear(namespace = namespace)
  invisible()
}

set_initial_drake_version <- function(cache) {
  if (cache$exists(
    key = "initial_drake_version",
    namespace = "session"
  )) {
    return()
  } else if (cache$exists(
    key = "sessionInfo",
    namespace = "session"
  )) {
    last_session <- drake_get_session_info(cache = cache)
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

drake_version <- function(session_info = NULL) { # nolint
  if (!length(session_info)) {
    return(packageVersion("drake"))
  }
  all_pkgs <- c(
    session_info$otherPkgs, # nolint
    session_info$loadedOnly # nolint
  )
  all_pkgs$drake$Version # nolint
}

is_default_cache <- function(cache) {
  "driver_rds" %in% class(cache$driver) &&
    identical(cache$driver$mangle_key, TRUE)
}

safe_get <- function(key, namespace, config) {
  out <- just_try(config$cache$get(key = key, namespace = namespace))
  if (inherits(out, "try-error")) {
    out <- NA
  }
  out
}

safe_get_hash <- function(key, namespace, config) {
  out <- just_try(config$cache$get_hash(key = key, namespace = namespace))
  if (inherits(out, "try-error")) {
    out <- NA
  }
  out
}

kernel_exists <- function(target, config) {
  config$cache$exists(key = target, namespace = "kernels")
}

target_exists <- kernel_exists

cache_vers_check <- function(cache) {
  if (is.null(cache)) {
    return(character(0))
  }
  err <- try(
    old <- drake_version(session_info = drake_get_session_info(cache = cache)), # nolint
    silent = TRUE
  )
  if (inherits(err, "try-error")) {
    return(invisible())
  }
  if (compareVersion(old, "5.4.0") < 0) {
    paste0(
      "This project was last run with drake version ",
      old, ".\nYour cache is not compatible with the current ",
      "version of drake (",
      packageVersion("drake"), ").\nTo run your project with version ",
      packageVersion("drake"), ", use make(force = TRUE).\n",
      "But be warned: if you do that, ",
      "all you targets will run from scratch.\nYou may instead wish to ",
      "downgrade drake to version ", old, "."
    )
  } else {
    character(0)
  }
}

cache_vers_stop <- function(cache){
  msg <- cache_vers_check(cache)
  if (length(msg)) {
    stop(msg, call. = FALSE)
  }
}

cache_vers_warn <- function(cache){
  msg <- cache_vers_check(cache)
  if (length(msg)) {
    warning(msg, call. = FALSE)
  }
}

memo_expr <- function(expr, cache, ...) {
  if (is.null(cache)) {
    return(force(expr))
  }
  lang <- match.call(expand.dots = FALSE)$expr
  key <- digest::digest(list(lang, ...), algo = cache$driver$hash_algorithm)
  if (cache$exists(key = key, namespace = "memoize")) {
    return(cache$get(key = key, namespace = "memoize"))
  }
  value <- force(expr)
  cache$set(key = key, value = value, namespace = "memoize")
  value
}

set_hash_algorithm <- function(hash_algorithm) {
  if (is.null(hash_algorithm)) {
    "xxhash64"
  } else {
    hash_algorithm
  }
}

deprecate_hash_algo_args <- function(
  short_hash_algo = NULL,
  long_hash_algo = NULL
) {
  if (!is.null(short_hash_algo) || !is.null(long_hash_algo)) {
    warning(
      "The long_hash_algo and short_hash_algo arguments to drake functions ",
      "are deprecated. drake now uses only one hash algorithm, ",
      "which you can set ",
      "with the hash_algorithm argument in new_cache().",
      call. = FALSE
    )
  }
}
