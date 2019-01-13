assert_cache <- function(cache) {
  cache_exists <- inherits(x = cache$driver, what = "driver_environment") ||
    file.exists(cache$driver$path)
  if (!cache_exists) {
    stop("drake cache missing.", call. = FALSE)
  }
}

# Return the file path where the cache is stored, if applicable.
cache_path_ <- function(cache = NULL) {
  if (is.null(cache)) {
    NULL
  } else if ("storr" %in% class(cache)) {
    cache$driver$path
  } else {
    NULL
  }
}

force_cache_path <- function(cache = NULL) {
  cache_path_(cache) %||% default_cache_path()
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
  verbose = 1L,
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
  path = NULL,
  force = FALSE,
  verbose = 1L,
  fetch_cache = NULL,
  console_log_file = NULL
) {
  path <- path %||% default_cache_path()
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
    "If all else fails, remove the folder at ", path, " and try again.",
    call. = FALSE
  )
}

drake_fetch_rds <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  hash_algo_file <- file.path(path, "config", "hash_algorithm")
  hash_algo <- scan(hash_algo_file, quiet = TRUE, what = character())
  storr::storr_rds(path = path, hash_algorithm = hash_algo)
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
  path = NULL,
  verbose = 1L,
  type = NULL,
  hash_algorithm = NULL,
  short_hash_algo = NULL,
  long_hash_algo = NULL,
  ...,
  console_log_file = NULL
) {
  path <- path %||% default_cache_path()
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
    mangle_key = FALSE,
    hash_algorithm = hash_algorithm
  )
  writeLines(
    text = c("*", "!/.gitignore"),
    con = file.path(path, ".gitignore")
  )
  console_cache(
    config = list(
      cache_path = cache_path_(cache),
      verbose = verbose,
      console_log_file = console_log_file
    )
  )
  cache
}

# Load an existing drake files system cache if it exists
# or create a new one otherwise.
recover_cache_ <- function(
  path = NULL,
  hash_algorithm = NULL,
  short_hash_algo = NULL,
  long_hash_algo = NULL,
  force = FALSE,
  verbose = 1L,
  fetch_cache = NULL,
  console_log_file = NULL
) {
  path <- path %||% default_cache_path()
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

#' @title Generate a flat text log file
#'   to represent the state of the cache.
#' @description
#' This functionality is like
#' `make(..., cache_log_file = TRUE)`,
#' but separated and more customizable.
#' The `drake_cache_log_file()` function writes a flat text file
#' to represents the state of all the targets and imports in the cache.
#' If you call it after each [make()]
#' and put the log file under version control,
#' you can track the changes to your results over time.
#' This way, your data is versioned alongside your code
#' in a easy-to-view format. Hopefully, this functionality
#' is a step toward better data versioning tools.
#' @seealso [drake_cache_log()], [make()], [get_cache()]
#' @export
#' @return There is no return value, but a log file is generated.
#' @param file character scalar, name of the flat text log file.
#'
#' @inheritParams cached
#'
#' @param jobs number of jobs/workers for parallel processing
#'
#' @param targets_only logical, whether to output information
#'   only on the targets in your workflow plan data frame.
#'   If `targets_only` is `FALSE`, the output will
#'   include the hashes of both targets and imports.
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' # Load drake's canonical example.
#' load_mtcars_example() # Get the code with drake_example()
#' # Run the project and save a flat text log file.
#' make(my_plan)
#' drake_cache_log_file() # writes drake_cache.log
#' # The above 2 lines are equivalent to make(my_plan, cache_log_file = TRUE) # nolint
#' # At this point, put drake_cache.log under version control
#' # (e.g. with 'git add drake_cache.log') alongside your code.
#' # Now, every time you run your project, your commit history
#' # of hash_lot.txt is a changelog of the project's results.
#' # It shows which targets and imports changed on every commit.
#' # It is extremely difficult to track your results this way
#' # by putting the raw '.drake/' cache itself under version control.
#' })
#' }
drake_cache_log_file <- function(
  file = "drake_cache.log",
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1L,
  jobs = 1,
  targets_only = FALSE
) {
  if (!length(file) || identical(file, FALSE)) {
    return(invisible())
  } else if (identical(file, TRUE)) {
    file <- formals(drake_cache_log_file)$file
  }
  out <- drake_cache_log(
    path = path,
    search = search,
    cache = cache,
    verbose = verbose,
    jobs = jobs,
    targets_only = targets_only
  )
  write.table(
    out,
    file = file,
    quote = FALSE,
    row.names = FALSE
  )
}

#' @title Get a table that represents the state of the cache.
#' @description
#' This functionality is like
#' `make(..., cache_log_file = TRUE)`,
#' but separated and more customizable. Hopefully, this functionality
#' is a step toward better data versioning tools.
#' @details A hash is a fingerprint of an object's value.
#' Together, the hash keys of all your targets and imports
#' represent the state of your project.
#' Use `drake_cache_log()` to generate a data frame
#' with the hash keys of all the targets and imports
#' stored in your cache.
#' This function is particularly useful if you are
#' storing your drake project in a version control repository.
#' The cache has a lot of tiny files, so you should not put it
#' under version control. Instead, save the output
#' of `drake_cache_log()` as a text file after each [make()],
#' and put the text file under version control.
#' That way, you have a changelog of your project's results.
#' See the examples below for details.
#' Depending on your project's
#' history, the targets may be different than the ones
#' in your workflow plan data frame.
#' Also, the keys depend on the hash algorithm
#' of your cache. To define your own hash algorithm,
#' you can create your own `storr` cache and give it a hash algorithm
#' (e.g. `storr_rds(hash_algorithm = "murmur32")`)
#' @seealso [drake_cache_log_file()], [cached()], [get_cache()]
#' @export
#' @return Data frame of the hash keys of the targets and imports
#'   in the cache
#'
#' @inheritParams cached
#'
#' @param jobs number of jobs/workers for parallel processing
#'
#' @param targets_only logical, whether to output information
#'   only on the targets in your workflow plan data frame.
#'   If `targets_only` is `FALSE`, the output will
#'   include the hashes of both targets and imports.
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' # Load drake's canonical example.
#' load_mtcars_example() # Get the code with drake_example()
#' # Run the project, build all the targets.
#' make(my_plan)
#' # Get a data frame of all the hash keys.
#' # If you want a changelog, be sure to do this after every make().
#' cache_log <- drake_cache_log()
#' head(cache_log)
#' # Save the hash log as a flat text file.
#' write.table(
#'   x = cache_log,
#'   file = "drake_cache.log",
#'   quote = FALSE,
#'   row.names = FALSE
#' )
#' # At this point, put drake_cache.log under version control
#' # (e.g. with 'git add drake_cache.log') alongside your code.
#' # Now, every time you run your project, your commit history
#' # of hash_lot.txt is a changelog of the project's results.
#' # It shows which targets and imports changed on every commit.
#' # It is extremely difficult to track your results this way
#' # by putting the raw '.drake/' cache itself under version control.
#' })
#' }
drake_cache_log <- function(
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1L,
  jobs = 1,
  targets_only = FALSE
) {
  if (is.null(cache)) {
    return(
      weak_tibble(
        hash = character(0),
        type = character(0),
        name = character(0)
      )
    )
  }
  out <- lightly_parallelize(
    X = cache$list(),
    FUN = single_cache_log,
    jobs = jobs,
    cache = cache
  )
  out <- weak_as_tibble(do.call(rbind, out))
  if (targets_only) {
    out <- out[out$type == "target", ]
  }
  out
}

single_cache_log <- function(key, cache) {
  hash <- cache$get_hash(key = key)
  imported <- get_from_subspace(
    key = key,
    subspace = "imported",
    namespace = "meta",
    cache = cache
  )
  imported <- ifelse(is.na(imported), TRUE, imported)
  type <- ifelse(imported, "import", "target")
  weak_tibble(hash = hash, type = type, name = key)
}

default_cache_path <- function(root = getwd()) {
  file.path(root, ".drake")
}

# Pre-set the values to avoid https://github.com/richfitz/storr/issues/80.
init_common_values <- function(cache) {
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

keys_are_mangled <- function(cache) {
  "driver_rds" %in% class(cache$driver) &&
    identical(cache$driver$mangle_key, TRUE)
}

safe_get <- function(key, namespace, config) {
  out <- just_try(config$cache$get(key = key, namespace = namespace))
  if (inherits(out, "try-error")) {
    out <- NA_character_
  }
  out
}

safe_get_hash <- function(key, namespace, config) {
  out <- just_try(config$cache$get_hash(key = key, namespace = namespace))
  if (inherits(out, "try-error")) {
    out <- NA_character_
  }
  out
}

target_exists <- function(target, config) {
  config$cache$exists(key = target)
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

#' @title Enumerate cached targets or check if a target is cached.
#' @description Read/load a cached item with [readd()]
#' or [loadd()].
#' @seealso [cached()], [readd()], [loadd()],
#'   [drake_plan()], [make()]
#' @export
#' @return Either a named logical indicating whether the given
#'   targets or cached or a character vector listing all cached
#'   items, depending on whether any targets are specified.
#'
#' @inheritParams drake_config
#'
#' @param ... objects to load from the cache, as names (unquoted)
#'   or character strings (quoted). Similar to `...` in
#'   `remove()`.
#'
#' @param list character vector naming objects to be loaded from the
#'   cache. Similar to the `list` argument of [remove()].
#'
#' @param no_imported_objects logical, applies only when
#'   no targets are specified and a list of cached targets is returned.
#'   If `no_imported_objects` is `TRUE`, then `cached()`
#'   shows built targets (with commands) plus imported files,
#'   ignoring imported objects. Otherwise, the full collection of
#'   all cached objects will be listed. Since all your functions and
#'   all their global variables are imported, the full list of
#'   imported objects could get really cumbersome.
#'
#' @param cache drake cache. See [new_cache()].
#'   If supplied, `path` and `search` are ignored.
#'
#' @param path Root directory of the drake project,
#'   or if `search` is `TRUE`, either the
#'   project root or a subdirectory of the project.
#'   Ignored if a `cache` is supplied.
#'
#' @param search logical. If `TRUE`, search parent directories
#'   to find the nearest drake cache. Otherwise, look in the
#'   current working directory only.
#'   Ignored if a `cache` is supplied.
#'
#' @param namespace character scalar, name of the storr namespace
#'   to use for listing objects
#'
#' @param jobs number of jobs/workers for parallel processing
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Load drake's canonical example.
#' make(my_plan) # Run the project, build all the targets.
#' cached(list = 'reg1') # Is 'reg1' in the cache?
#' # List all the targets and imported files in the cache.
#' # Exclude R objects imported from your workspace.
#' cached(no_imported_objects = TRUE)
#' # List all targets and imports in the cache.
#' cached()
#' # Clean the main data.
#' clean()
#' # The targets and imports are gone.
#' cached()
#' # But there is still metadata.
#' build_times()
#' cached(namespace = "build_times")
#' # Clear that too.
#' clean(purge = TRUE)
#' cached(namespace = "build_times")
#' build_times()
#' })
#' }
cached <- function(
  ...,
  list = character(0),
  no_imported_objects = FALSE,
  path = getwd(),
  search = TRUE,
  cache = NULL,
  verbose = 1L,
  namespace = NULL,
  jobs = 1
) {
  if (is.null(cache)) {
    cache <- get_cache(path = path, search = search, verbose = verbose)
  }
  if (is.null(cache)) {
    return(character(0))
  }
  if (is.null(namespace)) {
    namespace <- cache$default_namespace
  }
  dots <- match.call(expand.dots = FALSE)$...
  targets <- targets_from_dots(dots, list)
  if (!length(targets)) {
    list_cache(no_imported_objects = no_imported_objects,
      cache = cache, namespace = namespace, jobs = jobs)
  } else {
    is_cached(targets = targets, no_imported_objects = no_imported_objects,
      cache = cache, namespace = namespace, jobs = jobs)
  }
}

is_cached <- function(targets, no_imported_objects, cache, namespace, jobs) {
  if (no_imported_objects)
    targets <- no_imported_objects(
      targets = targets, cache = cache, jobs = jobs)
  inclusion <- lightly_parallelize(
    X = targets,
    FUN = function(target) {
      cache$exists(key = target, namespace = namespace)
    },
    jobs = jobs
  )
  inclusion <- unlist(inclusion)
  names(inclusion) <- display_keys(targets)
  inclusion
}

list_cache <- function(no_imported_objects, cache, namespace, jobs) {
  targets <- cache$list(namespace = namespace)
  if (no_imported_objects) {
    targets <- no_imported_objects(
      targets = targets, cache = cache, jobs = jobs)
  }
  display_keys(targets)
}

# from base::remove()
targets_from_dots <- function(dots, list) {
  if (length(dots) && !all(vapply(dots, function(x) is.symbol(x) ||
    is.character(x), NA, USE.NAMES = FALSE))) {
    stop("... must contain names or character strings", call. = FALSE)
  }
  names <- vapply(dots, as.character, "")
  targets <- unique(c(names, list))
  standardize_key(targets)
}

imported_only <- function(targets, cache, jobs) {
  parallel_filter(
    x = targets,
    f = function(target) {
      is_imported_cache(target = target, cache = cache)
    },
    jobs = jobs
  )
}

no_imported_objects <- function(targets, cache, jobs) {
  parallel_filter(
    x = targets,
    f = function(target) {
      is_built_or_imported_file(target = target, cache = cache)
    },
    jobs = jobs
  )
}

is_imported_cache <- Vectorize(function(target, cache) {
  cache$exists(key = target) &&
  diagnose(
    target = target,
    character_only = TRUE,
    cache = cache
  )$imported
},
"target", SIMPLIFY = TRUE)

is_built_or_imported_file <- Vectorize(function(target, cache) {
  imported <- is_imported_cache(target = target, cache = cache)
  !imported | (imported & is_encoded_path(target))
},
"target", SIMPLIFY = TRUE)
