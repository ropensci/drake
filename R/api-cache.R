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
#' @seealso [new_cache()], [drake_config()]
#' @export
#' @return A drake/storr cache in a folder called `.drake/`,
#'   if available. `NULL` otherwise.
#' @inheritParams cached
#' @inheritParams drake_config
#' @param path Character, either the root file path of a `drake` project
#'   or a folder containing the root (top-level working directory
#'   where you plan to call [make()]).
#'   If this is too confusing, feel free to just use `storr::storr_rds()`
#'   to get the cache.
#'   If `search = FALSE`, `path` must be the root.
#'   If `search = TRUE`, you can specify any
#'   subdirectory of the project. Let's say `"/home/you/my_project"`
#'   is the root. The following are equivalent and correct:
#'   - `get_cache(path = "/home/you/my_project", search = FALSE)`
#'   - `get_cache(path = "/home/you/my_project", search = TRUE)`
#'   - `get_cache(path = "/home/you/my_project/subdir/x", search = TRUE)`
#'   - `get_cache(path = "/home/you/my_project/.drake", search = TRUE)`
#'   - `get_cache(path = "/home/you/my_project/.drake/keys", search = TRUE)`
#' @param force Deprecated.
#' @param fetch_cache Character vector containing lines of code.
#'   The purpose of this code is to fetch the `storr` cache
#'   with a command like `storr_rds()` or `storr_dbi()`,
#'   but customized. This feature is experimental.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' clean(destroy = TRUE)
#' # No cache is available.
#' get_cache() # NULL
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' x <- get_cache() # Now, there is a cache.
#' y <- storr::storr_rds(".drake") # Equivalent.
#' # List the objects readable from the cache with readd().
#' x$list()
#' }
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
  this_cache_(
    path = path,
    verbose = verbose,
    fetch_cache = fetch_cache,
    console_log_file = console_log_file
  )
}

#' @title Search up the file system for the nearest drake cache.
#' @description Only works if the cache is a file system in a
#' hidden folder named `.drake` (default).
#' @seealso [drake_plan()], [make()],
#' @export
#' @return File path of the nearest drake cache or `NULL`
#'   if no cache is found.
#' @param path Starting path for search back for the cache.
#'   Should be a subdirectory of the drake project.
#' @param dir Character, name of the folder containing the cache.
#' @param directory Deprecated. Use `dir`.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the target.
#' # Find the file path of the project's cache.
#' # Search up through parent directories if necessary.
#' find_cache()
#' }
#' })
#' }
find_cache <- function(
  path = getwd(),
  dir = NULL,
  directory = NULL
) {
  if (!is.null(directory)) {
    warning(
      "Argument `directory` of find_cache() is deprecated. ",
      "use `dir` instead.",
      call. = FALSE
    )
  }
  dir <- dir %||% basename(default_cache_path())
  while (!(dir %in% list.files(path = path, all.files = TRUE))) {
    path <- dirname(path)
    # If we can search no higher...
    if (path == dirname(path)) {
      return(NULL) # The cache does not exist
    }
  }
  file.path(path, dir)
}

this_cache_ <- function(
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
#' @param path File path to the cache if the cache
#'   is a file system cache.
#' @param type Deprecated argument. Once stood for cache type.
#'   Use `storr` to customize your caches instead.
#' @param hash_algorithm Name of a hash algorithm to use.
#'   See the `algo` argument of the `digest` package for your options.
#' @param short_hash_algo Deprecated on 2018-12-12. Use `hash_algorithm` instead.
#' @param long_hash_algo Deprecated on 2018-12-12. Use `hash_algorithm` instead.
#' @param ... other arguments to the cache constructor.
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
  cache <- this_cache_(
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

# Generate a flat text log file to represent the state of the cache.
drake_cache_log_file_ <- function(
  file = "drake_cache.log",
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1L,
  jobs = 1L,
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
  # Suppress partial arg match warnings.
  suppressWarnings(
    write.table(
      x = out,
      file = file,
      quote = FALSE,
      row.names = FALSE
    )
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
#' @seealso [cached()], [get_cache()]
#' @export
#' @return Data frame of the hash keys of the targets and imports
#'   in the cache
#'
#' @inheritParams cached
#'
#' @param jobs Number of jobs/workers for parallel processing.
#'
#' @param targets_only Logical, whether to output information
#'   only on the targets in your workflow plan data frame.
#'   If `targets_only` is `FALSE`, the output will
#'   include the hashes of both targets and imports.
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' # Load drake's canonical example.
#' load_mtcars_example() # Get the code with drake_example()
#' # Run the project, build all the targets.
#' make(my_plan)
#' # Get a data frame of all the hash keys.
#' # If you want a changelog, be sure to do this after every make().
#' cache_log <- drake_cache_log()
#' head(cache_log)
#' # Suppress partial arg match warnings.
#' suppressWarnings(
#'   # Save the hash log as a flat text file.
#'   write.table(
#'     x = cache_log,
#'     file = "drake_cache.log",
#'     quote = FALSE,
#'     row.names = FALSE
#'   )
#' )
#' # At this point, put drake_cache.log under version control
#' # (e.g. with 'git add drake_cache.log') alongside your code.
#' # Now, every time you run your project, your commit history
#' # of hash_lot.txt is a changelog of the project's results.
#' # It shows which targets and imports changed on every commit.
#' # It is extremely difficult to track your results this way
#' # by putting the raw '.drake/' cache itself under version control.
#' }
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
  out$name <- display_keys(out$name)
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
  common_values <- list(TRUE, FALSE, "done", "running", "failed")
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

#' @title List targets in the cache.
#' @description Tip: read/load a cached item with [readd()]
#'   or [loadd()].
#' @seealso [readd()], [loadd()],
#'   [drake_plan()], [make()]
#' @export
#' @return Either a named logical indicating whether the given
#'   targets or cached or a character vector listing all cached
#'   items, depending on whether any targets are specified.
#'
#' @inheritParams drake_config
#'
#' @param ... Deprecated. Do not use.
#'   Objects to load from the cache, as names (unquoted)
#'   or character strings (quoted). Similar to `...` in
#'   `remove()`.
#'
#' @param list Deprecated. Do not use.
#'   Character vector naming objects to be loaded from the
#'   cache. Similar to the `list` argument of [remove()].
#'
#' @param targets_only Logical. If `TRUE` just list the targets.
#'   If `FALSE`, list files and imported objects too.
#'
#' @param no_imported_objects Logical, deprecated. Use
#'   `targets_only` instead.
#'
#' @param cache drake cache. See [new_cache()].
#'   If supplied, `path` and `search` are ignored.
#'
#' @param path Root directory of the drake project,
#'   or if `search` is `TRUE`, either the
#'   project root or a subdirectory of the project.
#'   Ignored if a `cache` is supplied.
#'
#' @param search Logical. If `TRUE`, search parent directories
#'   to find the nearest drake cache. Otherwise, look in the
#'   current working directory only.
#'   Ignored if a `cache` is supplied.
#'
#' @param namespace Character scalar, name of the storr namespace
#'   to use for listing objects.
#'
#' @param jobs Number of jobs/workers for parallel processing.
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' if (requireNamespace("lubridate")) {
#' load_mtcars_example() # Load drake's canonical example.
#' make(my_plan) # Run the project, build all the targets.
#' cached()
#' cached(targets_only = FALSE)
#' }
#' }
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
  jobs = 1,
  targets_only = TRUE
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
  targets <- c(list, match.call(expand.dots = FALSE)$...)
  if (length(targets)) {
    warning(
      "The `...` and `list` arguments of `cached()` are deprecated.",
      "`cached()` no longer accepts target names. It just lists ",
      "the targets in the cache.",
      call. = FALSE
    )
  }
  targets <- cache$list(namespace = namespace)
  if (targets_only) {
    targets <- targets_only(targets, cache, jobs)
  }
  display_keys(targets)
}

targets_only <- function(targets, cache, jobs) {
  parallel_filter(
    x = targets,
    f = function(target) {
      !is_imported_cache(target = target, cache = cache) &&
        !is_encoded_path(target)
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
