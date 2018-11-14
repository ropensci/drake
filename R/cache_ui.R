#' @title Enumerate cached targets or check if a target is cached.
#' @description Read/load a cached item with [readd()]
#' or [loadd()].
#' @seealso [built()], [imported()],
#'   [readd()], [loadd()],
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
  verbose = drake::default_verbose(),
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
  names(inclusion) <- targets
  inclusion
}

list_cache <- function(no_imported_objects, cache, namespace, jobs) {
  targets <- cache$list(namespace = namespace)
  if (no_imported_objects) {
    targets <- no_imported_objects(
      targets = targets, cache = cache, jobs = jobs)
  }
  targets
}

#' @title List all the built targets (non-imports) in the cache.
#' @description Targets are listed in the workflow plan
#' data frame (see [drake_plan()].
#' @seealso [cached()], [loadd()], [imported()]
#' @export
#' @return Character vector naming the built targets in the cache.
#' @inheritParams cached
#' @param jobs number of jobs/workers for parallel processing
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Load drake's canonical example.
#' make(my_plan) # Run the project, build all the targets.
#' built() # List all the cached targets (built objects and files).
#' # For file targets, only the fingerprints/hashes are stored.
#' })
#' }
built <- function(
  path = getwd(), search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = drake::default_verbose(),
  jobs = 1
) {
  if (is.null(cache)) {
    return(character(0))
  }
  out <- cache$list(namespace = cache$default_namespace)
  parallel_filter(
    out,
    f = function(target) {
      !is_imported(target = target, cache = cache)
    },
    jobs = jobs
  )
}

#' @title List all the imports in the drake cache.
#' @description An import is a non-target object processed
#' by [make()]. Targets in the workflow
#' plan data frame (see [drake_config()]
#' may depend on imports.
#' @seealso [cached()], [loadd()],
#'   [built()]
#' @export
#' @return Character vector naming the imports in the cache.
#'
#' @inheritParams cached
#'
#' @param files_only logical, whether to show imported files only
#'   and ignore imported objects. Since all your functions and
#'   all their global variables are imported, the full list of
#'   imported objects could get really cumbersome.
#'
#' @param jobs number of jobs/workers for parallel processing
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Load the canonical example.
#' make(my_plan) # Run the project, build the targets.
#' imported() # List all the imported objects/files in the cache.
#' # For imported files, only the fingerprints/hashes are stored.
#' })
#' }
imported <- function(
  files_only = FALSE, path = getwd(), search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = drake::default_verbose(),
  jobs = 1
) {
  if (is.null(cache)) {
    return(character(0))
  }
  targets <- cache$list(namespace = cache$default_namespace)
  targets <- parallel_filter(
    targets,
    f = function(target) {
      is_imported(target = target, cache = cache)
    },
    jobs = jobs
  )
  if (files_only)
    targets <- parallel_filter(targets, f = is_file, jobs = jobs)
  targets
}

# from base::remove()
targets_from_dots <- function(dots, list) {
  if (length(dots) && !all(vapply(dots, function(x) is.symbol(x) ||
    is.character(x), NA, USE.NAMES = FALSE))) {
    stop("... must contain names or character strings", call. = FALSE)
  }
  names <- vapply(dots, as.character, "")
  targets <- unique(c(names, list))
  standardize_filename(targets)
}

imported_only <- function(targets, cache, jobs) {
  parallel_filter(
    x = targets,
    f = function(target) {
      is_imported(target = target, cache = cache)
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

is_imported <- Vectorize(function(target, cache) {
  cache$exists(key = target) &&
  diagnose(
    target = target,
    character_only = TRUE,
    cache = cache
  )$imported
},
"target", SIMPLIFY = TRUE)

is_built_or_imported_file <- Vectorize(function(target, cache) {
  imported <- is_imported(target = target, cache = cache)
  !imported | (imported & is_file(target))
},
"target", SIMPLIFY = TRUE)
