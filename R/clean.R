#' @title Remove targets/imports from the cache.
#' @description Cleans up the work done by [make()].
#' @details
#' By default, `clean()` removes references to cached data.
#' To deep-clean the data to free up storage/memory, use
#' `clean(garbage_collection = TRUE)`. Garbage collection is slower,
#' but it purges data with no remaining references. To just do garbage
#' collection without cleaning, see [drake_gc()].
#' Also, for `clean()`, you must be in your project's working directory
#' or a subdirectory of it.
#' `clean(search = TRUE)` searches upwards in your folder structure
#' for the drake cache and acts on the first one it sees. Use
#' `search = FALSE` to look within the current working
#' directory only.
#' WARNING: This deletes ALL work done with [make()],
#' which includes
#' file targets as well as the entire drake cache. Only use `clean()`
#' if you're sure you won't lose anything important.
#' @seealso [drake_gc()], [make()]
#' @export
#' @return Invisibly return `NULL`.
#'
#' @inheritParams cached
#'
#' @param ... targets to remove from the cache: as names (symbols),
#'   character strings, or `dplyr`-style `tidyselect`
#'   commands such as `starts_with()`.
#'
#' @param list character vector naming targets to be removed from the
#'   cache. Similar to the `list` argument of [remove()].
#'
#' @param destroy logical, whether to totally remove the drake cache.
#'   If `destroy` is `FALSE`, only the targets
#'   from `make()`
#'   are removed. If `TRUE`, the whole cache is removed, including
#'   session metadata, etc.
#'
#' @param jobs Number of jobs for light parallelism
#'   (disabled on Windows).
#'
#' @param force logical, whether to try to clean the cache
#'   even though the project may not be back compatible with the
#'   current version of drake.
#'
#' @param garbage_collection logical, whether to call
#'   `cache$gc()` to do garbage collection.
#'   If `TRUE`, cached data with no remaining references
#'   will be removed.
#'   This will slow down `clean()`, but the cache
#'   could take up far less space afterwards.
#'   See the `gc()` method for `storr` caches.
#'
#' @param purge logical, whether to remove objects from
#'   metadata namespaces such as "meta", "build_times", and "errors".
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' # List objects in the cache, excluding R objects
#' # imported from your workspace.
#' cached(no_imported_objects = TRUE)
#' # Remove 'summ_regression1_large' and 'small' from the cache.
#' clean(summ_regression1_large, small)
#' # Those objects should be gone.
#' cached(no_imported_objects = TRUE)
#' # How about `tidyselect`?
#' clean(starts_with("coef"))
#' cached(no_imported_objects = TRUE)
#' # Rebuild the missing targets.
#' make(my_plan)
#' # Remove all the targets and imports.
#' # On non-Windows machines, parallelize over at most 2 jobs.
#' clean(jobs = 2)
#' # Make the targets again.
#' make(my_plan)
#' # Garbage collection removes data whose references are no longer present.
#' # It is slow, but you should enable it if you want to reduce the
#' # size of the cache.
#' clean(garbage_collection = TRUE)
#' # All the targets and imports are gone.
#' cached()
#' # But there is still cached metadata.
#' names(read_drake_meta())
#' build_times()
#' # To make even more room, use the "purge" flag.
#' clean(purge = TRUE)
#' names(read_drake_meta())
#' build_times()
#' # Completely remove the entire cache (default: '.drake/' folder).
#' clean(destroy = TRUE)
#' })
#' }
clean <- function(
  ...,
  list = character(0),
  destroy = FALSE,
  path = getwd(),
  search = TRUE,
  cache = NULL,
  verbose = drake::default_verbose(),
  jobs = 1,
  force = FALSE,
  garbage_collection = FALSE,
  purge = FALSE
){
  if (is.null(cache)){
    cache <- get_cache(
      path = path, search = search, verbose = verbose, force = force)
  }
  if (is.null(cache)){
    return(invisible())
  }
  targets <- drake_select(
    cache = cache,
    ...,
    namespaces = target_namespaces(),
    list = list
  )
  if (!length(targets) && is.null(c(...))){
    targets <- cache$list()
  }
  if (purge){
    namespaces <- target_namespaces(default = cache$default_namespace)
  } else {
    namespaces <- cleaned_namespaces(default = cache$default_namespace)
  }
  graph <- read_drake_graph(cache = cache)
  lightly_parallelize(
    X = targets,
    FUN = clean_single_target,
    jobs = jobs,
    cache = cache,
    namespaces = namespaces,
    graph = graph
  )
  if (destroy){
    cache$destroy()
  }
  if (garbage_collection){
    cache$gc()
  }
  invisible()
}

clean_single_target <- function(target, cache, namespaces, graph){
  files <- character(0)
  if (is_file(target)){
    files <- target
  }
  if (target %in% igraph::V(graph)$name){
    deps <- vertex_attr(
      graph = graph,
      name = "deps",
      index = target
    )[[1]]
    files <- sort(unique(deps$file_out))
  }
  unlink(drake_unquote(files), recursive = TRUE, force = TRUE)
  for (namespace in namespaces){
    for (key in c(target, files)){
      cache$del(key = key, namespace = namespace)
    }
  }
}

#' @title Do garbage collection on the drake cache.
#' @description The cache is a key-value store.
#' By default, the [clean()] function removes
#' values, but not keys.
#' Garbage collection removes the remaining dangling files.
#' @seealso [clean()]
#' @export
#' @return`NULL`
#' @inheritParams cached
#' @param force logical, whether to load the cache
#'   despite any back compatibility issues with the
#'   running version of drake.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' # At this point, check the size of the '.drake/' cache folder.
#' # Clean without garbage collection.
#' clean(garbage_collection = FALSE)
#' # The '.drake/' cache folder is still about the same size.
#' drake_gc() # Do garbage collection on the cache.
#' # The '.drake/' cache folder should have gotten much smaller.
#' })
#' }
drake_gc <- function(
  path = getwd(),
  search = TRUE,
  verbose = drake::default_verbose(),
  cache = NULL,
  force = FALSE
){
  if (is.null(cache)){
    cache <- get_cache(
      path = path,
      search = search,
      verbose = verbose,
      force = force
    )
  }
  if (!is.null(cache)){
    cache$gc()
    rm_bad_cache_filenames(cache)
  }
  invisible()
}

rm_bad_cache_filenames <- function(cache){
  if (is_default_cache(cache)){
    files <- list.files(path = cache$driver$path, recursive = TRUE)
    keep <- grepl(pattern = "^[-_./\\0-9a-zA-Z]*$", x = files)
    unlink(files[!keep], recursive = TRUE)
  }
}

#' @title Try to repair a drake cache that is prone
#'   to throwing `storr`-related errors.
#' @description Sometimes, `storr` caches may have
#' dangling orphaned files that prevent you from loading or cleaning.
#' This function tries to remove those files so you can use the
#' cache normally again.
#' @return The rescued drake/storr cache.
#' @export
#' @seealso [get_cache()], [cached()],
#'   [drake_gc()], [clean()]
#' @inheritParams get_cache
#' @param targets Character vector, names of the targets to rescue.
#'   As with many other drake utility functions, the word `target`
#'   is defined generally in this case, encompassing imports
#'   as well as true targets.
#'   If `targets` is `NULL`, everything in the
#'   cache is rescued.
#' @param cache a `storr` cache object
#' @param jobs number of jobs for light parallelism
#'   (disabled on Windows)
#' @param garbage_collection logical, whether to do garbage collection
#'   as a final step. See [drake_gc()] and [clean()]
#'   for details.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build targets. This creates the cache.
#' # Remove dangling cache files that could cause errors.
#' rescue_cache(jobs = 2)
#' # Alternatively, just rescue targets 'small' and 'large'.
#' # Rescuing specific targets is usually faster.
#' rescue_cache(targets = c("small", "large"))
#' })
#' }
rescue_cache <- function(
  targets = NULL,
  path = getwd(),
  search = TRUE,
  verbose = drake::default_verbose(),
  force = FALSE,
  cache = drake::get_cache(
    path = path, search = search, verbose = verbose, force = force),
  jobs = 1,
  garbage_collection = FALSE
){
  if (is.null(cache)){
    return(invisible())
  }
  for (namespace in cache$list_namespaces()){
    X <- cache$list(namespace = namespace)
    if (!is.null(targets)){
      X <- intersect(X, targets)
    }
    lightly_parallelize(
      X = X,
      FUN = rescue_del,
      jobs = jobs,
      cache = cache,
      namespace = namespace
    )
  }
  if (garbage_collection){
    cache$gc()
  }
  invisible(cache)
}

rescue_del <- function(key, cache, namespace){
  tryCatch(
    touch_storr_object(key = key, cache = cache, namespace = namespace),
    error = function(e){
      cache$del(key = key, namespace = namespace)
    }
  )
  invisible(NULL)
}

touch_storr_object <- function(key, cache, namespace){
  envir <- environment()
  hash <- cache$get_hash(key = key, namespace = namespace)
  value <- cache$driver$get_object(hash = hash)
  remove(value, envir = envir)
  invisible(NULL)
}
