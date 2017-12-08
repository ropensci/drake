#' @title Function \code{clean}
#' @description Cleans up work done by \code{\link{make}()}.
#' @details
#' By default, \code{clean()} removes references to cached data.
#' To deep-clean the data to free up storage/memory, use
#' \code{clean(garbage_collection = TRUE)}. Garbage collection is slower,
#' but it purges data with no remaining references. To just do garbage
#' collection without cleaning, see \code{\link{drake_gc}()}.
#' Also, for \code{clean()}, you must be in your project's working directory
#' or a subdirectory of it.
#' \code{clean(search = TRUE)} searches upwards in your folder structure
#' for the drake cache and acts on the first one it sees. Use
#' \code{search == FALSE} to look within the current working
#' directory only.
#' WARNING: This deletes ALL work done with \code{\link{make}()},
#' which includes
#' file targets as well as the entire drake cache. Only use \code{clean()}
#' if you're sure you won't lose anything important.
#' @seealso \code{\link{drake_gc}}, \code{\link{make}}
#' @export
#' @return Invisibly return \code{NULL}.
#'
#' @param ... targets to remove from the cache, as names (unquoted)
#' or character strings (quoted). Similar to \code{...} in
#' \code{\link{remove}(...)}.
#' The symbols must not match other (formal) arguments of \code{clean()},
#' such as \code{destroy}, \code{cache}, \code{path}, \code{search},
#' \code{verbose}, or \code{jobs}. If there are name conflicts,
#' use the \code{list} argument instead of \code{...}.
#'
#' @param list character vector naming targets to be removed from the
#' cache. Similar to the \code{list} argument of \code{\link{remove}()}.
#'
#' @param destroy logical, whether to totally remove the drake cache.
#' If \code{destroy} is \code{FALSE}, only the targets
#' from \code{make}()
#' are removed. If \code{TRUE}, the whole cache is removed, including
#' session metadata, etc.
#'
#' @param cache optional drake cache. See code{\link{new_cache}()}. If
#' If \code{cache} is supplied,
#' the \code{path} and \code{search} arguments are ignored.
#'
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#'
#' @param search logical. If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
#'
#' @param verbose whether to print console messages
#'
#' @param jobs Number of jobs for light parallelism
#' (disabled on Windows).
#'
#' @param force logical, whether to try to clean the cache
#' even though the project may not be back compatible with the
#' current version of drake.
#'
#' @param garbage_collection logical, whether to call
#' \code{cache$gc()} to do garbage collection.
#' If \code{TRUE}, cached data with no remaining references
#' will be removed.
#' This will slow down \code{clean()}, but the cache
#' could take up far less space afterwards.
#' See the \code{gc()} method for \code{storr} caches.
#'
#' @param purge logical, whether to remove objects from
#' metadata namespaces such as "meta", "build_times", and "errors".
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Load drake's canonical example.
#' make(my_plan) # Run the project, build the targets.
#' # List objects in the cache, excluding R objects
#' # imported from your workspace.
#' cached(no_imported_objects = TRUE)
#' # Remove 'summ_regression1_large' and 'small' from the cache.
#' clean(summ_regression1_large, small)
#' # Those objects should be gone.
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
  verbose = 1,
  jobs = 1,
  force = FALSE,
  garbage_collection = FALSE,
  purge = FALSE
){
  dots <- match.call(expand.dots = FALSE)$...
  targets <- targets_from_dots(dots, list)
  if (is.null(cache)){
    cache <- get_cache(
      path = path, search = search, verbose = verbose, force = force)
  }
  if (is.null(cache)){
    return(invisible())
  }
  uncache(targets = targets, cache = cache, jobs = jobs, purge = purge)
  if (destroy){
    cache$destroy()
  }
  if (garbage_collection){
    cache$gc()
  }
  invisible()
}

uncache <- function(targets, cache, jobs, purge){
  if (is.null(cache)){
    return()
  }
  plan <- read_drake_plan(cache = cache)
  if (purge){
    namespaces <- target_namespaces(default = cache$default_namespace)
  } else {
    namespaces <- cleaned_namespaces(default = cache$default_namespace)
  }
  for (namespace in namespaces){
    if (!length(targets)){
      remove_these <- cache$list(namespace = namespace)
    } else {
      remove_these <- parallel_filter(
        x = targets,
        f = function(target){
          cache$exists(key = target, namespace = namespace)
        },
        jobs = jobs
      )
    }
    files <- parallel_filter(
      x = remove_these, f = is_existing_file, jobs = jobs)
    lightly_parallelize(
      X = files,
      FUN = remove_file_target,
      jobs = jobs,
      plan = plan
    )
    lightly_parallelize(
      X = remove_these,
      FUN = uncache_single,
      jobs = jobs,
      cache = cache,
      namespace = namespace
    )
  }
  invisible()
}

uncache_single <- function(target, cache, namespace){
  cache$del(target, namespace = namespace)
  invisible()
}

remove_file_target <- function(target, plan){
  if (
    is_file(target) &&
    !is_imported(
      target = target,
      plan = plan
    )
  ){
    drake_unquote(target) %>%
      unlink(recursive = TRUE, force = TRUE)
  }
  invisible()
}

#' @title Function \code{drake_gc}
#' @description Do garbage collection on the cache
#' @seealso \code{\link{clean}}
#' @export
#' @return\code{NULL}
#' @param path file path to the folder containing the cache.
#' Yes, this is the parent directory containing the cache,
#' not the cache itself, and it assumes the cache is in the
#' `.drake` folder. If you are looking for a different cache
#' with a known folder different from `.drake`, use
#' the \code{\link{this_cache}()} function.
#' @param search logical, whether to search back in the file system
#' for the cache.
#' @param verbose logical, whether to print the location of the cache
#' @param cache the \code{drake}/\code{storr} cache object itself,
#' if available.
#' @param force logical, whether to load the cache
#' despite any back compatibility issues with the
#' running version of drake.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Load drake's canonical example.
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
  verbose = 1,
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
  }
  invisible()
}

#' @title Function \code{rescue_cache}
#' @description Sometimes, \code{storr} caches may have
#' dangling orphaned files that prevent you from loading or cleaning.
#' This function tries to remove those files so you can use the
#' cache normally again.
#' @return The rescued drake/storr cache.
#' @export
#' @seealso \code{\link{get_cache}}, \code{\link{cached}},
#' \code{\link{drake_gc}}, \code{\link{clean}}
#' @param targets Character vector, names of the targets to rescue.
#' As with many other drake utility functions, the word \code{target}
#' is defined generally in this case, encompassing imports
#' as well as true targets.
#' If \code{targets} is \code{NULL}, everything in the
#' cache is rescued.
#' @param path same as for \code{\link{get_cache}()}
#' @param search same as for \code{\link{get_cache}()}
#' @param verbose same as for \code{\link{get_cache}()}
#' @param force same as for \code{\link{get_cache}()}
#' @param cache a `storr` cache object
#' @param jobs number of jobs for light parallelism
#' (disabled on Windows)
#' @param garbage_collection logical, whether to do garbage collection
#' as a final step. See \code{\link{drake_gc}} and \code{\link{clean}}
#' for details.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Load the canonical example.
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
  path = getwd(), search = TRUE, verbose = 1, force = FALSE,
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
