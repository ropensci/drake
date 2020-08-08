#' @title Invalidate and deregister targets.
#' `r lifecycle::badge("stable")`
#' @description Force targets to be out of date and remove target names
#'   from the data in the cache. Be careful and run [which_clean()] before
#'   [clean()]. That way, you know beforehand which targets will be
#'   compromised.
#' @details By default, [clean()] invalidates **all** targets,
#'   so be careful. [clean()] always:
#'
#'   1. Forces targets to be out of date so the next [make()]
#'     does not skip them.
#'   2. Deregisters targets so `loadd(your_target)` and `readd(your_target)`
#'     no longer work.
#'
#' By default, `clean()` does not actually remove the underlying data.
#' Even old targets from the distant past are still in the cache
#' and recoverable via `drake_history()` and `make(recover = TRUE)`.
#' To actually remove target data from the cache, as well as any
#' [file_out()] files from any targets you are currently cleaning,
#' run `clean(garbage_collection = TRUE)`.
#' Garbage collection is slow, but it reduces the storage burden of the cache.
#' @seealso [which_clean()], [drake_gc()]
#' @export
#' @return Invisibly return `NULL`.
#' @param ... Symbols, individual targets to remove.
#' @param list Character vector of individual targets to remove.
#' @param search Deprecated
#' @param verbose Deprecated
#' @inheritParams which_clean
#' @param destroy Logical, whether to totally remove the drake cache.
#'   If `destroy` is `FALSE`, only the targets
#'   from `make()`
#'   are removed. If `TRUE`, the whole cache is removed, including
#'   session metadata, etc.
#' @param jobs Deprecated.
#' @param force Logical, whether to try to clean the cache
#'   even though the project may not be back compatible with the
#'   current version of drake.
#' @param garbage_collection Logical, whether to call
#'   `cache$gc()` to do garbage collection.
#'   If `TRUE`, cached data with no remaining references
#'   will be removed.
#'   This will slow down `clean()`, but the cache
#'   could take up far less space afterwards.
#'   See the `gc()` method for `storr` caches.
#' @param purge Logical, whether to remove objects from
#'   metadata namespaces such as "meta", "build_times", and "errors".
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' # Show all registered targets in the cache.
#' cached()
#' # Deregister 'summ_regression1_large' and 'small' in the cache.
#' clean(summ_regression1_large, small)
#' # Those objects are no longer registered as targets.
#' cached()
#' # Rebuild the invalidated/outdated targets.
#' make(my_plan)
#' # Clean everything.
#' clean()
#' # But the data objects and files are not actually gone!
#' file.exists("report.md")
#' drake_history()
#' make(my_plan, recover = TRUE)
#' # You need garbage collection to actually remove the data
#' # and any file_out() files of any uncleaned targets.
#' clean(garbage_collection = TRUE)
#' drake_history()
#' make(my_plan, recover = TRUE)
#' }
#' })
#' }
clean <- function(
  ...,
  list = character(0),
  destroy = FALSE,
  path = NULL,
  search = NULL,
  cache = drake::drake_cache(path = path),
  verbose = NULL,
  jobs = NULL,
  force = FALSE,
  garbage_collection = FALSE,
  purge = FALSE
) {
  deprecate_arg(jobs, "jobs") # 2020-03-06
  deprecate_force(force)
  deprecate_search(search)
  deprecate_verbose(verbose)
  if (is.null(cache)) {
    return(invisible())
  }
  if (destroy) {
    cache$destroy()
    return(invisible())
  }
  cache <- decorate_storr(cache)
  cache$lock()
  if (!destroy) {
    on.exit(cache$unlock(), add = TRUE)
  }
  if (garbage_collection && abort_gc(cache$path)) {
    return(invisible()) # tested manually in test-always-skipped.R # nocov
  }
  namespaces <- clean_select_namespaces(cache = cache, purge = purge)
  targets <- c(as.character(match.call(expand.dots = FALSE)$...), list)
  # Need to duplicate which_clean() because of trouble with dots
  # in R 3.3.0.
  if (requireNamespace("tidyselect", quietly = TRUE)) {
    targets <- drake_tidyselect_cache(
      ...,
      list = list,
      cache = cache,
      namespaces = target_namespaces_()
    )
  }
  if (!length(targets) && is.null(c(...))) {
    targets <- cache$list()
  }
  clean_impl(targets, namespaces, cache)
  clean_cleanup(cache, garbage_collection)
  invisible()
}

clean_impl <- function(targets, namespaces, cache) {
  for (namespace in namespaces) {
    try(cache$del(key = targets, namespace = namespace))
  }
}

clean_select_namespaces <- function(cache, purge) {
  if (purge) {
    target_namespaces_(default = cache$default_namespace)
  } else {
    cleaned_namespaces_(default = cache$default_namespace)
  }
}

clean_cleanup <- function(cache, garbage_collection) {
  if (garbage_collection) {
    cache$gc()
  } else {
    clean_recovery_msg()
  }
}

#' @title Which targets will `clean()` invalidate?
#' `r lifecycle::badge("stable")`
#' @description `which_clean()` is a safety check for `clean()`.
#'   It shows you the targets that `clean()` will
#'   invalidate (or remove if `garbage_collection` is `TRUE`).
#'   It helps you avoid accidentally removing targets you care about.
#' @export
#' @seealso [clean()]
#' @inheritParams cached
#' @param ... Targets to remove from the cache: as names (symbols) or
#'   character strings. If the `tidyselect` package is installed,
#'   you can also supply `dplyr`-style `tidyselect`
#'   commands such as `starts_with()`, `ends_with()`, and `one_of()`.
#' @param list Character vector naming targets to be removed from the
#'   cache. Similar to the `list` argument of [remove()].
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' plan <- drake_plan(x = 1, y = 2, z = 3)
#' make(plan)
#' cached()
#' which_clean(x, y) # [1] "x" "y"
#' clean(x, y)       # Invalidates targets x and y.
#' cached()          # [1] "z"
#' })
#' }
which_clean <- function(
  ...,
  list = character(0),
  path = NULL,
  cache = drake::drake_cache(path = path)
) {
  targets <- c(as.character(match.call(expand.dots = FALSE)$...), list)
  if (requireNamespace("tidyselect", quietly = TRUE)) {
    targets <- drake_tidyselect_cache(
      ...,
      list = list,
      cache = cache,
      namespaces = target_namespaces_()
    )
  }
  if (!length(targets) && is.null(c(...))) {
    targets <- cache$list()
  }
  targets
}

cleaned_namespaces_ <- function(
  default = storr::storr_environment()$default_namespace
) {
  out <- c(default, "meta")
  sort(out)
}

# List the `storr` cache namespaces that store target-level information.
target_namespaces_ <- function(
  default = storr::storr_environment()$default_namespace
) {
  out <- c(
    cleaned_namespaces_(default = default),
    "progress"
  )
  sort(out)
}

clean_recovery_msg <- function() {
  msg_enabled <- .pkg_envir[["drake_clean_recovery_msg"]] %|||%
    getOption("drake_clean_recovery_msg") %|||%
    TRUE
  if (!(interactive() && msg_enabled)) {
    return(FALSE)
  }
  # invoked manually in test-always-skipped.R
  # nocov start
  cli_msg("Undo clean() with recover = TRUE in make().")
  .pkg_envir[["drake_clean_recovery_msg"]] <- FALSE
  invisible()
  # nocov end
}

abort_gc <- function(path) {
  menu_enabled <- .pkg_envir[["drake_clean_menu"]] %|||%
    getOption("drake_clean_menu") %|||%
    TRUE
  if (!(interactive() && menu_enabled)) {
    return(FALSE)
  }
  # tested manually in test-always-skipped.R
  # nocov start
  title <- paste0(
    "clean(garbage_collection = TRUE), ",
    "rescue_cache(garbage_collection = TRUE), and drake_gc() ",
    "*actually* remove data, ",
    "including file_out() files of targets you are currently cleaning. ",
    "Really use garbage collection on the cache at ",
    shQuote(path),
    "? (Prompt shown once per session.)",
    sep = "\n"
  )
  response <- utils::menu(choices = c("yes", "no"), title = title)
  .pkg_envir[["drake_clean_menu"]] <- FALSE
  !identical(as.integer(response), 1L)
  # nocov end
}

#' @title Do garbage collection on the drake cache.
#' `r lifecycle::badge("stable")`
#' @description Garbage collection removes obsolete target values
#' from the cache.
#' @details Caution: garbage collection *actually* removes data
#' so it is no longer recoverable with [drake_history()] or
#' `make(recover = TRUE)`. You cannot undo this operation.
#' Use at your own risk.
#' @seealso [clean()]
#' @export
#' @return`NULL`
#' @inheritParams cached
#' @param force Logical, whether to load the cache
#'   despite any back compatibility issues with the
#'   running version of drake.
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' # At this point, check the size of the '.drake/' cache folder.
#' # Clean without garbage collection.
#' clean(garbage_collection = FALSE)
#' # The '.drake/' cache folder is still about the same size.
#' drake_gc() # Do garbage collection on the cache.
#' # The '.drake/' cache folder should have gotten much smaller.
#' }
#' })
#' }
drake_gc <- function(
  path = NULL,
  search = NULL,
  verbose = NULL,
  cache = drake::drake_cache(path = path),
  force = FALSE
) {
  deprecate_search(search)
  deprecate_verbose(verbose)
  if (is.null(cache)) {
    return()
  }
  cache <- decorate_storr(cache)
  cache$lock()
  on.exit(cache$unlock(), add = TRUE)
  cache$gc()
  rm_bad_cache_filenames(cache)
  invisible()
}

rm_bad_cache_filenames <- function(cache) {
  if (keys_are_mangled(cache)) {
    files <- list.files(path = cache$path, recursive = TRUE)
    keep <- grepl(pattern = "^[-_./\\0-9a-zA-Z]*$", x = files)
    unlink(files[!keep], recursive = TRUE)
  }
}

keys_are_mangled <- function(cache) {
  "driver_rds" %in% class(cache$driver) &&
    identical(cache$driver$mangle_key, TRUE)
}

#' @title Try to repair a drake cache that is prone
#'   to throwing `storr`-related errors.
#' `r lifecycle::badge("questioning")`
#' @description Sometimes, `storr` caches may have
#' dangling orphaned files that prevent you from loading or cleaning.
#' This function tries to remove those files so you can use the
#' cache normally again.
#' @return Nothing.
#' @export
#' @seealso [drake_cache()], [cached()],
#'   [drake_gc()], [clean()]
#' @inheritParams drake_cache
#' @param targets Character vector, names of the targets to rescue.
#'   As with many other drake utility functions, the word `target`
#'   is defined generally in this case, encompassing imports
#'   as well as true targets.
#'   If `targets` is `NULL`, everything in the
#'   cache is rescued.
#' @param cache A `storr` cache object.
#' @param jobs Number of jobs for light parallelism
#'   (disabled on Windows).
#' @param garbage_collection Logical, whether to do garbage collection
#'   as a final step. See [drake_gc()] and [clean()]
#'   for details.
#' @param search Deprecated.
#' @param force Deprecated.
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build targets. This creates the cache.
#' # Remove dangling cache files that could cause errors.
#' rescue_cache(jobs = 2)
#' # Alternatively, just rescue targets 'small' and 'large'.
#' # Rescuing specific targets is usually faster.
#' rescue_cache(targets = c("small", "large"))
#' }
#' })
#' }
rescue_cache <- function(
  targets = NULL,
  path = NULL,
  search = NULL,
  verbose = NULL,
  force = FALSE,
  cache = drake::drake_cache(path = path),
  jobs = 1,
  garbage_collection = FALSE
) {
  deprecate_search(search)
  deprecate_force(force)
  deprecate_verbose(verbose)
  if (is.null(cache)) {
    return(invisible())
  }
  cache <- decorate_storr(cache)
  cache$lock()
  on.exit(cache$unlock(), add = TRUE)
  for (namespace in cache$list_namespaces()) {
    X <- cache$list(namespace = namespace)
    if (!is.null(targets)) {
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
  if (garbage_collection) {
    if (abort_gc(cache$path)) {
      return(invisible()) # tested manually in test-always-skipped.R # nocov
    }
    cache$gc()
  }
  invisible()
}

rescue_del <- function(key, cache, namespace) {
  tryCatch(
    touch_storr_object(key = key, cache = cache, namespace = namespace),
    error = function(e) {
      cache$del(key = key, namespace = namespace)
    }
  )
  invisible(NULL)
}

touch_storr_object <- function(key, cache, namespace) {
  envir <- environment()
  hash <- cache$get_hash(key = key, namespace = namespace)
  value <- cache$get_value(hash = hash, use_cache = FALSE)
  remove(value, envir = envir)
  invisible(NULL)
}
