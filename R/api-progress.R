# List the targets that either (1) are currently being built, or
# were being built if the last make quit unexpectedly.
in_progress_ <- function(path = getwd(), search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1L
) {
  prog <- progress(path = path, search = search, cache = cache)
  prog$target[prog$progress == "running"]
}

#' @title List the targets that failed in the last call
#'   to [make()].
#' @description Together, functions `failed` and
#' [diagnose()] should eliminate the strict need
#' for ordinary error messages printed to the console.
#' @seealso [diagnose()], [drake_get_session_info()],
#'   [cached()], [readd()], [drake_plan()], [make()]
#' @export
#' @return A character vector of target names.
#' @inheritParams cached
#' @param upstream_only Deprecated.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' if (requireNamespace("knitr")) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' failed() # Should show that no targets failed.
#' # Build a workflow plan doomed to fail:
#' bad_plan <- drake_plan(x = function_doesnt_exist())
#' try(make(bad_plan), silent = TRUE) # error
#' failed() # "x"
#' diagnose(x) # Retrieve the cached error log of x.
#' }
#' })
#' }
failed <- function(path = getwd(), search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1L,
  upstream_only = NULL
) {
  if (!is.null(upstream_only)) {
    warning("argument upstream_only is deprecated.")
  }
  prog <- progress(path = path, search = search, cache = cache)
  prog$target[prog$progress == "failed"]
}

#' @title Get the build progress of your targets
#'   during a [make()].
#' @description Objects that drake imported, built, or attempted
#' to build are listed as `"done"` or `"running"`.
#' Skipped objects are not listed.
#' @seealso [diagnose()], [drake_get_session_info()],
#'   [cached()], [readd()], [drake_plan()], [make()]
#' @export
#'
#' @return The build progress of each target reached by
#'   the current [make()] so far.
#'
#' @inheritParams cached
#'
#' @param ... Objects to load from the cache, as names (unquoted)
#'   or character strings (quoted). Similar to `...` in
#'   `remove()` and `rm()`.
#'
#' @param list Character vector naming objects to be loaded from the
#'   cache. Similar to the `list` argument of [remove()].
#'
#' @param no_imported_objects Logical, whether to only return information
#'   about imported files and targets with commands (i.e. whether to ignore
#'   imported objects that are not files).
#'
#' @param jobs Number of jobs/workers for parallel processing.
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' if (requireNamespace("knitr")) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' # Watch the changing progress() as make() is running.
#' progress() # List all the targets reached so far.
#' progress(small, large) # Just see the progress of some targets.
#' progress(list = c("small", "large")) # Same as above.
#' progress(no_imported_objects = TRUE) # Ignore imported R objects.
#' }
#' })
#' }
progress <- function(
  ...,
  list = character(0),
  no_imported_objects = NULL,
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1L,
  jobs = 1
) {
  if (is.null(cache)) {
    return(character(0))
  }
  if (!is.null(no_imported_objects)) {
    warning(
      "Argument `no_imported_objects` of progress() is deprecated. ",
      "Only targets are returned now.",
      call. = FALSE
    )
  }
  dots <- match.call(expand.dots = FALSE)$...
  targets <- targets_from_dots(dots, list)
  if (!length(targets)) {
    targets <- cache$list(namespace = "progress")
  }
  progress <- vapply(
    targets,
    get_progress_single,
    cache = cache,
    FUN.VALUE = character(1)
  )
  weak_tibble(target = targets, progress = progress)
}

get_progress_single <- function(target, cache) {
  if (cache$exists(key = target, namespace = "progress")) {
    cache$get(key = target, namespace = "progress")
  } else{
    "none"
  }
}

set_progress <- function(target, meta, value, config) {
  if (!config$log_progress || meta$imported) {
    return()
  }
  config$cache$duplicate(
    key_src = value,
    key_dest = target,
    namespace_src = "common",
    namespace_dest = "progress"
  )
}
