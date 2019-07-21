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
#' @param progress Character vector for filtering the build progress results.
#'   Defaults to `NULL` (no filtering) to report progress of all objects.
#'   Supported filters are `"done"`, `"running"`, `"failed"` and `"none"`.
#'
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
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
  path = NULL,
  search = NULL,
  cache = drake::drake_cache(path = path, verbose = verbose),
  verbose = 1L,
  jobs = 1,
  progress = NULL
) {
  deprecate_search(search)
  if (is.null(cache)) {
    return(weak_tibble(target = character(0), progress = character(0)))
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
  progress_results <- vapply(
    targets,
    get_progress_single,
    cache = cache,
    FUN.VALUE = character(1)
  )
  out <- weak_tibble(target = targets, progress = progress_results)
  rownames(out) <- NULL

  if (is.null(progress)) {
    return(out)
  }

  progress <- match.arg(
    progress,
    choices = c("done", "running", "failed", "none"),
    several.ok = TRUE
  )

  out <- out[out$progress %in% progress, ]
  out
}

#' @title List running targets.
#' @description List the targets that either
#'   (1) are currently being built during a call to [make()], or
#'   (2) if [make()] was interrupted, the targets that were running
#'     at the time.
#' @seealso [failed()], [make()]
#' @export
#' @return A character vector of target names.
#' @inheritParams cached
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' running() # Everything should be done.
#' # nolint start
#' # Run make() in one R session...
#' # slow_plan <- drake_plan(x = Sys.sleep(2))
#' # make(slow_plan)
#' # and see the progress in another session.
#' # running()
#' # nolint end
#' }
#' })
#' }
running <- function(
  path = NULL,
  search = NULL,
  cache = drake::drake_cache(path = path, verbose = verbose),
  verbose = 1L
) {
  deprecate_search(search)
  prog <- progress(path = path, search = search, cache = cache)
  prog$target[prog$progress == "running"]
}

#' @title List failed targets.
#'   to [make()].
#' @description Together, functions `failed()` and
#' [diagnose()] should eliminate the strict need
#' for ordinary error messages printed to the console.
#' @seealso [running()], [make()]
#' @export
#' @return A character vector of target names.
#' @inheritParams cached
#' @param upstream_only Deprecated.
#' @examples
#' \dontrun{
#' isolate_example("contain side effects", {
#' if (suppressWarnings(require("knitr"))) {
#' # Build a plan doomed to fail:
#' bad_plan <- drake_plan(x = function_doesnt_exist())
#' cache <- storr::storr_environment() # optional
#' try(
#'   make(bad_plan, cache = cache, history = FALSE),
#'   silent = TRUE
#' ) # error
#' failed(cache = cache) # "x"
#' e <- diagnose(x, cache = cache) # Retrieve the cached error log of x.
#' names(e)
#' e$error
#' names(e$error)
#' }
#' })
#' }
failed <- function(
  path = NULL,
  search = NULL,
  cache = drake::drake_cache(path = path, verbose = verbose),
  verbose = 1L,
  upstream_only = NULL
) {
  deprecate_search(search)
  if (!is.null(upstream_only)) {
    warning("argument upstream_only is deprecated.")
  }
  prog <- progress(path = path, search = search, cache = cache)
  prog$target[prog$progress == "failed"]
}

get_progress_single <- function(target, cache) {
  if (cache$exists(key = target, namespace = "progress")) {
    hash <- cache$get_hash(key = target, namespace = "progress")
    switch(
      substr(hash, 1, 1),
      r = "running",
      d = "done",
      f = "failed",
      NA_character_
    )
  } else{
    "none"
  }
}

set_progress <- function(target, meta, value, config) {
  skip_progress <- !identical(config$running_make, TRUE) ||
    !config$log_progress ||
    (meta$imported %||% FALSE)
  if (skip_progress) {
    return()
  }
  config$cache$driver$set_hash(
    key = target,
    namespace = "progress",
    hash = config$progress_hashmap[[value]]
  )
}

progress_hashmap <- function(cache) {
  keys <- c("running", "done", "failed")
  out <- lapply(keys, progress_hash, cache = cache)
  names(out) <- keys
  out
}

progress_hash <- function(key, cache) {
  out <- digest::digest(
    key,
    algo = cache_hash_algorithm(cache),
    serialize = FALSE
  )
  gsub("^.", substr(key, 1, 1), out)
}
