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
#' test_with_dir("Quarantine side effects.", {
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
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1L,
  jobs = 1,
  progress = NULL
) {
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
  
  # Check then apply progress filters
  if (!is.null(progress)) {
    progress <- unique(progress)
    valid_filters <- c("done", "running", "failed", "none")
    
    if (!all(progress %in% valid_filters)) {
      invalid_filters <- setdiff(progress, valid_filters)
      
      l1_filter <- ngettext(length(progress), "filter", "filters")
      line1 <- paste0(
        "Unsupported progress ", l1_filter, ": ", 
        "`", rlang::expr_text(invalid_filters), "`."
      )
      line2 <- paste0(
        "Use `NULL` for no filtering or one of ", 
        "`", rlang::expr_text(valid_filters), "`."
      )
      warning(line1, "\n  ", line2)
    }
    
    out <- out[out$progress %in% progress, ]
  }
  
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
#' test_with_dir("Quarantine side effects.", {
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
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1L
) {
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
#' test_with_dir("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
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
failed <- function(
  path = getwd(),
  search = TRUE,
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

get_progress_single <- function(target, cache) {
  if (cache$exists(key = target, namespace = "progress")) {
    cache$get(key = target, namespace = "progress")
  } else{
    "none"
  }
}

set_progress <- function(target, meta, value, config) {
  if (!config$log_progress || (meta$imported %||% FALSE)) {
    return()
  }
  config$cache$duplicate(
    key_src = value,
    key_dest = target,
    namespace_src = "common",
    namespace_dest = "progress"
  )
}
