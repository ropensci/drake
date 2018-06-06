#' @title Return the [sessionInfo()]
#'   of the last call to [make()].
#' @description By default, session info is saved
#' during [make()] to ensure reproducibility.
#' Your loaded packages and their versions are recorded, for example.
#' @seealso [diagnose()], [built()], [imported()],
#'   [readd()], [drake_plan()], [make()]
#' @export
#' @return [sessionInfo()] of the last
#'   call to [make()]
#' @inheritParams cached
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' drake_session() # Retrieve the cached sessionInfo() of the last make().
#' })
#' }
drake_session <- function(path = getwd(), search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = drake::default_verbose()
){
  if (is.null(cache)) {
    stop("No drake::make() session detected.")
  }
  return(cache$get("sessionInfo", namespace = "session"))
}

#' @title List the targets that either
#'   (1) are currently being built during a [make()], or
#'   (2) were being built if the last [make()] quit unexpectedly.
#' @description Similar to [progress()].
#' @seealso [diagnose()], [session()],
#'   [built()], [imported()],
#'   [readd()], [drake_plan()], [make()]
#' @export
#' @return A character vector of target names.
#' @inheritParams cached
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Kill before targets finish.
#' # If you interrupted make(), some targets will probably be listed:
#' in_progress()
#' })
#' }
in_progress <- function(path = getwd(), search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = drake::default_verbose()
){
  prog <- progress(path = path, search = search, cache = cache)
  which(prog == "in progress") %>%
    names() %>%
    as.character()
}

#' @title List the targets that failed in the last call
#'   to [make()].
#' @description Together, functions `failed` and
#' [diagnose()] should eliminate the strict need
#' for ordinary error messages printed to the console.
#' @seealso [diagnose()], [session()],
#'   [built()], [imported()],
#'   [readd()], [drake_plan()], [make()]
#' @export
#' @return A character vector of target names.
#' @inheritParams cached
#' @param upstream_only logical, whether to list only those targets
#'   with no failed dependencies.
#'   Naturally accompanies `make(keep_going = TRUE)`.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' failed() # Should show that no targets failed.
#' # Build a workflow plan doomed to fail:
#' bad_plan <- drake_plan(x = function_doesnt_exist())
#' try(make(bad_plan), silent = TRUE) # error
#' failed() # "x"
#' diagnose(x) # Retrieve the cached error log of x.
#' })
#' }
failed <- function(path = getwd(), search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = drake::default_verbose(),
  upstream_only = FALSE
){
  prog <- progress(path = path, search = search, cache = cache)
  out <- which(prog == "failed") %>%
    names() %>%
    as.character()
  if (upstream_only){
    graph <- read_drake_graph(cache = cache)
    out <- filter_upstream(targets = out, graph = graph)
  }
  out
}

#' @title Get the build progress of your targets
#'   during a [make()].
#' @description Objects that drake imported, built, or attempted
#' to build are listed as `"finished"` or `"in progress"`.
#' Skipped objects are not listed.
#' @seealso [diagnose()], [session()],
#'   [built()], [imported()],
#'   [readd()], [drake_plan()], [make()]
#' @export
#'
#' @return The build progress of each target reached by
#'   the current [make()] so far.
#'
#' @inheritParams cached
#'
#' @param ... objects to load from the cache, as names (unquoted)
#'   or character strings (quoted). Similar to `...` in
#'   \code{\link{remove}(...)}.
#'
#' @param list character vector naming objects to be loaded from the
#'   cache. Similar to the `list` argument of [remove()].
#'
#' @param no_imported_objects logical, whether to only return information
#'   about imported files and targets with commands (i.e. whether to ignore
#'   imported objects that are not files).
#'
#' @param imported_files_only logical, deprecated. Same as
#'   `no_imported_objects`.  Use the `no_imported_objects` argument
#'   instead.
#'
#' @param jobs number of jobs/workers for parallel processing
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' # Watch the changing progress() as make() is running.
#' progress() # List all the targets reached so far.
#' progress(small, large) # Just see the progress of some targets.
#' progress(list = c("small", "large")) # Same as above.
#' progress(no_imported_objects = TRUE) # Ignore imported R objects.
#' })
#' }
progress <- function(
  ...,
  list = character(0),
  no_imported_objects = FALSE,
  imported_files_only = logical(0),
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = drake::default_verbose(),
  jobs = 1
){
  # deprecate imported_files_only
  if (length(imported_files_only)){
    warning(
      "The imported_files_only argument to progress() is deprecated ",
      "and will be removed the next major release. ",
      "Use the no_imported_objects argument instead.",
      call. = FALSE
    )
    no_imported_objects <- imported_files_only
  }
  if (is.null(cache)){
    return(character(0))
  }
  dots <- match.call(expand.dots = FALSE)$...
  targets <- targets_from_dots(dots, list)
  if (!length(targets)){
    return(
      list_progress(
        no_imported_objects = no_imported_objects,
        cache = cache,
        jobs = jobs
      )
    )
  }
  get_progress(targets = targets, cache = cache, jobs = jobs)
}

list_progress <- function(no_imported_objects, cache, jobs){
  all_marked <- cache$list(namespace = "progress")
  all_progress <- get_progress(
    targets = all_marked,
    cache = cache,
    jobs = jobs
  )
  abridged_marked <- parallel_filter(
    all_marked,
    f = function(target){
      is_built_or_imported_file(target = target, cache = cache)
    },
    jobs = jobs
  )
  abridged_progress <- all_progress[abridged_marked]
  if (no_imported_objects){
    out <- abridged_progress
  } else{
    out <- all_progress
  }
  if (!length(out)){
    out <- as.character(out)
  }
  return(out)
}

get_progress <- function(targets, cache, jobs){
  if (!length(targets)){
    return(character(0))
  }
  out <- lightly_parallelize(
    X = targets,
    FUN = get_progress_single,
    jobs = jobs,
    cache = cache
  ) %>%
    unlist
  names(out) <- targets
  out
}

get_progress_single <- function(target, cache){
  if (cache$exists(key = target, namespace = "progress")){
    cache$get(key = target, namespace = "progress")
  } else{
    "not built or imported"
  }
}

set_progress <- function(target, value, config){
  if (!config$log_progress){
    return()
  }
  just_try(
    config$cache$set(
      key = target,
      value = value,
      namespace = "progress"
    )
  )
}
