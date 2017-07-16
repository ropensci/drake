#' @title Function \code{session}
#' @description Load the \code{\link{sessionInfo}()}
#' of the last call to \code{\link{make}()}.
#' @seealso \code{\link{built}}, \code{\link{imported}},
#' \code{\link{readd}}, \code{\link{plan}}, \code{\link{make}}
#' @export
#' @return \code{\link{sessionInfo}()} of the last
#' call to \code{\link{make}()}
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#' @param search If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
#' @examples
#' \dontrun{
#' load_basic_example()
#' make(my_plan)
#' session()
#' }
session = function(path = getwd(), search = TRUE){
  cache = get_cache(path = path, search = search)
  if(is.null(cache)) stop("No drake::make() session detected.")
  cache$get("sessionInfo", namespace = "session")
}

#' @title Function \code{progress}
#' @description Get the build progress (overall or individual targets)
#' of the last call to \code{\link{make}()}. 
#' Objects that drake imported, built, or attempted
#' to build are listed as \code{"finished"} or \code{"in progress"}. 
#' Skipped objects are not listed.
#' @seealso \code{\link{session}},
#' \code{\link{built}}, \code{\link{imported}},
#' \code{\link{readd}}, \code{\link{plan}}, \code{\link{make}}
#' @export
#' @return Either the build progress of each target given (from the last
#' call to \code{\link{make}()} or \code{\link{make}()}), or if no 
#' targets are specified, a data frame containing the build progress
#' of the last session. 
#' In the latter case, only finished targets are listed.
#' @return Either a named logical indicating whether the given
#' targets or cached or a character vector listing all cached
#' items, depending on whether any targets are specified
#' @param ... objects to load from the cache, as names (unquoted)
#' or character strings (quoted). Similar to \code{...} in
#' \code{\link{remove}(...)}.
#' @param list character vector naming objects to be loaded from the
#' cache. Similar to the \code{list} argument of \code{\link{remove}()}.
#' @param no_imported_objects logical, whether to only return information
#' about imported files and targets with commands (i.e. whether to ignore 
#' imported objects that are not files).
#' @param imported_files_only logical, deprecated. Same as \code{no_imported_objects}.
#' Use the \code{no_imported_objects} argument instead.
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#' @param search If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
#' @examples
#' \dontrun{
#' load_basic_example()
#' make(my_plan)
#' progress()
#' progress(small, large)
#' progress(list = c("small", "large"))
#' progress(no_imported_objects = TRUE)
#' }
progress = function(..., list = character(0), no_imported_objects = FALSE, 
  imported_files_only = logical(0), path = getwd(), search = TRUE){
  if(length(imported_files_only)){ # deprecate imported_files_only
    warning("The imported_files_only argument to progress() is deprecated ",
      "and will be removed the next major release. ",
      "Use the no_imported_objects argument instead.")
    no_imported_objects = imported_files_only
  }
  cache = get_cache(path = path, search = search)
  if(is.null(cache)) stop("No drake::make() session detected.")
  dots = match.call(expand.dots = FALSE)$...
  targets = targets_from_dots(dots, list)
  if(!length(targets)) 
    return(list_progress(no_imported_objects = no_imported_objects, 
      cache = cache))
  get_progress(targets, cache)
}

list_progress = function(no_imported_objects, cache){
  all_marked = cache$list(namespace = "progress")
  all_progress = get_progress(target = all_marked, cache = cache)
  abridged_marked = Filter(all_marked, f = function(target)
    is_built_or_imported_file(target = target, cache = cache))
  abridged_progress = all_progress[abridged_marked]
  if(no_imported_objects) return(abridged_progress) 
  else return(all_progress) 
}

get_progress = Vectorize(function(target, cache){
  if(target %in% cache$list("progress")) 
    cache$get(key = target, namespace = "progress")
  else
    "not built or imported"
}, "target", SIMPLIFY = TRUE, USE.NAMES = TRUE)
