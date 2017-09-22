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
session <- function(path = getwd(), search = TRUE){
  cache <- get_cache(path = path, search = search)
  if (is.null(cache)){
    stop("No drake::make() session detected.")
  }
  return(cache$get("sessionInfo", namespace = "session"))
}

#' @title Function \code{in_progress}
#' @description List the targets that either
#' (1) are currently being built if \code{\link{make}()} is running, or
#' (2) were in the process of being built if the previous call to
#' \code{\link{make}()} quit unexpectedly.
#' @seealso \code{\link{session}},
#' \code{\link{built}}, \code{\link{imported}},
#' \code{\link{readd}}, \code{\link{plan}}, \code{\link{make}}
#' @export
#' @return A character vector of target names
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
#' in_progress() # nothing
#' bad_plan = plan(x = function_doesnt_exist())
#' make(bad_plan) # error
#' in_progress() # "x"
#' }
in_progress <- function(path = getwd(), search = TRUE){
  which(progress() == "in progress") %>%
    names() %>%
    as.character()
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
#' @return Statuses of targets
#' @param ... objects to load from the cache, as names (unquoted)
#' or character strings (quoted). Similar to \code{...} in
#' \code{\link{remove}(...)}.
#' @param list character vector naming objects to be loaded from the
#' cache. Similar to the \code{list} argument of \code{\link{remove}()}.
#' @param no_imported_objects logical, whether to only return information
#' about imported files and targets with commands (i.e. whether to ignore
#' imported objects that are not files).
#' @param imported_files_only logical, deprecated. Same as
#' \code{no_imported_objects}.  Use the \code{no_imported_objects} argument
#' instead.
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
progress <- function(
  ...,
  list = character(0),
  no_imported_objects = FALSE,
  imported_files_only = logical(0),
  path = getwd(),
  search = TRUE
  ){
  # deprecate imported_files_only
  if (length(imported_files_only)){
    warning(
      "The imported_files_only argument to progress() is deprecated ",
      "and will be removed the next major release. ",
      "Use the no_imported_objects argument instead."
      )
    no_imported_objects <- imported_files_only
  }
  cache <- get_cache(path = path, search = search)
  if (is.null(cache)){
    return(character(0))
  }
  dots <- match.call(expand.dots = FALSE)$...
  targets <- targets_from_dots(dots, list)
  if (!length(targets)){
    return(list_progress(
        no_imported_objects = no_imported_objects,
        cache = cache
        ))
  }
  return(get_progress(targets, cache))
}

list_progress <- function(no_imported_objects, cache){
  all_marked <- cache$list(namespace = "progress")
  all_progress <- get_progress(target = all_marked, cache = cache)
  abridged_marked <- Filter(
    all_marked,
    f = function(target){
      is_built_or_imported_file(target = target, cache = cache)
    }
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

get_progress <- Vectorize(
  function(target, cache){
    if (target %in% cache$list("progress")){
      cache$get(key = target, namespace = "progress")
    } else{
      "not built or imported"
    }
  },
  "target",
  SIMPLIFY = TRUE,
  USE.NAMES = TRUE
)
