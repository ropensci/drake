#' @title Function \code{cached}
#' @description Check whether targets are in the cache.
#' If no targets are specified with \code{...} or \code{list},
#' then \code{cached()} lists
#' all the items in the drake cache.
#' Read/load a cached item with \code{\link{readd}()} or
#' \code{\link{loadd}()}.
#' @seealso \code{\link{built}}, \code{\link{imported}},
#' \code{\link{readd}}, \code{\link{loadd}},
#' \code{\link{plan}}, \code{\link{make}}
#' @export
#' @return Either a named logical indicating whether the given
#' targets or cached or a character vector listing all cached
#' items, depending on whether any targets are specified
#' @param ... objects to load from the cache, as names (unquoted)
#' or character strings (quoted). Similar to \code{...} in
#' \code{\link{remove}(...)}.
#' @param list character vector naming objects to be loaded from the
#' cache. Similar to the \code{list} argument of \code{\link{remove}()}.
#' @param no_imported_objects logical, applies only when
#' no targets are specified and a list of cached targets is returned.
#' If \code{no_imported_objects} is \code{TRUE}, then \code{cached()}
#' shows built targets (with commands) plus imported files,
#' ignoring imported objects. Otherwise, the full collection of
#' all cached objects will be listed. Since all your functions and
#' all their global variables are imported, the full list of
#' imported objects could get really cumbersome.
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#' @param search logical. If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
#' @examples
#' \dontrun{
#' load_basic_example()
#' make(my_plan)
#' cached(list = 'reg1')
#' cached(no_imported_objects = TRUE)
#' cached()
#' }
cached <- function(..., list = character(0), no_imported_objects = FALSE,
  path = getwd(), search = TRUE) {
  cache <- get_cache(path = path, search = search)
  if (is.null(cache))
    return(character(0))
  dots <- match.call(expand.dots = FALSE)$...
  targets <- targets_from_dots(dots, list)
  if (!length(targets))
    list_cache(no_imported_objects = no_imported_objects,
      cache = cache)
  else
    is_cached(targets = targets, no_imported_objects = no_imported_objects,
      cache = cache)
}

is_cached <- function(targets, no_imported_objects, cache) {
  if (no_imported_objects)
    targets <- no_imported_objects(targets = targets, cache = cache)
  inclusion <- targets %in% cache$list()
  names(inclusion) <- targets
  inclusion
}

list_cache <- function(no_imported_objects, cache) {
  targets <- cache$list()
  if (no_imported_objects)
    targets <- no_imported_objects(targets = targets, cache = cache)
  targets
}

#' @title Function \code{built}
#' @description List all the built (non-imported) objects in the drake
#' cache.
#' @seealso \code{\link{cached}}, \code{\link{loadd}},
#' \code{link{imported}}
#' @export
#' @return list of imported objects in the cache
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#' @param search logical. If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
#' @examples
#' \dontrun{
#' load_basic_example()
#' make(my_plan)
#' built()
#' }
built <- function(path = getwd(), search = TRUE) {
  cache <- get_cache(path = path, search = search)
  if (is.null(cache))
    return(character(0))
  cache$list() %>% Filter(f = function(target) !is_imported(target = target,
    cache = cache))
}

#' @title Function \code{imported}
#' @description List all the imported objects in the drake cache
#' @seealso \code{\link{cached}}, \code{\link{loadd}},
#' \code{\link{built}}
#' @export
#' @return character vector naming the imported objects in the cache
#' @param files_only logical, whether to show imported files only
#' and ignore imported objects. Since all your functions and
#' all their global variables are imported, the full list of
#' imported objects could get really cumbersome.
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#' @param search logical. If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
#' @examples
#' \dontrun{
#' load_basic_example()
#' make(my_plan)
#' imported()
#' }
imported <- function(files_only = FALSE, path = getwd(), search = TRUE) {
  cache <- get_cache(path = path, search = search)
  if (is.null(cache))
    return(character(0))
  targets <- cache$list() %>%
    Filter(f = function(target) is_imported(target = target, cache = cache))
  if (files_only)
    targets <- Filter(targets, f = is_file)
  targets
}

#' @title Function get_cache
#' @description get the drake cache
#' @seealso \code{\link{config}}
#' @export
#' @param path path to the cache
#' @param search logical, whether to search back in the file system
#' for the cache.
#' @examples
#' \dontrun{
#' get_cache()
#' load_basic_example()
#' make(my_plan)
#' get_cache()
#' }
get_cache <- function(path = getwd(), search = TRUE) {
  if (search)
    path <- find_cache(path = path) else path <- file.path(path, cache_dir)
  if (is.null(path))
    return(NULL)
  if (!file.exists(path))
    return(NULL)
  hash_algos <- choose_hash_algos()
  cache <- storr_rds(
    path,
    mangle_key = TRUE,
    hash_algorithm = hash_algos$short
  )
  cache$driver$path <- normalizePath(cache$driver$path)
  cache
}

# from base::remove()
targets_from_dots <- function(dots, list) {
  if (length(dots) && !all(vapply(dots, function(x) is.symbol(x) ||
    is.character(x), NA, USE.NAMES = FALSE)))
    stop("... must contain names or character strings")
  names <- vapply(dots, as.character, "")
  if (length(names) == 0L)
    names <- character()
  .Primitive("c")(names, list) %>% unique
}

imported_only <- function(targets, cache) {
  Filter(targets, f = function(target) is_imported(target = target,
    cache = cache))
}

no_imported_objects <- function(targets, cache) {
  Filter(targets,
    f = function(target) is_built_or_imported_file(target = target,
      cache = cache))
}

is_imported <- Vectorize(function(target, cache) {
  if (!(target %in% cache$list()))
    return(FALSE)
  cache$get(target)$imported
},
"target", SIMPLIFY = TRUE)

is_built_or_imported_file <- Vectorize(function(target, cache) {
  imported <- is_imported(target = target, cache = cache)
  !imported | (imported & is_file(target))
},
"target", SIMPLIFY = TRUE)

cache_dir <- ".drake"
globalenvpath <- file.path(cache_dir, "globalenv.RData")
