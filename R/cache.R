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
#' @param imported_files_only logical, applies only when 
#' no targets are specified and a list of cached targets is returned.
#' If \code{imported_files_only} is \code{TRUE} and no targets are
#' specified in \code{...} or \code{list}, then \code{cached()} 
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
cached = function(..., list = character(0), imported_files_only = FALSE,
  path = getwd(), search = TRUE){
  cache = get_cache(path = path, search = search)
  if(is.null(cache)) return(character(0))
  dots = match.call(expand.dots = FALSE)$...
  targets = targets_from_dots(dots, list)
  if(!length(targets)) return(list_cached(
    imported_files_only = imported_files_only, cache = cache,
    path = path, search = search))
  all_cached = intersect(cache$list(), cache$list(namespace = "depends"))
  presence = targets %in% all_cached
  names(presence) = targets
  presence
}

list_cached = function(imported_files_only, cache, path, search){
  all_cached = intersect(cache$list(), cache$list(namespace = "depends"))
  abridged_cached = Filter(all_cached, f = function(target)
    is_built_or_imported_file(target = target, path = path, search = search))
  if(imported_files_only) return(abridged_cached) 
  else return(all_cached)
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
built = function(path = getwd(), search = TRUE){
  setdiff(cached(path = path, search = search), 
    imported(files_only = FALSE, path = path, search = search))
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
imported = function(files_only = FALSE, path = getwd(), search = TRUE){
  targets = cached(imported_files_only = files_only, 
    path = path, search = search) 
  select = is_imported(targets, path = path, search = search)
  if(!length(select)) return(character(0))
  targets[select]
}

get_cache = function(path = getwd(), search = TRUE){
  if(search) path = find_cache(path = path)
  else path = file.path(path, cachepath)
  if(is.null(path)) return(NULL)
  if(!file.exists(path)) return(NULL)
  storr_rds(path, mangle_key = TRUE)
}

# from base::remove()
targets_from_dots = function(dots, list){
  if (length(dots) && !all(vapply(dots, function(x) is.symbol(x) ||
    is.character(x), NA, USE.NAMES = FALSE)))
    stop("... must contain names or character strings")
  names = vapply(dots, as.character, "")
  if (length(names) == 0L) names = character()
  .Primitive("c")(names, list) %>% unique
}

is_imported = Vectorize(function(target, path, search){
  cache = get_cache(path = path, search = search)
  if(is.null(cache)) return(FALSE)
  if(!(target %in% cache$list())) return(FALSE)
  cache$get(target)$imported
}, "target", SIMPLIFY = TRUE)

is_built_or_imported_file = Vectorize(function(target, path, search){
  imported = is_imported(target = target, path = path, search = search)
  !imported | (imported & is_file(target))
}, "target", SIMPLIFY = TRUE)

cachepath = ".drake"
