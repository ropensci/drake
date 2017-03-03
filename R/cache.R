#' @title Function \code{cached}
#' @description Check whether targets are in the cache.
#' If no targets are specified with \code{...} or \code{list}, 
#' then \code{cached()} lists 
#' all the items in the drake cache.
#' Read/load a cached item with \code{\link{readd}()} or 
#' \code{\link{loadd}()}.
#' @seealso \code{\link{built}}, \code{\link{imported}},
#' \code{\link{readd}}, \code{\link{loadd}},
#' \code{\link{plan}}, \code{\link{run}}
#' @export
#' @return Either a named logical indicating whether the given
#' targets or cached or a character vector listing all cached
#' items, depending on whether any targets are specified
#' @param ... objects to load from the cache, as names (unquoted)
#' or character strings (quoted). Similar to \code{...} in
#' \code{\link{remove}(...)}.
#' @param list character vector naming objects to be loaded from the
#' cache. Similar to the \code{list} argument of \code{\link{remove}()}.
#' @param path Root directory of the drake project, 
#' or if \code{search} is \code{TRUE}, either the 
#' project root or a subdirectory of the project.
#' @param search logical. If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
cached = function(..., list = character(0), 
  path = getwd(), search = FALSE){
  cache = get_cache(path = path, search = search)
  if(is.null(cache)) return(character(0))
  dots = match.call(expand.dots = FALSE)$...
  targets = targets_from_dots(dots, list)
  all_cached = intersect(cache$list(), cache$list(namespace = "depends"))
  if(!length(targets)) return(all_cached)
  presence = targets %in% all_cached
  names(presence) = targets
  presence
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
built = function(path = getwd(), search = FALSE){
  setdiff(cached(path = path, search = search), 
    imported(path = path, search = search))
}

#' @title Function \code{imported}
#' @description List all the imported objects in the drake cache
#' @seealso \code{\link{cached}}, \code{\link{loadd}},
#' \code{\link{built}}
#' @export
#' @return character vector naming the imported objects in the cache
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#' @param search logical. If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
imported = function(path = getwd(), search = FALSE){
  targets = cached(path = path, search = search) 
  select = is_imported(targets, path = path, search = search)
  if(!length(select)) return(character(0))
  targets[select]
}

#' @title Function \code{readd}
#' @description Read a drake target object from the cache.
#' Does not delete the item from the cache.
#' @seealso \code{\link{loadd}}, \code{\link{cached}}, 
#' \code{\link{built}}, \code{link{imported}}, \code{\link{plan}}, 
#' \code{\link{run}}
#' @export
#' @return drake target item from the cache
#' @param target If \code{character_only} is \code{TRUE}, 
#' \code{target} is a character string naming the object to read.
#' Otherwise, \code{target} is an unquoted symbol with the name of the 
#' object. Note: \code{target} could be the name of an imported object.
#' @param character_only logical, whether \code{name} should be treated
#' as a character or a symbol
#' (just like \code{character.only} in \code{\link{library}()}).
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#' @param search logical. If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
#' @param envir environment of imported functions (for lexical scoping)
readd = function(target, character_only = FALSE, path = getwd(), 
  search = FALSE, envir = parent.frame()){
  force(envir)
  cache = get_cache(path = path, search = search)
  if(is.null(cache)) stop("cannot find drake cache.")
  if(!character_only) target = as.character(substitute(target))
  store = cache$get(target)
  value = store$value
  if(store$type == "function"){
    value = eval(parse(text = value), envir = envir)
    environment(value) = envir
  }
  value
}

#' @title Function \code{loadd}
#' @description Load object(s) from the drake cache into the  
#' current workspace (or \code{envir} if given). Defaults
#' to loading the whole cache if arguments \code{...} 
#' and \code{list} are not set 
#' (or all the imported objects if in addition 
#' imported_only is \code{TRUE}).
#' @seealso \code{\link{cached}}, \code{\link{built}}, 
#' \code{\link{imported}}, \code{\link{plan}}, \code{\link{run}},
#' @export
#' @param ... objects to load from the cache, as names (unquoted)
#' or character strings (quoted). Similar to \code{...} in
#' \code{\link{remove}(...)}.
#' @param list character vector naming objects to be loaded from the
#' cache. Similar to the \code{list} argument of \code{\link{remove}()}.
#' @param imported_only logical, whether only imported objects
#' should be loaded.
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#' @param search logical. If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
#' @param envir environment to load objects into. Defaults to the
#' calling environment (current workspace).
loadd = function(..., list = character(0),
  imported_only = FALSE, path = getwd(), 
  search = FALSE, envir = parent.frame()){
  force(envir)
  dots = match.call(expand.dots = FALSE)$...
  targets = targets_from_dots(dots, list)
  if(!length(targets)) targets = cached(path = path, search = search)
  if(imported_only) 
    targets = Filter(targets, 
      f = function(target) is_imported(target, 
        path = path, search = search))
  if(!length(targets)) 
    stop("no targets specified or Either objects not cached or cache not found.")
  lapply(targets, function(target)
    assign(x = target,
      value = readd(target, character_only = TRUE, path = path, 
        search = search, envir = envir), envir = envir))
  invisible()
}

#' @title Function \code{find_cache}
#' @description Return the file path of the nearest drake
#' cache (searching upwards for directories containing a drake cache).
#' @seealso \code{\link{plan}}, \code{\link{run}},
#' @export
#' @return File path of the nearest drake cache or \code{NULL}
#' if no cache is found.
#' @param path starting path for search back for the cache.
#' Should be a subdirectory of the drake project.
find_cache = function(path = getwd()){
  while (!(cachepath %in% list.files(path = path, all.files = TRUE))){
    path = dirname(path)
    if (path == dirname(path)) return(NULL)
  }
  path = file.path(path, cachepath)
  if(!file.exists(path)) return(NULL)
  path
}

#' @title Function \code{find_project}
#' @description Return the file path of the nearest drake
#' project (searching upwards for directories
#' containing a drake cache).
#' @export
#' @seealso \code{\link{plan}}, \code{\link{run}}
#' @return File path of the nearest drake project or \code{NULL}
#' if no drake project is found.
#' @param path starting path for search back for the project.
#' Should be a subdirectory of the drake project.
find_project = function(path = getwd()){
  cache = find_cache(path = path)
  if(is.null(cache)) return()
  dirname(cache)
}

#' @title Function \code{session}
#' @description Load the \code{\link{sessionInfo}()}
#' of the last call to \code{\link{run}()}.
#' @seealso \code{\link{built}}, \code{\link{imported}},
#' \code{\link{readd}}, \code{\link{plan}}, \code{\link{run}}
#' @export
#' @return \code{\link{sessionInfo}()} of the last
#' call to \code{\link{run}()}
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#' @param search If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
session = function(path = getwd(), search = FALSE){
  cache = get_cache(path = path, search = search)
  if(is.null(cache)) stop("No drake::make() session detected.")
  cache$get("sessionInfo", namespace = "session")
}

#' @title Function \code{status}
#' @description Get the build status of the last call to 
#' \code{\link{run}()}. Objects that drake imported, built, or attempted
#' to build are listed as \code{"finished"} or \code{"in progress"}. 
#' Skipped objects are not listed.
#' @seealso \code{\link{session}},
#' \code{\link{built}}, \code{\link{imported}},
#' \code{\link{readd}}, \code{\link{plan}}, \code{\link{run}}
#' @export
#' @return data frame containing the build status
#' of the last session
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#' @param search If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
status = function(path = getwd(), search = FALSE){
  cache = get_cache(path = path, search = search)
  if(is.null(cache)) stop("No drake::make() session detected.")
  target = cache$list(namespace = "status")
  status = sapply(target, cache$get, namespace = "status",
    USE.NAMES = FALSE)
  data.frame(target = target, status = status, stringsAsFactors = FALSE)
}

get_cache = function(path = getwd(), search = FALSE){
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
  if(!(target %in% cached())) return(FALSE)
  get_cache(path = path, search = search)$get(target)$imported
}, "target", SIMPLIFY = TRUE)

uncache = Vectorize(function(target, path = getwd(), search = FALSE){
  cache = get_cache(path = path, search = search)
  for(space in c("objects", "depends", "filemtime"))
    if(target %in% cache$list(namespace = space))
      cache$del(target, namespace = space)
}, "target")

cachepath = ".drake"
