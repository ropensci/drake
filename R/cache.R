#' @title Function \code{cached}
#' @description See the cached items stored by drake.
#' Load an item with \code{\link{readd}}.
#' @seealso \code{\link{built}}, \code{\link{imported}},
#' \code{\link{readd}}
#' \code{\link{plan}}, \code{\link{run}}
#' @export
#' @return character vector of cached items
#' @param path Root directory of the drake project, 
#' or if \code{search} is \code{TRUE}, either the 
#' project root or a subdirectory of the project.
#' @param search If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
cached = function(path = getwd(), search = FALSE){
  x = get_cache(path = path, search = search)
  if(is.null(x)) return(character(0))
  x$list()
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
#' @param search If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
built = function(path = getwd(), search = FALSE){
  setdiff(cached(path = path, search = search), 
          imported(path = path, search = search))
}

#' @title Function \code{imported}
#' @description List all the imported objects in the drake cache
#' @seealso \code{\link{cached}}, \code{\link{loadd}},
#' \code{link{built}}
#' @export
#' @return list of imported objects in the cache
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#' @param search If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
imported = function(path = getwd(), search = FALSE){
  x = cached(path = path, search = search) 
  y = sapply(x, is_imported, path = path, search = search)
  if(!length(y)) return(character(0))
  x[y]
}

#' @title Function \code{readd}
#' @description Read a drake target object from the cache.
#' Does not delete the item from the cache.
#' @seealso \code{\link{loadd}}, \code{\link{cached}}, 
#' \code{\link{built}}, \code{link{imported}}, \code{\link{plan}}, 
#' \code{\link{run}}
#' @export
#' @return drake target item from the cache
#' @param x If \code{character_only} is \code{TRUE}, 
#' \code{x} is a character variable storing the name of the item to get 
#' from the cache. Otherwise, \code{x} is a symbol with the literal 
#' object name.
#' @param character_only \code{TRUE}/\code{FALSE}, whether \code{x} 
#' can be assumed to be a character string 
#' (just as in \code{\link{library}()}).
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#' @param search If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
#' @param envir environment of imported functions (for lexical scoping)
readd = function(x, character_only = FALSE, path = getwd(), search = FALSE,
                 envir = parent.frame()){
  force(envir)
  y = get_cache(path = path, search = search)
  if(is.null(y)) stop("cannot find drake cache.")
  if(!character_only) x = as.character(substitute(x))
  out = y$get(x)
  value = out$value
  if(out$type == "function"){
    value = eval(parse(text = value))
    environment(value) = envir
  }
  value
}

#' @title Function \code{loadd}
#' @description Load object(s) from the drake cache into the calling 
#' (or other environment if you set the \code{envir} arg). Defaults
#' to loading the whole cache if arguments \code{...} and \code{list}
#' are not set (or all the imported objects if in addition 
#' imported_only is \code{TRUE}).
#' @seealso \code{\link{cached}}, \code{\link{built}}, 
#' \code{\link{imported}}, \code{\link{plan}}, \code{\link{run}},
#' @export
#' @param ... objects to load from the cache, as names (unquoted)
#' or character strings (quoted). Similar to \code{...} in
#' \code{\link{remove}(...)}.
#' @param list character vector naming objects to be loaded from the
#' cache. Similar to the \code{list} argument of \code{\link{remove}()}.
#' @param imported_only logical, indicates whether only imported objects
#' should be loaded rather than everything in \code{x}
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#' @param search If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
#' @param envir environment to load the cache into. Defaults to the
#' calling environment.
loadd = function(..., list = character(0),
  imported_only = FALSE, path = getwd(), 
  search = FALSE, envir = parent.frame()){
  force(envir)
  dots = match.call(expand.dots = FALSE)$...
  x = parse_dots(dots, list)
  if(!length(x)) x = cached(path = path, search = search)
  if(imported_only) x = Filter(x, 
                               f = function(y) is_imported(y, path = path, search = search))
  if(!length(x)) 
    stop("nothing to load. Either objects not cached or cache not found.")
  lapply(x, function(x)
    assign(x = x,
      value = readd(x, character_only = TRUE, path = path, 
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
  x = find_cache(path = path)
  if(is.null(x)) return()
  dirname(x)
}

#' @title Function \code{session}
#' @description Load the \code{\link{sessionInfo}()}
#' of the last call to \code{\link{run}()}.
#' @seealso \code{\link{built}}, \code{\link{imported}},
#' \code{\link{readd}}, 
#' \code{\link{plan}}, \code{\link{run}}
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
  x = get_cache(path = path, search = search)
  if(is.null(x)) stop("No drake::make() session detected.")
  x$get("session", namespace = "session")
}

#' @title Function \code{status}
#' @description Get the build status of the last call to 
#' \code{\link{run}()}: which objects were skipped, built,
#' and imported.
#' @seealso \code{\link{session}},
#' \code{\link{built}}, \code{\link{imported}},
#' \code{\link{readd}}, 
#' \code{\link{plan}}, \code{\link{run}}
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
  x = get_cache(path = path, search = search)
  if(is.null(x)) stop("No drake::make() session detected.")
  target = x$list(namespace = "status")
  status = sapply(target, x$get, namespace = "status",
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

is_imported = function(x, path = getwd(), search = F){

# NEEDS TO BE CLEANED UP

#  cs = get_cache(path = path, search = search)
#  if(is.null(cs)) return(FALSE)
#  if(!(x %in% cs$list())) return(FALSE)
#  cs$get(x, namespace = "depends")$command == hash_command(as.character(NA))
}

# from base::remove()
parse_dots = function(dots, list){
  if (length(dots) && !all(vapply(dots, function(x) is.symbol(x) ||
                                  is.character(x), NA, USE.NAMES = FALSE)))
    stop("... must contain names or character strings")
  names = vapply(dots, as.character, "")
  if (length(names) == 0L) names = character()
  .Primitive("c")(list, names)
}

uncache_imported = function(cache = get_cache()){
  if(is.null(cache)) return()
  lapply(imported(), function(x){
    cache$del(x)
    cache$del(x, namespace = "depends")
  })
}
