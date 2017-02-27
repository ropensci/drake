#' @title Function \code{cached}
#' @description List the names of the objects in the cache.
#' Load an item with \code{\link{readd}}.
#' @seealso \code{\link{built}}, \code{\link{imported}},
#' \code{\link{readd}}, 
#' \code{\link{plan}}, \code{\link{run}}
#' @export
#' @return character vector of cached items
#' @param root Root directory of your project where the hidden
#' cache is stored.
cached = function(root = getwd()){
  x = get_cache(path = path, search = search)
  if(is.null(x)) return(character(0))
  x$list()
}

#' @title Function \code{finished}
#' @description List all finished targets
#' @seealso \code{\link{cached}}, \code{\link{loadd}},
#' @export
#' @return list of imported objects in the cache
#' @param root Root directory of your project where the hidden
#' cache is stored.
finished = function(root = getwd()){
  setdiff(cached(path = path, search = search), 
    imported(path = path, search = search))
}

#' @title Function \code{readd}
#' @description Read an object from the cache.
#' Does not delete the item from the cache.
#' @seealso \code{\link{loadd}}, \code{\link{cached}}, 
#' \code{\link{run}}
#' @export
#' @return object from the cache
#' @param x If \code{character_only} is \code{TRUE}, 
#' \code{x} is a character variable storing the name of the item to get 
#' from the cache. Otherwise, \code{x} is a symbol with the literal 
#' object name.
#' @param character_only \code{TRUE}/\code{FALSE}, whether \code{x} 
#' can be assumed to be a character string 
#' (just as in \code{\link{library}()}).
#' @param root Root directory of your project where
#' the hidden cache is located.
#' @param envir environment of imported functions (for lexical scoping)
readd = function(x, character_only = FALSE, root = getwd(), 
  envir = parent.frame()){
  force(envir)
  y = get_cache(root)
  if(is.null(y)) stop("cannot find cache.")
  if(!character_only) x = as.character(substitute(x))
  out = y$get(x)
  if(y$get(x, namespace = "depends")$type == "function"){
    out = eval(parse(text = out$value))
    environment(out) = envir
  }
  out
}

#' @title Function \code{loadd}
#' @description Load object(s) from the cache into the calling 
#' (or other environment if you set the \code{envir} arg). Defaults
#' to loading the whole cache if arguments \code{...} and \code{list}
#' are not set (or all the imported objects if in addition 
#' imported_only is \code{TRUE}).
#' @seealso \code{\link{cached}}, \code{\link{built}}, 
#' \code{\link{imported}}, \code{\link{plan}}, \code{\link{make}},
#' @export
#' @param ... objects to load from the cache, as names (unquoted)
#' or character strings (quoted). Similar to \code{...} in
#' \code{\link{remove}(...)}.
#' @param list character vector naming objects to be loaded from the
#' cache. Similar to the \code{list} argument of \code{\link{remove}()}.
#' @param root Root directory of your project where
#' the hidden cache is located.
#' @param envir environment to load the cache into. Defaults to the
#' calling environment.
loadd = function(..., list = character(0), root = getwd(), 
  envir = parent.frame()){
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
        search = search, envir = envir),
      envir = envir))
  invisible()
}

#' @title Function \code{session}
#' @description Load the \code{\link{sessionInfo}()}
#' of the last call to \code{\link{make}()}.
#' @seealso \code{\link{built}}, \code{\link{imported}},
#' \code{\link{readd}}, 
#' \code{\link{plan}}, \code{\link{make}}
#' @export
#' @return \code{\link{sessionInfo}()} of the last
#' call to \code{\link{run}()}
#' @param root Root directory of your project where the hidden
#' cache is stored.
session = function(root = getwd()){
  x = get_cache(root = root)
  if(is.null(x)) stop("No run() session detected.")
  x$get("session", namespace = "session")
}

#' @title Function \code{status}
#' @description Get the build status of the last call to 
#' \code{\link{make}()}: which objects were skipped, built,
#' and imported.
#' @seealso \code{\link{session}},
#' \code{\link{built}}, \code{\link{imported}},
#' \code{\link{readd}}, 
#' \code{\link{plan}}, \code{\link{make}}
#' @export
#' @return data frame containing the build status
#' of the last session
#' @param root Root directory of your project where the hidden
#' cache is stored
status = function(root = getwd()){
  x = get_cache(path = path)
  if(is.null(x)) stop("No run() session detected.")
  output = x$list(namespace = "status")
  status = sapply(output, x$get, namespace = "status",
    USE.NAMES = FALSE)
  data.frame(output = output, status = status, stringsAsFactors = FALSE)
}

get_cache = function(root = getwd()){
  path = file.path(root, cachepath)
  if(!file.exists(path)) return(NULL)
  storr_rds(path, mangle_key = TRUE)
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
