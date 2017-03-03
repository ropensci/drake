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
#' @param ... targets to load from the cache, as names (unquoted)
#' or character strings (quoted). Similar to \code{...} in
#' \code{\link{remove}(...)}.
#' @param list character vector naming targets to be loaded from the
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
