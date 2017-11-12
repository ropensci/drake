#' @title Function \code{readd}
#' @description Read a drake target object from the cache.
#' Does not delete the item from the cache.
#' @seealso \code{\link{loadd}}, \code{\link{cached}},
#' \code{\link{built}}, \code{link{imported}}, \code{\link{workplan}},
#' \code{\link{make}}
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
#' @param cache optional drake cache. See code{\link{new_cache}()}.
#' If \code{cache} is supplied,
#' the \code{path} and \code{search} arguments are ignored.
#' @param verbose whether to print console messages
#' @examples
#' \dontrun{
#' load_basic_example() # Load drake's canonical example.
#' make(my_plan) # Run the project, build the targets.
#' readd(reg1) # Return imported object 'reg1' from the cache.
#' readd(small) # Return targets 'small' from the cache.
#' readd("large", character_only = TRUE) Return target 'large' from the cache.
#' # For external files, only the fingerprint/hash is stored.
#' readd("'report.md'")
#' }
readd <- function(
  target,
  character_only = FALSE,
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = TRUE
  ){
  # if the cache is null after trying get_cache:
  if (is.null(cache)){
    stop("cannot find drake cache.")
  }
  if (!character_only){
    target <- as.character(substitute(target))
  }
  cache$get(target, namespace = "readd")
}

#' @title Function \code{loadd}
#' @description Load object(s) from the drake cache into the
#' current workspace (or \code{envir} if given). Defaults
#' to loading the whole cache if arguments \code{...}
#' and \code{list} are not set
#' (or all the imported objects if in addition
#' imported_only is \code{TRUE}).
#' @seealso \code{\link{cached}}, \code{\link{built}},
#' \code{\link{imported}}, \code{\link{workplan}}, \code{\link{make}},
#' @export
#'
#' @param ... targets to load from the cache, as names (unquoted)
#' or character strings (quoted). Similar to \code{...} in
#' \code{\link{remove}(...)}.
#'
#' @param list character vector naming targets to be loaded from the
#' cache. Similar to the \code{list} argument of \code{\link{remove}()}.
#'
#' @param imported_only logical, whether only imported objects
#' should be loaded.
#'
#' @param cache optional drake cache. See code{\link{new_cache}()}.
#' If \code{cache} is supplied,
#' the \code{path} and \code{search} arguments are ignored.
#'
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#'
#' @param search logical. If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
#'
#' @param envir environment to load objects into. Defaults to the
#' calling environment (current workspace).
#'
#' @param jobs number of parallel jobs for loading objects. On
#' non-Windows systems, the loading process for multiple objects
#' can be lightly parallelized via \code{parallel::mclapply()}.
#' just set jobs to be an integer greater than 1. On Windows,
#' \code{jobs} is automatically demoted to 1.
#'
#' @param verbose whether to print console messages
#'
#' @examples
#' \dontrun{
#' load_basic_example() # Load drake's canonical example.
#' make(my_plan) # Run the projects, build the targets.
#' loadd(small) # Load target 'small' into your workspace.
#' small
#' # For many targets, you can parallelize loadd()
#' # using the 'jobs' argument.
#' loadd(list = c("small", "large"), jobs = 2)
#' loadd(imported_only = TRUE) # Load all the imported objects/functions.
#' # Load everything, including built targets. Be sure your computer
#' # has enough memory.
#' loadd()
#' }
loadd <- function(
  ...,
  list = character(0),
  imported_only = FALSE,
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  envir = parent.frame(),
  jobs = 1,
  verbose = TRUE
){
  if (is.null(cache)){
    stop("cannot find drake cache.")
  }
  force(envir)
  dots <- match.call(expand.dots = FALSE)$...
  targets <- targets_from_dots(dots, list)
  if (!length(targets)){
    targets <- cache$list(namespace = "readd")
  }
  if (imported_only){
    targets <- imported_only(targets = targets, cache = cache)
  }
  if (!length(targets)){
    stop("no targets to load.")
  }
  lightly_parallelize(
    X = targets, FUN = load_target, cache = cache,
    envir = envir, verbose = verbose
  )
  invisible()
}

load_target <- function(target, cache, envir, verbose){
  value <- readd(
    target,
    character_only = TRUE,
    cache = cache,
    verbose = verbose
  )
  assign(x = target, value = value, envir = envir)
}

#' @title Function \code{read_config}
#' @description Read all the configuration parameters
#' from your last attempted call to \code{\link{make}()}.
#' These include the workflow plan
#' @seealso \code{\link{make}}
#' @export
#' @return a named list of configuration items
#' @param cache optional drake cache. See code{\link{new_cache}()}.
#' If \code{cache} is supplied,
#' the \code{path} and \code{search} arguments are ignored.
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#' @param search logical. If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
#' @param verbose whether to print console messages
#' @examples
#' \dontrun{
#' load_basic_example() # Load drake's canonical example.
#' make(my_plan) # Run the project, build the targets.
#' # Retrieve the master internal configuration list from the cache.
#' read_config()
#' }
read_config <- function(path = getwd(), search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = TRUE
){
  if (is.null(cache)) {
    stop("cannot find drake cache.")
  }
  keys <- cache$list(namespace = "config")
  out <- lapply(
    X = keys,
    FUN = function(item){
      cache$get(key = item, namespace = "config")
    }
  )
  names(out) <- keys
  out
}

#' @title Function \code{read_plan}
#' @description Read the workflow plan
#' from your last attempted call to \code{\link{make}()}.
#' @seealso \code{\link{read_config}}
#' @export
#' @return a workflow plan data frame
#' @param cache optional drake cache. See code{\link{new_cache}()}.
#' If \code{cache} is supplied,
#' the \code{path} and \code{search} arguments are ignored.
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#' @param search logical. If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
#' @param verbose whether to print console messages
#' @examples
#' \dontrun{
#' load_basic_example() # Load the canonical example.
#' make(my_plan) # Run the project, build the targets.
#' read_plan() # Retrieve the workflow plan data frame from the cache.
#' }
read_plan <- function(path = getwd(), search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = TRUE
){
  read_config(path = path, search = search, cache = cache)$plan
}

#' @title Function \code{read_graph}
#' @description Read the igraph-style dependency graph of your targets
#' from your last attempted call to \code{\link{make}()}.
#' For better graphing utilities, see \code{\link{plot_graph}()}
#' and related functions.
#' @seealso \code{\link{plot_graph}}, \code{\link{read_config}}
#' @export
#' @return either a plot or an igraph object, depending
#' on \code{plot}
#' @param cache optional drake cache. See code{\link{new_cache}()}.
#' If \code{cache} is supplied,
#' the \code{path} and \code{search} arguments are ignored.
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#' @param search logical. If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
#' @param verbose logical, whether to print console messages
#' @param ... arguments to \code{visNetwork()} via \code{\link{plot_graph}()}
#' @examples
#' \dontrun{
#' load_basic_example() # Load the canonical example.
#' make(my_plan) # Run the project, build the targets.
#' # Retrieve the igraph network from the cache.
#' g <- read_graph()
#' class(g) # "igraph"
#' }
read_graph <- function(path = getwd(), search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = TRUE,
  ...
){
  config <- read_config(path = path, search = search, cache = cache)
  return(config$graph)
}
