#' @title Function \code{readd}
#' @description Read a drake target object from the cache.
#' Does not delete the item from the cache.
#' @seealso \code{\link{loadd}}, \code{\link{cached}},
#' \code{\link{built}}, \code{link{imported}}, \code{\link{workplan}},
#' \code{\link{make}}
#' @export
#' @return The cached value of the \code{target}.
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
  cache$get(target, namespace = cache$default_namespace)
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
#' @return \code{NULL}
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
#' @param verbose logical, whether to print console messages
#'
#' @param deps logical, whether to load any cached
#' dependencies of the targets
#' instead of the targets themselves.
#' This is useful if you know your
#' target failed and you want to debug the command in an interactive
#' session with the dependencies in your workspace.
#' One caveat: to find the dependencies,
#' \code{\link{loadd}()} uses information that was stored
#' in a \code{\link{drake_config}()} list and cached
#' during the last \code{\link{make}()}.
#' That means you need to have already called \code{\link{make}()}
#' if you set \code{deps} to \code{TRUE}.
#'
#' @param lazy logical, whether to lazy load with
#' \code{delayedAssign()} rather than the more eager
#' \code{assign()}.
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
#' # Load the dependencies of the target, coef_regression2_small
#' loadd(coef_regression2_small, deps = TRUE)
#' # Load all the imported objects/functions.
#' loadd(imported_only = TRUE)
#' # Load everything, including built targets.
#' # Be sure your computer has enough memory.
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
  verbose = 1,
  deps = FALSE,
  lazy = FALSE
){
  if (is.null(cache)){
    stop("cannot find drake cache.")
  }
  force(envir)
  dots <- match.call(expand.dots = FALSE)$...
  targets <- targets_from_dots(dots, list)
  if (!length(targets)){
    targets <- cache$list(namespace = cache$default_namespace)
  }
  if (imported_only){
    plan <- read_drake_plan(cache = cache)
    targets <- imported_only(targets = targets, plan = plan)
  }
  if (!length(targets)){
    stop("no targets to load.")
  }
  if (deps){
    config <- read_drake_config(cache = cache)
    targets <- dependencies(targets = targets, config = config)
    exists <- lightly_parallelize(
      X = targets,
      FUN = cache$exists,
      jobs = jobs
    ) %>%
      unlist
    targets <- targets[exists]
  }
  lightly_parallelize(
    X = targets, FUN = load_target, cache = cache,
    envir = envir, verbose = verbose, lazy = lazy
  )
  invisible()
}

load_target <- function(target, cache, envir, verbose, lazy){
  if (lazy){
    lazy_load_target(
      target = target,
      cache = cache,
      envir = envir,
      verbose = verbose
    )
  } else {
    eager_load_target(
      target = target,
      cache = cache,
      envir = envir,
      verbose = verbose
    )
  }
}

eager_load_target <- function(target, cache, envir, verbose){
  value <- readd(
    target,
    character_only = TRUE,
    cache = cache,
    verbose = verbose
  )
  assign(x = target, value = value, envir = envir)
  local <- environment()
  rm(value, envir = local)
  invisible()
}

lazy_load_target <- function(target, cache, envir, verbose){
  eval_env <- environment()
  delayedAssign(
    x = target,
    value = readd(
      target,
      character_only = TRUE,
      cache = cache,
      verbose = verbose
    ),
    eval.env = eval_env,
    assign.env = envir
  )
}

#' @title Function \code{read_drake_config}
#' @description Read all the configuration parameters
#' from your last attempted call to \code{\link{make}()}.
#' These include the workflow plan
#' @seealso \code{\link{make}}
#' @export
#' @return The cached master internal configuration list
#' of the last \code{\link{make}()}.
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
#' @param jobs number of jobs for light parallelism.
#' Supports 1 job only on Windows.
#' @param envir Optional environment to fill in if
#' \code{config$envir} was not cached. Defaults to your workspace.
#' @examples
#' \dontrun{
#' load_basic_example() # Load drake's canonical example.
#' make(my_plan) # Run the project, build the targets.
#' # Retrieve the master internal configuration list from the cache.
#' read_drake_config()
#' }
read_drake_config <- function(
  path = getwd(),
  search = TRUE,
  cache = NULL,
  verbose = 1,
  jobs = 1,
  envir = parent.frame()
){
  force(envir)
  if (is.null(cache)) {
    cache <- get_cache(path = path, search = search, verbose = verbose)
  }
  if (is.null(cache)) {
    stop("cannot find drake cache.")
  }
  keys <- cache$list(namespace = "config")
  out <- lightly_parallelize(
    X = keys,
    FUN = function(item){
      cache$get(key = item, namespace = "config")
    },
    jobs = jobs
  )
  names(out) <- keys
  if (is.null(out$envir)){
    out$envir <- envir
  }
  out
}

#' @title Function \code{read_drake_plan}
#' @description Read the workflow plan
#' from your last attempted call to \code{\link{make}()}.
#' @seealso \code{\link{read_drake_config}}
#' @export
#' @return A workflow plan data frame.
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
#' read_drake_plan() # Retrieve the workflow plan data frame from the cache.
#' }
read_drake_plan <- function(
  path = getwd(),
  search = TRUE,
  cache = NULL,
  verbose = TRUE
){
  if (is.null(cache)){
    cache <- get_cache(path = path, search = search, verbose = verbose)
  }
  if (is.null(cache)){
    stop("cannot find drake cache.")
  }
  if (cache$exists(key = "plan", namespace = "config")){
    cache$get(key = "plan", namespace = "config")
  } else {
    workplan()
  }
}

#' @title Function \code{read_drake_graph}
#' @description Read the igraph-style dependency graph of your targets
#' from your last attempted call to \code{\link{make}()}.
#' For better graphing utilities, see \code{\link{vis_drake_graph}()}
#' and related functions.
#' @seealso \code{\link{vis_drake_graph}}, \code{\link{read_drake_config}}
#' @export
#' @return An \code{igraph} object representing the dependency
#' network of the workflow.
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
#' @param ... arguments to \code{visNetwork()} via
#' \code{\link{vis_drake_graph}()}
#' @examples
#' \dontrun{
#' load_basic_example() # Load the canonical example.
#' make(my_plan) # Run the project, build the targets.
#' # Retrieve the igraph network from the cache.
#' g <- read_drake_graph()
#' class(g) # "igraph"
#' }
read_drake_graph <- function(
  path = getwd(),
  search = TRUE,
  cache = NULL,
  verbose = 1,
  ...
){
  if (is.null(cache)){
    cache <- get_cache(path = path, search = search, verbose = verbose)
  }
  if (is.null(cache)){
    stop("cannot find drake cache.")
  }
  if (cache$exists(key = "graph", namespace = "config")){
    cache$get(key = "graph", namespace = "config")
  } else {
    make_empty_graph()
  }
}

#' @title Function \code{read_drake_meta}
#' @description For each target, read the information from
#' \code{drake:::meta()} stored from the last
#' \code{\link{make}(..., store_meta = TRUE)}.
#' This metadata was computed right before the target was built,
#' and it was used in \code{drake:::should_build_target})
#' to decide whether to build or skip the target.
#' @seealso \code{\link{make}}
#' @export
#' @return The cached master internal configuration list
#' of the last \code{\link{make}()}.
#' @param targets character vector, names of the targets
#' to get metadata. If \code{NULL}, all metadata is collected.
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
#' @param jobs number of jobs for light parallelism.
#' Supports 1 job only on Windows.
#' @examples
#' \dontrun{
#' load_basic_example() # Load drake's canonical example.
#' make(my_plan) # Run the project, build the targets.
#' # Retrieve the build decision metadata for one target.
#' read_drake_meta(targets = "small")
#' # Retrieve the build decision metadata for all targets,
#' # parallelizing over 2 jobs.
#' read_drake_meta(jobs = 2)
#' }
read_drake_meta <- function(
  targets = NULL,
  path = getwd(),
  search = TRUE,
  cache = NULL,
  verbose = 1,
  jobs = 1
){
  if (is.null(cache)) {
    cache <- get_cache(path = path, search = search, verbose = verbose)
  }
  if (is.null(cache)) {
    stop("cannot find drake cache.")
  }
  if (is.null(targets)){
    targets <- cache$list(namespace = "meta")
  } else {
    targets <- intersect(targets, cache$list(namespace = "meta"))
  }
  out <- lightly_parallelize(
    X = targets,
    FUN = function(target){
      cache$get(key = target, namespace = "meta")
    },
    jobs = jobs
  )
  names(out) <- targets
  out
}
