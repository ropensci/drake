#' @title Read and return a drake target or import from the cache.
#' @description Does not delete the item from the cache.
#' @seealso [loadd()], [cached()],
#'   [built()], \code{link{imported}}, [drake_plan()],
#'   [make()]
#' @export
#' @return The cached value of the `target`.
#' @param target If `character_only` is `TRUE`,
#'   `target` is a character string naming the object to read.
#'   Otherwise, `target` is an unquoted symbol with the name of the
#'   object. Note: `target` could be the name of an imported object.
#' @param character_only logical, whether `name` should be treated
#'   as a character or a symbol
#'   (just like `character.only` in [library()]).
#' @param path Root directory of the drake project,
#'   or if `search` is `TRUE`, either the
#'   project root or a subdirectory of the project.
#' @param search logical. If `TRUE`, search parent directories
#'   to find the nearest drake cache. Otherwise, look in the
#'   current working directory only.
#' @param cache optional drake cache. See code{\link{new_cache}()}.
#'   If `cache` is supplied,
#'   the `path` and `search` arguments are ignored.
#' @param namespace character scalar,
#'   name of an optional storr namespace to read from.
#' @param verbose whether to print console messages
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Get the code with drake_example("basic").
#' make(my_plan) # Run the project, build the targets.
#' readd(reg1) # Return imported object 'reg1' from the cache.
#' readd(small) # Return targets 'small' from the cache.
#' readd("large", character_only = TRUE) # Return 'large' from the cache.
#' # For external files, only the fingerprint/hash is stored.
#' readd("'report.md'")
#' })
#' }
readd <- function(
  target,
  character_only = FALSE,
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  namespace = NULL,
  verbose = TRUE
){
  # if the cache is null after trying get_cache:
  if (is.null(cache)){
    stop("cannot find drake cache.")
  }
  if (!character_only){
    target <- as.character(substitute(target))
  }
  if (is.null(namespace)){
    namespace <- cache$default_namespace
  }
  cache$get(target, namespace = namespace)
}

#' @title Load multiple targets or imports from the drake cache.
#' @description Loads the object(s) into the
#' current workspace (or `envir` if given). Defaults
#' to loading the whole cache if arguments `...`
#' and `list` are not set
#' (or all the imported objects if in addition
#' imported_only is `TRUE`).
#' @seealso [cached()], [built()],
#'   [imported()], [drake_plan()], [make()],
#' @export
#' @return `NULL`
#'
#' @param ... targets to load from the cache, as names (unquoted)
#'   or character strings (quoted). Similar to `...` in
#'   \code{\link{remove}(...)}.
#'
#' @param list character vector naming targets to be loaded from the
#'   cache. Similar to the `list` argument of [remove()].
#'
#' @param imported_only logical, whether only imported objects
#'   should be loaded.
#'
#' @param cache optional drake cache. See code{\link{new_cache}()}.
#'   If `cache` is supplied,
#'   the `path` and `search` arguments are ignored.
#'
#' @param path Root directory of the drake project,
#'   or if `search` is `TRUE`, either the
#'   project root or a subdirectory of the project.
#'
#' @param search logical. If `TRUE`, search parent directories
#'   to find the nearest drake cache. Otherwise, look in the
#'   current working directory only.
#'
#' @param namespace character scalar,
#'   name of an optional storr namespace to load from.
#'
#' @param envir environment to load objects into. Defaults to the
#'   calling environment (current workspace).
#'
#' @param jobs number of parallel jobs for loading objects. On
#'   non-Windows systems, the loading process for multiple objects
#'   can be lightly parallelized via `parallel::mclapply()`.
#'   just set jobs to be an integer greater than 1. On Windows,
#'   `jobs` is automatically demoted to 1.
#'
#' @param verbose logical, whether to print console messages
#'
#' @param deps logical, whether to load any cached
#'   dependencies of the targets
#'   instead of the targets themselves.
#'   This is useful if you know your
#'   target failed and you want to debug the command in an interactive
#'   session with the dependencies in your workspace.
#'   One caveat: to find the dependencies,
#'   [loadd()] uses information that was stored
#'   in a [drake_config()] list and cached
#'   during the last [make()].
#'   That means you need to have already called [make()]
#'   if you set `deps` to `TRUE`.
#'
#' @param lazy either a string or a logical. Choices:
#'   - `"eager"`: no lazy loading. The target is loaded right away
#'     with [assign()].
#'   - `"promise"`: lazy loading with [delayedAssign()]
#'   - `"binding"`: lazy loading with active bindings:
#'     [bindr::populate_env()].
#'   - `TRUE`: same as `"promise"`.
#'   - `FALSE`: same as `"eager"`.
#'
#' @param graph optional igraph object, representation
#'   of the workflow network for getting dependencies
#'   if `deps` is `TRUE`. If none is supplied,
#'   it will be read from the cache.
#'
#' @param replace logical. If `FALSE`,
#'   items already in your enviroment
#'   will not be replaced.
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Get the code with drake_example("basic").
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
#' })
#' }
loadd <- function(
  ...,
  list = character(0),
  imported_only = FALSE,
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  namespace = NULL,
  envir = parent.frame(),
  jobs = 1,
  verbose = 1,
  deps = FALSE,
  lazy = "eager",
  graph = NULL,
  replace = TRUE
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
    targets <- imported_only(targets = targets, plan = plan, jobs = jobs)
  }
  if (!length(targets)){
    stop("no targets to load.")
  }
  if (deps){
    if (is.null(graph)){
      graph <- read_drake_graph(cache = cache)
    }
    targets <- dependencies(targets = targets, config = list(graph = graph))
    exists <- lightly_parallelize(
      X = targets,
      FUN = cache$exists,
      jobs = jobs
    ) %>%
      unlist
    targets <- targets[exists]
  }
  if (!replace){
    targets <- setdiff(targets, ls(envir, all.names = TRUE))
  }
  lightly_parallelize(
    X = targets, FUN = load_target, cache = cache,
    namespace = namespace, envir = envir,
    verbose = verbose, lazy = lazy
  )
  invisible()
}

parse_lazy_arg <- function(lazy){
  if (identical(lazy, FALSE)){
    "eager"
  } else if (identical(lazy, TRUE)){
    "promise"
  } else {
    match.arg(arg = lazy, choices = c("eager", "promise", "binding"))
  }
}

load_target <- function(target, cache, namespace, envir, verbose, lazy){
  lazy <- parse_lazy_arg(lazy)
  switch(
    lazy,
    eager = eager_load_target(
      target = target,
      cache = cache,
      namespace = namespace,
      envir = envir,
      verbose = verbose
    ),
    promise = promise_load_target(
      target = target,
      cache = cache,
      namespace = namespace,
      envir = envir,
      verbose = verbose
    ),
    binding = binding_load_target(
      target = target,
      cache = cache,
      namespace = namespace,
      envir = envir,
      verbose = verbose
    )
  )
}

#' @title Load a target right away (internal function)
#' @description This function is only exported
#' to make active bindings work safely.
#' It is not actually a user-side function.
#' @keywords internal
#' @export
#' @inheritParams loadd
eager_load_target <- function(target, cache, namespace, envir, verbose){
  value <- readd(
    target,
    character_only = TRUE,
    cache = cache,
    namespace = namespace,
    verbose = verbose
  )
  assign(x = target, value = value, envir = envir)
  local <- environment()
  rm(value, envir = local)
  invisible()
}

promise_load_target <- function(target, cache, namespace, envir, verbose){
  eval_env <- environment()
  delayedAssign(
    x = target,
    value = readd(
      target,
      character_only = TRUE,
      cache = cache,
      namespace = namespace,
      verbose = verbose
    ),
    eval.env = eval_env,
    assign.env = envir
  )
}

binding_load_target <- function(target, cache, namespace, envir, verbose){
  # Allow active bindings to overwrite existing variables.
  if (target %in% ls(envir)){
    message(
      "Replacing already-loaded variable ", target,
      " with an active binding."
    )
    remove(list = target, envir = envir)
  }
  bindr::populate_env(
    env = envir,
    names = as.character(target),
    fun = function(key, cache, namespace){
      cache$get(key = as.character(key), namespace = as.character(namespace))
    },
    cache = cache,
    namespace = as.character(namespace)
  )
}

#' @title Read the cached [drake_config()]
#'   list from the last [make()].
#' @description See [drake_config()] for more information
#' about drake's internal runtime configuration parameter list.
#' @seealso [make()]
#' @export
#' @return The cached master internal configuration list
#'   of the last [make()].
#' @param cache optional drake cache. See code{\link{new_cache}()}.
#'   If `cache` is supplied,
#'   the `path` and `search` arguments are ignored.
#' @param path Root directory of the drake project,
#'   or if `search` is `TRUE`, either the
#'   project root or a subdirectory of the project.
#' @param search logical. If `TRUE`, search parent directories
#'   to find the nearest drake cache. Otherwise, look in the
#'   current working directory only.
#' @param verbose whether to print console messages
#' @param jobs number of jobs for light parallelism.
#'   Supports 1 job only on Windows.
#' @param envir Optional environment to fill in if
#'   `config$envir` was not cached. Defaults to your workspace.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Get the code with drake_example("basic").
#' make(my_plan) # Run the project, build the targets.
#' # Retrieve the master internal configuration list from the cache.
#' read_drake_config()
#' })
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
  # The file system of the original config$cache could have moved.
  out$cache <- cache
  cache_path <- force_cache_path(cache)
  out
}

#' @title Read the igraph dependency network
#'   from your last attempted call to [make()].
#' @description For more user-friendly graphing utilities,
#' see [vis_drake_graph()]
#' and related functions.
#' @seealso [vis_drake_graph()], [read_drake_config()]
#' @export
#' @return An `igraph` object representing the dependency
#'   network of the workflow.
#' @param cache optional drake cache. See code{\link{new_cache}()}.
#'   If `cache` is supplied,
#'   the `path` and `search` arguments are ignored.
#' @param path Root directory of the drake project,
#'   or if `search` is `TRUE`, either the
#'   project root or a subdirectory of the project.
#' @param search logical. If `TRUE`, search parent directories
#'   to find the nearest drake cache. Otherwise, look in the
#'   current working directory only.
#' @param verbose logical, whether to print console messages
#' @param ... arguments to [visNetwork()] via
#'   [vis_drake_graph()]
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Get the code with drake_example("basic").
#' make(my_plan) # Run the project, build the targets.
#' # Retrieve the igraph network from the cache.
#' g <- read_drake_graph()
#' class(g) # "igraph"
#' })
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

#' @title Read the metadata of a target or import.
#' @description The metadata helps determine if the
#' target is up to date or outdated. The metadata of imports
#' is used to compute the metadata of targets.
#' @details Target metadata is computed
#' with [drake_meta()] and then
#' `drake:::finish_meta()`.
#' This metadata corresponds
#' to the state of the target immediately after it was built
#' or imported in the last [make()] that
#' did not skip it.
#' The exception to this is the `$missing` element
#' of the metadata, which indicates if the target/import
#' was missing just *before* it was built.
#' @seealso [dependency_profile()], [make()]
#' @export
#' @return The cached master internal configuration list
#'   of the last [make()].
#' @param targets character vector, names of the targets
#'   to get metadata. If `NULL`, all metadata is collected.
#' @param cache optional drake cache. See code{\link{new_cache}()}.
#'   If `cache` is supplied,
#'   the `path` and `search` arguments are ignored.
#' @param path Root directory of the drake project,
#'   or if `search` is `TRUE`, either the
#'   project root or a subdirectory of the project.
#' @param search logical. If `TRUE`, search parent directories
#'   to find the nearest drake cache. Otherwise, look in the
#'   current working directory only.
#' @param verbose whether to print console messages
#' @param jobs number of jobs for light parallelism.
#'   Supports 1 job only on Windows.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Get the code with drake_example("basic").
#' make(my_plan) # Run the project, build the targets.
#' # Retrieve the build decision metadata for one target.
#' read_drake_meta(targets = "small")
#' # Retrieve the build decision metadata for all targets,
#' # parallelizing over 2 jobs.
#' read_drake_meta(jobs = 2)
#' })
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
    targets <- parallel_filter(
      x = targets,
      f = function(target){
        cache$exists(key = target, namespace = "meta")
      },
      jobs = jobs
    )
  }
  out <- lightly_parallelize(
    X = targets,
    FUN = function(target){
      cache$get(key = target, namespace = "meta")
    },
    jobs = jobs
  )
  names(out) <- targets
  if (length(out) == 1){
    out <- out[[1]]
  }
  out
}

#' @title Read the workflow plan
#'   from your last attempted call to [make()].
#' @description Uses the cache.
#' @seealso [read_drake_config()]
#' @export
#' @return A workflow plan data frame.
#' @param cache optional drake cache. See code{\link{new_cache}()}.
#'   If `cache` is supplied,
#'   the `path` and `search` arguments are ignored.
#' @param path Root directory of the drake project,
#'   or if `search` is `TRUE`, either the
#'   project root or a subdirectory of the project.
#' @param search logical. If `TRUE`, search parent directories
#'   to find the nearest drake cache. Otherwise, look in the
#'   current working directory only.
#' @param verbose whether to print console messages
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Get the code with drake_example("basic").
#' make(my_plan) # Run the project, build the targets.
#' read_drake_plan() # Retrieve the workflow plan data frame from the cache.
#' })
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
    drake_plan()
  }
}

#' @title Read the pseudo-random number generator seed of the project.
#' @description When a project is created with [make()]
#' or [drake_config()], the project's pseudo-random number generator
#' seed is cached. Then, unless the cache is destroyed,
#' the seeds of all the targets will deterministically depend on
#' this one central seed. That way, reproducibility is protected,
#' even under randomness.
#' @seealso [read_drake_config()]
#' @export
#' @return A workflow plan data frame.
#' @param cache optional drake cache. See code{\link{new_cache}()}.
#'   If `cache` is supplied,
#'   the `path` and `search` arguments are ignored.
#' @param path Root directory of the drake project,
#'   or if `search` is `TRUE`, either the
#'   project root or a subdirectory of the project.
#' @param search logical. If `TRUE`, search parent directories
#'   to find the nearest drake cache. Otherwise, look in the
#'   current working directory only.
#' @param verbose whether to print console messages
#' @examples
#' cache <- storr::storr_environment() # Just for the examples.
#' my_plan <- drake_plan(
#'   target1 = sqrt(1234),
#'   target2 = rnorm(n = 1, mean = target1)
#' )
#' tmp <- runif(1) # Needed to get a .Random.seed, but not for drake.
#' digest::digest(.Random.seed) # Fingerprint of the current R session's seed.
#' make(my_plan, cache = cache) # Run the project, build the targets.
#' digest::digest(.Random.seed) # Your session's seed did not change.
#' # Drake uses a hard-coded seed if you do not supply one.
#' read_drake_seed(cache = cache)
#' readd(target2, cache = cache) # Randomly-generated target data.
#' clean(target2, cache = cache) # Oops, I removed the data!
#' tmp <- runif(1) # Maybe the R session's seed also changed.
#' make(my_plan, cache = cache) # Rebuild target2.
#' # Same as before:
#' read_drake_seed(cache = cache)
#' readd(target2, cache = cache)
#' # You can also supply a seed.
#' # If your project already exists, it must agree with the project's
#' # preexisting seed (default: 0)
#' clean(target2, cache = cache)
#' make(my_plan, cache = cache, seed = 0)
#' read_drake_seed(cache = cache)
#' readd(target2, cache = cache)
#' # If you want to supply a different seed than 0,
#' # you need to destroy the cache and start over first.
#' clean(destroy = TRUE, cache = cache)
#' cache <- storr::storr_environment() # Just for the examples.
#' make(my_plan, cache = cache, seed = 1234)
#' read_drake_seed(cache = cache)
#' readd(target2, cache = cache)
read_drake_seed <- function(
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
  if (cache$exists(key = "seed", namespace = "config")){
    cache$get(key = "seed", namespace = "config")
  } else {
    stop("Pseudo-random seed not found in the cache.")
  }
}
