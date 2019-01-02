#' @title Read and return a drake target/import from the cache.
#' @description [readd()] returns an object from the cache,
#' and [loadd()] loads one or more objects from the cache
#' into your environment or session. These objects are usually
#' targets built by [make()].
#' @details There are two uses for the
#' [loadd()] and [readd()] functions:
#' 1. Exploring the results outside the `drake`/`make()` pipeline.
#'   When you call [make()] to run your project,
#'   `drake` puts the targets in a cache, usually a folder called `.drake`.
#'   You may want to inspect the targets afterwards, possibly in an
#'   interactive R session. However, the files in the `.drake` folder
#'   are organized in a special format created by the
#'   [`storr`](https://github.com/richfitz/storr) package,
#'   which is not exactly human-readable.
#'   To retrieve a target for manual viewing, use [readd()].
#'   To load one or more targets into your session, use [loadd()].
#' 2. In `knitr` / R Markdown reports.
#'   You can borrow `drake` targets in your active code chunks
#'   if you have the right calls to [loadd()] and [readd()].
#'   These reports can either run outside the `drake` pipeline,
#'   or better yet, as part of the pipeline itself.
#'   If you call `knitr_in("your_report.Rmd")` inside a [drake_plan()]
#'   command, then [make()] will scan `"your_report.Rmd"` for
#'   calls to `loadd()` and `readd()` in active code chunks,
#'   and then treat those loaded targets as dependencies.
#'   That way, [make()] will automatically (re)run the report if those
#'   dependencies change.
#' @note Please do not put calls to [loadd()] or [readd()] inside
#' your custom (imported) functions or the commands in your [drake_plan()].
#' This creates confusion inside [make()], which has its own ways of
#' interacting with the cache.
#' @seealso [cached()], [built()], [imported()], [drake_plan()], [make()]
#' @export
#' @return The cached value of the `target`.
#' @inheritParams cached
#' @param target If `character_only` is `TRUE`, then
#'   `target` is a character string naming the object to read.
#'   Otherwise, `target` is an unquoted symbol with the name of the
#'   object.
#' @param character_only logical, whether `name` should be treated
#'   as a character or a symbol
#'   (just like `character.only` in [library()]).
#' @param namespace optional character string,
#'   name of the `storr` namespace to read from.
#' @param show_source logical, option to show the command
#'   that produced the target or indicate that the object
#'   was imported (using [show_source()]).
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' readd(reg1) # Return imported object 'reg1' from the cache.
#' readd(small) # Return targets 'small' from the cache.
#' readd("large", character_only = TRUE) # Return 'large' from the cache.
#' # For external files, only the fingerprint/hash is stored.
#' readd(file_store("report.md"), character_only = TRUE)
#' })
#' }
readd <- function(
  target,
  character_only = FALSE,
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  namespace = NULL,
  verbose = drake::default_verbose(),
  show_source = FALSE
) {
  # if the cache is null after trying get_cache:
  if (is.null(cache)) {
    stop("cannot find drake cache.")
  }
  if (!character_only) {
    target <- as.character(substitute(target))
  }
  if (is.null(namespace)) {
    namespace <- cache$default_namespace
  }
  if (show_source) {
    show_source(
      target = target,
      config = list(cache = cache),
      character_only = TRUE
    )
  }
  cache$get(
    standardize_key(target),
    namespace = namespace,
    use_cache = TRUE
  )
}

#' @title Load one or more targets or imports from the drake cache.
#' @rdname readd
#' @seealso [cached()], [built()], [imported()], [drake_plan()], [make()]
#' @export
#'
#' @inheritParams cached
#' @inheritParams readd
#'
#' @param ... targets to load from the cache: as names (symbols) or
#'   character strings. If the `tidyselect` package is installed,
#'   you can also supply `dplyr`-style `tidyselect`
#'   commands such as `starts_with()`, `ends_with()`, and `one_of()`.
#'
#' @param list character vector naming targets to be loaded from the
#'   cache. Similar to the `list` argument of [remove()].
#'
#' @param imported_only logical, whether only imported objects
#'   should be loaded.
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
#'   - `"bind"`: lazy loading with active bindings:
#'     `bindr::populate_env()`.
#'   - `TRUE`: same as `"promise"`.
#'   - `FALSE`: same as `"eager"`.
#'
#' @param graph optional igraph object, representation
#'   of the workflow network for getting dependencies
#'   if `deps` is `TRUE`. If none is supplied,
#'   it will be read from the cache.
#'
#' @param replace logical. If `FALSE`,
#'   items already in your environment
#'   will not be replaced.
#'  
#' @param tidyselect logical, whether to enable
#'   `tidyselect` expressions in `...` like
#'   `starts_with("prefix")` and `ends_with("suffix")`.
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the projects, build the targets.
#' loadd(small) # Load target 'small' into your workspace.
#' small
#' # For many targets, you can parallelize loadd()
#' # using the 'jobs' argument.
#' loadd(list = c("small", "large"), jobs = 2)
#' ls()
#' # Load the dependencies of the target, coef_regression2_small
#' loadd(coef_regression2_small, deps = TRUE)
#' ls()
#' # Load all the imported objects/functions.
#' loadd(imported_only = TRUE)
#' ls()
#' # Load all the targets listed in the workflow plan
#' # of the previous `make()`.
#' # Be sure your computer has enough memory.
#' loadd()
#' ls()
#' # With files, you just get the fingerprint.
#' loadd(list = file_store("report.md"))
#' ls() # Should include "\"report.md\"".
#' get(file_store("report.md"))
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
  verbose = drake::default_verbose(),
  deps = FALSE,
  lazy = "eager",
  graph = NULL,
  replace = TRUE,
  show_source = FALSE,
  tidyselect = TRUE
) {
  force(envir)
  lazy <- parse_lazy_arg(lazy)
  if (is.null(cache)) {
    stop("cannot find drake cache.")
  }
  if (is.null(namespace)) {
    namespace <- cache$default_namespace
  }
  targets <- c(as.character(match.call(expand.dots = FALSE)$...), list)
  if (tidyselect) {
    if (exists_tidyselect()) {
      targets <- drake_tidyselect(
        cache = cache,
        ...,
        namespaces = namespace,
        list = list
      )
    }
  }
  if (!length(targets) && !length(list(...))) {
    targets <- cache$list()
  }
  if (imported_only) {
    targets <- imported_only(targets = targets, cache = cache, jobs = jobs)
  }
  if (!length(targets)) {
    stop("no targets to load.")
  }
  if (deps) {
    if (is.null(graph)) {
      graph <- read_drake_graph(cache = cache)
    }
    targets <- dependencies(targets = targets, config = list(graph = graph))
    exists <- lightly_parallelize(
      X = targets,
      FUN = cache$exists,
      jobs = jobs
    )
    exists <- unlist(exists)
    targets <- targets[exists]
  }
  if (!replace) {
    targets <- setdiff(targets, ls(envir, all.names = TRUE))
  }
  if (show_source) {
    lapply(
      X = targets,
      FUN = show_source,
      config = list(cache = cache),
      character_only = TRUE
    )
  }
  lightly_parallelize(
    X = targets, FUN = load_target, cache = cache,
    namespace = namespace, envir = envir,
    verbose = verbose, lazy = lazy
  )
  invisible()
}

parse_lazy_arg <- function(lazy) {
  if (identical(lazy, FALSE)) {
    "eager"
  } else if (identical(lazy, TRUE)) {
    "promise"
  } else {
    match.arg(arg = lazy, choices = c("eager", "promise", "bind"))
  }
}

load_target <- function(target, cache, namespace, envir, verbose, lazy) {
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
    bind = bind_load_target(
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
eager_load_target <- function(target, cache, namespace, envir, verbose) {
  value <- cache$get(key = target, namespace = namespace)
  assign(x = target, value = value, envir = envir)
  local <- environment()
  rm(value, envir = local)
  invisible()
}

promise_load_target <- function(target, cache, namespace, envir, verbose) {
  eval_env <- environment()
  delayedAssign(
    x = target,
    value = cache$get(key = target, namespace = namespace),
    eval.env = eval_env,
    assign.env = envir
  )
}

bind_load_target <- function(target, cache, namespace, envir, verbose) {
  assert_pkg("bindr")
  # Allow active bindings to overwrite existing variables.
  if (exists(x = target, envir = envir, inherits = FALSE)) {
    message(
      "Replacing already-loaded variable ", target,
      " with an active binding."
    )
    remove(list = target, envir = envir)
  }
  bindr::populate_env(
    env = envir,
    names = as.character(target),
    fun = function(key, cache, namespace) {
      if (!length(namespace)) {
        # Now impractical to cover because loadd() checks the namespace,
        # but good to have around anyway.
        namespace <- cache$default_namespace # nocov
      }
      cache$get(
        key = as.character(key),
        namespace = as.character(namespace),
        use_cache = TRUE
      )
    },
    cache = cache,
    namespace = namespace
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
#'
#' @inheritParams cached
#'
#' @param jobs number of jobs for light parallelism.
#'   Supports 1 job only on Windows.
#'
#' @param envir Optional environment to fill in if
#'   `config$envir` was not cached. Defaults to your workspace.
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' # Retrieve the master internal configuration list from the cache.
#' read_drake_config()
#' })
#' }
read_drake_config <- function(
  path = getwd(),
  search = TRUE,
  cache = NULL,
  verbose = drake::default_verbose(),
  jobs = 1,
  envir = parent.frame()
) {
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
    FUN = function(item) {
      cache$get(key = item, namespace = "config", use_cache = FALSE)
    },
    jobs = jobs
  )
  names(out) <- keys
  if (is.null(out$envir)) {
    out$envir <- envir
  }
  # The file system of the original config$cache could have moved.
  out$cache <- cache
  cache_path <- force_cache_path(cache)
  out
}

#' @title Read the igraph dependency network
#'   from your last attempted call to [make()].
#' @description To build the targets, [make()]
#'   advances along the graph from leaves to roots.
#' @export
#' @return An `igraph` object representing the dependency
#'   network of the workflow.
#' @inheritParams cached
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
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
  verbose = drake::default_verbose()
) {
  if (is.null(cache)) {
    cache <- get_cache(path = path, search = search, verbose = verbose)
  }
  if (is.null(cache)) {
    stop("cannot find drake cache.")
  }
  if (cache$exists(key = "graph", namespace = "config")) {
    cache$get(key = "graph", namespace = "config", use_cache = FALSE)
  } else {
    make_empty_graph()
  }
}

#' @title Read the workflow plan
#'   from your last attempted call to [make()].
#' @description Uses the cache.
#' @seealso [read_drake_config()]
#' @export
#' @return A workflow plan data frame.
#'
#' @inheritParams cached
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' read_drake_plan() # Retrieve the workflow plan data frame from the cache.
#' })
#' }
read_drake_plan <- function(
  path = getwd(),
  search = TRUE,
  cache = NULL,
  verbose = drake::default_verbose()
) {
  if (is.null(cache)) {
    cache <- get_cache(path = path, search = search, verbose = verbose)
  }
  if (is.null(cache)) {
    stop("cannot find drake cache.")
  }
  if (cache$exists(key = "plan", namespace = "config")) {
    cache$get(key = "plan", namespace = "config", use_cache = FALSE)
  } else {
    drake_plan()
  }
}

read_drake_layout <- function(cache){
  if (cache$exists(key = "layout", namespace = "config")) {
    cache$get(
      key = "layout",
      namespace = "config",
      use_cache = FALSE
    )
  } else {
    list()
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
#' @return An integer vector.
#'
#' @inheritParams cached
#'
#' @examples
#' cache <- storr::storr_environment() # Just for the examples.
#' my_plan <- drake_plan(
#'   target1 = sqrt(1234),
#'   target2 = sample.int(n = 12, size = 1) + target1
#' )
#' tmp <- sample.int(1) # Needed to get a .Random.seed, but not for drake.
#' digest::digest(.Random.seed) # Fingerprint of the current R session's seed.
#' make(my_plan, cache = cache) # Run the project, build the targets.
#' digest::digest(.Random.seed) # Your session's seed did not change.
#' # drake uses a hard-coded seed if you do not supply one.
#' read_drake_seed(cache = cache)
#' readd(target2, cache = cache) # Randomly-generated target data.
#' clean(target2, cache = cache) # Oops, I removed the data!
#' tmp <- sample.int(1) # Maybe the R session's seed also changed.
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
  verbose = drake::default_verbose()
) {
  if (is.null(cache)) {
    cache <- get_cache(path = path, search = search, verbose = verbose)
  }
  if (is.null(cache)) {
    stop("cannot find drake cache.")
  }
  if (cache$exists(key = "seed", namespace = "config")) {
    cache$get(key = "seed", namespace = "config")
  } else {
    stop("Pseudo-random seed not found in the cache.")
  }
}
