#' @title Read and return a drake target/import from the cache.
#' `r lifecycle::badge("stable")`
#' @description [readd()] returns an object from the cache,
#' and [loadd()] loads one or more objects from the cache
#' into your environment or session. These objects are usually
#' targets built by [make()]. If `target` is dynamic,
#' [readd()] and [loadd()] retrieve a list of sub-target values.
#' You can restrict which sub-targets to include using the `subtargets`
#' argument.
#' @details There are three uses for the
#' [loadd()] and [readd()] functions:
#' 1. Exploring the results outside the `drake`/`make()` pipeline.
#'   When you call [make()] to run your project,
#'   `drake` puts the targets in a cache, usually a folder called `.drake`.
#'   You may want to inspect the targets afterwards, possibly in an
#'   interactive R session. However, the files in the `.drake` folder
#'   are organized in a special format created by the
#'   `storr` package,
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
#' 3. If you are using `make(memory_strategy = "none")`
#'   or `make(memory_strategy = "unload")`,
#'   [loadd()] and [readd()] can manually load dependencies
#'   into memory for the target that is being built.
#'   If you do this, you must carefully inspect [deps_target()]
#'   and [vis_drake_graph()] before running [make()]
#'   to be sure the dependency relationships among targets
#'   are correct. If you do not wish to incur extra dependencies
#'   with [loadd()] or [readd()], you will need to use [ignore()],
#'   e.g. `drake_plan(x = 1, y = ignore(readd(x)))` or
#'   `drake_plan(x = 1, y = readd(ignore("x"), character_only = TRUE))`.
#'   Compare those plans to `drake_plan(x = 1, y = readd(x))`
#'   and `drake_plan(x = 1, y = readd("x", character_only = TRUE))`
#'   using [vis_drake_graph()] and [deps_target()].
#' @seealso [cached()], [drake_plan()], [make()]
#' @export
#' @return The cached value of the `target`.
#' @inheritParams cached
#' @param target If `character_only` is `TRUE`, then
#'   `target` is a character string naming the object to read.
#'   Otherwise, `target` is an unquoted symbol with the name of the
#'   object.
#' @param character_only Logical, whether `name` should be treated
#'   as a character or a symbol
#'   (just like `character.only` in [library()]).
#' @param namespace Optional character string,
#'   name of the `storr` namespace to read from.
#' @param show_source Logical, option to show the command
#'   that produced the target or indicate that the object
#'   was imported (using [show_source()]).
#' @param subtargets A numeric vector of indices.
#'   If `target` is dynamic, [loadd()] and [readd()] retrieve
#'   a list of sub-targets. You can restrict which sub-targets
#'   to retrieve with the `subtargets` argument. For example,
#'   `readd(x, subtargets = seq_len(3))` only retrieves the
#'   first 3 sub-targets of dynamic target `x`.
#' @param subtarget_list Logical, for dynamic targets only.
#'   If `TRUE`, the dynamic target is loaded as a named
#'   list of sub-target values. If `FALSE`, `drake`
#'   attempts to concatenate the sub-targets with `vctrs::vec_c()`
#'   (and returns an unnamed list if such concatenation is not possible).
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' readd(reg1) # Return imported object 'reg1' from the cache.
#' readd(small) # Return targets 'small' from the cache.
#' readd("large", character_only = TRUE) # Return 'large' from the cache.
#' # For external files, only the fingerprint/hash is stored.
#' readd(file_store("report.md"), character_only = TRUE)
#' }
#' })
#' }
readd <- function(
  target,
  character_only = FALSE,
  path = NULL,
  search = NULL,
  cache = drake::drake_cache(path = path),
  namespace = NULL,
  verbose = 1L,
  show_source = FALSE,
  subtargets = NULL,
  subtarget_list = FALSE
) {
  deprecate_search(search)
  # if the cache is null after trying drake_cache:
  if (is.null(cache)) {
    stop("cannot find drake cache.")
  }
  cache <- decorate_storr(cache)
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
  value <- cache$get(
    standardize_key(target),
    namespace = namespace,
    use_cache = FALSE
  )
  get_subtargets(value, target, cache, subtargets, subtarget_list)
}

#' @title Load one or more targets or imports from the drake cache.
#' `r lifecycle::badge("stable")`
#' @rdname readd
#' @seealso [cached()], [drake_plan()], [make()]
#' @export
#'
#' @inheritParams cached
#' @inheritParams readd
#'
#' @param ... Targets to load from the cache: as names (symbols) or
#'   character strings. If the `tidyselect` package is installed,
#'   you can also supply `dplyr`-style `tidyselect`
#'   commands such as `starts_with()`, `ends_with()`, and `one_of()`.
#'
#' @param list Character vector naming targets to be loaded from the
#'   cache. Similar to the `list` argument of [remove()].
#'
#' @param imported_only Logical, deprecated.
#'
#' @param envir Environment to load objects into. Defaults to the
#'   calling environment (current workspace).
#'
#' @param jobs Number of parallel jobs for loading objects. On
#'   non-Windows systems, the loading process for multiple objects
#'   can be lightly parallelized via `parallel::mclapply()`.
#'   just set jobs to be an integer greater than 1. On Windows,
#'   `jobs` is automatically demoted to 1.
#'
#' @param deps Logical, whether to load any cached
#'   dependencies of the targets
#'   instead of the targets themselves.
#'
#'   Important note:
#'   `deps = TRUE` disables `tidyselect` functionality. For example,
#'   `loadd(starts_with("model_"), config = config, deps = TRUE)`
#'   does not work. For the selection mechanism to work,
#'   the `model_*` targets to need to already be in the cache,
#'   which is not always the case when you are debugging your projects.
#'   To help `drake` understand what you mean,
#'   you must name the targets *explicitly* when `deps` is `TRUE`, e.g.
#'   `loadd(model_A, model_B, config = config, deps = TRUE)`.
#'
#' @param lazy Either a string or a logical. Choices:
#'   - `"eager"`: no lazy loading. The target is loaded right away
#'     with [assign()].
#'   - `"promise"`: lazy loading with [delayedAssign()]
#'   - `"bind"`: lazy loading with active bindings:
#'     `bindr::populate_env()`.
#'   - `TRUE`: same as `"promise"`.
#'   - `FALSE`: same as `"eager"`.
#'
#' @param graph Deprecated.
#'
#' @param replace Logical. If `FALSE`,
#'   items already in your environment
#'   will not be replaced.
#'
#' @param tidyselect Logical, whether to enable
#'   `tidyselect` expressions in `...` like
#'   `starts_with("prefix")` and `ends_with("suffix")`.
#'
#' @param config Optional [drake_config()] object.
#'   You should supply one if `deps` is `TRUE`.
#'
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the projects, build the targets.
#' config <- drake_config(my_plan)
#' loadd(small) # Load target 'small' into your workspace.
#' small
#' # For many targets, you can parallelize loadd()
#' # using the 'jobs' argument.
#' loadd(list = c("small", "large"), jobs = 2)
#' ls()
#' # Load the dependencies of the target, coef_regression2_small
#' loadd(coef_regression2_small, deps = TRUE, config = config)
#' ls()
#' # Load all the targets listed in the workflow plan
#' # of the previous `make()`.
#' # If you do not supply any target names, `loadd()` loads all the targets.
#' # Be sure your computer has enough memory.
#' loadd()
#' ls()
#' }
#' })
#' }
loadd <- function(
  ...,
  list = character(0),
  imported_only = NULL,
  path = NULL,
  search = NULL,
  cache = drake::drake_cache(path = path),
  namespace = NULL,
  envir = parent.frame(),
  jobs = 1,
  verbose = 1L,
  deps = FALSE,
  lazy = "eager",
  graph = NULL,
  replace = TRUE,
  show_source = FALSE,
  tidyselect = !deps,
  config = NULL,
  subtargets = NULL,
  subtarget_list = FALSE
) {
  deprecate_search(search)
  deprecate_arg(graph, "graph") # 2019-01-04 # nolint
  deprecate_arg(imported_only, "imported_only") # 2019-01-01 # nolint
  force(envir)
  lazy <- parse_lazy_arg(lazy)
  assert_cache(cache)
  cache <- decorate_storr(cache)
  namespace <- namespace %|||% cache$default_namespace
  tidyselect <- loadd_use_tidyselect(tidyselect, deps)
  if (tidyselect && requireNamespace("tidyselect", quietly = TRUE)) {
    targets <- drake_tidyselect_cache(
      ...,
      list = list,
      cache = cache,
      namespaces = namespace
    )
  } else {
    targets <- c(as.character(match.call(expand.dots = FALSE)$...), list)
  }
  targets <- loadd_handle_empty_targets(
    targets = targets,
    cache = cache,
    ...
  )
  targets <- loadd_use_deps(targets = targets, config = config, deps = deps)
  exists <- lightly_parallelize(
    X = targets,
    FUN = cache$exists,
    jobs = jobs
  )
  exists <- unlist(exists)
  targets <- targets[exists]
  targets <- targets_only(targets, cache = cache, jobs = jobs)
  if (!loadd_any_targets(targets, deps, verbose)) {
    return(invisible())
  }
  targets <- loadd_handle_replace(targets, envir, replace)
  loadd_show_source(targets, cache = cache, show_source = show_source)
  lightly_parallelize(
    X = targets,
    FUN = load_target,
    cache = cache,
    namespace = namespace,
    envir = envir,
    verbose = verbose,
    lazy = lazy
  )
  lapply(
    targets,
    load_subtargets,
    cache = cache,
    envir = envir,
    subtargets = subtargets,
    subtarget_list = subtarget_list
  )
  invisible()
}

# TODO: should be a validate method.
assert_cache <- function(cache) {
  if (is.null(cache)) {
    stop0("cannot find drake cache.")
  }
}

load_subtargets <- function(target, cache, envir, subtargets, subtarget_list) {
  hashes <- get(target, envir = envir, inherits = FALSE)
  load_targets_impl(hashes, target, cache, envir, subtargets, subtarget_list)
}

load_targets_impl <- function(
  hashes,
  target,
  cache,
  envir,
  subtargets,
  subtarget_list
) {
  UseMethod("load_targets_impl")
}

#' @export
load_targets_impl.drake_dynamic <- function( # nolint
  hashes,
  target,
  cache,
  envir,
  subtargets,
  subtarget_list
) {
  value <- get_subtargets(hashes, target, cache, subtargets, subtarget_list)
  rm(list = as.character(target), envir = envir, inherits = FALSE)
  assign(target, value, envir = envir, inherits = FALSE)
}

#' @export
load_targets_impl.default <- function(
  hashes,
  target,
  cache,
  envir,
  subtargets,
  subtarget_list
) {
  NULL
}

get_subtargets <- function(hashes, target, cache, subtargets, subtarget_list) {
  UseMethod("get_subtargets")
}

#' @export
get_subtargets.drake_dynamic <- function(
  hashes,
  target,
  cache,
  subtargets,
  subtarget_list
) {
  if (!is.null(subtargets)) {
    hashes <- hashes[subtargets]
  }
  out <- lapply(hashes, cache$get_value, use_cache = FALSE)
  if (subtarget_list) {
    keys <- cache$get(target, namespace = "meta", use_cache = FALSE)$subtargets
    if (!is.null(subtargets)) {
      keys <- keys[subtargets]
    }
    names(out) <- keys
  } else {
    out <- do.call(safe_vec_c, out)
  }
  out
}

#' @export
get_subtargets.default <- function(
  hashes,
  target,
  cache,
  subtargets,
  subtarget_list
) {
  hashes
}

loadd_handle_empty_targets <- function(targets, cache, ...) {
  if (!length(targets) && !length(list(...))) {
    targets <- cache$list()
  }
  targets
}

loadd_use_tidyselect <- function(tidyselect, deps) {
  if (tidyselect && deps) {
    cli_msg("Disabled tidyselect in loadd() because deps is TRUE.")
    return(FALSE)
  }
  tidyselect
}

loadd_use_deps <- function(targets, config, deps) {
  if (!deps) {
    return(targets)
  }
  if (is.null(config)) {
    stop0(
      "loadd(deps = TRUE) needs a drake_config() object in config."
    )
  }
  assert_config(config)
  targets <- deps_memory(targets = targets, config = config)
}

loadd_show_source <- function(targets, cache, show_source) {
  if (!show_source) {
    return()
  }
  lapply(
    X = targets,
    FUN = show_source,
    config = list(cache = cache),
    character_only = TRUE
  )
}

loadd_handle_replace <- function(targets, envir, replace) {
  if (!replace) {
    targets <- setdiff(targets, names(envir))
  }
  targets
}

loadd_any_targets <- function(targets, deps, verbose) {
  if (!length(targets) && !deps) {
    if (verbose) {
      cli_msg("No targets to load in loadd().")
    }
    return(FALSE)
  }
  TRUE
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

#' @title Show how a target/import was produced.
#' `r lifecycle::badge("stable")`
#' @description Show the command that produced a target
#'   or indicate that the object or file was imported.
#' @export
#' @param target Symbol denoting the target or import
#'   or a character vector if character_only is `TRUE`.
#' @param config A [drake_config()] list.
#' @param character_only Logical, whether to interpret
#'   `target` as a symbol (`FALSE`) or character vector
#'   (`TRUE`).
#' @examples
#' \dontrun{
#' isolate_example("contain side effects", {
#' plan <- drake_plan(x = sample.int(15))
#' cache <- storr::storr_environment() # custom in-memory cache
#' make(plan, cache = cache)
#' config <- drake_config(plan, cache = cache, history = FALSE)
#' show_source(x, config)
#' })
#' }
show_source <- function(target, config, character_only = FALSE) {
  if (!character_only) {
    target <- as.character(substitute(target))
  }
  cache <- config$cache
  meta <- diagnose(target = target, cache = cache, character_only = TRUE)
  prefix <- ifelse(is_encoded_path(target), "File", "Target")
  if (meta$imported) {
    cli_msg(prefix, target, "was imported.")
  } else {
    command <- gsub("^\\{\n ", "", meta$command)
    command <- gsub(" \n\\}$", "", command)
    cli_msg(
      prefix,
      target,
      "was built from command:\n  ",
      target,
      " = ",
      command
    )
  }
}

#' @title Read the pseudo-random number generator seed of the project.
#' `r lifecycle::badge("stable")`
#' @description When a project is created with [make()]
#' or [drake_config()], the project's pseudo-random number generator
#' seed is cached. Then, unless the cache is destroyed,
#' the seeds of all the targets will deterministically depend on
#' this one central seed. That way, reproducibility is protected,
#' even under randomness.
#' @export
#' @return An integer vector.
#'
#' @inheritParams cached
#' @param verbose Deprecated on 2019-09-11.
#'
#' @examples
#' \dontrun{
#' isolate_example("contain side effects", {
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
#' })
#' }
read_drake_seed <- function(
  path = NULL,
  search = NULL,
  cache = NULL,
  verbose = NULL
) {
  deprecate_verbose(verbose)
  deprecate_search(search)
  if (is.null(cache)) {
    cache <- drake_cache(path = path)
  }
  if (is.null(cache)) {
    stop("cannot find drake cache.")
  }
  cache <- decorate_storr(cache)
  if (cache$exists(key = "seed", namespace = "session")) {
    cache$get(key = "seed", namespace = "session")
  } else {
    stop("Pseudo-random seed not found in the cache.")
  }
}

#' @title List targets in the cache.
#' `r lifecycle::badge("stable")`
#' @description Tip: read/load a cached item with [readd()]
#'   or [loadd()].
#' @seealso [cached_planned()], [cached_unplanned()],
#'   [readd()], [loadd()],
#'   [drake_plan()], [make()]
#' @export
#' @return Either a named logical indicating whether the given
#'   targets or cached or a character vector listing all cached
#'   items, depending on whether any targets are specified.
#'
#' @param ... Deprecated. Do not use.
#'   Objects to load from the cache, as names (unquoted)
#'   or character strings (quoted). Similar to `...` in
#'   `remove()`.
#'
#' @param list Deprecated. Do not use.
#'   Character vector naming objects to be loaded from the
#'   cache. Similar to the `list` argument of [remove()].
#'
#' @param targets_only Logical. If `TRUE` just list the targets.
#'   If `FALSE`, list files and imported objects too.
#'
#' @param no_imported_objects Logical, deprecated. Use
#'   `targets_only` instead.
#'
#' @param cache drake cache. See [new_cache()].
#'   If supplied, `path` is ignored.
#'
#' @param path Path to a `drake` cache
#'   (usually a hidden `.drake/` folder) or `NULL`.
#'
#' @param search Deprecated.
#'
#' @param namespace Character scalar, name of the storr namespace
#'   to use for listing objects.
#'
#' @param jobs Number of jobs/workers for parallel processing.
#'
#' @param verbose Deprecated on 2019-09-11.
#'
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' if (requireNamespace("lubridate")) {
#' load_mtcars_example() # Load drake's canonical example.
#' make(my_plan) # Run the project, build all the targets.
#' cached()
#' cached(targets_only = FALSE)
#' }
#' }
#' })
#' }
cached <- function(
  ...,
  list = character(0),
  no_imported_objects = FALSE,
  path = NULL,
  search = NULL,
  cache = drake::drake_cache(path = path),
  verbose = NULL,
  namespace = NULL,
  jobs = 1,
  targets_only = TRUE
) {
  deprecate_verbose(verbose)
  deprecate_search(search)
  if (is.null(cache)) {
    return(character(0))
  }
  cache <- decorate_storr(cache)
  if (is.null(namespace)) {
    namespace <- cache$default_namespace
  }
  targets <- c(list, match.call(expand.dots = FALSE)$...)
  if (length(targets)) {
    warn0(
      "The `...` and `list` arguments of `cached()` are deprecated.",
      "`cached()` no longer accepts target names. It just lists ",
      "the targets in the cache."
    )
  }
  targets <- cache$list(namespace = namespace)
  if (targets_only) {
    targets <- targets_only(targets, cache, jobs)
  }
  redisplay_keys(targets)
}

#' @title List targets in both the plan and the cache.
#' `r lifecycle::badge("stable")`
#' @description Includes dynamic sub-targets as well.
#'   See examples for details.
#' @seealso [cached()], [cached_unplanned]
#' @export
#' @return A character vector of target and sub-target names.
#' @inheritParams cached
#' @param plan A drake plan.
#' @examples
#' \dontrun{
#' isolate_example("cache_planned() example", {
#' plan <- drake_plan(w = 1)
#' make(plan)
#' cached_planned(plan)
#' plan <- drake_plan(
#'   x = seq_len(2),
#'   y = target(x, dynamic = map(x))
#' )
#' cached_planned(plan)
#' make(plan)
#' cached_planned(plan)
#' cached()
#' })
#' }
cached_planned <- function(
  plan,
  path = NULL,
  cache = drake::drake_cache(path = path),
  namespace = NULL,
  jobs = 1
) {
  if (is.null(cache)) {
    return(character(0))
  }
  cache <- decorate_storr(cache)
  namespace <- namespace %|||% cache$default_namespace
  cached <- cache$list(namespace = namespace)
  targets <- intersect(plan$target, cached)
  subtargets <- unlist(
    lapply(targets, subtargets, character_only = TRUE, cache = cache)
  )
  planned <- c(targets, subtargets)
  intersect(cached, planned)
}

#' @title List targets in the cache but not the plan.
#' `r lifecycle::badge("stable")`
#' @description Includes dynamic sub-targets as well.
#'   See examples for details.
#' @seealso [cached()], [cached_planned]
#' @export
#' @return A character vector of target and sub-target names.
#' @inheritParams cached
#' @param plan A drake plan.
#' @examples
#' \dontrun{
#' isolate_example("cache_unplanned() example", {
#' plan <- drake_plan(w = 1)
#' make(plan)
#' cached_unplanned(plan)
#' plan <- drake_plan(
#'   x = seq_len(2),
#'   y = target(x, dynamic = map(x))
#' )
#' cached_unplanned(plan)
#' make(plan)
#' cached_unplanned(plan)
#' # cached_unplanned() helps clean superfluous targets.
#' cached()
#' clean(list = cached_unplanned(plan))
#' cached()
#' })
#' }
cached_unplanned <- function(
  plan,
  path = NULL,
  cache = drake::drake_cache(path = path),
  namespace = NULL,
  jobs = 1
) {
  if (is.null(cache)) {
    return(character(0))
  }
  cache <- decorate_storr(cache)
  namespace <- namespace %|||% cache$default_namespace
  cached <- cache$list(namespace = namespace)
  targets <- intersect(plan$target, cached)
  subtargets <- unlist(
    lapply(targets, subtargets, character_only = TRUE, cache = cache)
  )
  planned <- c(targets, subtargets)
  setdiff(cached, planned)
}

targets_only <- function(targets, cache, jobs) {
  parallel_filter(
    x = targets,
    f = function(target) {
      !is_imported_cache(target = target, cache = cache) &&
        !is_encoded_path(target)
    },
    jobs = jobs
  )
}

is_imported_cache <- Vectorize(function(target, cache) {
  exists <- cache$exists(key = target) && (
    imported <- diagnose(
      target = target,
      character_only = TRUE,
      cache = cache
    )$imported %|||%
      FALSE
  )
},
"target", SIMPLIFY = TRUE)

#' @title Get the cache of a `drake` project.
#' `r lifecycle::badge("stable")`
#' @description [make()] saves the values of your targets so
#'   you rarely need to think about output files. By default,
#'   the cache is a hidden folder called `.drake/`.
#'   You can also supply your own `storr` cache to the `cache`
#'   argument of `make()`. The `drake_cache()` function retrieves
#'   this cache.
#' @details `drake_cache()` actually returns a *decorated* `storr`,
#'   an object that *contains* a `storr` (plus bells and whistles).
#'   To get the *actual* inner `storr`, use `drake_cache()$storr`.
#'   Most methods are delegated to the inner `storr`.
#'   Some methods and objects are new or overwritten. Here
#'   are the ones relevant to users.
#'   - `history`: `drake`'s history (which powers [drake_history()])
#'     is a `txtq`. Access it
#'     with `drake_cache()$history`.
#'   - `import()`: The `import()` method is a function that can import
#'     targets, function dependencies, etc. from one decorated `storr`
#'     to another. History is not imported. For that, you have to work
#'     with the history `txtq`s themselves, Arguments to `import()`:
#'     - `...` and `list`: specify targets to import just like with [loadd()].
#'       Leave these blank to import everything.
#'     - `from`: the decorated `storr` from which to import targets.
#'     - `jobs`: number of local processes for parallel computing.
#'     - `gc`: `TRUE` or `FALSE`, whether to run garbage collection for memory
#'       after importing each target. Recommended, but slow.
#'   - `export()`: Same as `import()`, except the `from` argument is replaced
#'     by `to`: the decorated `storr` where the targets end up.
#' @seealso [new_cache()], [drake_config()]
#' @export
#' @return A drake/storr cache in a folder called `.drake/`,
#'   if available. `NULL` otherwise.
#' @param path Character.
#'   Set `path` to the path of a `storr::storr_rds()` cache
#'   to retrieve a specific cache generated by `storr::storr_rds()`
#'   or `drake::new_cache()`. If the `path` argument is `NULL`,
#'   `drake_cache()` searches up through parent directories
#'   to find a folder called `.drake/`.
#' @param verbose Deprecated on 2019-09-11.
#' @param console_log_file Deprecated on 2019-09-11.
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' clean(destroy = TRUE)
#' # No cache is available.
#' drake_cache() # NULL
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' x <- drake_cache() # Now, there is a cache.
#' y <- storr::storr_rds(".drake") # Nearly equivalent.
#' # List the objects readable from the cache with readd().
#' x$list()
#' # drake_cache() actually returns a *decorated* storr.
#' # The *real* storr is inside.
#' drake_cache()$storr
#' }
#' # You can import and export targets to and from decorated storrs.
#' plan1 <- drake_plan(w = "w", x = "x")
#' plan2 <- drake_plan(a = "a", x = "x2")
#' cache1 <- new_cache("cache1")
#' cache2 <- new_cache("cache2")
#' make(plan1, cache = cache1)
#' make(plan2, cache = cache2)
#' cache1$import(cache2, a)
#' cache1$get("a")
#' cache1$get("x")
#' cache1$import(cache2)
#' cache1$get("x")
#' # With txtq >= 0.1.6.9002, you can import history from one cache into
#' # another.
#' # nolint start
#' # drake_history(cache = cache1)
#' # cache1$history$import(cache2$history)
#' # drake_history(cache = cache1)
#' # nolint end
#' })
#' }
drake_cache <- function(
  path = NULL,
  verbose = NULL,
  console_log_file = NULL
) {
  deprecate_verbose(verbose)
  deprecate_console_log_file(console_log_file)
  if (is.null(path)) {
    path <- find_cache(path = getwd())
  }
  this_cache_(path = path)
}

#' @title Search up the file system for the nearest drake cache.
#' `r lifecycle::badge("stable")`
#' @description Only works if the cache is a file system in a
#' hidden folder named `.drake/` (default).
#' @seealso [drake_plan()], [make()],
#' @export
#' @return File path of the nearest drake cache or `NULL`
#'   if no cache is found.
#' @param path Starting path for search back for the cache.
#'   Should be a subdirectory of the drake project.
#' @param dir Character, name of the folder containing the cache.
#' @param directory Deprecated. Use `dir`.
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the target.
#' # Find the file path of the project's cache.
#' # Search up through parent directories if necessary.
#' find_cache()
#' }
#' })
#' }
find_cache <- function(
  path = getwd(),
  dir = NULL,
  directory = NULL
) {
  if (!is.null(directory)) {
    warn0(
      "Argument `directory` of find_cache() is deprecated. ",
      "use `dir` instead."
    )
  }
  dir <- dir %|||% basename(default_cache_path())
  while (!(dir %in% list.files(path = path, all.files = TRUE))) {
    path <- dirname(path)
    # If we can search no higher...
    if (path == dirname(path)) {
      return(NULL) # The cache does not exist
    }
  }
  file.path(path, dir)
}

#' @title  Make a new `drake` cache. `r lifecycle::badge("stable")`
#' @description Uses the [storr::storr_rds()] function
#' from the `storr` package.
#' @export
#' @return A newly created drake cache as a storr object.
#' @seealso [make()]
#' @param path File path to the cache if the cache
#'   is a file system cache.
#' @param type Deprecated argument. Once stood for cache type.
#'   Use `storr` to customize your caches instead.
#' @param hash_algorithm Name of a hash algorithm to use.
#'   See the `algo` argument of the `digest` package for your options.
#' @param short_hash_algo Deprecated on 2018-12-12.
#'   Use `hash_algorithm` instead.
#' @param long_hash_algo Deprecated on 2018-12-12.
#'   Use `hash_algorithm` instead.
#' @param verbose Deprecated on 2019-09-11.
#' @param console_log_file Deprecated on 2019-09-11.
#' @param ... other arguments to the cache constructor.
#' @examples
#' \dontrun{
#' isolate_example("Quarantine new_cache() side effects.", {
#' clean(destroy = TRUE) # Should not be necessary.
#' unlink("not_hidden", recursive = TRUE) # Should not be necessary.
#' cache1 <- new_cache() # Creates a new hidden '.drake' folder.
#' cache2 <- new_cache(path = "not_hidden", hash_algorithm = "md5")
#' clean(destroy = TRUE, cache = cache2)
#' })
#' }
new_cache <- function(
  path = NULL,
  verbose = NULL,
  type = NULL,
  hash_algorithm = NULL,
  short_hash_algo = NULL,
  long_hash_algo = NULL,
  ...,
  console_log_file = NULL
) {
  deprecate_verbose(verbose)
  deprecate_console_log_file(console_log_file)
  path <- path %|||% default_cache_path()
  hash_algorithm <- sanitize_hash_algorithm(hash_algorithm)
  if (!is.null(type)) {
    warning(
      "The 'type' argument of new_cache() is deprecated. ",
      "Please see the storage guide in the manual for the new cache API:",
      "https://books.ropensci.org/drake/storage.html"
    )
  }
  deprecate_hash_algo_args(short_hash_algo, long_hash_algo)
  cache <- storr::storr_rds(
    path = path,
    mangle_key = FALSE,
    hash_algorithm = hash_algorithm
  )
  cache <- decorate_storr(cache)
  writeLines(
    text = c("*", "!/.gitignore"),
    con = file.path(path, ".gitignore")
  )
  cache
}

sanitize_hash_algorithm <- function(hash_algorithm) {
  if (is.null(hash_algorithm)) {
    "xxhash64"
  } else {
    hash_algorithm
  }
}

this_cache_ <- function(path = NULL) {
  path <- path %|||% default_cache_path()
  usual_path_missing <- is.null(path) || !file.exists(path)
  if (usual_path_missing) {
    return(NULL)
  }
  cache <- drake_fetch_rds(path = path)
  cache_vers_warn(cache = cache)
  cache
}

default_cache_path <- function(root = getwd()) {
  file.path(root, ".drake")
}

drake_fetch_rds <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  decorate_storr(storr::storr_rds(path = path))
}

cache_vers_stop <- function(cache) {
  msg <- cache_vers_check(cache)
  if (length(msg)) {
    stop0(msg)
  }
}

cache_vers_warn <- function(cache) {
  msg <- cache_vers_check(cache)
  if (length(msg)) {
    warn0(msg)
  }
}

cache_vers_check <- function(cache) {
  if (is.null(cache)) {
    return(character(0))
  }
  old <- drake_cache_version(cache = cache)
  if (compareVersion(old, "6.2.1") <= 0) {
    paste0(
      "This project was last run with drake version ",
      old, ".\nYour cache is not compatible with the current ",
      "version of drake (",
      packageVersion("drake"), ").\nTo run your project with version ",
      packageVersion("drake"), ", either:\n",
      "    1. Run `clean(destroy = TRUE)` ",
      "to clear the cache first (recommended) or\n",
      "    2. Run `make(force = TRUE)` ",
      "to overwrite targets in the cache on the fly\n",
      "       (which saves more of your work but may cause issues like\n",
      "       https://github.com/ropensci/drake/issues/725).\n",
      "Be warned: if you do either, ",
      "all you targets will run from scratch.\nYou may wish to ",
      "downgrade drake to version ", old, " instead."
    )
  } else {
    character(0)
  }
}

drake_cache_version <- function(cache) {
  if (cache$exists(key = "drake_version", namespace = "session")) {
    cache$get(key = "drake_version", namespace = "session")
  } else if (cache$exists(key = "sessionInfo", namespace = "session")) {
    session_info <- drake_get_session_info(cache = cache)
    all_pkgs <- c(
      session_info$otherPkgs, # nolint
      session_info$loadedOnly # nolint
    )
    as.character(all_pkgs$drake$Version)
  } else {
    as.character(utils::packageVersion("drake"))
  }
}

#' @title Session info of the last call to [make()].
#' `r lifecycle::badge("stable")`
#' @description By default, session info is saved
#' during [make()] to ensure reproducibility.
#' Your loaded packages and their versions are recorded, for example.
#' @seealso [diagnose()], [cached()],
#'   [readd()], [drake_plan()], [make()]
#' @export
#' @return [sessionInfo()] of the last
#'   call to [make()]
#' @inheritParams cached
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' drake_get_session_info() # Get the cached sessionInfo() of the last make().
#' }
#' })
#' }
drake_get_session_info <- function(
  path = NULL,
  search = NULL,
  cache = drake::drake_cache(path = path),
  verbose = 1L
) {
  if (is.null(cache)) {
    stop("No drake::make() session detected.")
  }
  cache <- decorate_storr(cache)
  return(cache$get("sessionInfo", namespace = "session"))
}

#' @title Get the state of the cache.
#' `r lifecycle::badge("stable")`
#' @description Get the fingerprints of all the targets in a data frame.
#' This functionality is like
#' `make(..., cache_log_file = TRUE)`,
#' but separated and more customizable. Hopefully, this functionality
#' is a step toward better data versioning tools.
#' @details A hash is a fingerprint of an object's value.
#' Together, the hash keys of all your targets and imports
#' represent the state of your project.
#' Use `drake_cache_log()` to generate a data frame
#' with the hash keys of all the targets and imports
#' stored in your cache.
#' This function is particularly useful if you are
#' storing your drake project in a version control repository.
#' The cache has a lot of tiny files, so you should not put it
#' under version control. Instead, save the output
#' of `drake_cache_log()` as a text file after each [make()],
#' and put the text file under version control.
#' That way, you have a changelog of your project's results.
#' See the examples below for details.
#' Depending on your project's
#' history, the targets may be different than the ones
#' in your workflow plan data frame.
#' Also, the keys depend on the hash algorithm
#' of your cache. To define your own hash algorithm,
#' you can create your own `storr` cache and give it a hash algorithm
#' (e.g. `storr_rds(hash_algorithm = "murmur32")`)
#' @seealso [cached()], [drake_cache()]
#' @export
#' @return Data frame of the hash keys of the targets and imports
#'   in the cache
#'
#' @inheritParams cached
#'
#' @param jobs Number of jobs/workers for parallel processing.
#'
#' @param targets_only Logical, whether to output information
#'   only on the targets in your workflow plan data frame.
#'   If `targets_only` is `FALSE`, the output will
#'   include the hashes of both targets and imports.
#'
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' # Load drake's canonical example.
#' load_mtcars_example() # Get the code with drake_example()
#' # Run the project, build all the targets.
#' make(my_plan)
#' # Get a data frame of all the hash keys.
#' # If you want a changelog, be sure to do this after every make().
#' cache_log <- drake_cache_log()
#' head(cache_log)
#' # Suppress partial arg match warnings.
#' suppressWarnings(
#'   # Save the hash log as a flat text file.
#'   write.table(
#'     x = cache_log,
#'     file = "drake_cache.log",
#'     quote = FALSE,
#'     row.names = FALSE
#'   )
#' )
#' # At this point, put drake_cache.log under version control
#' # (e.g. with 'git add drake_cache.log') alongside your code.
#' # Now, every time you run your project, your commit history
#' # of hash_lot.txt is a changelog of the project's results.
#' # It shows which targets and imports changed on every commit.
#' # It is extremely difficult to track your results this way
#' # by putting the raw '.drake/' cache itself under version control.
#' }
#' })
#' }
drake_cache_log <- function(
  path = NULL,
  search = NULL,
  cache = drake::drake_cache(path = path),
  verbose = 1L,
  jobs = 1,
  targets_only = FALSE
) {
  deprecate_search(search)
  if (is.null(cache)) {
    return(
      weak_tibble(
        hash = character(0),
        type = character(0),
        name = character(0)
      )
    )
  }
  cache <- decorate_storr(cache)
  out <- lightly_parallelize(
    X = cache$list(),
    FUN = single_cache_log,
    jobs = jobs,
    cache = cache
  )
  out <- weak_as_tibble(do.call(rbind, out))
  if (targets_only) {
    out <- out[out$type == "target", ]
  }
  out$name <- redisplay_keys(out$name)
  out
}

single_cache_log <- function(key, cache) {
  hash <- cache$get_hash(key = key)
  imported <- read_from_meta(
    key = key,
    field = "imported",
    cache = cache
  )
  imported <- imported %|||% TRUE
  imported <- imported %|||NA% TRUE
  type <- ifelse(imported, "import", "target")
  weak_tibble(hash = hash, type = type, name = key)
}

#' @title Get diagnostic metadata on a target.
#' `r lifecycle::badge("stable")`
#' @description Diagnostics include errors, warnings,
#'   messages, runtimes, and other context/metadata from when a
#'   target was built or an import was processed.
#'   If your target's last build succeeded,
#'   then `diagnose(your_target)` has the most current information
#'   from that build.
#'   But if your target failed, then only
#'   `diagnose(your_target)$error`,
#'   `diagnose(your_target)$warnings`,
#'   and `diagnose(your_target)$messages` correspond to the failure,
#'   and all the other metadata correspond to the last build that completed
#'   without an error.
#' @seealso
#'   [drake_failed()], [drake_progress()],
#'   [readd()], [drake_plan()], [make()]
#' @export
#' @return Either a character vector of target names or an object
#'   of class `"error"`.
#'
#' @inheritParams cached
#'
#' @param target Name of the target of the error to get.
#'   Can be a symbol if `character_only` is `FALSE`,
#'   must be a character if `character_only` is `TRUE`.
#'
#' @param character_only Logical, whether `target` should be treated
#'   as a character or a symbol.
#'   Just like `character.only` in [library()].
#'
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' diagnose() # List all the targets with recorded error logs.
#' # Define a function doomed to failure.
#' f <- function() {
#'   stop("unusual error")
#' }
#' # Create a workflow plan doomed to failure.
#' bad_plan <- drake_plan(my_target = f())
#' # Running the project should generate an error
#' # when trying to build 'my_target'.
#' try(make(bad_plan), silent = FALSE)
#' drake_failed() # List the failed targets from the last make() (my_target).
#' # List targets that failed at one point or another
#' # over the course of the project (my_target).
#' # drake keeps all the error logs.
#' diagnose()
#' # Get the error log, an object of class "error".
#' error <- diagnose(my_target)$error # See also warnings and messages.
#' str(error) # See what's inside the error log.
#' error$calls # View the traceback. (See the rlang::trace_back() function).
#' })
#' }
diagnose <- function(
  target = NULL,
  character_only = FALSE,
  path = NULL,
  search = NULL,
  cache = drake::drake_cache(path = path),
  verbose = 1L
) {
  deprecate_search(search)
  if (is.null(cache)) {
    return(character(0))
  }
  cache <- decorate_storr(cache)
  if (!character_only) {
    target <- as.character(substitute(target))
  }
  if (!length(target)) {
    return(cache$list(namespace = "meta"))
  }
  target <- standardize_key(target)
  if (!cache$exists(key = target, namespace = "meta")) {
    stop("No metadata for target ", target, ".")
  }
  cache$get(key = target, namespace = "meta")
}

#' @title List running targets.
#' `r lifecycle::badge("stable")`
#' @description List the targets that either
#'   1. Are currently being built during a call to [make()], or
#'   2. Were in progress when [make()] was interrupted.
#' @seealso [drake_done()], [drake_failed()], [drake_cancelled()],
#'   [drake_progress()], [make()]
#' @export
#' @return A character vector of target names.
#' @inheritParams cached
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' drake_running() # Everything should be done.
#' # nolint start
#' # Run make() in one R session...
#' # slow_plan <- drake_plan(x = Sys.sleep(2))
#' # make(slow_plan)
#' # and see the progress in another session.
#' # drake_running()
#' # nolint end
#' }
#' })
#' }
drake_running <- function(
  cache = drake::drake_cache(path = path),
  path = NULL
) {
  drake_progress_field(cache = cache, path = path, field = "running")
}

#' @title List failed targets.
#' `r lifecycle::badge("stable")`
#' @description List the targets that quit in error during [make()].
#' @seealso [drake_done()], [drake_running()], [drake_cancelled()],
#'   [drake_progress()], [make()]
#' @export
#' @return A character vector of target names.
#' @inheritParams cached
#' @examples
#' \dontrun{
#' isolate_example("contain side effects", {
#' if (suppressWarnings(require("knitr"))) {
#' # Build a plan doomed to fail:
#' bad_plan <- drake_plan(x = function_doesnt_exist())
#' cache <- storr::storr_environment() # optional
#' try(
#'   make(bad_plan, cache = cache, history = FALSE),
#'   silent = TRUE
#' ) # error
#' drake_failed(cache = cache) # "x"
#' e <- diagnose(x, cache = cache) # Retrieve the cached error log of x.
#' names(e)
#' e$error
#' names(e$error)
#' }
#' })
#' }
drake_failed <- function(
  cache = drake::drake_cache(path = path),
  path = NULL
) {
  drake_progress_field(cache = cache, path = path, field = "failed")
}

#' @title List done targets.
#' `r lifecycle::badge("stable")`
#' @description List the targets that completed in the current or
#'   previous call to [make()].
#' @seealso [drake_running()], [drake_failed()], [drake_cancelled()],
#'   [drake_progress()], [make()]
#' @export
#' @return A character vector of target names.
#' @inheritParams cached
#' @examples
#' \dontrun{
#' isolate_example("contain side effects", {
#' plan <- drake_plan(x = 1, y = x)
#' make(plan)
#' drake_done()
#' })
#' }
drake_done <- function(cache = drake::drake_cache(path = path), path = NULL) {
  drake_progress_field(cache = cache, path = path, field = "done")
}

#' @title List cancelled targets.
#' `r lifecycle::badge("stable")`
#' @description List the targets that were cancelled in the current or
#'   previous call to [make()] using [cancel()] or [cancel_if()].
#' @seealso [drake_running()], [drake_failed()], [make()]
#' @export
#' @return A character vector of target names.
#' @inheritParams cached
#' @examples
#' \dontrun{
#' isolate_example("contain side effects", {
#' plan <- drake_plan(x = 1, y = cancel_if(x > 0))
#' make(plan)
#' drake_cancelled()
#' })
#' }
drake_cancelled <- function(
  cache = drake::drake_cache(path = path),
  path = NULL
) {
  drake_progress_field(cache = cache, path = path, field = "cancelled")
}

drake_progress_field <- function(cache, path, field) {
  prog <- drake_progress(path = path, cache = cache)
  prog$target[prog$progress == field]
}

#' @title Get the build progress of your targets
#' `r lifecycle::badge("stable")`
#' @description Objects that drake imported, built, or attempted
#' to build are listed as `"done"` or `"running"`.
#' Skipped objects are not listed.
#' @seealso [diagnose()], [drake_get_session_info()],
#'   [cached()], [readd()], [drake_plan()], [make()]
#' @export
#'
#' @return The build progress of each target reached by
#'   the current [make()] so far.
#'
#' @inheritParams cached
#'
#' @param ... Objects to load from the cache, as names (unquoted)
#'   or character strings (quoted). If the `tidyselect` package is installed,
#'   you can also supply `dplyr`-style `tidyselect`
#'   commands such as `starts_with()`, `ends_with()`, and `one_of()`.
#'
#' @param list Character vector naming objects to be loaded from the
#'   cache. Similar to the `list` argument of [remove()].
#'
#' @param progress Character vector for filtering the build progress results.
#'   Defaults to `NULL` (no filtering) to report progress of all objects.
#'   Supported filters are `"done"`, `"running"`, and `"failed"`.
#'
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' # Watch the changing drake_progress() as make() is running.
#' drake_progress() # List all the targets reached so far.
#' drake_progress(small, large) # Just see the progress of some targets.
#' drake_progress(list = c("small", "large")) # Same as above.
#' }
#' })
#' }
drake_progress <- function(
  ...,
  list = character(0),
  cache = drake::drake_cache(path = path),
  path = NULL,
  progress = NULL
) {
  if (is.null(cache)) {
    return(weak_tibble(target = character(0), progress = character(0)))
  }
  cache <- decorate_storr(cache)
  targets <- c(as.character(match.call(expand.dots = FALSE)$...), list)
  if (requireNamespace("tidyselect", quietly = TRUE)) {
    targets <- drake_tidyselect_cache(
      ...,
      list = list,
      cache = cache,
      namespaces = "meta"
    )
  }
  if (!length(targets)) {
    targets <- cache$list(namespace = "progress")
  }
  progress_results <- cache$get_progress(targets)
  out <- weak_tibble(target = targets, progress = progress_results)
  rownames(out) <- NULL
  if (is.null(progress)) {
    # to enforce consistent behavior with and without tidyselect:
    return(out[out$progress != "none",, drop = FALSE]) # nolint
  }
  progress <- match.arg(
    progress,
    choices = c("done", "running", "cancelled", "failed"),
    several.ok = TRUE
  )
  out[out$progress %in% progress,, drop = FALSE] # nolint
}

memo_expr <- function(expr, cache, ...) {
  if (is.null(cache)) {
    return(force(expr))
  }
  lang <- match.call(expand.dots = FALSE)$expr
  lang <- safe_deparse(lang, backtick = TRUE)
  key <- cache$digest(list(lang, ...))
  if (cache$exists(key = key, namespace = "memoize")) {
    return(cache$get(key = key, namespace = "memoize", use_cache = TRUE))
  }
  value <- force(expr)
  cache$set(
    key = key,
    value = value,
    namespace = "memoize",
    use_cache = TRUE
  )
  value
}

list_multiple_namespaces <- function(cache, namespaces, jobs = 1) {
  out <- lightly_parallelize(
    X = namespaces,
    FUN = function(namespace) {
      cache$list(namespace = namespace)
    },
    jobs = jobs
  )
  Reduce(out, f = base::union)
}

read_from_meta <- function(key, field, cache) {
  meta <- old_meta(key = key, cache = cache)
  meta_elt(field, meta)
}

old_meta <- function(key, cache) {
  cache$safe_get(key = key, namespace = "meta")
}

meta_elt <- function(field, meta) {
  meta[[field]] %|||% NA_character_
}

drake_tidyselect_cache <- function(
  ...,
  list = character(0),
  cache,
  namespaces = cache$default_namespace
) {
  out <- tidyselect::vars_select(
    .vars = list_multiple_namespaces(cache = cache, namespaces = namespaces),
    ...,
    .strict = FALSE
  )
  out <- unname(out)
  c(out, list)
}

# Borrowed from dplyr source under the MIT license.
#' @aliases tar_tidyselect
#' @export
tidyselect::all_of
#' @export
tidyselect::any_of
#' @export
tidyselect::contains
#' @export
tidyselect::ends_with
#' @export
tidyselect::everything
#' @export
tidyselect::last_col
#' @export
tidyselect::matches
#' @export
tidyselect::num_range
#' @export
tidyselect::one_of
#' @export
tidyselect::starts_with
