#' @title Deprecated. List the available hash algorithms for drake caches.
#' @export
#' @keywords internal
#' @description Deprecated on 2018-12-12.
#' @return A character vector of names of available hash algorithms.
#' @examples
#' # deprecated
available_hash_algos <- function() {
  .Deprecated(
    "available_hash_algos",
    package = "drake",
    msg = "drake::available_hash_algos() is deprecated."
  )
  eval(formals(digest::digest)$algo)
}

#' @title Deprecated function `build_drake_graph`
#' @description Use [drake_config()] instead.
#' @details Deprecated on 2018-11-02.
#' @export
#' @keywords internal
#' @return An `igraph` object.
#' @inheritParams drake_config
#' @examples
#' # See ?drake_config for examples.
build_drake_graph <- function(
  plan,
  targets = plan$target,
  envir = parent.frame(),
  verbose = 1L,
  jobs = 1,
  console_log_file = NULL,
  trigger = drake::trigger(),
  cache = NULL
) {
  .Deprecated(
    "build_graph",
    package = "drake",
    msg = paste(
      "drake::build_drake_graph() is deprecated.",
      "Use drake_config()$graph instead."
    )
  )
  drake_config(
    plan = plan,
    targets = targets,
    envir = envir,
    verbose = verbose,
    jobs = jobs,
    console_log_file = console_log_file,
    trigger = trigger,
    cache = cache
  )$graph
}

#' @title Deprecated. Configure the hash algorithms, etc. of a drake cache.
#' @export
#' @keywords internal
#' @description The purpose of this function is
#'   to prepare the cache to be called from [make()].
#'   `drake` only uses a single hash algorithm now,
#'   so we no longer need this configuration step.
#' @details Deprecated on 2018-12-12.
#' @return A drake/storr cache.
#'
#' @inheritParams cached
#'
#' @param cache cache to configure
#'
#' @param short_hash_algo short hash algorithm for drake.
#'   The short algorithm must be among [available_hash_algos()],
#'   which is just the collection of algorithms available to the `algo`
#'   argument in [digest::digest()].
#'   See [default_short_hash_algo()] for more.
#'
#' @param long_hash_algo long hash algorithm for drake.
#'   The long algorithm must be among [available_hash_algos()],
#'   which is just the collection of algorithms available to the `algo`
#'   argument in `digest::digest()`.
#'   See [default_long_hash_algo()] for more.
#'
#' @param log_progress deprecated logical.
#'   Previously toggled whether to clear the recorded
#'   build progress if this cache was used for previous calls to
#'   [make()].
#'
#' @param overwrite_hash_algos logical, whether to try to overwrite
#'   the hash algorithms in the cache with any user-specified ones.
#'
#' @param jobs number of jobs for parallel processing
#'
#' @param init_common_values logical, whether to set the initial `drake`
#'   version in the cache and other common values.
#'   Not always a thread safe operation, so should only be `TRUE`
#'   on the master process
#'
#' @examples
#' # deprecated
configure_cache <- function(
  cache = drake::get_cache(verbose = verbose),
  short_hash_algo = drake::default_short_hash_algo(cache = cache),
  long_hash_algo = drake::default_long_hash_algo(cache = cache),
  log_progress = FALSE,
  overwrite_hash_algos = FALSE,
  verbose = 1L,
  jobs = 1,
  init_common_values = FALSE
) {
  .Deprecated(
    "configure_cache",
    package = "drake",
    msg = "configure_cache() is deprecated."
  )
  short_hash_algo <- match.arg(short_hash_algo,
                               choices = available_hash_algos())
  long_hash_algo <- match.arg(long_hash_algo,
                              choices = available_hash_algos())
  if (log_progress) {
    warning(
      "The `log_progress` argument of `configure_cache()` is deprecated.",
      call. = FALSE
    )
  }
  short_exists <- cache$exists(key = "short_hash_algo", namespace = "config")
  long_exists <- cache$exists(key = "long_hash_algo", namespace = "config")
  if (overwrite_hash_algos | !short_exists) {
    cache$set(
      key = "short_hash_algo",
      value = short_hash_algo,
      namespace = "config"
    )
  }
  if (overwrite_hash_algos | !long_exists) {
    cache$set(
      key = "long_hash_algo",
      value = long_hash_algo,
      namespace = "config"
    )
  }
  chosen_algo <- short_hash(cache)
  if (init_common_values) {
    init_common_values(cache)
  }
  cache
}

#' @title Deprecated. Return the default long hash algorithm for `make()`.
#' @export
#' @keywords internal
#' @description Deprecated. drake now only uses one hash algorithm per cache.
#' @details Deprecated on 2018-12-12
#' @return A character vector naming a hash algorithm.
#' @param cache optional drake cache.
#'   When you [configure_cache()] without
#'   supplying a long hash algorithm,
#'   `default_long_hash_algo(cache)` is the long
#'   hash algorithm that drake picks for you.
#' @examples
#' # deprecated
default_long_hash_algo <- function(cache = NULL) {
  .Deprecated(
    "default_long_hash_algo",
    package = "drake",
    msg = "default_long_hash_algo() is deprecated."
  )
  # nocov start
  out <- "sha256"
  if (is.null(cache)) {
    return(out)
  }
  if (cache$exists(key = "long_hash_algo", namespace = "config")) {
    out <- cache$get(
      key = "long_hash_algo",
      namespace = "config"
    )
  }
  out
  # nocov end
}

#' @title Deprecated. Return the default short hash algorithm for `make()`.
#' @export
#' @keywords internal
#' @description Deprecated. drake now only uses one hash algorithm per cache.
#' @details Deprecated on 2018-12-12
#' @return A character vector naming a hash algorithm.
#' @param cache optional drake cache.
#'   When you [configure_cache()] without
#'   supplying a short hash algorithm,
#'   `default_short_hash_algo(cache)` is the short
#'   hash algorithm that drake picks for you.
#' @examples
#' # deprecated
default_short_hash_algo <- function(cache = NULL) {
  .Deprecated(
    "default_short_hash_algo",
    package = "drake",
    msg = "default_short_hash_algo() is deprecated."
  )
  # nocov start
  out <- "xxhash64"
  if (is.null(cache)) {
    return(out)
  }
  if (cache$exists(key = "short_hash_algo", namespace = "config")) {
    out <- cache$get(
      key = "short_hash_algo",
      namespace = "config"
    )
  }
  if ("storr" %in% class(cache)) {
    out <- cache$driver$hash_algorithm
  }
  out
  # nocov end
}

# 2018-10-27 # nolint
deprecate_force <- function(force) {
  if (!identical(force, FALSE)) {
    warning(
      "Argument `force` is deprecated.",
      call. = FALSE
    )
  }
}

#' @title Deprecated.
#'   List the dependencies of a function, workflow plan command,
#'   or knitr report source file.
#' @description Deprecated. Use [deps_code()] or [deps_target()] instead.
#'   These functions are intended for debugging and checking your project.
#'   The dependency structure of the components of your analysis
#'   decides which targets are built and when.
#' @details Deprecated on 2018-05-08.
#' @export
#' @keywords internal
#' @param x Either a function or a string.
#'   Strings are commands from your workflow plan data frame.
#' @return A character vector, names of dependencies.
#'   Files wrapped in single quotes.
#'   The other names listed are functions or generic R objects.
#' @examples
#' # See deps_code() for examples.
deps <- function(x) {
  .Deprecated(
    "deps_code()",
    package = "drake",
    msg = paste(
      "drake::deps() is deprecated.",
      "Use deps_code() or deps_target() instead."
    )
  )
  deps_code(x)
}

#' @title Deprecated.
#' @description Deprecated. Use [deps_target()] (singular) instead.
#' @details Deprecated on 2018-08-30.
#' @export
#' @keywords internal
#' @param targets a character vector of target names
#' @param config an output list from [drake_config()]
#' @param reverse logical, whether to compute reverse dependencies
#'   (targets immediately downstream) instead of ordinary dependencies.
#' @return Names of dependencies listed by type (object, input file, etc).
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' config <- drake_config(my_plan)
#' deps_targets("regression1_small", config = config)
#' deps_targets(c("small", "large"), config = config, reverse = TRUE)
#' })
#' }
deps_targets <- function(
  targets,
  config,
  reverse = FALSE
) {
  .Deprecated(
    "deps_code()",
    package = "drake",
    msg = paste(
      "drake::deps_targets() is deprecated.",
      "Use deps_target() (singular) instead."
    )
  )
  deps_target(target = targets, config = config)
}

#' @title Deprecated. Get a template file for execution on a cluster.
#' @description Deprecated. Use [drake_hpc_template_file()] instead.
#' @details Deprecated on 2018-06-27.
#' @export
#' @keywords internal
#' @inheritParams drake_hpc_template_file
#' @param example name of template file
#' @examples
#' # See drake_hpc_template_file() for examples.
drake_batchtools_tmpl_file <- function(
  example = drake::drake_hpc_template_files(),
  to = getwd(),
  overwrite = FALSE
) {
  .Deprecated(
    "drake_batchtools_tmpl_file",
    package = "drake",
    msg = paste(
      "drake_batchtools_tmpl_file() is deprecated. ",
      "Use drake_hpc_template_file() instead."
    )
  )
  drake_hpc_template_file(file = example, to = to, overwrite = overwrite)
}

#' @title Deprecated. Return the [sessionInfo()]
#'   of the last call to [make()].
#' @description Deprecated. Use [drake_get_session_info()] instead.
#' @details Deprecated on 2018-12-06.
#' @export
#' @return [sessionInfo()] of the last call to [make()]
#' @inheritParams cached
#' @examples
#' # See ?drake_get_session_info for examples.
drake_session <- function(
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1L
) {
  .Deprecated(
    "drake_session",
    package = "drake",
    msg = paste(
      "drake_session() is deprecated.",
      "Use drake_get_session_info() instead."
    )
  )
  if (is.null(cache)) {
    stop("No drake::make() session detected.")
  }
  return(cache$get("sessionInfo", namespace = "session"))
}

#' @title Deprecated. `drake` now has just one hash algorithm per cache.
#' @export
#' @keywords internal
#' @description Deprecated on 2018-12-12
#' @return A character vector naming a hash algorithm.
#' @inheritParams cached
#' @examples
#' # deprecated
long_hash <- function(
  cache = drake::get_cache(verbose = verbose),
  verbose = 1L
) {
  .Deprecated(
    "long_hash",
    package = "drake",
    msg = "long_hash() is deprecated."
  )
  # nocov start
  if (!cache$exists(key = "long_hash_algo", namespace = "config")) {
    return(NULL)
  }
  cache$get("long_hash_algo", namespace = "config")
  # nocov end
}

#' @title Deprecated function
#' @description Do not use this function. `drake`'s parallel algorithm
#'   has changed since version 5.1.2, so `max_useful_jobs()`
#'   will give you the wrong idea of how many jobs to use. Instead,
#'   use the [predict_runtime()] function with a sensible value
#'   for `known_times` and `default_time`
#'   to cover any targets not built so far.
#' @details Deprecated on May 4, 2018.
#' @export
#' @keywords internal
#' @return A numeric scalar, the maximum number of useful jobs for
#'   \code{\link{make}(..., jobs = ...)}.
#' @param config internal configuration list of \code{\link{make}(...)},
#'   produced also with [drake_config()].
#' @param imports Set the `imports` argument to change your
#'   assumptions about how fast objects/files are imported.
#' @param from_scratch logical, whether to assume
#'   the next [make()] will run from scratch
#'   so that all targets are attempted.
#' @examples
#' # Do not use this function. Use predict_runtime() instead.
#' # Pay special attention to the force_times and default_time
#' # arguments.
max_useful_jobs <- function(
  config,
  imports = c("files", "all", "none"),
  from_scratch = FALSE
) {
  .Deprecated(
    "predict_runtime",
    package = "drake",
    msg = c(
      "Do not use max_useful_jobs(). ",
      "drake's parallel scheduling algorithm has changed, ",
      "so max_useful_jobs() will give you the wrong idea about ",
      "how many jobs to assign to `make()`. For a better estimate, ",
      "play around with predict_runtime() with sensible values, ",
      "for force_times and default_time."
    )
  )
  # nocov start
  imports <- match.arg(imports)
  nodes <- drake_graph_info(config, from_scratch = from_scratch)$nodes
  if (imports == "none") {
    nodes <- nodes[nodes$status != "imported", ]
  } else if (imports == "files") {
    nodes <- nodes[nodes$status != "imported" | nodes$type == "file", ]
  }
  if (!from_scratch) {
    nodes <- nodes[nodes$status != "outdated", ]
  }
  if (!nrow(nodes)) {
    return(0)
  }
  per_level <- split(x = nodes, f = nodes$level)
  max(vapply(per_level, nrow, FUN.VALUE = integer(1)))
  # nocov end
}

#' @title Deprecated: reconfigure an old project (built with drake <= 4.4.0)
#'   to be compatible with later versions of drake.
#' @export
#' @keywords internal
#' @param path Full path to the cache.
#' @param jobs number of jobs for light parallelism.
#' @description Deprecated on May 4, 2018.
#' This function was intended to migrate a project/cache from
#' drake 4.4.0 or earlier
#' to be compatible with the version of drake on your system.
#' @examples
#' # This function is deprecated.
migrate_drake_project <- function(
  path = NULL, jobs = 1
) {
  .Deprecated(
    package = "drake",
    msg = c(
      "migrate_drake_project() is deprecated. Please run ",
      "make() again on projects built with drake version <= 4.4.0"
    )
  )
}

#' @title Defunct
#' @description This function is now moot because
#' staged parallelism in `drake` was replaced
#' by a much better scheduling algorithm.
#' @export
#' @keywords internal
#' @details Made defunct on May 4, 2018
#' @examples
#' # Do not use this function.
#' @return A data frame of times of the worst-case scenario
#'   rate-limiting targets in each parallelizable stage.
#' @param config option internal runtime parameter list of
#'   \code{\link{make}(...)},
#'   produced by both [make()] and
#'   [drake_config()].
#' @param targets Character vector, names of targets.
#'   Find the rate-limiting times for building these targets
#'   plus dependencies.
#'   Defaults to all targets.
#' @param from_scratch logical, whether to assume
#'   next hypothetical call to [make()]
#'   is a build from scratch (after [clean()]).
#' @param targets_only logical, whether to factor in just the
#'   targets or use times from everything, including the imports.
#' @param future_jobs hypothetical number of jobs
#'   assumed for the predicted runtime.
#'   assuming this number of jobs.
#' @param digits number of digits for rounding the times.
rate_limiting_times <- function(
  config,
  targets = NULL,
  from_scratch = FALSE,
  targets_only = FALSE,
  future_jobs = 1,
  digits = 3
) {
  .Defunct(
    package = "drake",
    msg = c(
      "The rate_limiting_times() function is moot ",
      "because drake has replaced staged parallelism ",
      "with a much better algorithm. ",
      "Do not use rate_limiting_times()."
    )
  )
}

#' @title Deprecated: render a `ggraph`/`ggplot2` representation
#'   of your drake project.
#' @description Use [render_drake_ggraph()] instead.
#' @details Deprecated on 2018-25-07.
#' @export
#' @keywords internal
#' @return A `ggplot2` object, which you can modify with more layers,
#'   show with `plot()`, or save as a file with `ggsave()`.
#' @inheritParams render_drake_ggraph
#' @examples
#' # See render_drake_ggraph()
render_static_drake_graph <- function(
  graph_info,
  main = graph_info$default_title
) {
  .Deprecated(
    "render_static_drake_graph",
    package = "drake",
    msg = paste(
      "render_static_drake_graph() is deprecated.",
      "Use render_drake_ggraph() instead."
    )
  )
  render_drake_ggraph(graph_info = graph_info, main = main)
}

#' @title Defunct function
#' @description Staged parallelism is removed from drake,
#' so this function is moot.
#' drake uses a much better parallel algorithm now.
#' @details Made defunct on May 4, 2018.
#' @export
#' @keywords internal
#' @return A data frame of information spelling out how
#'   targets are divided into parallelizable stages
#'   (according to the `stage` column).
#' @param config An configuration list output by
#'   [make()] or [drake_config()].
#' @param from_scratch logical, whether to assume
#'   that the next [make()] will run from scratch
#'   so that all targets are attempted.
#' @examples
#' # Do not use this function.
parallel_stages <- function(
  config,
  from_scratch = FALSE
) {
  .Defunct(
    package = "drake",
    msg = c(
      "Staged parallelism is removed from drake, ",
      "so the parallel_stages() function is moot. ",
      "drake uses a much better parallel algorithm now."
    )
  )
}

#' @title Deprecated. `drake` now only uses one hash algorithm per cache.
#' @export
#' @keywords internal
#' @description Deprecated on 2018-12-12.
#' @return A character vector naming a hash algorithm.
#' @inheritParams cached
#' @examples
#' # deprecated
short_hash <- function(
  cache = drake::get_cache(verbose = verbose),
  verbose = 1L
) {
  .Deprecated(
    "short_hash",
    package = "drake",
    msg = "short_hash() is deprecated."
  )
  # nocov start
  if (!cache$exists(key = "short_hash_algo", namespace = "config")) {
    return(NULL)
  }
  chosen_algo <- cache$get("short_hash_algo", namespace = "config")
  cache$get("short_hash_algo", namespace = "config")
  # nocov end
}

#' @title Deprecated: show a `ggraph`/`ggplot2` representation
#'   of your drake project.
#' @description Use [drake_ggraph()] instead.
#' @details Deprecated on 2018-25-07.
#' @export
#' @keywords internal
#' @return A `ggplot2` object, which you can modify with more layers,
#'   show with `plot()`, or save as a file with `ggsave()`.
#' @inheritParams drake_ggraph
#' @examples
#' # See drake_ggraph()
static_drake_graph <- function(
  config,
  build_times = "build",
  digits = 3,
  targets_only = FALSE,
  main = NULL,
  from = NULL,
  mode = c("out", "in", "all"),
  order = NULL,
  subset = NULL,
  make_imports = TRUE,
  from_scratch = FALSE,
  full_legend = FALSE,
  group = NULL,
  clusters = NULL
) {
  .Deprecated(
    "static_drake_graph",
    package = "drake",
    msg = paste(
      "static_drake_graph() is deprecated",
      "Use drake_ggraph() instead."
    )
  )
  drake_ggraph(
    config = config,
    build_times = build_times,
    digits = digits,
    targets_only = targets_only,
    main = main,
    from = from,
    mode = mode,
    order = order,
    subset = subset,
    make_imports = make_imports,
    from_scratch = from_scratch,
    full_legend = full_legend,
    group = group,
    clusters = clusters
  )
}

#' @title Deprecated. List the old drake triggers.
#' @export
#' @keywords internal
#' @description Triggers are target-level rules
#' that tell [make()] how to know if a target
#' is outdated or up to date.
#' @details Deprecated on 2018-07-22.
#' @return A character vector with the names of the old triggers.
#' @examples
#' # Deprecated. See the trigger() function instead (singular).
triggers <- function() {
  .Deprecated(
    "triggers",
    package = "drake",
    msg = paste(
      "drake::triggers() is deprecated",
      "and the trigger interface has changed.",
      "See trigger() (singular) for details."
    )
  )
  out <- c(
    "any",
    "always",
    "command",
    "depends",
    "file",
    "missing"
  )
  sort(out)
}

convert_old_trigger <- function(x) {
  if (!is.character(x)) {
    return(x)
  }
  if (!(x %in% suppressWarnings(triggers()))) {
    return(x)
  }
  warning(
    "The old trigger interface is deprecated. ",
    "See the trigger() function (singular) ",
    "to learn about the new trigger interface.",
    call. = FALSE
  )
  if (identical(x, "any")) {
    "trigger()"
  } else if (identical(x, "always")) {
    "trigger(condition = TRUE)"
  } else if (identical(x, "command")) {
    "trigger(command = TRUE, depend = FALSE, file = FALSE)"
  } else if (identical(x, "depends")) {
    "trigger(command = FALSE, depend = TRUE, file = FALSE)"
  } else if (identical(x, "file")) {
    "trigger(command = FALSE, depend = FALSE, file = TRUE)"
  } else if (identical(x, "missing")) {
    "trigger(command = FALSE, depend = FALSE, file = FALSE)"
  }
}


# Helper function: check for deprecated `fetch_cache` parameter
# See ...drake/pull/608#pullrequestreview-182943763
deprecate_fetch_cache <- function(fetch_cache) {
  if (!is.null(fetch_cache)) {
    warning(
      "Argument `fetch_cache` is deprecated.",
      call. = FALSE
    ) # 2018-12-08 # nolint
  }
}

deprecate_targets_only <- function(targets_only) {
  if (!is.null(targets_only)) {
    warning(
      "Argument `targets_only` is deprecated. ",
      "build_times(), graph visualizations, and runtime predictions ",
      "now always focus only on the targets (ignoring the imports).",
      call. = FALSE
    ) # build times, vis, and predictions: 2019-01-03 # nolint
  }
}

#' @title Deprecated: load the main example.
#' @description The main example lives at
#' <https://github.com/wlandau/drake-examples/tree/master/main>.
#' Use `drake_example("main")` to download its code.
#' The chapter of the user manual at
#' <https://ropenscilabs.github.io/drake-manual/main.html>
#' also walks through the main example.
#' This function also writes/overwrites
#' the files `report.Rmd` and `raw_data.xlsx`.
#' @export
#' @return A [drake_config()] configuration list.
#' @inheritParams drake_config
#' @param envir The environment to load the example into.
#'   Defaults to your workspace.
#'   For an insulated workspace,
#'   set `envir = new.env(parent = globalenv())`.
#' @param report_file where to write the report file `report.Rmd`.
#' @param overwrite logical, whether to overwrite an
#'   existing file `report.Rmd`
#' @param force deprecated
#' @keywords internal
#' @details Deprecated December 2018.
#' @examples
#' \dontrun{
#' # The code for this example is hosted at
#' # https://github.com/wlandau/drake-examples/tree/master/main
#' # You can download it iwth drake_example("main")
#' # or watch a video tutorial about it at
#' # https://ropenscilabs.github.io/drake-manual/.
#' }
load_main_example <- function(
  envir = parent.frame(),
  report_file = "report.Rmd",
  overwrite = FALSE,
  force = FALSE
) {
  deprecate_force(force)
  .Deprecated(
    "drake_example",
    package = "drake",
    msg = paste("load_main_example() is deprecated.",
                'Use drake_example("main") instead.')
  )
  dir <- tempfile()
  drake_example(example = "main", to = dir)
  source(file.path(dir, "main", "R", "setup.R"), local = envir)
  envir$plan <- source(
    file.path(dir, "main", "R", "plan.R"),
    local = TRUE
  )$value
  for (file in c("report.Rmd", "raw_data.xlsx")) {
    if (file.exists(file) & overwrite) {
      warning("Overwriting file ", file, call. = FALSE)
    }
    file.copy(
      from = file.path(dir, "main", file),
      to = file,
      overwrite = overwrite
    )
  }
  invisible()
}

#' @title Deprecated: clean the main example from `drake_example("main")`
#' @description This function deletes files. Use at your own risk.
#'   Destroys the `.drake/` cache and the `report.Rmd` file
#'   in the current working directory. Your working directory
#'   (`getcwd()`) must be the folder from which you first ran
#'   `load_main_example()` and `make(my_plan)`.
#' @export
#' @return Nothing.
#' @keywords internal
#' @details Deprecated December 2018.
#' @examples
#' \dontrun{
#' # The code for this example is hosted at
#' # https://github.com/wlandau/drake-examples/tree/master/main
#' # You can download it with drake_example("main")
#' # or watch a video tutorial about it at
#' # https://ropenscilabs.github.io/drake-manual/.
#' }
clean_main_example <- function() {
  deprecate_force(force)
  .Deprecated(
    "clean",
    package = "drake",
    msg = paste("clean_main_example() is deprecated.")
  )
  clean(destroy = TRUE, search = FALSE)
  unlink(c("report.Rmd", "raw_data.xlsx"))
  invisible()
}

#' @title Deprecated
#' @description Deprecated on 2019-01-01
#' @export
#' @keywords internal
#' @return 1
default_verbose <- function() {
  .Deprecated(
    "default_verbose",
    package = "drake",
    msg = paste("default_verbose() is deprecated.")
  )
  1L
}

#' @title Put quotes around each element of a character vector.
#' @description Deprecated on 2019-01-01
#' @export
#' @keywords internal
#' @return Character vector with quotes around it.
#' @param x character vector or object to be coerced to character.
#' @param single Add single quotes if `TRUE`
#'   and double quotes otherwise.
#' @examples
#' # deprecated
drake_quotes <- function(x = NULL, single = FALSE) {
  .Deprecated(
    "drake_quotes",
    package = "drake",
    msg = paste("drake_quotes() is deprecated.")
  )
  stopifnot(is.logical(single))
  if (!length(x)) {
    return(character(0))
  }
  if (single) {
    paste0("'", x, "'")
  } else {
    paste0("\"", x, "\"")
  }
}

#' @title Remove leading and trailing
#'   escaped quotes from character strings.
#' @description Deprecated on 2019-01-01
#' @export
#' @keywords internal
#' @return Character vector without leading
#'   or trailing escaped quotes around
#'   the elements.
#' @param x character vector
#' @examples
#' # deprecated
drake_unquote <- function(x = NULL) {
  .Deprecated(
    "drake_unquote",
    package = "drake",
    msg = paste("drake_unquote() is deprecated.")
  )
  gsub(pattern = "^(?:'(.*)'|\"(.*)\")$", replacement = "\\1\\2", x = x)
}

#' @title Turn valid expressions into character strings.
#' @description Deprecated on 2019-01-01
#' @export
#' @keywords internal
#' @return A character vector.
#' @param ... unquoted symbols to turn into character strings.
#' @examples
#' # deprecated
drake_strings <- function(...) {
  .Deprecated(
    "drake_strings",
    package = "drake",
    msg = paste("drake_strings() is deprecated.")
  )
  args <- structure(as.list(match.call()[-1]), class = "uneval")
  keys <- names(args)
  out <- as.character(args)
  names(out) <- keys
  out
}

#' @title Deprecated. List all the built targets (non-imports) in the cache.
#' @description Deprecated on 2019-01-08.
#' @details Targets are listed in the workflow plan
#' data frame (see [drake_plan()].
#' @seealso [cached()], [loadd()]
#' @export
#' @return Character vector naming the built targets in the cache.
#' @inheritParams cached
#' @param jobs number of jobs/workers for parallel processing
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Load drake's canonical example.
#' make(my_plan) # Run the project, build all the targets.
#' built() # List all the cached targets (built objects and files).
#' # For file targets, only the fingerprints/hashes are stored.
#' })
#' }
built <- function(
  path = getwd(), search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1L,
  jobs = 1
) {
  .Deprecated(
    "built",
    package = "drake",
    msg = paste("built() is deprecated.",
                "Use cached(no_imported_objects = TRUE)) instead.")
  )
  if (is.null(cache)) {
    return(character(0))
  }
  out <- cache$list(namespace = cache$default_namespace)
  out <- parallel_filter(
    out,
    f = function(target) {
      !is_imported_cache(target = target, cache = cache)
    },
    jobs = jobs
  )
  display_keys(out)
}

#' @title Deprecated. Search up the file system
#'   for the nearest root path of a drake project.
#' @description Deprecated on 2019-01-08.
#' @details Only works if the cache is a file system
#' in a folder named `.drake` (default).
#' @export
#' @seealso [drake_plan()], [make()]
#' @return File path of the nearest drake project or `NULL`
#'   if no drake project is found.
#' @param path starting path for search back for the project.
#'   Should be a subdirectory of the drake project.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the target.
#' # Find the root directory of the current drake project.
#' # Search up through parent directories if necessary.
#' find_cache()
#' })
#' }
find_project <- function(path = getwd()) {
  .Deprecated(
    "find_project",
    package = "drake",
    msg = paste("find_project() is deprecated.",
                "Use find_cache() instead.")
  )
  
  cache <- find_cache(path = path)
  if (is.null(cache)) {
    return(NULL)
  }
  return(dirname(cache))
}

#' @title Deprecated
#' @description 2019-01-03
#' @export
#' @keywords internal
#' @return `args` for `system2(command, args)`
#' @inheritParams drake_config
#' @param jobs number of jobs
#' @examples
#' # deprecated
default_Makefile_args <- function(jobs, verbose) {
  .Deprecated(
    "default_Makefile_args",
    package = "drake",
    msg = paste(
      "default_Makefile_args() and",
      "Makefile parallelism are deprecated."
    )
  )
  out <- paste0("--jobs=", head(jobs, 1))
  if (verbose < 1) {
    out <- c(out, "--silent")
  }
  return(out)
}

#' @title Deprecated
#' @description 2019-01-03
#' @keywords internal
#' @return A character scalar
#' @export
#' @examples
#' # deprecated
default_Makefile_command <- function() {
  .Deprecated(
    "default_Makefile_command",
    package = "drake",
    msg = paste(
      "default_Makefile_command() and",
      "Makefile parallelism are deprecated."
    )
  )
  "make"
}

#' @title Deprecated
#' @export
#' @keywords internal
#' @description 2019-01-03
#' @return A character scalar
#' @param recipe_command character scalar
#' @param target character scalar
#' @param cache_path character scalar
#' @examples
#' # deprecated
Makefile_recipe <- function( # nolint
  recipe_command = drake::default_recipe_command(),
  target = "your_target",
  cache_path = NULL
) {
  .Deprecated(
    "Makefile_recipe",
    package = "drake",
    msg = paste(
      "Makefile_recipe() and",
      "Makefile parallelism are deprecated."
    )
  )
  character(0)
}

#' @title Deprecated
#' @export
#' @keywords internal
#' @description 2019-01-02
#' @return A character scalar with the default recipe command.
#' @examples
#' # deprecated
default_recipe_command <- function() {
  .Deprecated(
    "default_recipe_command",
    package = "drake",
    msg = paste(
      "default_recipe_command() and",
      "Makefile parallelism are deprecated."
    )
  )
  paste0("Rscript -e '", r_recipe_wildcard(), "'")
}

#' @title deprecated
#' @export
#' @keywords internal
#' @description 2019-01-02
#' @return The R recipe wildcard.
#' @examples
#' # deprecated
r_recipe_wildcard <- function() {
  .Deprecated(
    "r_recipe_wildcard",
    package = "drake",
    msg = paste(
      "r_recipe_wildcard() and",
      "Makefile parallelism are deprecated."
    )
  )
  "R_RECIPE"
}

#' @title Deprecated
#' @description 2019-01-03
#' @export
#' @keywords internal
#' @return character vector
#' @param distributed_only logical
#' @examples
#' # deprecated
parallelism_choices <- function(distributed_only = FALSE) {
  .Deprecated(
    "parallelism_choices",
    package = "drake",
    msg = paste(
      "parallelism_choices() and",
      "Makefile parallelism are deprecated."
    )
  )
  local <- c(
    "loop"
  )
  distributed <- c(
    "clustermq",
    "future",
    "future_lapply_staged",
    "hasty"
  )
  if (distributed_only) {
    sort(distributed)
  } else {
    sort(c(local, distributed))
  }
}

#' @title Deprecated
#' @description 2019-01-03
#' @export
#' @keywords internal
#' @return logical
#' @param path character
#' @param overwrite logical
#' @examples
#' # deprecated
shell_file <- function(
  path = "shell.sh",
  overwrite = FALSE
) {
  .Deprecated(
    "shell_file",
    package = "drake",
    msg = paste(
      "shell_file() and",
      "Makefile parallelism are deprecated."
    )
  )
  FALSE
}

#' @title Deprecated
#' @description 2019-01-02
#' @export
#' @keywords internal
#' @return character
#' @examples
#' # deprecated
default_parallelism <- function() {
  .Deprecated(
    "default_parallelism",
    package = "drake",
    msg = paste(
      "default_parallelism() is deprecated."
    )
  )
  "loop"
}

#' @title deprecated
#' @description Deprecated on 2019-01-04
#' @export
#' @keywords internal
#' @seealso [make()], [drake_config()]
#' @return nothing
#' @param config a configuration list returned by [drake_config()]
#' @examples
#' # deprecated
make_imports <- function(config) {
  .Deprecated(
    "make_imports",
    package = "drake",
    msg = paste(
      "make_imports() is deprecated. Use make()."
    )
  )
  config$skip_imports <- FALSE
  config$skip_targets <- TRUE
  make(config = config)
}

#' @title deprecated
#' @description Deprecated on 2019-01-04
#' @export
#' @keywords internal
#' @seealso [make()], [drake_config()]
#' @return nothing
#' @param config a configuration list returned by [drake_config()]
#' @examples
#' # deprecated
make_targets <- function(config) {
  .Deprecated(
    "make_targets",
    package = "drake",
    msg = paste(
      "make_targets() is deprecated. Use make()."
    )
  )
  config$skip_imports <- TRUE
  config$skip_targets <- FALSE
  make(config = config)
}

#' @title deprecated
#' @description Deprecated on 2019-01-04
#' @export
#' @keywords internal
#' @seealso [make()], [drake_config()]
#' @return nothing
#' @param config a configuration list returned by [drake_config()]
#' @examples
#' # deprecated
make_with_config <- function(config) {
  .Deprecated(
    "make_with_config",
    package = "drake",
    msg = paste(
      "make_with_config() is deprecated. Use make()."
    )
  )
  make(config = config)
}

#' @title Deprecated
#' @description drake no longer stores the config object,
#'   the plan, etc. in the cache during `make()`. This change
#'   improves speed.
#' @details 2019-01-06
#' @export
#' @keywords internal
#' @inheritParams cached
#' @examples
#' # deprecated
read_drake_config <- function(
  path = getwd(),
  search = TRUE,
  cache = NULL,
  verbose = 1L,
  jobs = 1,
  envir = parent.frame()
) {
  .Deprecated(
    "read_drake_config",
    package = "drake",
    msg = paste(
      "read_drake_config() is deprecated.",
      "drake no longer stores the config object,",
      "the plan, etc. in the cache during `make()`. This change",
      "improves speed."
    )
  )
  list()
}

#' @title Deprecated
#' @description drake no longer stores the config object,
#'   the plan, etc. in the cache during `make()`. This change
#'   improves speed.
#' @details 2019-01-06
#' @export
#' @keywords internal
#' @inheritParams cached
#' @examples
#' # deprecated
read_drake_graph <- function(
  path = getwd(),
  search = TRUE,
  cache = NULL,
  verbose = 1L
) {
  .Deprecated(
    "read_drake_graph",
    package = "drake",
    msg = paste(
      "read_drake_graph() is deprecated.",
      "drake no longer stores the config object,",
      "the plan, etc. in the cache during `make()`. This change",
      "improves speed."
    )
  )
  igraph::make_empty_graph()
}

#' @title Deprecated
#' @description drake no longer stores the config object,
#'   the plan, etc. in the cache during `make()`. This change
#'   improves speed.
#' @details 2019-01-06
#' @export
#' @keywords internal
#' @inheritParams cached
#' @examples
#' # deprecated
read_drake_plan <- function(
  path = getwd(),
  search = TRUE,
  cache = NULL,
  verbose = 1L
) {
  .Deprecated(
    "read_drake_plan",
    package = "drake",
    msg = paste(
      "read_drake_plan() is deprecated.",
      "drake no longer stores the config object,",
      "the plan, etc. in the cache during `make()`. This change",
      "improves speed."
    )
  )
  drake_plan()
}

#' @title Deprecated. List all the imports in the drake cache.
#' @description Deprecated on 2019-01-08.
#' @details An import is a non-target object processed
#' by [make()]. Targets in the workflow
#' plan data frame (see [drake_config()]
#' may depend on imports.
#' @seealso [cached()], [loadd()]
#' @export
#' @return Character vector naming the imports in the cache.
#'
#' @inheritParams cached
#'
#' @param files_only logical, whether to show imported files only
#'   and ignore imported objects. Since all your functions and
#'   all their global variables are imported, the full list of
#'   imported objects could get really cumbersome.
#'
#' @param jobs number of jobs/workers for parallel processing
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Load the canonical example.
#' make(my_plan) # Run the project, build the targets.
#' imported() # List all the imported objects/files in the cache.
#' # For imported files, only the fingerprints/hashes are stored.
#' })
#' }
imported <- function(
  files_only = FALSE, path = getwd(), search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1L,
  jobs = 1
) {
  .Deprecated(
    "imported",
    package = "drake",
    msg = paste(
      "imported() is deprecated.",
      "Use setdiff(cached(), cached(no_imported_objects = TRUE)) instead."
    )
  )
  if (is.null(cache)) {
    return(character(0))
  }
  targets <- cache$list(namespace = cache$default_namespace)
  targets <- parallel_filter(
    targets,
    f = function(target) {
      is_imported_cache(target = target, cache = cache)
    },
    jobs = jobs
  )
  if (files_only)
    targets <- parallel_filter(targets, f = is_encoded_path, jobs = jobs)
  display_keys(targets)
}

#' @title deprecated
#' @export
#' @keywords internal
#' @description 2019-01-08
#' @return an `igraph` object
#' @param graph an igraph object
#' @param to character vector of vertices
#' @param jobs number of jobs for parallelism
#' @examples
#' # deprecated
prune_drake_graph <- function(
  graph, to = igraph::V(graph)$name, jobs = 1
) {
  .Deprecated(
    "prune_drake_graph",
    package = "drake",
    msg = "prune_drake_graph() is deprecated."
  )
  nbhd_graph(
    graph = graph,
    vertices = to,
    mode = "in",
    order = igraph::gorder(graph)
  )
}

#' @title Deprecated. Show the analysis wildcard
#'   used in [plan_summaries()].
#' @description Deprecated on 2019-01-12.
#' @details Used to generate workflow plan data frames.
#' @export
#' @seealso [plan_summaries()]
#' @return The analysis wildcard used in [plan_summaries()].
analysis_wildcard <- function() {
  .Deprecated(
    "analysis_wildcard",
    package = "drake",
    msg = "analysis_wildcard() is deprecated."
  )
  analysis_wildcard_()
} #

#' @title Deprecated. Return the file path where the cache is stored,
#' if applicable.
#' @export
#' @description Deprecated on 2019-01-12.
#' @details Currently only works with
#' [storr::storr_rds()] file system caches.
#' @return File path where the cache is stored.
#' @param cache the cache whose file path you want to know
cache_path <- function(cache = NULL) {
  .Deprecated(
    "cache_path",
    package = "drake",
    msg = "cache_path() is deprecated."
  )
  cache_path_(cache)
}

#' @title Deprecated. List all the `storr` cache namespaces used by drake.
#' @description Deprecated on 2019-01-12.
#' @return A character vector of `storr` namespaces used for drake.
#' @details Ordinary users do not need to worry about this function.
#' It is just another window into `drake`'s internals.
#' @param default name of the default `storr` namespace
#' @export
#' @seealso [make()]
cache_namespaces <- function(
  default = storr::storr_environment()$default_namespace
) {
  .Deprecated(
    "cache_namespaces",
    package = "drake",
    msg = "cache_namespaces() is deprecated."
  )
  cache_namespaces_(default)
}

#' @title Deprecated. Check a workflow plan data frame for obvious errors.
#' @description Deprecated on 2019-01-12.
#' @details Possible obvious errors include circular dependencies and
#' missing input files.
#' @seealso [drake_plan()], [make()]
#' @export
#' @return Invisibly return `plan`.
#' @inheritParams cached
#' @param plan workflow plan data frame, possibly from
#'   [drake_plan()].
#' @param targets character vector of targets to make
#' @param envir environment containing user-defined functions
#' @param cache optional drake cache. See [new_cache()].
check_plan <- function(
  plan = NULL,
  targets = NULL,
  envir = parent.frame(),
  cache = drake::get_cache(verbose = verbose),
  verbose = 1L,
  jobs = 1
) {
  .Deprecated(
    "check_plan",
    package = "drake",
    msg = "check_plan() is deprecated."
  )
  force(envir)
  config <- drake_config(
    plan = plan,
    targets = targets,
    envir = envir,
    verbose = verbose,
    cache = cache,
    jobs = jobs
  )
  config_checks(config)
  invisible(plan)
}

#' @title Show the dataset wildcard
#'   used in [plan_analyses()] and [plan_summaries()].
#' @details Used to generate workflow plan data frames.
#' @description Deprecated on 2019-01-12.
#' @export
#' @seealso [plan_analyses()]
#' @return The dataset wildcard used in
#'   [plan_analyses()] and [plan_summaries()].
dataset_wildcard <- function() {
  .Deprecated(
    "dataset_wildcard",
    package = "drake",
    msg = "dataset_wildcard() is deprecated."
  )
  dataset_wildcard_()
}

#' @title Deprecated. Compute the initial pre-build metadata of a target or import.
#' @description Deprecated on 2019-01-12.
#' @details The metadata helps determine if the
#' target is up to date or outdated. The metadata of imports
#' is used to compute the metadata of targets.
#' Target metadata is computed with `drake_meta()`, and then
#' `drake:::store_outputs()` completes the metadata
#' after the target is built.
#' In other words, the output of `drake_meta()` corresponds
#' to the state of the target immediately before [make()]
#' builds it.
#' See [diagnose()] to read the final metadata of a target,
#' including any errors, warnings, and messages in the last build.
#' @seealso [diagnose()], [dependency_profile()], [make()]
#' @export
#' @return A list of metadata on a target. Does not include
#'   the file modification time if the target is a file.
#'   That piece is computed later in [make()] by
#'   `drake:::store_outputs()`.
#' @param target Character scalar, name of the target
#'   to get metadata.
#' @param config Master internal configuration list produced
#'   by [drake_config()].
drake_meta <- function(target, config) {
  .Deprecated(
    "drake_meta",
    package = "drake",
    msg = "drake_meta() is deprecated."
  )
  drake_meta_(target, config)
}

#' @title Deprecated. An internal function only used 
#' inside process_imports().
#' @description Deprecated on 2019-01-12.
#' @export
#' @keywords internal
#' @param import character, name of an import to process
#' @param config [drake_config()] object
process_import <- function(import, config) {
  .Deprecated(
    "process_import",
    package = "drake",
    msg = "process_import() is deprecated."
  )
  process_import_(import, config)
}

#' @title Deprecated. Show drake's color palette.
#' @description Deprecated on 2019-01-12.
#' @export
#' @details This function is
#' used in both the console and graph visualizations.
#' Your console must have the crayon package enabled.
#' This palette applies to console output
#' (internal functions `console()` and
#' `console_many_targets()`) and the node colors
#' in the graph visualizations.
#' So if you want to contribute improvements to the palette,
#' please both `drake_palette()` and
#' `visNetwork::visNetwork(nodes = legend_nodes())`
#' @return There is a console message,
#'   but the actual return value is `NULL`.
drake_palette <- function() {
  .Deprecated(
    "drake_palette",
    package = "drake",
    msg = "drake_palette() is deprecated."
  )
  drake_palette_()
}

#' @title Deprecated. Output a random tip about drake.
#' @description Deprecated on 2019-01-12.
#' @details Tips are usually related to news and usage.
#' @export
#' @return A character scalar with a tip on how to use drake.
drake_tip <- function() {
  .Deprecated(
    "drake_tip",
    package = "drake",
    msg = "drake_tip() is deprecated."
  )
  drake_tip_()
}

#' @title Deprecated. List the targets that either
#'   (1) are currently being built during a [make()], or
#'   (2) were being built if the last [make()] quit unexpectedly.
#' @description Deprecated on 2019-01-13.
#' @details Similar to [progress()].
#' @seealso [diagnose()], [drake_get_session_info()],
#'   [cached()], [readd()], [drake_plan()], [make()]
#' @export
#' @return A character vector of target names.
#' @inheritParams cached
in_progress <- function(path = getwd(), search = TRUE,
                        cache = drake::get_cache(path = path, search = search, verbose = verbose),
                        verbose = 1L
) {
  .Deprecated(
    "in_progress",
    package = "drake",
    msg = "in_progress() is deprecated."
  )
  in_progress_(path, search, cache, verbose )
}

#' @title Deprecated. Load an existing drake files system cache 
#' if it exists or create a new one otherwise.
#' @description Deprecated on 2019-01-13.
#' @export
#' @seealso [new_cache()], [this_cache()],
#'   [get_cache()]
#' @details Does not work with
#' in-memory caches such as [storr_environment()].
#' @return A drake/storr cache.
#' @inheritParams cached
#' @inheritParams new_cache
#' @inheritParams this_cache
#' @inheritParams drake_config
#' @param path file path of the cache
#' @param force logical, whether to load the cache
#'   despite any back compatibility issues with the
#'   running version of drake.
recover_cache <- function(
  path = NULL,
  hash_algorithm = NULL,
  short_hash_algo = NULL,
  long_hash_algo = NULL,
  force = FALSE,
  verbose = 1L,
  fetch_cache = NULL,
  console_log_file = NULL
) {
  .Deprecated(
    "recover_cache",
    package = "drake",
    msg = "recover_cache() is deprecated."
  )
  recover_cache_(path, hash_algorithm, short_hash_algo, long_hash_algo,
                 force, verbose, fetch_cache, console_log_file)
}

#' @title Deprecated. For drake caches,
#'   list the `storr` cache namespaces
#'   that store target-level information.
#' @description Deprecated on 2019-01-13.
#' @export
#' @seealso [make()]
#' @return A character vector of `storr` namespaces that store
#'   target-level information.
#' @details Ordinary users do not need to worry about this function.
#' It is just another window into `drake`'s internals.
#' @param default name of the default `storr` namespace
target_namespaces <- function(
  default = storr::storr_environment()$default_namespace
) {
  .Deprecated(
    "target_namespaces",
    package = "drake",
    msg = "target_namespaces() is deprecated."
  )
  target_namespaces_(default)
}
