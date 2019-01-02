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
  plan = read_drake_plan(),
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
  config = read_drake_config(),
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
  dependencies(targets = targets, config = config, reverse = reverse)
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
#' @seealso [diagnose()], [built()], [imported()],
#'   [readd()], [drake_plan()], [make()]
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

#' @title Deprecated.
#' @description Deprecated on 2018-10-24.
#' @export
#' @keywords internal
#' @return Evaluated code.
#' @param code Placeholder for the code to build a target/import.
empty_hook <- function(code) {
  .Deprecated(
    "empty_hook",
    package = "drake",
    msg = "empty_hook() is deprecated."
  )
  invisible()
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
#' @seealso [predict_runtime()]
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
  config = drake::read_drake_config(),
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

#' @title Deprecated.
#' @description Deprecated on 2018-10-24.
#' @export
#' @keywords internal
#' @return Evaluated code.
#' @param code Placeholder for the code to build a target/import.
message_sink_hook <- function(code) {
  .Deprecated(
    "message_sink_hook",
    package = "drake",
    msg = "message_sink_hook() is deprecated."
  )
  message <- file(paste0("message", Sys.getpid(), ".txt"), "w")
  on.exit({
    suppressWarnings(sink(type = "message"))
    close(message)
  })
  sink(message, type = "message")
  code
}

#' @title Deprecated: reconfigure an old project (built with drake <= 4.4.0)
#'   to be compatible with later versions of drake.
#' @export
#' @keywords internal
#' @seealso [rescue_cache()], [make()]
#' @param path Full path to the cache.
#' @param jobs number of jobs for light parallelism.
#' @description Deprecated on May 4, 2018.
#' This function was intended to migrate a project/cache from
#' drake 4.4.0 or earlier
#' to be compatible with the version of drake on your system.
#' @examples
#' # This function is deprecated.
migrate_drake_project <- function(
  path = drake::default_cache_path(), jobs = 1
) {
  .Deprecated(
    package = "drake",
    msg = c(
      "migrate_drake_project() is deprecated. Please run ",
      "make() again on projects built with drake version <= 4.4.0"
    )
  )
}

#' @title Deprecated.
#' @description Deprecated on 2018-10-24.
#' @export
#' @keywords internal
#' @return Evaluated code.
#' @param code Placeholder for the code to build a target/import.
output_sink_hook <- function(code) {
  .Deprecated(
    "output_sink_hook",
    package = "drake",
    msg = "output_sink_hook() is deprecated."
  )
  output <- paste0("output", Sys.getpid(), ".txt")
  on.exit(suppressWarnings(sink(type = "output")))
  sink(output, type = "output")
  code
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
  config = drake::read_drake_config(),
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
#' @seealso [render_drake_ggraph()]
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
  config = drake::read_drake_config(),
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
#' @seealso [drake_ggraph()]
#' @return A `ggplot2` object, which you can modify with more layers,
#'   show with `plot()`, or save as a file with `ggsave()`.
#' @inheritParams drake_ggraph
#' @examples
#' # See drake_ggraph()
static_drake_graph <- function(
  config = drake::read_drake_config(),
  build_times = "build",
  digits = 3,
  targets_only = FALSE,
  split_columns = NULL,
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
    split_columns = split_columns,
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
#' @seealso [drake_plan()], [make()]
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
#' @seealso [clean_main_example()]
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
#' @seealso [drake_unquote()], [drake_strings()]
#' @export
#' @return Character vector with quotes around it.
#' @param x character vector or object to be coerced to character.
#' @param single Add single quotes if `TRUE`
#'   and double quotes otherwise.
#' @examples
#' # Single-quote this string.
#' drake_quotes("abcd", single = TRUE) # "'abcd'"
#' # Double-quote this string.
#' drake_quotes("abcd") # "\"abcd\""
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
#' @seealso [drake_quotes()], [drake_strings()]
#' @export
#' @return Character vector without leading
#'   or trailing escaped quotes around
#'   the elements.
#' @param x character vector
#' @examples
#' x <- "'abcd'"
#' # Remove the literal quotes around x.
#' drake_unquote(x) # "abcd"
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
#' @seealso [drake_quotes()], [drake_unquote()]
#' @export
#' @return A character vector.
#' @param ... unquoted symbols to turn into character strings.
#' @examples
#' # Turn symbols into strings.
#' drake_strings(a, b, c, d) # [1] "a" "b" "c" "d"
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

