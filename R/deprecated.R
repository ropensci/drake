#' @title List the available hash algorithms for drake caches.
#' `r lifecycle::badge("deprecated")`
#' @export
#' @keywords internal
#' @description Deprecated on 2018-12-12.
#' @return A character vector of names of available hash algorithms.
available_hash_algos <- function() {
  .Deprecated(
    new = "",
    package = "drake",
    msg = "available_hash_algos() in drake is deprecated."
  )
  eval(formals(digest::digest)$algo)
}

#' @title Function `build_drake_graph`
#' `r lifecycle::badge("deprecated")`
#' @description Use [drake_config()] instead.
#' @details Deprecated on 2018-11-02.
#' @export
#' @keywords internal
#' @return An `igraph` object.
#' @inheritParams drake_config
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
    new = "drake_config",
    package = "drake",
    msg = paste(
      "drake::build_drake_graph() in drake is deprecated.",
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

#' @title Configure the hash algorithms, etc. of a drake cache.
#' `r lifecycle::badge("deprecated")`
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
#' @param cache Cache to configure
#'
#' @param short_hash_algo Short hash algorithm for drake.
#'   The short algorithm must be among [available_hash_algos()],
#'   which is just the collection of algorithms available to the `algo`
#'   argument in `digest::digest()`.
#'   See [default_short_hash_algo()] for more.
#'
#' @param long_hash_algo Long hash algorithm for drake.
#'   The long algorithm must be among [available_hash_algos()],
#'   which is just the collection of algorithms available to the `algo`
#'   argument in `digest::digest()`.
#'   See [default_long_hash_algo()] for more.
#'
#' @param log_progress Deprecated logical.
#'   Previously toggled whether to clear the recorded
#'   build progress if this cache was used for previous calls to
#'   [make()].
#'
#' @param overwrite_hash_algos Logical, whether to try to overwrite
#'   the hash algorithms in the cache with any user-specified ones.
#'
#' @param jobs Number of jobs for parallel processing
#'
#' @param init_common_values Logical, whether to set the initial `drake`
#'   version in the cache and other common values.
#'   Not always a thread safe operation, so should only be `TRUE`
#'   on the main process
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
    new = "",
    package = "drake",
    msg = "configure_cache() in drake is deprecated."
  )
  short_hash_algo <- match.arg(short_hash_algo,
                               choices = available_hash_algos())
  long_hash_algo <- match.arg(long_hash_algo,
                              choices = available_hash_algos())
  if (log_progress) {
    warn0(
      "The `log_progress` argument of `configure_cache()` is deprecated."
    )
  }
  short_exists <- cache$exists(key = "short_hash_algo", namespace = "config")
  long_exists <- cache$exists(key = "long_hash_algo", namespace = "config")
  if (overwrite_hash_algos || !short_exists) {
    cache$set(
      key = "short_hash_algo",
      value = short_hash_algo,
      namespace = "config"
    )
  }
  if (overwrite_hash_algos || !long_exists) {
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

# Pre-set the values to avoid https://github.com/richfitz/storr/issues/80.
init_common_values <- function(cache) {
  common_values <- list(TRUE, FALSE)
  for (val in as.character(common_values)) {
    cache$set(
      key = as.character(val),
      value = val,
      namespace = "common"
    )
  }
}

#' @title Return the default long hash algorithm for `make()`.
#' `r lifecycle::badge("deprecated")`
#' @export
#' @keywords internal
#' @description Deprecated. drake now only uses one hash algorithm per cache.
#' @details Deprecated on 2018-12-12
#' @return A character vector naming a hash algorithm.
#' @param cache Optional drake cache.
#'   When you [configure_cache()] without
#'   supplying a long hash algorithm,
#'   `default_long_hash_algo(cache)` is the long
#'   hash algorithm that drake picks for you.
default_long_hash_algo <- function(cache = NULL) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = "default_long_hash_algo() in drake is deprecated."
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

#' @title Return the default short hash algorithm for `make()`.
#' `r lifecycle::badge("deprecated")`
#' @export
#' @keywords internal
#' @description Deprecated. drake now only uses one hash algorithm per cache.
#' @details Deprecated on 2018-12-12
#' @return A character vector naming a hash algorithm.
#' @param cache Optional drake cache.
#'   When you [configure_cache()] without
#'   supplying a short hash algorithm,
#'   `default_short_hash_algo(cache)` is the short
#'   hash algorithm that drake picks for you.
default_short_hash_algo <- function(cache = NULL) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = "default_short_hash_algo() in drake is deprecated."
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
    warn0(
      "Argument `force` is deprecated in small drake utility functions."
    )
  }
}

#' @title See the dependencies of a target
#' `r lifecycle::badge("deprecated")`
#' @description Use [deps_target()] (singular) instead.
#' @details Deprecated on 2018-08-30.
#' @export
#' @keywords internal
#' @param targets A character vector of target names.
#' @param config An output list from [drake_config()]
#' @param reverse Logical, whether to compute reverse dependencies
#'   (targets immediately downstream) instead of ordinary dependencies.
#' @return Names of dependencies listed by type (object, input file, etc).
deps_targets <- function(
  targets,
  config,
  reverse = FALSE
) {
  .Deprecated(
    new = "deps_target",
    package = "drake",
    msg = paste(
      "deps_targets() in drake is deprecated.",
      "Use deps_target() (singular) instead."
    )
  )
  deps_target(target = targets, config = config)
}

#' @title Get a template file for execution on a cluster.
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated. Use [drake_hpc_template_file()] instead.
#' @details Deprecated on 2018-06-27.
#' @export
#' @keywords internal
#' @inheritParams drake_hpc_template_file
#' @param example Name of template file.
drake_batchtools_tmpl_file <- function(
  example = drake::drake_hpc_template_files(),
  to = getwd(),
  overwrite = FALSE
) {
  .Deprecated(
    new = "drake_hpc_template_file",
    package = "drake",
    msg = paste(
      "drake_batchtools_tmpl_file() is deprecated. ",
      "Use drake_hpc_template_file() instead."
    )
  )
  drake_hpc_template_file(file = example, to = to, overwrite = overwrite)
}

#' @title Session info of the last call to [make()].
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated. Use [drake_get_session_info()] instead.
#' @details Deprecated on 2018-12-06.
#' @export
#' @keywords internal
#' @return [sessionInfo()] of the last call to [make()]
#' @inheritParams cached
drake_session <- function(
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1L
) {
  .Deprecated(
    new = "drake_get_session_info",
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

#' @title `drake` now has just one hash algorithm per cache.
#' `r lifecycle::badge("deprecated")`
#' @export
#' @keywords internal
#' @description Deprecated on 2018-12-12
#' @return A character vector naming a hash algorithm.
#' @inheritParams cached
long_hash <- function(
  cache = drake::get_cache(verbose = verbose),
  verbose = 1L
) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = "long_hash() in drake is deprecated."
  )
  # nocov start
  if (!cache$exists(key = "long_hash_algo", namespace = "config")) {
    return(NULL)
  }
  cache$get("long_hash_algo", namespace = "config")
  # nocov end
}

#' @title Deprecated: render a `ggraph`/`ggplot2` representation
#'   of your drake project.
#' `r lifecycle::badge("deprecated")`
#' @description Use [render_drake_ggraph()] instead.
#' @details Deprecated on 2018-07-25.
#' @export
#' @keywords internal
#' @return A `ggplot2` object, which you can modify with more layers,
#'   show with `plot()`, or save as a file with `ggsave()`.
#' @inheritParams render_drake_ggraph
render_static_drake_graph <- function(
  graph_info,
  main = graph_info$default_title
) {
  .Deprecated(
    new = "render_drake_ggraph",
    package = "drake",
    msg = paste(
      "render_static_drake_graph() is deprecated.",
      "Use render_drake_ggraph() instead."
    )
  )
  render_drake_ggraph(graph_info = graph_info, main = main)
}

#' @title `drake` now only uses one hash algorithm per cache.
#' `r lifecycle::badge("deprecated")`
#' @export
#' @keywords internal
#' @description Deprecated on 2018-12-12.
#' @return A character vector naming a hash algorithm.
#' @inheritParams cached
short_hash <- function(
  cache = drake::get_cache(verbose = verbose),
  verbose = 1L
) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = "short_hash() in drake is deprecated."
  )
  # nocov start
  if (!cache$exists(key = "short_hash_algo", namespace = "config")) {
    return(NULL)
  }
  chosen_algo <- cache$get("short_hash_algo", namespace = "config")
  cache$get("short_hash_algo", namespace = "config")
  # nocov end
}

deprecate_hash_algo_args <- function(
  short_hash_algo = NULL,
  long_hash_algo = NULL
) {
  if (!is.null(short_hash_algo) || !is.null(long_hash_algo)) {
    warn0(
      "The long_hash_algo and short_hash_algo arguments to drake functions ",
      "are deprecated. drake now uses only one hash algorithm, ",
      "which you can set ",
      "with the hash_algorithm argument in new_cache()."
    )
  }
}

#' @title Deprecated: show a `ggraph`/`ggplot2` representation
#'   of your drake project.
#' `r lifecycle::badge("deprecated")`
#' @description Use [drake_ggraph()] instead.
#' @details Deprecated on 2018-07-25.
#' @export
#' @keywords internal
#' @return A `ggplot2` object, which you can modify with more layers,
#'   show with `plot()`, or save as a file with `ggsave()`.
#' @inheritParams drake_ggraph
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
    new = "drake_ggraph",
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

#' @title List the old drake triggers.
#' `r lifecycle::badge("deprecated")`
#' @export
#' @keywords internal
#' @description Triggers are target-level rules
#' that tell [make()] how to know if a target
#' is outdated or up to date.
#' @details Deprecated on 2018-07-22.
#' @return A character vector with the names of the old triggers.
triggers <- function() {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste(
      "triggers() in drake is deprecated",
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
  warn0(
    "The old trigger interface in drake is deprecated. ",
    "See the trigger() function (singular) ",
    "to learn about the new trigger interface."
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
    warn0(
      "Argument `fetch_cache` is deprecated."
    ) # 2018-12-08 # nolint
  }
}

deprecate_targets_only <- function(targets_only) {
  if (!is.null(targets_only)) {
    warn0(
      "Argument `targets_only` is deprecated. ",
      "build_times(), graph visualizations, and runtime predictions ",
      "now always focus only on the targets (ignoring the imports)."
    ) # build times, vis, and predictions: 2019-01-03 # nolint
  }
}

#' @title Load the main example.
#' `r lifecycle::badge("deprecated")`
#' @description The main example lives at
#' `https://github.com/wlandau/drake-examples/tree/main/main`.
#' Use `drake_example("main")` to download its code.
#' This function also writes/overwrites
#' the files `report.Rmd` and `raw_data.xlsx`.
#' @export
#' @return A [drake_config()] configuration list.
#' @param envir The environment to load the example into.
#'   Defaults to your workspace.
#'   For an insulated workspace,
#'   set `envir = new.env(parent = globalenv())`.
#' @param report_file Where to write the report file `report.Rmd`.
#' @param overwrite Logical, whether to overwrite an
#'   existing file `report.Rmd`
#' @param force Deprecated.
#' @keywords internal
#' @details Deprecated 2018-12-31.
load_main_example <- function(
  envir = parent.frame(),
  report_file = "report.Rmd",
  overwrite = FALSE,
  force = FALSE
) {
  deprecate_force(force)
  .Deprecated(
    new = "drake_example",
    package = "drake",
    msg = paste("load_main_example() in drake is deprecated.",
                'Use drake_example("main") instead.')
  )
  dir <- tempfile()
  drake_example(example = "main", to = dir)
  source(file.path(dir, "main", "R", "packages.R"), local = envir)
  source(file.path(dir, "main", "R", "functions.R"), local = envir)
  envir$plan <- source(
    file.path(dir, "main", "R", "plan.R"),
    local = TRUE
  )$value
  for (file in c("report.Rmd", "raw_data.xlsx")) {
    if (file.exists(file) && overwrite) {
      warn0("Overwriting file ", file)
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
#' `r lifecycle::badge("deprecated")`
#' @description This function deletes files. Use at your own risk.
#'   Destroys the `.drake/` cache and the `report.Rmd` file
#'   in the current working directory. Your working directory
#'   (`getcwd()`) must be the folder from which you first ran
#'   `load_main_example()` and `make(my_plan)`.
#' @export
#' @return Nothing.
#' @keywords internal
#' @details Deprecated 2018-12-31.
clean_main_example <- function() {
  deprecate_force(force)
  .Deprecated(
    new = "clean",
    package = "drake",
    msg = paste("clean_main_example() in drake is deprecated.")
  )
  clean(destroy = TRUE, search = FALSE)
  unlink(c("report.Rmd", "raw_data.xlsx"))
  invisible()
}

#' @title Default verbosity
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-01-01
#' @export
#' @keywords internal
#' @return 1
default_verbose <- function() {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste("default_verbose() in drake is deprecated.")
  )
  1L
}

#' @title Put quotes around each element of a character vector.
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-01-01
#' @export
#' @keywords internal
#' @return Character vector with quotes around it.
#' @param x Character vector or object to be coerced to character.
#' @param single Add single quotes if `TRUE`
#'   and double quotes otherwise.
drake_quotes <- function(x = NULL, single = FALSE) {
  .Deprecated(
    new = "",
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
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-01-01
#' @export
#' @keywords internal
#' @return Character vector without leading
#'   or trailing escaped quotes around
#'   the elements.
#' @param x Character vector.
drake_unquote <- function(x = NULL) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste("drake_unquote() is deprecated.")
  )
  gsub(pattern = "^(?:'(.*)'|\"(.*)\")$", replacement = "\\1\\2", x = x)
}

#' @title Turn valid expressions into character strings.
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-01-01
#' @export
#' @keywords internal
#' @return A character vector.
#' @param ... Unquoted symbols to turn into character strings.
drake_strings <- function(...) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste("drake_strings() is deprecated.")
  )
  args <- structure(as.list(match.call()[-1]), class = "uneval")
  keys <- names(args)
  out <- as.character(args)
  names(out) <- keys
  out
}

#' @title List all the built targets (non-imports) in the cache.
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-01-08.
#' @details Targets are listed in the workflow plan
#' data frame (see [drake_plan()].
#' @seealso [cached()], [loadd()]
#' @export
#' @keywords internal
#' @return Character vector naming the built targets in the cache.
#' @inheritParams cached
#' @param jobs Number of jobs/workers for parallel processing.
built <- function(
  path = getwd(), search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1L,
  jobs = 1
) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste(
      "built() in drake is deprecated.",
      "Use cached(targets_only = TRUE)) instead."
    )
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
  redisplay_keys(out)
}

#' @title Search up the file system
#'   for the nearest root path of a drake project.
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-01-08.
#' @details Only works if the cache is a file system
#' in a folder named `.drake` (default).
#' @export
#' @keywords internal
#' @seealso [drake_plan()], [make()]
#' @return File path of the nearest drake project or `NULL`
#'   if no drake project is found.
#' @param path Starting path for search back for the project.
#'   Should be a subdirectory of the drake project.
find_project <- function(path = getwd()) {
  .Deprecated(
    "find_cache",
    package = "drake",
    msg = paste(
      "find_project() in drake is deprecated.",
      "Use find_cache() instead."
    )
  )
  cache <- find_cache(path = path)
  if (is.null(cache)) {
    return(NULL)
  }
  return(dirname(cache))
}

#' @title Default arguments of Makefile parallelism
#' `r lifecycle::badge("deprecated")`
#' @description 2019-01-03
#' @export
#' @keywords internal
#' @return `args` for `system2(command, args)`
#' @inheritParams drake_config
#' @param jobs Number of jobs.
default_Makefile_args <- function(jobs, verbose) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste(
      "default_Makefile_args() and",
      "Makefile parallelism in drake are deprecated."
    )
  )
  out <- paste0("--jobs=", head(jobs, 1))
  if (verbose < 1) {
    out <- c(out, "--silent")
  }
  return(out)
}

#' @title Default Makefile command
#' `r lifecycle::badge("deprecated")`
#' @description 2019-01-03
#' @keywords internal
#' @return A character scalar
#' @export
default_Makefile_command <- function() {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste(
      "default_Makefile_command() and",
      "Makefile parallelism in drake are deprecated."
    )
  )
  "make"
}

#' @title Default Makefile recipe
#' `r lifecycle::badge("deprecated")`
#' @export
#' @keywords internal
#' @description 2019-01-03
#' @return A character scalar
#' @param recipe_command Character scalar.
#' @param target Character scalar.
#' @param cache_path Character scalar.
Makefile_recipe <- function( # nolint
  recipe_command = drake::default_recipe_command(),
  target = "your_target",
  cache_path = NULL
) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste(
      "Makefile_recipe() and",
      "Makefile parallelism in drake are deprecated."
    )
  )
  character(0)
}

#' @title Default Makefile recipe command
#' `r lifecycle::badge("deprecated")`
#' @export
#' @keywords internal
#' @description 2019-01-02
#' @return A character scalar with the default recipe command.
default_recipe_command <- function() {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste(
      "default_recipe_command() and",
      "Makefile parallelism in drake are deprecated."
    )
  )
  paste0("Rscript -e '", r_recipe_wildcard(), "'")
}

#' @title Default Makefile recipe wildcard
#' `r lifecycle::badge("deprecated")`
#' @export
#' @keywords internal
#' @description 2019-01-02
#' @return The R recipe wildcard.
r_recipe_wildcard <- function() {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste(
      "r_recipe_wildcard() and",
      "Makefile parallelism in drake are deprecated."
    )
  )
  "R_RECIPE"
}

#' @title Names of old parallel backends
#' `r lifecycle::badge("deprecated")`
#' @description 2019-01-03
#' @export
#' @keywords internal
#' @return character vector
#' @param distributed_only Logical.
parallelism_choices <- function(distributed_only = FALSE) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste(
      "parallelism_choices() and",
      "Makefile parallelism in drake are deprecated."
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

#' @title Shell file for Makefile parallelism
#' `r lifecycle::badge("deprecated")`
#' @description 2019-01-03
#' @export
#' @keywords internal
#' @return logical
#' @param path Character.
#' @param overwrite Logical.
shell_file <- function(
  path = "shell.sh",
  overwrite = FALSE
) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste(
      "shell_file() and",
      "Makefile parallelism in drake are deprecated."
    )
  )
  FALSE
}

#' @title Default parallel backend
#' `r lifecycle::badge("deprecated")`
#' @description 2019-01-02
#' @export
#' @keywords internal
#' @return character
default_parallelism <- function() {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste(
      "default_parallelism() in drake is deprecated."
    )
  )
  "loop"
}

#' @title Just process the imports
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-01-04
#' @export
#' @keywords internal
#' @seealso [make()], [drake_config()]
#' @return nothing
#' @param config A configuration list returned by [drake_config()].
make_imports <- function(config) {
  .Deprecated(
    new = "make",
    package = "drake",
    msg = paste(
      "make_imports() in drake is deprecated. Use make()."
    )
  )
  config$settings$skip_imports <- FALSE
  config$settings$skip_targets <- TRUE
  make_impl(config = config)
}

#' @title Just make the targets
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-01-04
#' @export
#' @keywords internal
#' @seealso [make()], [drake_config()]
#' @return nothing
#' @param config A configuration list returned by [drake_config()].
make_targets <- function(config) {
  .Deprecated(
    new = "make",
    package = "drake",
    msg = paste(
      "make_targets() in drake is deprecated. Use make()."
    )
  )
  config$settings$skip_imports <- TRUE
  config$settings$skip_targets <- FALSE
  make_impl(config = config)
}

#' @title Apply make() with a pre-computed config object
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-01-04
#' @export
#' @keywords internal
#' @seealso [make()], [drake_config()]
#' @return nothing
#' @param config A configuration list returned by [drake_config()].
make_with_config <- function(config) {
  .Deprecated(
    new = "make",
    package = "drake",
    msg = paste(
      "make_with_config() in drake is deprecated. Use make()."
    )
  )
  make_impl(config = config)
}

#' @title Read a config object from the cache
#' `r lifecycle::badge("deprecated")`
#' @description drake no longer stores the config object,
#'   the plan, etc. in the cache during `make()`. This change
#'   improves speed.
#' @details 2019-01-06
#' @export
#' @keywords internal
#' @inheritParams cached
read_drake_config <- function(
  path = getwd(),
  search = TRUE,
  cache = NULL,
  verbose = 1L,
  jobs = 1,
  envir = parent.frame()
) {
  .Deprecated(
    new = "",
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

#' @title Read a workflow graph from the cache
#' `r lifecycle::badge("deprecated")`
#' @description drake no longer stores the config object,
#'   the plan, etc. in the cache during `make()`. This change
#'   improves speed.
#' @details 2019-01-06
#' @export
#' @keywords internal
#' @inheritParams cached
read_drake_graph <- function(
  path = getwd(),
  search = TRUE,
  cache = NULL,
  verbose = 1L
) {
  .Deprecated(
    new = "",
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

#' @title Read the plan from the cache
#' `r lifecycle::badge("deprecated")`
#' @description drake no longer stores the config object,
#'   the plan, etc. in the cache during `make()`. This change
#'   improves speed.
#' @details 2019-01-06
#' @export
#' @keywords internal
#' @inheritParams cached
read_drake_plan <- function(
  path = getwd(),
  search = TRUE,
  cache = NULL,
  verbose = 1L
) {
  .Deprecated(
    new = "",
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

#' @title List all the imports in the drake cache.
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-01-08.
#' @details An import is a non-target object processed
#' by [make()]. Targets in the workflow
#' plan data frame (see [drake_config()]
#' may depend on imports.
#' @seealso [cached()], [loadd()]
#' @export
#' @keywords internal
#' @return Character vector naming the imports in the cache.
#' @inheritParams cached
#' @param files_only Logical, whether to show imported files only
#'   and ignore imported objects. Since all your functions and
#'   all their global variables are imported, the full list of
#'   imported objects could get really cumbersome.
#' @param jobs Number of jobs/workers for parallel processing.
imported <- function(
  files_only = FALSE, path = getwd(), search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1L,
  jobs = 1
) {
  .Deprecated(
    new = "cached",
    package = "drake",
    msg = paste(
      "imported() in drake is deprecated. Instead, use ",
      "setdiff(cached(targets_only = FALSE), cached(targets_only = TRUE))."
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
  redisplay_keys(targets)
}

#' @title Prune the graph
#' `r lifecycle::badge("deprecated")`
#' @export
#' @keywords internal
#' @description 2019-01-08
#' @return An `igraph` object
#' @param graph An igraph object.
#' @param to Character vector of vertices.
#' @param jobs Number of jobs for parallelism.
prune_drake_graph <- function(
  graph, to = igraph::V(graph)$name, jobs = 1
) {
  .Deprecated(
    new = "",
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

#' @title Show the analysis wildcard
#'   used in [plan_summaries()].
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-01-12.
#' @details Used to generate workflow plan data frames.
#' @export
#' @keywords internal
#' @seealso [plan_summaries()]
#' @return The analysis wildcard used in [plan_summaries()].
analysis_wildcard <- function() {
  .Deprecated(
    new = "",
    package = "drake",
    msg = "analysis_wildcard() in drake is deprecated."
  )
  analysis_wildcard_()
} #

#' @title Return the file path where the cache is stored,
#' if applicable.
#' `r lifecycle::badge("deprecated")`
#' @export
#' @keywords internal
#' @description Deprecated on 2019-01-12.
#' @details Currently only works with
#' [storr::storr_rds()] file system caches.
#' @return File path where the cache is stored.
#' @param cache The cache whose file path you want to know.
cache_path <- function(cache = NULL) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = "cache_path() in drake is deprecated."
  )
  cache$driver$path
}

#' @title List all the `storr` cache namespaces used by drake.
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-01-12.
#' @return A character vector of `storr` namespaces used for drake.
#' @details Ordinary users do not need to worry about this function.
#' It is just another window into `drake`'s internals.
#' @param default Name of the default `storr` namespace.
#' @export
#' @keywords internal
#' @seealso [make()]
cache_namespaces <- function(
  default = storr::storr_environment()$default_namespace
) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = "cache_namespaces() in drake is deprecated."
  )
  out <- c(
    target_namespaces_(default = default),
    "change",   # value returned by the "change" trigger
    "config",   # elements of the config list
    "memoize",  # for the memoization in preprocessing
    "progress", # build progress: running, done, failed, etc.
    "session"   # session info
  )
  sort(out)
}

#' @title Check a workflow plan data frame for obvious errors.
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-01-12.
#' @details Possible obvious errors include circular dependencies and
#' missing input files.
#' @seealso [drake_plan()], [make()]
#' @export
#' @keywords internal
#' @return Invisibly return `plan`.
#' @inheritParams cached
#' @param plan Workflow plan data frame, possibly from
#'   [drake_plan()].
#' @param targets Character vector of targets to make.
#' @param envir Environment containing user-defined functions.
#' @param cache Optional drake cache. See [new_cache()].
check_plan <- function(
  plan = NULL,
  targets = NULL,
  envir = parent.frame(),
  cache = drake::get_cache(verbose = verbose),
  verbose = 1L,
  jobs = 1
) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = "check_plan() in drake is deprecated."
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
#' `r lifecycle::badge("deprecated")`
#' @details Used to generate workflow plan data frames.
#' @description Deprecated on 2019-01-12.
#' @export
#' @keywords internal
#' @seealso [plan_analyses()]
#' @return The dataset wildcard used in
#'   [plan_analyses()] and [plan_summaries()].
dataset_wildcard <- function() {
  .Deprecated(
    new = "",
    package = "drake",
    msg = "dataset_wildcard() in drake is deprecated."
  )
  dataset_wildcard_()
}

#' @title Compute the initial pre-build metadata
#'   of a target or import.
#' `r lifecycle::badge("deprecated")`
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
#' @seealso [diagnose()], [deps_profile()], [make()]
#' @export
#' @keywords internal
#' @return A list of metadata on a target. Does not include
#'   the file modification time if the target is a file.
#'   That piece is computed later in [make()] by
#'   `drake:::store_outputs()`.
#' @param target Character scalar, name of the target
#'   to get metadata.
#' @param config Top-level internal configuration list produced
#'   by [drake_config()].
drake_meta <- function(target, config) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = "drake_meta() is deprecated."
  )
  drake_meta_(target, config)
}

#' @title Show drake's color palette.
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-01-12.
#' @export
#' @keywords internal
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
    new = "",
    package = "drake",
    msg = "drake_palette() is deprecated."
  )
  drake_palette_()
}

# Show drake's color palette.
drake_palette_ <- function() {
  assert_pkg("crayon")
  colors <- c(
    default = "dodgerblue3",
    target = "green3",
    recover = "dodgerblue3",
    retry = "#9400d3",
    missing = "#9400d3",
    fail = "red",
    up_to_date = "forestgreen",
    outdated = "#000000",
    failed = "#aa0000",
    import_node = "dodgerblue3",
    missing_node = "darkorchid3",
    running = "#ff7221",
    other = "#888888"
  )
  for (i in seq_along(colors)) {
    message(crayon::make_style(colors[i])(names(colors)[i]))
  }
}

#' @title Output a random tip about drake.
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-01-12.
#' @details Tips are usually related to news and usage.
#' @export
#' @keywords internal
#' @return A character scalar with a tip on how to use drake.
drake_tip <- function() {
  .Deprecated(
    new = "",
    package = "drake",
    msg = "drake_tip() is deprecated."
  )
  drake_tip_()
}

#' @title List the targets in progress
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-01-13.
#' @details Similar to [progress()].
#' @seealso [diagnose()], [drake_get_session_info()],
#'   [cached()], [readd()], [drake_plan()], [make()]
#' @export
#' @keywords internal
#' @return A character vector of target names.
#' @inheritParams cached
in_progress <- function(
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(
    path = path,
    search = search,
    verbose = verbose
  ),
  verbose = 1L
) {
  .Deprecated(
    new = "running",
    package = "drake",
    msg = "in_progress() in drake is deprecated. Use running() instead."
  )
  drake_running(path = path, cache = cache)
}

#' @title Load or create a drake cache
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-01-13.
#' @export
#' @keywords internal
#' @seealso [new_cache()], [get_cache()]
#' @details Does not work with
#' in-memory caches such as [storr::storr_environment()].
#' @return A drake/storr cache.
#' @inheritParams cached
#' @inheritParams new_cache
#' @inheritParams get_cache
#' @inheritParams drake_config
#' @param path File path of the cache.
#' @param force Logical, whether to load the cache
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
    new = "get_cache",
    package = "drake",
    msg = "recover_cache() in drake is deprecated. Use get_cache() instead."
  )
  deprecate_force(force)
  deprecate_hash_algo_args(short_hash_algo, long_hash_algo)
  recover_cache_(path, hash_algorithm)
}

#' @title Storr namespaces for targets
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-01-13.
#' @export
#' @keywords internal
#' @seealso [make()]
#' @return A character vector of `storr` namespaces that store
#'   target-level information.
#' @details Ordinary users do not need to worry about this function.
#' It is just another window into `drake`'s internals.
#' @param default Name of the default `storr` namespace.
target_namespaces <- function(
  default = storr::storr_environment()$default_namespace
) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = "target_namespaces() in drake is deprecated."
  )
  target_namespaces_(default)
}

#' @title Specialized wildcard for analyses
#' `r lifecycle::badge("deprecated")`
#' @description Use [drake_plan()] instead.
#'   See `https://books.ropensci.org/drake/plans.html#large-plans`
#'   for details.
#' @details 2019-01-13
#' @export
#' @keywords internal
#' @return An evaluated workflow plan data frame of analysis targets.
#' @param plan Workflow plan data frame of analysis methods.
#'   The commands in the `command` column must
#'   have the `dataset__` wildcard where the datasets go.
#'   For example, one command could be `lm(dataset__)`. Then,
#'   the commands in the output will include `lm(your_dataset_1)`,
#'   `lm(your_dataset_2)`, etc.
#' @param datasets Workflow plan data frame with instructions
#'   to make the datasets.
#' @param sep character Scalar, delimiter for creating
#'   the names of new targets.
plan_analyses <- function(plan, datasets, sep = "_") {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste(
      "plan_analyses() in drake is deprecated.",
      "Use drake_plan() with transformations instead. See",
      "https://books.ropensci.org/drake/plans.html#large-plans",
      "for details."
    )
  )
  evaluate_plan(
    plan,
    wildcard = dataset_wildcard_(),
    values = datasets$target,
    sep = sep
  )
}

#' @title Specialized wildcard for summaries
#' `r lifecycle::badge("deprecated")`
#' @description Use [drake_plan()] with transformations instead. See
#'   `https://books.ropensci.org/drake/plans.html#large-plans`
#'   for details.
#' @details 2019-01-13
#' @export
#' @keywords internal
#' @return An evaluated workflow plan data frame of instructions
#'   for computing summaries of analyses and datasets.
#'   analyses of multiple datasets in multiple ways.
#' @param plan Workflow plan data frame with commands for the summaries.
#'   Use the `analysis__` and `dataset__` wildcards
#'   just like the `dataset__` wildcard in [plan_analyses()].
#' @param analyses Workflow plan data frame of analysis instructions.
#' @param datasets Workflow plan data frame with instructions to make
#'   or import the datasets.
#' @param gather Character vector, names of functions to gather the
#'   summaries. If not `NULL`, the length must be the number of
#'   rows in the `plan`. See the [gather_plan()] function
#'   for more.
#' @param sep Character scalar, delimiter for creating the
#'   new target names.
plan_summaries <- function(
  plan,
  analyses,
  datasets,
  gather = rep("list", nrow(plan)),
  sep = "_"
) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste(
      "plan_summaries() in drake is deprecated.",
      "Use drake_plan() with transformations instead:",
      "https://books.ropensci.org/drake/plans.html#large-plans"
    )
  )
  plan <- with_analyses_only(plan)
  out <- plan
  group <- paste(colnames(out), collapse = sep)
  out[[group]] <- out$target
  if (!any(grepl(analysis_wildcard_(), out$command, fixed = TRUE))) {
    stop(
      "no 'analysis__' wildcard found in plan$command. ",
      "Use plan_analyses() instead."
    )
  }
  out <- evaluate_plan(
    out,
    wildcard = analysis_wildcard_(),
    values = analyses$target,
    sep = sep
  )
  out <- evaluate_plan(
    out,
    wildcard = dataset_wildcard_(),
    values = datasets$target,
    expand = FALSE,
    sep = sep
  )
  if (!length(gather)) {
    return(out[setdiff(names(out), group)])
  }
  if (length(gather) == 1) {
    gather <- rep(gather, dim(plan)[1])
  }
  if (!(length(gather) == dim(plan)[1])) {
    stop("gather must be NULL or have length 1 or nrow(plan)")
  }
  gathered <- map_by(
    .x = out,
    .by = group,
    .f = function(x) {
      summary_type <- x[[group]][1]
      gather_plan(
        x,
        target = summary_type,
        gather = gather[which(summary_type == plan$target)],
        append = FALSE
      )
    }
  )
  target <- command <- NULL
  out <- bind_plans(gathered, out)
  out[, c("target", "command")]
}

with_analyses_only <- function(plan) {
  has_analysis <- grepl(analysis_wildcard_(), plan$command, fixed = TRUE)
  if (any(!has_analysis)) {
    warn0(
      "removing ",
      sum(has_analysis),
      " rows with no 'analysis__' wildcard in the command.",
      "Use plan_analyses() for these."
    )
  }
  return(plan[has_analysis, ])
}

# Show the analysis wildcard used in [plan_summaries()].
analysis_wildcard_ <- function() {
  "analysis__"
}

# Show the dataset wildcard used in
# [plan_analyses()] and [plan_summaries()].
dataset_wildcard_ <- function() {
  "dataset__"
}

#' @title Auxiliary storr namespaces
#' `r lifecycle::badge("deprecated")`
#' @description 2019-02-13
#' @export
#' @keywords internal
#' @return A character vector of `storr` namespaces
#'   that are cleaned during [clean()].
#' @param default Name of the default `storr` namespace.
cleaned_namespaces <- function(
  default = storr::storr_environment()$default_namespace
) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = "cleaned_namespaces() in drake is deprecated."
  )
  out <- c(default, "meta")
  sort(out)
}

#' @title Dependencies of a knitr report
#' `r lifecycle::badge("deprecated")`
#' @export
#' @keywords internal
#' @description Deprecated on 2019-02-14
#' `knit("your_report.Rmd")` or
#' `knit("your_report.Rmd", quiet = TRUE)`.
#' @return Data frame of dependencies
#' @param target Encoded file path
knitr_deps <- function(target) {
  .Deprecated(
    new = "deps_knitr",
    package = "drake",
    msg = "knitr_deps() in drake is deprecated. Use deps_knitr() instead."
  )
  deps_knitr(target)
}

#' @title States of the dependencies of a target
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-02-14.
#' @return A data frame of the old hashes and
#'   new hashes of the data frame, along with
#'   an indication of which hashes changed since
#'   the last [make()].
#' @export
#' @keywords internal
#' @inheritParams deps_profile
dependency_profile <- function(
  target,
  config,
  character_only = FALSE
) {
  .Deprecated(
    new = "deps_profile",
    package = "drake",
    msg = paste(
      "dependency_profile() in drake is deprecated.",
      "Use deps_profile() instead."
    )
  )
  if (!character_only) {
    target <- as.character(substitute(target))
  }
  deps_profile(
    target = target,
    config = config,
    character_only = TRUE
  )
}

#' @title Predict parallel computing behavior
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-02-14.
#' @export
#' @keywords internal
#' @return A data frame showing one likely arrangement
#'   of targets assigned to parallel workers.
#' @inheritParams predict_workers
predict_load_balancing <- function(
  config,
  targets = NULL,
  from_scratch = FALSE,
  targets_only = NULL,
  jobs = 1,
  known_times = numeric(0),
  default_time = 0,
  warn = TRUE
) {
  .Deprecated(
    new = "predict_workers",
    package = "drake",
    msg = paste(
      "predict_load_balancing() in drake is deprecated.",
      "Use predict_workers() instead."
    )
  )
  worker_prediction_info(
    config,
    targets = targets,
    from_scratch = from_scratch,
    targets_only = targets_only,
    jobs = jobs,
    known_times = known_times,
    default_time = default_time,
    warn = warn
  )$workers
}

#' @title Get the cache at the exact file path specified.
#' `r lifecycle::badge("deprecated")`
#' @export
#' @keywords internal
#' @description This function does not apply to
#' in-memory caches such as `storr_environment()`.
#' @return A drake/storr cache at the specified path, if it exists.
#' @inheritParams cached
#' @inheritParams drake_config
#' @param path File path of the cache.
#' @param force Deprecated.
this_cache <- function(
  path = NULL,
  force = FALSE,
  verbose = 1L,
  fetch_cache = NULL,
  console_log_file = NULL
) {
  .Deprecated(
    new = "get_cache",
    package = "drake",
    msg = paste(
      "this_cache() in drake is deprecated.",
      "Use get_cache() or storr::storr_rds() instead."
    )
  )
  this_cache_(path = path)
}

#' @title Generate a flat text log file to represent the state of
#'   the cache.
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-03-09.
#' @details Calling this function to create a log file and later calling
#'   `make()` makes the log file out of date. Therefore, we recommend using
#'   `make()` with the `cache_log_file` argument to create the cache log. This
#'   way ensures that the log is always up to date with `make()` results.
#' @seealso [drake_cache_log()], [make()], [get_cache()]
#' @export
#' @inheritParams cached
#' @param file character scalar, name of the flat text log file.
#' @param jobs Number of jobs/workers for parallel processing.
#' @param targets_only Logical, whether to output information only on the
#'   targets in your workflow plan data frame. If `targets_only` is `FALSE`, the
#'   output will include the hashes of both targets and imports.
#' @keywords internal
#' @return There is no return value, but a log file is generated.
drake_cache_log_file <- function(
  file = "drake_cache.log",
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1L,
  jobs = 1L,
  targets_only = FALSE
) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste(
      "`drake_cache_log_file()` is deprecated.",
      "To ensure cache log is always up to date, create the cache log using",
      "`make()` with the `cache_log_file` argument."
    )
  )
  drake_cache_log_file_(file, path, search, cache, verbose, jobs, targets_only)
}

#' @title Use wildcard templating to create a
#'   workflow plan data frame from a template data frame.
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-05-16. Use [drake_plan()]
#'   transformations instead. See
#'   `https://books.ropensci.org/drake/plans.html#large-plans`
#'   for the details.
#'
#' @details The commands in workflow plan data frames can have
#' wildcard symbols that can stand for datasets, parameters, function
#' arguments, etc. These wildcards can be evaluated over a set of
#' possible values using `evaluate_plan()`.
#'
#' Specify a single wildcard with the `wildcard`
#' and `values` arguments. In each command, the text in
#' `wildcard` will be replaced by each value in `values`
#' in turn. Specify multiple wildcards with the `rules` argument,
#' which overrules `wildcard` and `values` if
#' not `NULL`. Here, `rules` should be a list with wildcards
#' as names and vectors of possible values as list elements.
#' @export
#' @keywords internal
#' @seealso [drake_plan()]
#' @return A workflow plan data frame with the wildcards evaluated.
#'
#' @param plan Workflow plan data frame, similar to one produced by
#'   [drake_plan()].
#'
#' @param rules Named list with wildcards as names and vectors of
#'   replacements
#'   as values. This is a way to evaluate multiple wildcards at once.
#'   When not `NULL`, `rules` overrules `wildcard` and
#'   `values` if
#'   not `NULL`.
#'
#' @param wildcard Character scalar denoting a wildcard placeholder.
#'
#' @param values Vector of values to replace the wildcard
#'   in the drake instructions. Will be treated as a character vector.
#'   Must be the same length as `plan$command` if `expand` is
#'   `TRUE`.
#'
#' @param expand If `TRUE`, create a new rows in the workflow plan
#'   data frame
#'   if multiple values are assigned to a single wildcard.
#'   If `FALSE`, each occurrence of the wildcard
#'   is replaced with the next entry in the `values` vector,
#'   and the values are recycled.
#'
#' @param rename Logical, whether to rename the targets
#'   based on the values supplied for the wildcards
#'   (based on `values` or `rules`).
#'
#' @param trace Logical, whether to add columns that
#'   trace the wildcard expansion process. These new
#'   columns indicate which targets were evaluated and with which
#'   wildcards.
#'
#' @param columns Character vector of names of columns
#'   to look for and evaluate the wildcards.
#'
#' @param sep Character scalar, separator for the names
#'   of the new targets generated. For example, in
#'   `evaluate_plan(drake_plan(x = sqrt(y__)), list(y__ = 1:2), sep = ".")`,
#'   the names of the new targets are `x.1` and `x.2`.
evaluate_plan <- function(
  plan,
  rules = NULL,
  wildcard = NULL,
  values = NULL,
  expand = TRUE,
  rename = expand,
  trace = FALSE,
  columns = "command",
  sep = "_"
) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste(
      "evaluate_plan() is deprecated. For the new interface, visit",
      "https://books.ropensci.org/drake/plans.html#large-plans"
    )
  )
  plan <- deparse_lang_cols(plan)
  if (!is.null(rules)) {
    check_wildcard_rules(rules)
    plan <- evaluate_wildcard_rules(
      plan = plan,
      rules = rules,
      expand = expand,
      rename = rename,
      trace = trace,
      columns = columns,
      sep = sep
    )
  } else if (!is.null(wildcard) && !is.null(values)) {
    plan <- evaluate_single_wildcard(
      plan = plan,
      wildcard = wildcard,
      values = values,
      expand = expand,
      rename = rename,
      trace = trace,
      columns = columns,
      sep = sep
    )
  }
  sanitize_plan(plan)
}

evaluate_single_wildcard <- function(
  plan,
  wildcard,
  values,
  expand,
  rename,
  trace,
  columns,
  sep
) {
  if (!length(columns)) {
    return(plan)
  }
  if ("target" %in% columns) {
    stop0(
      "'target' cannot be in the `columns` argument of evaluate_plan()."
    )
  }
  missing_cols <- setdiff(columns, colnames(plan))
  if (length(missing_cols)) {
    stop0(
      "some columns you selected for evaluate_plan() are not in the plan:\n",
      multiline_message(missing_cols)
    )
  }
  values <- as.character(values)
  matches <- rep(FALSE, nrow(plan))
  for (col in columns) {
    matches <- matches | grepl(wildcard, plan[[col]], fixed = TRUE)
  }
  if (!any(matches)) {
    return(plan)
  }
  major <- make.names(tempfile(), unique = FALSE, allow_ = TRUE)
  minor <- make.names(tempfile(), unique = FALSE, allow_ = TRUE)
  plan[[major]] <- seq_len(nrow(plan))
  plan[[minor]] <- plan[[major]]
  matching <- plan[matches, ]
  if (expand) {
    matching <- expand_plan(
      matching, values, rename = FALSE, sanitize = FALSE
    )
  }
  matched_targets <- matching$target
  if (rename) {
    matching$target <- paste(matching$target, values, sep = sep)
  }
  values <- rep(values, length.out = nrow(matching))
  for (col in columns) {
    matching[[col]] <- Vectorize(
      function(value, command) {
        gsub(wildcard, value, command, fixed = TRUE)
      }
    )(values, matching[[col]])
  }
  if (trace) {
    matching[[wildcard]] <- values
    matching[[paste0(wildcard, "_from")]] <- matched_targets
  }
  rownames(matching) <- NULL
  rownames(plan) <- NULL
  matching[[minor]] <- seq_len(nrow(matching))
  out <- drake_bind_rows(matching, plan[!matches, ])
  out <- out[order(out[[major]], out[[minor]]), ]
  out[[minor]] <- NULL
  out[[major]] <- NULL
  rownames(out) <- NULL
  out
}

evaluate_wildcard_rules <- function(
  plan, rules, expand, rename, trace, columns, sep
) {
  for (index in seq_len(length(rules))) {
    plan <- evaluate_single_wildcard(
      plan,
      wildcard = names(rules)[index],
      values = rules[[index]],
      expand = expand,
      rename = rename,
      trace = trace,
      columns = columns,
      sep = sep
    )
  }
  plan
}

check_wildcard_rules <- function(rules) {
  stopifnot(is.list(rules))
  wildcards <- names(rules)
  all_values <- unlist(rules)
  for (i in seq_along(wildcards)) {
    matches <- grep(wildcards[i], all_values, fixed = TRUE, value = TRUE)
    if (length(matches)) {
      stop0(
        "No wildcard name can match the name of any replacement value. ",
        "Conflicts: \"", wildcards[i], "\" with:\n",
        multiline_message(paste0("\"", matches, "\""))
      )
    }
    matches <- grep(wildcards[i], wildcards[-i], fixed = TRUE, value = TRUE)
    if (length(matches)) {
      stop0(
        "The name of a wildcard cannot be a substring ",
        "of any other wildcard name. ",
        "Conflicts: \"", wildcards[i], "\" with:\n",
        multiline_message(paste0("\"", matches, "\""))
      )
    }
  }
}

#' @title Deprecated: create replicates of targets.
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-05-16. Use [drake_plan()]
#'   transformations instead. See
#'   `https://books.ropensci.org/drake/plans.html#large-plans`
#'   for the details.
#'
#' @details Duplicates the rows of a workflow plan data frame.
#' Prefixes are appended to the new target names
#' so targets still have unique names.
#' @export
#' @keywords internal
#' @seealso [drake_plan()]
#' @return An expanded workflow plan data frame (with replicated targets).
#' @param plan Workflow plan data frame.
#' @param values Values to expand over. These will be appended to
#'   the names of the new targets.
#' @param rename Logical, whether to rename the targets
#'   based on the `values`. See the examples for a demo.
#' @param sep Character scalar, delimiter between the original
#'   target names and the values to append to create the new
#'   target names. Only relevant when `rename` is `TRUE`.
#' @param sanitize Logical, whether to sanitize the plan.
expand_plan <- function(
  plan, values = NULL, rename = TRUE, sep = "_", sanitize = TRUE
) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste(
      "expand_plan() is deprecated. For the new interface, visit",
      "https://books.ropensci.org/drake/plans.html#large-plans"
    )
  )
  if (!length(values)) {
    return(plan)
  }
  nrows <- nrow(plan)
  repeat_targets <- rep(seq_len(nrows), each = length(values))
  plan <- plan[repeat_targets, ]
  values <- as.character(values)
  values <- rep(values, times = nrows)
  if (rename) {
    plan$target <- paste(plan$target, values, sep = sep)
  }
  rownames(plan) <- NULL
  if (sanitize) {
    plan <- sanitize_plan(plan, allow_duplicated_targets = TRUE)
  }
  plan
}

#' @title Create a plan that maps a function to a grid of arguments.
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-05-16. Use [drake_plan()]
#'   transformations instead. See
#'   `https://books.ropensci.org/drake/plans.html#large-plans`
#'   for the details.
#' @details `map_plan()` is like `base::Map()`:
#'   it takes a function name and a grid of arguments, and
#'   writes out all the commands calls to apply the function to
#'   each row of arguments.
#' @export
#' @keywords internal
#' @seealso [drake_plan()]
#' @return A workflow plan data frame.
#' @param args A data frame (or better yet, a `tibble`)
#'   of function arguments to `fun`.
#'   Here, the column names should be the names of the arguments
#'   of `fun`, and each row of `args` corresponds to a
#'   call to `fun`.
#' @param fun Name of a function to apply the arguments
#'   row-by-row. Supply a symbol if `character_only` is
#'   `FALSE` and a character scalar otherwise.
#' @param id Name of an optional column in `args`
#'   giving the names of the targets. If not supplied,
#'   target names will be generated automatically.
#'   `id` should be a symbol if `character_only` is `FALSE`
#'   and a character scalar otherwise.
#' @param character_only Logical, whether to interpret
#'   the `fun` and `id` arguments as character scalars or symbols.
#' @param trace Logical, whether to append the columns of `args`
#'   to the output workflow plan data frame. The added columns
#'   help "trace back" the original settings that went into building
#'   each target. Similar to the `trace` argument of [drake_plan()].
map_plan <- function(
  args,
  fun,
  id = "id",
  character_only = FALSE,
  trace = FALSE
) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste(
      "map_plan() is deprecated. For the new interface, visit",
      "https://books.ropensci.org/drake/plans.html#large-plans"
    )
  )
  args <- weak_as_tibble(args)
  if (!character_only) {
    fun <- as.character(substitute(fun))
    id <- as.character(substitute(id))
  }
  cols <- setdiff(colnames(args), id)
  if (id %in% colnames(args)) {
    target <- args[[id]]
  } else {
    target <- paste0(
      fun, "_",
      apply(X = args, MARGIN = 1, FUN = digest_murmur32)
    )
  }
  command <- as.character(unlist(drake_pmap(
    .l = args[, cols, drop = FALSE],
    .f = function(...) {
      out <- list(as.name(fun), ...)
      out <- as.call(out)
      safe_deparse(out, backtick = TRUE)
    }
  )))
  out <- weak_tibble(target = target, command = command)
  if (trace) {
    out <- weak_as_tibble(cbind(out, args))
  }
  sanitize_plan(out)
}

#' @title Combine targets
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-05-16. Use [drake_plan()]
#'   transformations instead. See
#'   `https://books.ropensci.org/drake/plans.html#large-plans`
#'   for the details.
#' @details Creates a new workflow plan to aggregate
#'   existing targets in the supplied plan.
#' @export
#' @keywords internal
#' @seealso [drake_plan()]
#' @return A workflow plan data frame that aggregates multiple
#'   prespecified targets into one additional target downstream.
#' @param plan Workflow plan data frame of prespecified targets.
#' @param target Name of the new aggregated target.
#' @param gather Function used to gather the targets. Should be
#'   one of `list(...)`, `c(...)`, `rbind(...)`, or similar.
#' @param append Logical. If `TRUE`, the output will include the
#'   original rows in the `plan` argument.
#'   If `FALSE`, the output will only include the new
#'   targets and commands.
gather_plan <- function(
  plan = NULL,
  target = "target",
  gather = "list",
  append = FALSE
) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste(
      "gather_plan() is deprecated. For the new interface, visit",
      "https://books.ropensci.org/drake/plans.html#large-plans"
    )
  )
  command <- paste(plan$target, "=", plan$target)
  command <- paste(command, collapse = ", ")
  command <- paste0(gather, "(", command, ")")
  new_plan <- weak_tibble(target = target, command = command)
  new_plan <- sanitize_plan(new_plan)
  if (append) {
    bind_plans(plan, new_plan)
  } else {
    new_plan
  }
}

#' @title Gather multiple groupings of targets
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-05-16. Use [drake_plan()]
#'   transformations instead. See
#'   `https://books.ropensci.org/drake/plans.html#large-plans`
#'   for the details.
#' @details Perform several calls to `gather_plan()`
#'   based on groupings from columns in the plan,
#'   and then row-bind the new targets to the plan.
#' @export
#' @keywords internal
#' @seealso [drake_plan()]
#' @return A workflow plan data frame.
#' @inheritParams gather_plan
#' @param ... Symbols, columns of `plan` to define target groupings.
#'   A `gather_plan()` call is applied for each grouping.
#'   Groupings with all `NA`s in the selector variables are ignored.
#' @param prefix Character, prefix for naming the new targets.
#'   Suffixes are generated from the values of the columns
#'   specified in `...`.
#' @param filter An expression like you would pass to `dplyr::filter()`.
#'   The rows for which `filter` evaluates to `TRUE` will be gathered,
#'   and the rest will be excluded from gathering.
#'   Why not just call `dplyr::filter()` before `gather_by()`?
#'   Because `gather_by(append = TRUE, filter = my_column == "my_value")`
#'   gathers on some targets while including all the original targets
#'   in the output. See the examples for a demonstration.
#' @param sep Character scalar, delimiter for creating the names
#'   of new targets.
gather_by <- function(
  plan,
  ...,
  prefix = "target",
  gather = "list",
  append = TRUE,
  filter = NULL,
  sep = "_"
) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste(
      "gather_by() is deprecated. For the new interface, visit",
      "https://books.ropensci.org/drake/plans.html#large-plans"
    )
  )
  gathered <- plan
  if (!is.null(substitute(filter))) {
    filter <- rlang::enquo(filter)
    selection <- rlang::eval_tidy(expr = filter, data = gathered)
    selection[is.na(selection)] <- FALSE
    gathered <- gathered[selection, ]
  }
  col_names <- as.character(match.call(expand.dots = FALSE)$...)
  gathered <- map_by(
    .x = gathered,
    .by = col_names,
    .f = gather_plan,
    target = prefix,
    gather = gather,
    append = FALSE
  )
  cols <- gathered[, col_names, drop = FALSE]
  suffix <- apply(X = cols, MARGIN = 1, FUN = paste, collapse = sep)
  if (length(suffix)) {
    suffix[nzchar(suffix)] <- paste0(sep, suffix[nzchar(suffix)])
    gathered$target <- paste0(gathered$target, suffix)
  }
  if (append) {
    out <- bind_plans(plan, gathered)
  } else {
    out <- gathered
  }
  arrange_plan_cols(out)
}

#' @title Write commands to reduce several targets down to one.
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-05-16. Use [drake_plan()]
#'   transformations instead. See
#'   `https://books.ropensci.org/drake/plans.html#large-plans`
#'   for the details.
#' @details Creates a new workflow plan data frame with the
#'   commands to do a reduction (i.e. to repeatedly apply a binary
#'   operator to pairs of targets to produce one target).
#' @export
#' @keywords internal
#' @seealso [drake_plan()]
#' @return A workflow plan data frame that aggregates multiple
#'   prespecified targets into one additional target downstream.
#' @param plan Workflow plan data frame of prespecified targets.
#' @param target Name of the new reduced target.
#' @param begin Character, code to place at the beginning
#'   of each step in the reduction.
#' @param op Binary operator to apply in the reduction
#' @param end Character, code to place at the end
#'   of each step in the reduction.
#' @param pairwise Logical, whether to create multiple
#'   new targets, one for each pair/step in the reduction (`TRUE`),
#'   or to do the reduction all in one command.
#' @param append Logical. If `TRUE`, the output will include the
#'   original rows in the `plan` argument.
#'   If `FALSE`, the output will only include the new
#'   targets and commands.
#' @param sep Character scalar, delimiter for creating new target names.
reduce_plan <- function(
  plan = NULL,
  target = "target",
  begin = "",
  op = " + ",
  end = "",
  pairwise = TRUE,
  append = FALSE,
  sep = "_"
) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste(
      "reduce_plan() is deprecated. For the new interface, visit",
      "https://books.ropensci.org/drake/plans.html#large-plans"
    )
  )
  if (pairwise) {
    pairs <- reduction_pairs(
      x = plan$target,
      base_name = paste0(target, sep)
    )
    pairs$names[nrow(pairs)] <- target
    command <- paste0(begin, pairs$odds, op, pairs$evens, end)
    out <- weak_tibble(
      target = pairs$names,
      command = command[seq_len(length(pairs$names))]
    )
  } else {
    command <- Reduce(
      x = plan$target,
      f = function(x, y) {
        paste0(begin, x, op, y, end)
      }
    )
    out <- weak_tibble(target = target, command = command)
  }
  out <- sanitize_plan(out)
  if (append) {
    out <- bind_plans(plan, out)
  }
  out
}

#' @title Reduce multiple groupings of targets
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2019-05-16. Use [drake_plan()]
#'   transformations instead. See
#'   `https://books.ropensci.org/drake/plans.html#large-plans`
#'   for the details.
#' @details Perform several calls to `reduce_plan()`
#'   based on groupings from columns in the plan,
#'   and then row-bind the new targets to the plan.
#' @export
#' @keywords internal
#' @seealso [drake_plan()]
#' @return A workflow plan data frame.
#' @inheritParams reduce_plan
#' @param ... Symbols, columns of `plan` to define target groupings.
#'   A `reduce_plan()` call is applied for each grouping.
#'   Groupings with all `NA`s in the selector variables are ignored.
#' @param prefix Character, prefix for naming the new targets.
#'   Suffixes are generated from the values of the columns
#'   specified in `...`.
#' @param filter An expression like you would pass to `dplyr::filter()`.
#'   The rows for which `filter` evaluates to `TRUE` will be gathered,
#'   and the rest will be excluded from gathering.
#'   Why not just call `dplyr::filter()` before `gather_by()`?
#'   Because `gather_by(append = TRUE, filter = my_column == "my_value")`
#'   gathers on some targets while including all the original targets
#'   in the output. See the examples for a demonstration.
#' @param sep Character scalar, delimiter for creating the names
#'   of new targets.
reduce_by <- function(
  plan,
  ...,
  prefix = "target",
  begin = "",
  op = " + ",
  end = "",
  pairwise = TRUE,
  append = TRUE,
  filter = NULL,
  sep = "_"
) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste(
      "reduce_by() is deprecated. For the new interface, visit",
      "https://books.ropensci.org/drake/plans.html#large-plans"
    )
  )
  reduced <- plan
  if (!is.null(substitute(filter))) {
    filter <- rlang::enquo(filter)
    selection <- rlang::eval_tidy(expr = filter, data = reduced)
    selection[is.na(selection)] <- FALSE
    reduced <- reduced[selection, ]
  }
  col_names <- as.character(match.call(expand.dots = FALSE)$...)
  reduced <- map_by(
    .x = reduced,
    .by = col_names,
    .f = reduce_plan,
    target = prefix,
    begin = begin,
    op = op,
    end = end,
    pairwise = pairwise,
    append = FALSE,
    sep = sep
  )
  cols <- reduced[, col_names, drop = FALSE]
  suffix <- apply(X = cols, MARGIN = 1, FUN = paste, collapse = sep)
  if (length(suffix)) {
    suffix[nzchar(suffix)] <- paste0(sep, suffix[nzchar(suffix)])
    reduced$target <- paste0(reduced$target, suffix)
  }
  if (append) {
    out <- bind_plans(plan, reduced)
  } else {
    out <- reduced
  }
  arrange_plan_cols(out)
}

reduction_pairs <- function(x, pairs = NULL, base_name = "reduced_") {
  if (length(x) < 2) {
    return(pairs)
  }
  evens <- x[seq(from = 2, to = length(x), by = 2)]
  odds <- x[seq(from = 1, to = length(x), by = 2)]
  names <- new_x <- paste0(base_name, seq_along(odds) + (nrow(pairs) %||% 0))
  if (length(odds) > length(evens)) {
    evens[length(evens) + 1] <- names[1]
    new_x <- new_x[-1]
  }
  new_pairs <- data.frame(
    names = names, odds = odds, evens = evens,
    stringsAsFactors = FALSE
  )
  reduction_pairs(
    x = new_x,
    pairs = rbind(pairs, new_pairs),
    base_name = base_name
  )
}

#' @title The default cache of a `drake` project.
#' `r lifecycle::badge("deprecated")`
#' @description Use [drake_cache()] instead.
#' @details Deprecated on 2019-05-25.
#' @keywords internal
#' @export
#' @inheritParams cached
#' @inheritParams drake_config
#' @param path Character, either the root file path of a `drake` project
#'   or a folder containing the root (top-level working directory
#'   where you plan to call [make()]).
#'   If this is too confusing, feel free to just use `storr::storr_rds()`
#'   to get the cache.
#'   If `search = FALSE`, `path` must be the root.
#'   If `search = TRUE`, you can specify any
#'   subdirectory of the project. Let's say `"/home/you/my_project"`
#'   is the root. The following are equivalent and correct:
#'   - `get_cache(path = "/home/you/my_project", search = FALSE)`
#'   - `get_cache(path = "/home/you/my_project", search = TRUE)`
#'   - `get_cache(path = "/home/you/my_project/subdir/x", search = TRUE)`
#'   - `get_cache(path = "/home/you/my_project/.drake", search = TRUE)`
#'   - `get_cache(path = "/home/you/my_project/.drake/keys", search = TRUE)`
#' @param force Deprecated.
#' @param fetch_cache Deprecated.
get_cache <- function(
  path = getwd(),
  search = TRUE,
  verbose = 1L,
  force = FALSE,
  fetch_cache = NULL,
  console_log_file = NULL
) {
  .Deprecated(
    new = "",
    package = "drake",
    msg = paste(
      "get_cache() is deprecated. Use drake_cache() instead."
    )
  )
  if (search) {
    path <- find_cache(path = path)
  } else {
    path <- default_cache_path(root = path)
  }
  this_cache_(path = path)
}

# 2019-05-25 # nolint
deprecate_search <- function(search) {
  if (!is.null(search)) {
    warn0(
      "Argument ",
      shQuote("search"),
      " is deprecated in drake functions."
    )
  }
}

# 2019-09-11 # nolint
deprecate_verbose <- function(verbose) {
  if (!identical(verbose, NULL)) {
    warn0(
      "Argument `verbose` is deprecated some minor drake utility functions."
    )
  }
}

# 2019-09-11 # nolint
deprecate_console_log_file <- function(console_log_file) {
  if (!identical(console_log_file, NULL)) {
    warn0(
      "Argument `console_log_file` ",
      "is deprecated some minor drake utility functions."
    )
  }
}

deprecate_arg <- function(value, name, alt = NULL) {
  if (is.null(value)) {
    return()
  }
  msg <- paste("argument", name, "is deprecated.")
  if (!is.null(alt)) {
    msg <- paste(msg, "Use", alt, "instead.")
  }
  warn0(msg)
}

#' @title Deprecated, get a trace of a dynamic target's value.
#' `r lifecycle::badge("deprecated")`
#' @export
#' @keywords internal
#' @description Deprecated on 2019-12-10. Use [read_trace()] instead.
#' @return The dynamic trace of one target in another:
#'   a vector of values from a grouping variable.
#' @param trace Character, name of the trace
#'   you want to extract. Such trace names are declared
#'   in the `.trace` argument of `map()`, `cross()` or `group()`..
#' @param value Value of the dynamic target
get_trace <- function(trace, value) {
  .Deprecated(
    new = "read_trace",
    package = "drake",
    msg = paste(
      "get_trace() is deprecated. Use read_trace() instead."
    )
  )
  attr(value, "dynamic_trace")[[trace]]
}

#' @title List running targets.
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2020-03-23. Use [drake_running()] instead.
#' @export
#' @keywords internal
#' @return A character vector of target names.
#' @inheritParams cached
running <- function(
  path = NULL,
  search = NULL,
  cache = drake::drake_cache(path = path),
  verbose = 1L
) {
  .Deprecated(
    new = "drake_running",
    package = "drake",
    msg = paste(
      "running() is deprecated. Use drake_running() instead."
    )
  )
  deprecate_search(search)
  prog <- drake_progress(path = path, search = search, cache = cache)
  prog$target[prog$progress == "running"]
}

#' @title List failed targets.
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2020-03-23. Use [drake_failed()] instead.
#' @export
#' @keywords internal
#' @return A character vector of target names.
#' @inheritParams cached
#' @param upstream_only Deprecated.
failed <- function(
  path = NULL,
  search = NULL,
  cache = drake::drake_cache(path = path),
  verbose = 1L,
  upstream_only = NULL
) {
  .Deprecated(
    new = "drake_failed",
    package = "drake",
    msg = paste(
      "failed() is deprecated. Use drake_failed() instead."
    )
  )
  deprecate_search(search)
  if (!is.null(upstream_only)) {
    warning("argument upstream_only is deprecated.")
  }
  prog <- drake_progress(path = path, search = search, cache = cache)
  prog$target[prog$progress == "failed"]
}

#' @title Get the build progress of your targets
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2020-03-23. Use [drake_progress()] instead.
#' @export
#' @keywords internal
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
#' @param no_imported_objects Logical, whether to only return information
#'   about imported files and targets with commands (i.e. whether to ignore
#'   imported objects that are not files).
#'
#' @param jobs Number of jobs/workers for parallel processing.
#'
#' @param progress Character vector for filtering the build progress results.
#'   Defaults to `NULL` (no filtering) to report progress of all objects.
#'   Supported filters are `"done"`, `"running"`, and `"failed"`.
progress <- function(
  ...,
  list = character(0),
  no_imported_objects = NULL,
  path = NULL,
  search = NULL,
  cache = drake::drake_cache(path = path),
  verbose = 1L,
  jobs = 1,
  progress = NULL
) {
  .Deprecated(
    new = "drake_progress",
    package = "drake",
    msg = paste(
      "progress() is deprecated. Use drake_progress() instead."
    )
  )
  deprecate_search(search)
  if (is.null(cache)) {
    return(weak_tibble(target = character(0), progress = character(0)))
  }
  cache <- decorate_storr(cache)
  if (!is.null(no_imported_objects)) {
    warn0(
      "Argument no_imported_objects of progress() is deprecated. ",
      "Only targets are returned now."
    )
  }
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

#' @title Deprecated: expose package functions and objects for
#'   analysis with drake.
#' `r lifecycle::badge("deprecated")`
#' @description Deprecated on 2020-06-24.
#' @keywords internal
#' @details Deprecated. This function assigns the objects and functions
#'   from the package environment to the user's environment (usually global)
#'   so `drake` can watch them for changes. This used to be the standard
#'   way to make `drake` compatible with workflows implemented as custom
#'   analysis packages. Now, the recommendation is to supply
#'   `getNamespace("yourPackage")` to the `envir` argument of [make()]
#'   and friends. Read `https://github.com/ropensci/drake/issues/1286`,
#'   especially `https://github.com/ropensci/drake/issues/1286#issuecomment-649088321`, # nolint
#'   for details.
#' @export
#' @return The environment that the exposed imports are loaded into.
#'   Defaults to your R workspace.
#' @param package Name of the package, either a symbol or a string,
#'   depending on `character_only`.
#' @param character_only Logical, whether to interpret `package`
#'   as a character string or a symbol (quoted vs unquoted).
#' @param envir Environment to load the exposed package imports.
#'   You will later pass this `envir` to [make()].
#' @param jobs Number of parallel jobs for the parallel processing
#'   of the imports.
#' @examples
#' # nolint start
#' \dontrun{
#' isolate_example("contain side effects", {
#' # Consider a simple plan that depends on the biglm package.
#' # library(biglm)
#' plan <- drake_plan(model = biglm(y ~ x, data = huge_dataset))
#' # Even if you load the biglm package, drake still ignores
#' # the biglm() function as a dependency. The function is missing
#' # from the graph:
#' # vis_drake_graph(plan)
#' # And if you install an updated version of biglm with a revised
#' # biglm() function, this will not cause drake::make(plan)
#' # to rerun the model.
#' # This is because biglm() is not in your environment.
#' # ls()
#' # biglm() exists in its own special package environment,
#' # which drake does not scan.
#' # ls("package:biglm")
#' # To depend on biglm(), use expose_imports(biglm)
#' # to bring the objects and functions in biglm into
#' # your own (non-package) environment.
#' # expose_imports(biglm)
#' # Now, the biglm() function should be in your environment.
#' # ls()
#' # biglm() now appears in the graph.
#' # vis_drake_graph(plan)
#' # And subsequent make()s respond to changes to biglm()
#' # and its dependencies.
#' })
#' }
#' # nolint end
expose_imports <- function(
  package,
  character_only = FALSE,
  envir = parent.frame(),
  jobs = 1
) {
  .Deprecated(
    package = "drake",
    msg = paste(
      "expose_imports() is deprecated because of the edge cases it causes.",
      "Instead, please supply getNamespace('yourPackage') to the envir",
      "argument of make() and related functions. See",
      "https://github.com/ropensci/drake/issues/1286 for details."
    )
  )
  force(envir)
  if (!character_only) {
    package <- as.character(substitute(package))
  }
  expose_envir(from = getNamespace(package), to = envir, jobs = jobs)
}

expose_envir <- function(from, to, jobs, keep = names(from)) {
  from <- as.list(from, all.names = TRUE)[keep]
  from <- list2env(from, parent = globalenv())
  lightly_parallelize(
    X = ls(from, all.names = TRUE),
    FUN = function(name) {
      value <- get(name, envir = from)
      if (typeof(value) == "closure") {
        assign(
          x = name,
          envir = to,
          value = `environment<-`(value, from)
        )
      } else {
        assign(x = name, envir = to, value = value)
      }
    },
    jobs = jobs
  )
  to
}
