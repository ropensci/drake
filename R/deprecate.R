#' @title Deprecated function `analyses`
#' @description Use [plan_analyses()] instead.
#' @details Deprecated on 2017-11-12.
#' @export
#' @keywords internal
#' @seealso [plan_analyses()]
#' @return The same return value as [plan_analyses()].
#' @param plan Same as for [plan_analyses()].
#' @param datasets Same as for [plan_analyses()].
#' @examples
#' # See ?plan_analyses for examples.
analyses <- function(plan, datasets){
  .Deprecated(
    "backend",
    package = "drake",
    msg = paste(
      "drake::analyses() is deprecated",
      "due to possible name conflicts.",
      "Use plan_analyses() instead."
    )
  )
  plan_analyses(plan = plan, datasets = datasets)
}

#' @title Deprecated function `as_drake_filename`
#' @description Use [file_store()] instead.
#' @details Deprecated on 2018-03-02.
#' @export
#' @keywords internal
#' @seealso [file_store()]
#' @return The same return value as [file_store()].
#' @param x Same as for [file_store()].
#' @examples
#' # See ?file_store for examples.
as_drake_filename <- function(x){
  .Deprecated(
    "backend",
    package = "drake",
    msg = paste(
      "drake::as_drake_filename() is deprecated.",
      "Use file_store() instead."
    )
  )
  file_store(x)
}

#' @title Deprecated function `as_file`
#' @description Use [file_store()] instead.
#' @details Deprecated on 2017-11-12.
#' @export
#' @keywords internal
#' @seealso [file_store()]
#' @return The same return value as [file_store()].
#' @param x Same as for [file_store()].
#' @examples
#' # See ?file_store for examples.
as_file <- function(x){
  .Deprecated(
    "backend",
    package = "drake",
    msg = paste(
      "drake::as_file() is deprecated",
      "due to possible name conflicts.",
      "Use file_store() instead."
    )
  )
  file_store(x)
}

#' @title Deprecated function `backend`
#' @description Use [future::plan()] instead.
#' Avoid `drake::plan()`.
#' @details Deprecated on 2017-11-12.
#' @export
#' @keywords internal
#' @return The same return value as `future::plan()`.
#' @param ... Arguments to `future::plan()`.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' # Choose future's multicore parallel backend.
#' library(future)
#' future::plan(multicore) # Instead of backend(). Avoid drake::plan().
#' # Run the project, build the targets.
#' # Future knows that you chose the multicore backend.
#' make(my_plan, parallelism = "future_lapply")
#' })
#' }
backend <- function(...){
  .Deprecated(
    "backend",
    package = "drake",
    msg = paste(
      "drake::backend() is deprecated.",
      "Use future::plan() directly.",
      "drake::backend() only exists because of a name conflict",
      "between drake::plan() and future::plan().",
      "Once enough time has passed for users to adjust,",
      "drake::plan() will be removed."
    )
  )
  future::plan(...)
}

#' @title Deprecated function `build_graph`
#' @description Use [build_drake_graph()] instead.
#' @details Deprecated on 2017-11-12.
#' @export
#' @keywords internal
#' @seealso [build_drake_graph()]
#' @return The same return value as [build_drake_graph()].
#' @param plan Same as for [build_drake_graph()].
#' @param targets Same as for [build_drake_graph()].
#' @param envir Same as for [build_drake_graph()].
#' @param verbose Same as for [build_drake_graph()].
#' @param jobs Same as for [build_drake_graph()].
#' @examples
#' # See ?as_drake_filename for examples.
build_graph <- function(
  plan = read_drake_plan(),
  targets = plan$target,
  envir = parent.frame(),
  verbose = 1,
  jobs = 1
){
  .Deprecated(
    "backend",
    package = "drake",
    msg = paste(
      "drake::build_graph() is deprecated",
      "due to possible name conflicts.",
      "Use build_drake_graph() instead."
    )
  )
  build_drake_graph(
    plan = plan, targets = targets, envir = envir, verbose = verbose,
    jobs = jobs
  )
}

#' @title Deprecated function `check`
#' @description Use [check_plan()] instead.
#' @details Deprecated on 2017-11-12.
#' @seealso [check_plan()]
#' @export
#' @keywords internal
#' @return Same as for [check_plan()].
#' @param plan Same as for [check_plan()].
#' @param targets Same as for [check_plan()].
#' @param envir Same as for [check_plan()].
#' @param cache Same as for [check_plan()].
#' @param verbose Same as for [check_plan()].
#' @examples
#' # See ?check_plan for examples.
check <- function(
  plan = read_drake_plan(),
  targets = plan$target,
  envir = parent.frame(),
  cache = drake::get_cache(verbose = verbose),
  verbose = TRUE
){
  .Deprecated(
    "check",
    package = "drake",
    msg = paste(
      "drake::check() is deprecated",
      "due to a conflict with devtools::check().",
      "Use check_plan() instead."
    )
  )
  check_plan(
    plan = plan,
    targets = targets,
    envir = envir,
    cache = cache,
    verbose = verbose
  )
}

#' @title Deprecated function `config`
#' @description Use [drake_config()] instead.
#' @details Deprecated on 2017-11-12.
#' @seealso [drake_config()]
#' @export
#' @keywords internal
#' @return The master internal configuration list of a project.
#' @seealso [drake_config()]
#' @param plan same as for [make()]
#' @param targets same as for [make()]
#' @param envir same as for [make()]
#' @param verbose same as for [make()]
#' @param hook same as for [make()]
#' @param parallelism same as for [make()]
#' @param jobs same as for [make()]
#' @param packages same as for [make()]
#' @param prework same as for [make()]
#' @param prepend same as for [make()]
#' @param command same as for [make()]
#' @param args same as for [make()]
#' @param recipe_command same as for [make()]
#' @param cache same as for [make()]
#' @param timeout same as for [make()]
#' @param cpu same as for [make()]
#' @param elapsed same as for [make()]
#' @param retries same as for [make()]
#' @param force same as for [make()]
#' @param log_progress same as for [drake_config()]
#' @param graph same as for [drake_config()]
#' @param trigger same as for [make()]
#' @param skip_imports same as for [drake_config()]
#' @examples
#' # See ?drake_config for the examples.
config <- function(
  plan = read_drake_plan(),
  targets = plan$target,
  envir = parent.frame(),
  verbose = 1,
  hook = default_hook,
  cache = drake::get_cache(verbose = verbose, force = force),
  parallelism = drake::default_parallelism(),
  jobs = 1,
  packages = rev(.packages()),
  prework = character(0),
  prepend = character(0),
  command = drake::default_Makefile_command(),
  args = drake::default_Makefile_args(
    jobs = jobs,
    verbose = verbose
  ),
  recipe_command = drake::default_recipe_command(),
  timeout = Inf,
  cpu = timeout,
  elapsed = timeout,
  retries = 0,
  force = FALSE,
  log_progress = FALSE,
  graph = NULL,
  trigger = "any",
  skip_imports = FALSE
){
  .Deprecated(
    "evaluate",
    package = "drake",
    msg = paste(
      "drake::config() is deprecated",
      "due to possible name conflicts.",
      "Use drake_config() instead."
    )
  )
  drake_config(
    plan = plan,
    targets = targets,
    envir = envir,
    verbose = verbose,
    hook = hook,
    cache = cache,
    parallelism = parallelism,
    jobs = jobs,
    packages = packages,
    prework = prework,
    prepend = prepend,
    command = command,
    args = args,
    recipe_command = recipe_command,
    timeout = timeout,
    cpu = cpu,
    elapsed = elapsed,
    retries = retries,
    force = force,
    log_progress = log_progress,
    graph = graph,
    trigger = trigger,
    skip_imports = skip_imports
  )
}

#' @title Deprecated function `dataframes_graph`
#' @description Use [drake_graph_info()] instead.
#' @details Deprecated on 2018-06-27.
#' @export
#' @keywords internal
#' @return `args` for \code{\link{system2}(command, args)}
#' @param config deprecated
#' @param from deprecated
#' @param mode deprecated
#' @param order deprecated
#' @param subset deprecated
#' @param build_times deprecated
#' @param digits deprecated
#' @param targets_only deprecated
#' @param split_columns deprecated
#' @param font_size deprecated
#' @param from_scratch deprecated
#' @param make_imports deprecated
#' @param full_legend deprecated
#' @examples
#' # See ?drake_graph_info for examples.
dataframes_graph <- function(
  config = drake::read_drake_config(),
  from = NULL,
  mode = c("out", "in", "all"),
  order = NULL,
  subset = NULL,
  build_times = "build",
  digits = 3,
  targets_only = FALSE,
  split_columns = NULL,
  font_size = 20,
  from_scratch = FALSE,
  make_imports = TRUE,
  full_legend = TRUE
) {
  .Deprecated(
    "dataframes_graph",
    package = "drake",
    msg = paste(
      "dataframes_graph() is deprecated.",
      "Use drake_graph_info() instead."
    )
  )
  drake_graph_info(
    config = config,
    from = from,
    mode = mode,
    order = order,
    subset = subset,
    build_times = build_times,
    digits = digits,
    targets_only = targets_only,
    split_columns = split_columns,
    font_size = font_size,
    from_scratch = from_scratch,
    make_imports = make_imports,
    full_legend = full_legend
  )
}

#' @title Deprecated function `default_system2_args`
#' @description Use [default_Makefile_args()] instead.
#' @details Deprecated on 2017-10.
#' @seealso [default_Makefile_args()]
#' @export
#' @keywords internal
#' @return `args` for \code{\link{system2}(command, args)}
#' @param jobs number of jobs
#' @param verbose logical, whether to be verbose
#' @examples
#' # See ?default_Makefile_args for examples.
default_system2_args <- function(jobs, verbose){
  .Deprecated(
    "default_system2_args",
    package = "drake",
    msg = paste(
      "default_system2_args() is deprecated.",
      "Use default_Makefile_args() instead."
    )
  )
  out <- paste0("--jobs=", jobs)
  if (verbose < 1){
    out <- c(out, "--silent")
  }
  return(out)
}

# Deprecated ..analysis.. and ..dataset.. on 2017-11-12
# in favor of analysis__ and dataset__
deprecate_wildcard <- function(plan, old, replacement){
  if (any(grepl(old, plan$command, fixed = TRUE))){
    warning(
      "The '", old, "' wildcard is deprecated. ",
      "Use '", replacement, "' instead.",
      call. = FALSE
    )
  }
  plan$command <- gsub(
    pattern = old,
    replacement = replacement,
    x = plan$command,
    fixed = TRUE
  )
  plan
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
deps <- function(x){
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
){
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

# Deprecated on 2018-02-15
doc_of_function_call <- function(expr){
  args <- as.list(expr)[-1]
  if (!length(args)){
    return(character(0))
  }
  if (is.null(names(args))){
    names(args) <- rep("", length(args))
  }
  if (!is.null(args$input)){
    as.character(args$input)
  } else {
    unnamed <- which(!nzchar(names(args)))
    if (!length(unnamed)){
      return(character(0))
    }
    input_index <- min(unnamed)
    as.character(args[[input_index]])
  }
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
){
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

#' @title Deprecated function `evaluate`
#' @description Use [evaluate_plan()] instead.
#' @details Deprecated on 2017-11-12.
#' @export
#' @keywords internal
#' @seealso [evaluate_plan()]
#' @return Same as for [evaluate_plan()]
#' @param plan Same as for [evaluate_plan()]
#' @param rules Same as for [evaluate_plan()]
#' @param wildcard Same as for [evaluate_plan()]
#' @param values Same as for [evaluate_plan()]
#' @param expand Same as for [evaluate_plan()]
#' @examples
#' # See ?evaluate_plan for examples.
evaluate <- function(
  plan,
  rules = NULL,
  wildcard = NULL,
  values = NULL,
  expand = TRUE
){
  .Deprecated(
    "evaluate",
    package = "drake",
    msg = paste(
      "drake::evaluate() is deprecated",
      "due to a conflict with evaluate::evaluate().",
      "Use evaluate_plan() instead."
    )
  )
  evaluate_plan(
    plan = plan,
    rules = rules,
    wildcard = wildcard,
    values = values,
    expand = expand
  )
}

#' @title Deprecated function `example_drake`
#' @description Use [drake_example()] instead.
#' @details Deprecated on 2017-11-12.
#' @seealso [drake_example()]
#' @export
#' @keywords internal
#' @return `NULL`
#' @param example Same as for [drake_example()]
#' @param destination Same as for [drake_example()]
#' @examples
#' # See ?drake_example for examples.
example_drake <- function(
  example = "main",
  destination = getwd()
){
  .Deprecated(
    "example_drake",
    package = "drake",
    msg = paste(
      "drake::example_drake() is deprecated.",
      "Use drake_example() instead."
    )
  )
  drake_example(
    example = example,
    destination = destination
  )
}

#' @title Deprecated function `examples_drake`
#' @description Use [drake_examples()] instead.
#' @details Deprecated on 2017-11-12.
#' @seealso [drake_examples()]
#' @export
#' @keywords internal
#' @return Names of all the drake examples.
#' @examples
#' # See ?drake_examples for the examples.
examples_drake <- function() {
  .Deprecated(
    "example_drake",
    package = "drake",
    msg = paste(
      "drake::example_drake() is deprecated.",
      "Use drake_example() instead."
    )
  )
  drake_examples()
}

#' @title Deprecated function `expand`
#' @description Use [expand_plan()] instead.
#' @details Deprecated on 2017-11-12.
#' @export
#' @keywords internal
#' @seealso [expand_plan()]
#' @return Same as for [expand_plan()]
#' @param plan Same as for [expand_plan()]
#' @param values Same as for [expand_plan()]
#' @examples
#' # See ?expand_plan for examples.
expand <- function(
  plan,
  values = NULL
){
  .Deprecated(
    "expand",
    package = "drake",
    msg = paste(
      "drake::expand() is deprecated",
      "due to a conflict with tidyr::expand().",
      "Use expand_plan() instead."
    )
  )
  expand_plan(
    plan = plan,
    values = values
  )
}

#' @title Deprecated function `gather`
#' @description Use [gather_plan()] instead.
#' @details Deprecated on 2017-11-12.
#' @export
#' @keywords internal
#' @seealso [gather_plan()]
#' @return Same as for [gather_plan()]
#' @param plan Same as for [gather_plan()]
#' @param target Same as for [gather_plan()]
#' @param gather Same as for [gather_plan()]
#' @examples
#' # See ?gather_plan for examples.
gather <- function(
  plan = NULL,
  target = "target",
  gather = "list"
){
  .Deprecated(
    "gather",
    package = "drake",
    msg = paste(
      "drake::gather() is deprecated",
      "due to a conflict with tidyr::gather().",
      "Use gather_plan() instead."
    )
  )
  gather_plan(
    plan = plan,
    target = target,
    gather = gather
  )
}

# Deprecated on 2018-02-15
find_knitr_doc <- function(expr, result = character(0)){
  if (!length(expr)){
    return(result)
  }
  if (is.character(expr)) {
    if (is_parsable(expr)) {
      expr <- parse(text = expr)
    } else {
      return(result)
    }
  }
  if (is.function(expr)){
    result <- find_knitr_doc(body(expr), result = result)
  } else if (is.call(expr) & length(expr) > 1){
    does_knitting <-
      is_function_call(expr, package = "knitr", what = "knit") ||
      is_function_call(expr, package = "rmarkdown", what = "render")
    if (does_knitting){
      result <- doc_of_function_call(expr)
    } else {
      result <- lapply(as.list(expr), find_knitr_doc,
                       result = result) %>%
        clean_dependency_list
    }
  } else if (is.recursive(expr)){
    result <- lapply(as.list(expr), find_knitr_doc,
                     result = result) %>%
      clean_dependency_list
  }
  setdiff(result, drake_fn_patterns)
}

# Deprecated on 2018-02-15
is_function_call <- function(
  expr,
  package = c("drake", "knitr", "rmarkdown"),
  what = c("loadd", "readd", "knit", "render")
){
  package <- match.arg(package)
  what <- match.arg(what)
  drake::drake_unquote(deparse(expr[[1]])) %in%
    paste0(c("", paste0(package, c("::", ":::"))), what)
}

#' @title Deprecated function `load_basic_example`
#' @description Use [load_mtcars_example()] instead.
#' @details Deprecated on 2018-04-21.
#' @seealso [load_mtcars_example()]
#' @export
#' @keywords internal
#' @return A config list, as in [load_mtcars_example()].
#' @inheritParams load_mtcars_example
#' @examples
#' # See ?load_mtcars_example for examples.
load_basic_example <- function(
  envir = parent.frame(),
  seed = NULL,
  cache = NULL,
  report_file = "report.Rmd",
  overwrite = FALSE,
  to = report_file,
  verbose = drake::default_verbose(),
  force = FALSE
){
  .Deprecated(
    "load_basic_example",
    package = "drake",
    msg = paste(
      "load_basic_example() is deprecated",
      "Use load_mtcars_example() instead."
    )
  )
  load_mtcars_example(
    envir = envir,
    report_file = report_file,
    overwrite = overwrite,
    force = force
  )
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
){
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
  if (imports == "none"){
    nodes <- nodes[nodes$status != "imported", ]
  } else if (imports == "files"){
    nodes <- nodes[nodes$status != "imported" | nodes$type == "file", ]
  }
  if (!from_scratch){
    nodes <- nodes[nodes$status != "outdated", ]
  }
  if (!nrow(nodes)){
    return(0)
  }
  level <- NULL
  n_per_level <- group_by(nodes, level) %>%
    mutate(nrow = n())
  max(n_per_level$nrow)
  # nocov end
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
#' \dontrun{
#' # This function is deprecated.
#' }
migrate_drake_project <- function(
  path = drake::default_cache_path(), jobs = 1
){
  .Deprecated(
    package = "drake",
    msg = c(
      "migrate_drake_project() is deprecated. Please run ",
      "make() again on projects built with drake version <= 4.4.0"
    )
  )
}
  
#' @title Deprecated function `plan`
#' @description Use [drake_plan()] instead.
#' @details Deprecated on 2017-10.
#' @seealso [drake_plan()]
#' @export
#' @keywords internal
#' @return A data frame of targets and commands.
#' @param ... Same as for [drake_plan()].
#' @param list Same as for [drake_plan()].
#' @param file_targets Same as for [drake_plan()].
#' @param strings_in_dots Same as for [drake_plan()].
#' @examples
#' # See ?drake_plan for examples.
plan <- function(
  ...,
  list = character(0),
  file_targets = FALSE,
  strings_in_dots = c("filenames", "literals")
){
  .Deprecated(
    "plan",
    package = "drake",
    msg = paste(
      "drake::plan() is deprecated due to a",
      "conflict with future::plan().",
      "Use drake_plan() instead."
    )
  )
  strings_in_dots <- match.arg(strings_in_dots)
  dots <- match.call(expand.dots = FALSE)$...
  commands_dots <- lapply(dots, wide_deparse)
  names(commands_dots) <- names(dots)
  commands <- c(commands_dots, list)
  targets <- names(commands)
  commands <- as.character(commands)
  if (!length(commands)){
    return(
      data.frame(
        target = character(0),
        command = character(0)
      )
    )
  }
  plan <- data.frame(
    target = targets,
    command = commands,
    stringsAsFactors = FALSE
  )
  from_dots <- plan$target %in% names(commands_dots)
  if (file_targets){
    plan$target <- drake::drake_quotes(plan$target, single = TRUE)
  }
  if (strings_in_dots == "filenames"){
    plan$command[from_dots] <- gsub("\"", "'", plan$command[from_dots])
  }
  sanitize_plan(plan)
}

#' @title Deprecated function `plan_drake`
#' @description Use [drake_plan()] instead.
#' @details Deprecated on 2017-12-12.
#' @seealso [drake_plan()]
#' @export
#' @keywords internal
#' @return A data frame of targets and commands.
#' @param ... Same as for [drake_plan()].
#' @param list Same as for [drake_plan()].
#' @param file_targets Same as for [drake_plan()].
#' @param strings_in_dots Same as for [drake_plan()].
#' @examples
#' # See ?drake_plan for examples.
plan_drake <- function(
  ...,
  list = character(0),
  file_targets = FALSE,
  strings_in_dots = c("filenames", "literals")
){
  .Deprecated(
    "plan_drake",
    package = "drake",
    msg = paste(
      "plan_drake() is deprecated.",
      "Use drake_plan() instead."
    )
  )
  strings_in_dots <- match.arg(strings_in_dots)
  dots <- match.call(expand.dots = FALSE)$...
  commands_dots <- lapply(dots, wide_deparse)
  names(commands_dots) <- names(dots)
  commands <- c(commands_dots, list)
  targets <- names(commands)
  commands <- as.character(commands)
  if (!length(commands)){
    return(
      data.frame(
        target = character(0),
        command = character(0)
      )
    )
  }
  plan <- data.frame(
    target = targets,
    command = commands,
    stringsAsFactors = FALSE
  )
  from_dots <- plan$target %in% names(commands_dots)
  if (file_targets){
    plan$target <- drake::drake_quotes(plan$target, single = TRUE)
  }
  if (strings_in_dots == "filenames"){
    plan$command[from_dots] <- gsub("\"", "'", plan$command[from_dots])
  }
  sanitize_plan(plan)
}

#' @title Deprecated function `plot_graph`
#' @description Use [vis_drake_graph()] instead.
#' @details Deprecated on 2017-10.
#' @export
#' @keywords internal
#' @return a `visNetwork` graph
#' @param plan deprecated
#' @param envir deprecated
#' @param hook deprecated
#' @param cache deprecated
#' @param jobs deprecated
#' @param packages deprecated
#' @param prework deprecated
#' @param verbose deprecated
#' @param config deprecated
#' @param file deprecated
#' @param build_times deprecated
#' @param digits deprecated
#' @param targets_only deprecated
#' @param split_columns deprecated
#' @param font_size deprecated
#' @param layout deprecated
#' @param main deprecated
#' @param direction deprecated
#' @param hover deprecated
#' @param navigationButtons deprecated
#' @param from deprecated
#' @param mode deprecated
#' @param order deprecated
#' @param subset deprecated
#' @param ncol_legend deprecated
#' @param make_imports deprecated
#' @param from_scratch deprecated
#' @param ... deprecated
#' @examples
#' # See ?vis_drake_graph for examples.
plot_graph <- function(
  plan = read_drake_plan(), targets = plan$target,
  envir = parent.frame(), verbose = 1,
  hook = default_hook,
  cache = drake::get_cache(verbose = verbose),
  jobs = 1,
  parallelism = drake::default_parallelism(),
  packages = rev(.packages()),
  prework = character(0),
  config = NULL,
  file = character(0),
  selfcontained = FALSE,
  build_times = TRUE,
  digits = 3,
  targets_only = FALSE,
  split_columns = FALSE,
  font_size = 20,
  layout = "layout_with_sugiyama",
  main = NULL,
  direction = "LR",
  hover = TRUE,
  navigationButtons = TRUE, # nolint
  from = NULL,
  mode = c("out", "in", "all"),
  order = NULL,
  subset = NULL,
  ncol_legend = 1,
  make_imports = TRUE,
  from_scratch = FALSE,
  ...
){
  .Deprecated(
    "plot_graph",
    package = "drake",
    msg = paste(
      "drake::plot_graph() is deprecated",
      "due to possible name conflicts.",
      "Use vis_drake_graph() instead."
    )
  )
  vis_drake_graph(
    plan = plan,
    envir = envir,
    verbose = verbose,
    hook = hook,
    cache = cache,
    jobs = jobs,
    parallelism = parallelism,
    packages = packages,
    prework = prework,
    config = config,
    file = file,
    selfcontained = selfcontained,
    build_times = build_times,
    digits = digits,
    targets_only = targets_only,
    split_columns = split_columns,
    font_size = font_size,
    layout = layout,
    main = main,
    direction = direction,
    hover = hover,
    navigationButtons = navigationButtons, # nolint
    from = from,
    mode = mode,
    order = order,
    subset = subset,
    ncol_legend = ncol_legend,
    make_imports = make_imports,
    from_scratch = from_scratch,
    ... = ...
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
  config = drake::read_drake_config(),
  targets = NULL,
  from_scratch = FALSE,
  targets_only = FALSE,
  future_jobs = 1,
  digits = 3
){
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

#' @title Deprecated function `read_config`
#' @description Use [read_drake_config()] instead.
#' @details Deprecated on 2017-11-12.
#' @seealso [read_drake_config()]
#' @export
#' @keywords internal
#' @return The master internal configuration list of a project.
#' @param path same as [read_drake_config()]
#' @param search same as [read_drake_config()]
#' @param cache same as [read_drake_config()]
#' @param verbose same as [read_drake_config()]
#' @examples
#' # See ?read_drake_config for examples.
read_config <- function(
  path = getwd(), search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = TRUE
){
  .Deprecated(
    "read_config",
    package = "drake",
    msg = paste(
      "drake::read_config() is deprecated",
      "due to possible name conflicts.",
      "Use read_drake_config() instead."
    )
  )
  read_drake_config(
    path = path,
    search = search,
    cache = cache,
    verbose = verbose
  )
}

#' @title Deprecated function `read_graph`
#' @description Use [read_drake_graph()] instead.
#' @details Deprecated on 2017-11-12.
#' @seealso [read_drake_graph()].
#' @export
#' @keywords internal
#' @return An `igraph` object representing the dependency
#' network of the workflow.
#' @param cache Same as for [read_drake_graph()].
#' @param path Same as for [read_drake_graph()].
#' @param search Same as for [read_drake_graph()].
#' @param verbose Same as for [read_drake_graph()].
#' @param ... Same as for [read_drake_graph()].
#' @examples
#' # See ?read_drake_graph for the examples.
read_graph <- function(
  path = getwd(), search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1, ...
){
  .Deprecated(
    "read_graph",
    package = "drake",
    msg = paste(
      "drake::read_graph() is deprecated",
      "due to possible name conflicts.",
      "Use read_drake_graph() instead."
    )
  )
  read_drake_graph(
    path = path,
    search = search,
    cache = cache,
    verbose = verbose
  )
}

#' @title Deprecated: read the metadata of a target or import.
#' @description Use [diagnose()] instead.
#' @details Deprecated on 2018-02-09
#' @seealso [diagnose()] [dependency_profile()], [make()]
#' @keywords internal
#' @export
#' @return The cached master internal configuration list
#'   of the last [make()].
#'
#' @inheritParams cached
#'
#' @param targets character vector, names of the targets
#'   to get metadata. If `NULL`, all metadata is collected.
#'
#' @param verbose whether to print console messages
#'
#' @param jobs number of jobs for light parallelism.
#'   Supports 1 job only on Windows.
#'
#' @examples
#' # See ?diagnose() for examples.
read_drake_meta <- function(
  targets = NULL,
  path = getwd(),
  search = TRUE,
  cache = NULL,
  verbose = 1,
  jobs = 1
){
  .Deprecated(
    "read_drake_meta",
    package = "drake",
    msg = paste(
      "read_drake_meta() is deprecated.",
      "Use diagnose() instead."
    )
  )
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

#' @title Deprecated function `read_plan`
#' @description Use [read_drake_plan()] instead.
#' @details Deprecated on 2017-11-21.
#' @seealso [read_drake_plan()].
#' @export
#' @keywords internal
#' @return An workflow plan data frame.
#' @param cache Same as for [read_drake_plan()].
#' @param path Same as for [read_drake_plan()].
#' @param search Same as for [read_drake_plan()].
#' @param verbose Same as for [read_drake_plan()].
#' @param ... Same as for [read_drake_plan()].
#' @examples
#' # See ?read_drake_plan for the examples.
read_plan <- function(
  path = getwd(), search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1, ...
){
  .Deprecated(
    "read_plan",
    package = "drake",
    msg = paste(
      "drake::read_plan() is deprecated",
      "due to possible name conflicts.",
      "Use read_drake_plan() instead."
    )
  )
  read_drake_plan(
    path = path,
    search = search,
    cache = cache,
    verbose = verbose
  )
}

#' @title Deprecated function `render_graph`
#' @description Use [render_drake_graph()] instead.
#' @details Deprecated on 2017-10.
#' @export
#' @keywords internal
#' @return a `visNetwork` graph.
#' @param graph_info deprecated
#' @param file deprecated
#' @param layout deprecated
#' @param direction deprecated
#' @param hover deprecated
#' @param main deprecated
#' @param selfcontained deprecated
#' @param navigationButtons deprecated
#' @param ncol_legend deprecated
#' @param ... deprecated
#' @examples
#' # See ?render_drake_graph for examples.
render_graph <- function(
  graph_info,
  file = character(0),
  layout = "layout_with_sugiyama",
  direction = "LR",
  hover = TRUE,
  main = graph_info$default_title,
  selfcontained = FALSE,
  navigationButtons = TRUE, # nolint
  ncol_legend = 1,
  ...
){
  .Deprecated(
    "render_graph",
    package = "drake",
    msg = paste(
      "drake::render_graph() is deprecated",
      "due to possible name conflicts.",
      "Use render_drake_graph() instead."
    )
  )
  render_drake_graph(
    graph_info = graph_info,
    file = file,
    layout = layout,
    direction = direction,
    hover = hover,
    main = main,
    selfcontained = selfcontained,
    navigationButtons = navigationButtons, # nolint
    ncol_legend = ncol_legend,
    ... = ...
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
){
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

#' @title Deprecated function `session`
#' @description Use [drake_session()] instead
#' @details Deprecated on 2017-11-12.
#' @seealso [drake_session()]
#' @export
#' @keywords internal
#' @return Same as for [drake_session()].
#' @param cache Same as for [drake_session()].
#' @param path Same as for [drake_session()].
#' @param search Same as for [drake_session()].
#' @param verbose Same as for [drake_session()].
#' @examples
#' # See ?drake_session for examples.
session <- function(
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = TRUE
){
  .Deprecated(
    "session",
    package = "drake",
    msg = paste(
      "drake::session() is deprecated",
      "due to a possible conlict with the R/shiny lexicon.",
      "Use drake_session() instead."
    )
  )
  drake_session(
    path = path,
    search = search,
    cache = cache,
    verbose = verbose
  )
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
){
  .Defunct(
    package = "drake",
    msg = c(
      "Staged parallelism is removed from drake, ",
      "so the parallel_stages() function is moot. ",
      "drake uses a much better parallel algorithm now."
    )
  )
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
){
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

#' @title Deprecated function `summaries`
#' @description Use [summaries()] instead
#' @details Deprecated on 2017-11-12.
#' @seealso [summaries()]
#' @export
#' @keywords internal
#' @return Same as for [plan_summaries()].
#' @param plan Same as for [plan_summaries()].
#' @param analyses Same as for [plan_summaries()].
#' @param datasets Same as for [plan_summaries()].
#' @param gather Same as for [plan_summaries()].
#' @examples
#' # See ?drake_session for examples.
summaries <- function(
  plan,
  analyses,
  datasets,
  gather = rep("list", nrow(plan))
){
  .Deprecated(
    "summaries",
    package = "drake",
    msg = paste(
      "drake::summaries() is deprecated",
      "due to possible name conflicts.",
      "Use plan_summaries() instead."
    )
  )
  plan_summaries(
    plan = plan,
    analyses = analyses,
    datasets = datasets,
    gather = gather
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
triggers <- function(){
  .Deprecated(
    "triggers",
    package = "drake",
    msg = paste(
      "drake::triggers() is deprecated",
      "and the trigger interface has changed.",
      "See trigger() (singular) for details."
    )
  )
  c(
    "any",
    "always",
    "command",
    "depends",
    "file",
    "missing"
  ) %>%
    sort
}

convert_old_trigger <- function(x){
  if (!is.character(x)){
    return(x)
  }
  if (!(x %in% suppressWarnings(triggers()))){
    return(x)
  }
  warning(
    "The old trigger interface is deprecated. ",
    "See the trigger() function (singular) ",
    "to learn about the new trigger interface.",
    call. = FALSE
  )
  if (identical(x, "any")){
    "trigger()"
  } else if (identical(x, "always")){
    "trigger(condition = TRUE)"
  } else if (identical(x, "command")){
    "trigger(command = TRUE, depend = FALSE, file = FALSE)"
  } else if (identical(x, "depends")){
    "trigger(command = FALSE, depend = TRUE, file = FALSE)"
  } else if (identical(x, "file")){
    "trigger(command = FALSE, depend = FALSE, file = TRUE)"
  } else if (identical(x, "missing")){
    "trigger(command = FALSE, depend = FALSE, file = FALSE)"
  }
}

#' @title Deprecated function `workflow`
#' @description Turns a named collection of command/target pairs into
#' a workflow plan data frame for [make()] and
#' [check()].
#' @details Deprecated on 2017-10
#' @export
#' @keywords internal
#' @return A data frame of targets and commands.
#' @param ... same as for \code{drake::\link{drake_plan}()}
#' @param list same as for \code{drake::\link{drake_plan}()}
#' @param file_targets same as for \code{drake::\link{drake_plan}()}
#' @param strings_in_dots same as for \code{drake::\link{drake_plan}()}
#' @examples
#' # See ?drake_plan for examples.
workflow <- function(
  ...,
  list = character(0),
  file_targets = FALSE,
  strings_in_dots = c("filenames", "literals")
){
  .Deprecated(
    "workflow",
    package = "drake",
    msg = "workflow() is deprecated. Use drake_plan() instead."
  )
  strings_in_dots <- match.arg(strings_in_dots)
  dots <- match.call(expand.dots = FALSE)$...
  commands_dots <- lapply(dots, wide_deparse)
  names(commands_dots) <- names(dots)
  commands <- c(commands_dots, list)
  targets <- names(commands)
  commands <- as.character(commands)
  if (!length(commands)){
    return(
      data.frame(
        target = character(0),
        command = character(0)
      )
    )
  }
  plan <- data.frame(
    target = targets,
    command = commands,
    stringsAsFactors = FALSE
  )
  from_dots <- plan$target %in% names(commands_dots)
  if (file_targets){
    plan$target <- drake::drake_quotes(plan$target, single = TRUE)
  }
  if (strings_in_dots == "filenames"){
    plan$command[from_dots] <- gsub("\"", "'", plan$command[from_dots])
  }
  sanitize_plan(plan)
}

#' @title Deprecated function `workplan`
#' @description Turns a named collection of command/target pairs into
#' a workflow plan data frame for [make()] and
#' [check()].
#' @details Deprecated on 2017-11-29
#' @export
#' @keywords internal
#' @return A data frame of targets and commands.
#' @param ... same as for \code{drake::\link{drake_plan}()}
#' @param list same as for \code{drake::\link{drake_plan}()}
#' @param file_targets same as for \code{drake::\link{drake_plan}()}
#' @param strings_in_dots same as for \code{drake::\link{drake_plan}()}
#' @examples
#' # See ?drake_plan for examples.
workplan <- function(
  ...,
  list = character(0),
  file_targets = FALSE,
  strings_in_dots = c("filenames", "literals")
){
  .Deprecated(
    "drake_plan",
    package = "drake",
    msg = "workplan() is deprecated. Use drake_plan() instead."
  )
  strings_in_dots <- match.arg(strings_in_dots)
  dots <- match.call(expand.dots = FALSE)$...
  commands_dots <- lapply(dots, wide_deparse)
  names(commands_dots) <- names(dots)
  commands <- c(commands_dots, list)
  targets <- names(commands)
  commands <- as.character(commands)
  if (!length(commands)){
    return(
      data.frame(
        target = character(0),
        command = character(0)
      )
    )
  }
  plan <- data.frame(
    target = targets,
    command = commands,
    stringsAsFactors = FALSE
  )
  from_dots <- plan$target %in% names(commands_dots)
  if (file_targets){
    plan$target <- drake::drake_quotes(plan$target, single = TRUE)
  }
  if (strings_in_dots == "filenames"){
    plan$command[from_dots] <- gsub("\"", "'", plan$command[from_dots])
  }
  sanitize_plan(plan)
}
