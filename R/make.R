#' @title Run your project (build the outdated targets).
#' @description This is the central, most important function
#' of the drake package. It runs all the steps of your
#' workflow in the correct order, skipping any work
#' that is already up to date.
#' See <https://github.com/ropensci/drake/blob/master/README.md#documentation>
#' for an overview of the documentation.
#' @seealso 
#'   [drake_plan()],
#'   [vis_drake_graph()],
#'   [parallelism_choices()],
#'   [max_useful_jobs()],
#'   [triggers()],
#'   [make_with_config()]
#' @export
#' @return The master internal configuration list, mostly
#'   containing arguments to `make()` and important objects
#'   constructed along the way. See [config()]
#'   for more details.
#' @inheritParams drake_config
#' @param config Master configuration list produced by both
#'   [make()] and [drake_config()].
#' @param return_config Logical, whether to return the internal list
#'   of runtime configuration parameters used by `make()`.
#'   This argument is deprecated. Now, a configuration list
#'   is always invisibly returned.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' config <- drake_config(my_plan)
#' outdated(config) # Which targets need to be (re)built?
#' my_jobs = max_useful_jobs(config) # Depends on what is up to date.
#' make(my_plan, jobs = 2) # Build what needs to be built.
#' outdated(config) # Everything is up to date.
#' # Change one of your imported function dependencies.
#' reg2 = function(d){
#'   d$x3 = d$x^3
#'   lm(y ~ x3, data = d)
#' }
#' outdated(config) # Some targets depend on reg2().
#' vis_drake_graph(config) # See how they fit in an interactive graph.
#' make(my_plan) # Rebuild just the outdated targets.
#' outdated(config) # Everything is up to date again.
#' make(my_plan, cache_log_file = TRUE) # Write a text log file this time.
#' vis_drake_graph(config) # The colors changed in the graph.
#' clean() # Start from scratch.
#' # Run with at most 2 jobs at a time for the imports
#' # and at most 4 jobs at a time for the targets.
#' make(my_plan, jobs = c(imports = 2, targets = 4))
#' clean() # Start from scratch.
#' # Rerun with "Makefile" parallelism with at most 4 jobs.
#' # Requires Rtools on Windows.
#' # make(my_plan, parallelism = "Makefile", jobs = 4) # nolint
#' clean() # Start from scratch.
#' # Specify your own Makefile recipe.
#' # Requires Rtools on Windows.
#' # make(my_plan, parallelism = "Makefile", jobs = 4, # nolint
#' #   recipe_command = "R -q -e") # nolint
#' #
#' # make() respects tidy evaluation as implemented in the rlang package.
#' # This workflow plan uses rlang's quasiquotation operator `!!`.
#' my_plan <- drake_plan(list = c(
#'   little_b = "\"b\"",
#'   letter = "!!little_b"
#' ))
#' my_plan
#' make(my_plan)
#' readd(letter) # "b"
#' })
#' }
make <- function(
  plan = read_drake_plan(),
  targets = drake::possible_targets(plan),
  envir = parent.frame(),
  verbose = drake::default_verbose(),
  hook = default_hook,
  cache = drake::get_cache(verbose = verbose, force = force),
  fetch_cache = NULL,
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
  log_progress = TRUE,
  imports_only = FALSE,
  timeout = Inf,
  cpu = NULL,
  elapsed = NULL,
  retries = 0,
  force = FALSE,
  return_config = NULL,
  graph = NULL,
  trigger = drake::default_trigger(),
  skip_imports = FALSE,
  skip_safety_checks = FALSE,
  config = NULL,
  lazy_load = "eager",
  session_info = TRUE,
  cache_log_file = NULL,
  seed = NULL,
  caching = "worker",
  keep_going = FALSE,
  session = NULL
){
  force(envir)
  if (!is.null(return_config)){
    warning(
      "The return_config argument to make() is deprecated. ",
      "Now, an internal configuration list is always invisibly returned.",
      call. = FALSE
    )
  }
  if (is.null(config)){
    config <- drake_config(
      plan = plan,
      targets = targets,
      envir = envir,
      seed = seed,
      verbose = verbose,
      hook = hook,
      parallelism = parallelism,
      jobs = jobs,
      packages = packages,
      prework = prework,
      prepend = prepend,
      command = command,
      args = args,
      recipe_command = recipe_command,
      log_progress = log_progress,
      cache = cache,
      fetch_cache = fetch_cache,
      timeout = timeout,
      cpu = cpu,
      elapsed = elapsed,
      retries = retries,
      force = force,
      graph = graph,
      trigger = trigger,
      imports_only = imports_only,
      skip_imports = skip_imports,
      skip_safety_checks = skip_safety_checks,
      lazy_load = lazy_load,
      session_info = session_info,
      cache_log_file = cache_log_file,
      caching = caching,
      keep_going = keep_going,
      session = session
    )
  }
  make_with_config(config = config)
}

#' @title Run [make()],
#'   on an existing internal configuration list.
#' @description Use [drake_config()]
#' to create the `config` argument.
#' @export
#' @seealso [make()], [drake_config()]
#' @return An output internal configuration list
#' @param config An input internal configuration list
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' # The following lines are the same as make(my_plan)
#' config <- drake_config(my_plan) # Create the internal config list.
#' make_with_config(config = config) # Run the project, build the targets.
#' })
#' }
make_with_config <- function(config = drake::read_drake_config()){
  if (is.null(config$session)){
    make_session(config = config)
  } else {
    globals <- global_imports(config)
    args <- as.list(globalenv(), all.names = TRUE)[globals]
    args$config <- config
    config$session(
      func = function(config, ...){
        args <- list(...)
        envir <- globalenv()
        for (var in names(args)){
          assign(x = var, value = args[[var]], envir = envir)
        }
        drake::make_session(config = config)
      },
      args = args,
      libpath = .libPaths()
    )
  }
  return(invisible(config))
}

global_imports <- function(config){
  setdiff(V(config$graph)$name, config$plan$target) %>%
    intersect(ls(envir = globalenv()))
}

#' @title Internal function to be called by [make_with_config()]
#' @description For internal use only. Not for the API.
#' @keywords internal
#' @export
#' @inheritParams make_with_config
make_session <- function(config){
  check_drake_config(config = config)
  store_drake_config(config = config)
  initialize_session(config = config)
  do_prework(config = config, verbose_packages = config$verbose)
  if (!config$skip_imports){
    make_imports(config = config)
  }
  if (!config$imports_only){
    make_targets(config = config)
  }
  drake_cache_log_file(
    file = config$cache_log_file,
    cache = config$cache,
    jobs = config$jobs
  )
  return(invisible(config))
}

#' @title Just make the imports.
#' @description [make()] is the central, most important function
#' of the drake package. [make()] runs all the steps of your
#' workflow in the correct order, skipping any work
#' that is already up to date. During [make()],
#' there are two kinds of processing steps: "imports",
#' which are pre-existing functions and input data files
#' that are loaded or checked, and targets, which are
#' serious reproducibly-tracked data analysis steps
#' that have commands in your workflow plan data frame.
#' The [make_targets()] function just makes the targets
#' (skipping any targets that are already up to date)
#' and [make_imports()] just makes the imports.
#' Most users should just use [make()]
#' instead of either [make_imports()] or
#' [make_targets()].
#' See <https://github.com/ropensci/drake/blob/master/README.md#documentation>
#' for an overview of the documentation.
#' @export
#' @seealso [make()], [config()],
#'   [make_targets()]
#' @return The master internal configuration list
#'   used by [make()].
#' @param config a configuration list returned by [config()]
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' # Generate the master internal configuration list.
#' con <- drake_config(my_plan)
#' # Just cache the imports, do not build any targets.
#' make_imports(config = con)
#' # Just make the targets
#' make_targets(config = con)
#' })
#' }
make_imports <- function(config = drake::read_drake_config()){
  config$schedule <- imports_graph(config = config)
  config$jobs <- jobs_imports(jobs = config$jobs)
  config$parallelism <- use_default_parallelism(config$parallelism)
  run_parallel_backend(config = config)
  invisible(config)
}

imports_graph <- function(config){
  delete_these <- intersect(config$plan$target, V(config$graph)$name)
  delete_vertices(config$graph, v = delete_these)
}

#' @title Just build the targets.
#' @description [make()] is the central, most important function
#' of the drake package. [make()] runs all the steps of your
#' workflow in the correct order, skipping any work
#' that is already up to date. During [make()],
#' there are two kinds of processing steps: "imports",
#' which are pre-existing functions and input data files
#' that are loaded or checked, and targets, which are
#' serious reproducibly-tracked data analysis steps
#' that have commands in your workflow plan data frame.
#' The [make_targets()] function just makes the targets
#' (skipping any targets that are already up to date)
#' and [make_imports()] just makes the imports.
#' Most users should just use [make()]
#' instead of either [make_imports()] or
#' [make_targets()].
#' See <https://github.com/ropensci/drake/blob/master/README.md#documentation>
#' for an overview of the documentation.
#' @export
#' @seealso [make()], [config()],
#'   [make_imports()]
#' @return The master internal configuration list
#'   used by [make()].
#' @param config a configuration list returned by [config()]
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' # Generate the master internal configuration list.
#' con <- drake_config(my_plan)
#' # Just cache the imports, do not build any targets.
#' make_imports(config = con)
#' # Just make the targets
#' make_targets(config = con)
#' })
#' }
make_targets <- function(config = drake::read_drake_config()){
  config$schedule <- targets_graph(config = config)
  config$jobs <- jobs_targets(jobs = config$jobs)
  run_parallel_backend(config = config)
  console_up_to_date(config = config)
  invisible(config)
}

targets_graph <- function(config){
  delete_these <- setdiff(V(config$graph)$name, config$plan$target)
  delete_vertices(config$graph, v = delete_these)
}

initialize_session <- function(config){
  if (config$log_progress){
    clear_progress(cache = config$cache, jobs = jobs_imports(config$jobs))
  }
  config$cache$clear(namespace = "session")
  if (config$session_info){
    config$cache$set(
      key = "sessionInfo",
      value = sessionInfo(),
      namespace = "session"
    )
  }
}
