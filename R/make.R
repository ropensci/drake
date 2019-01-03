#' @title Run your project (build the outdated targets).
#' @description This is the central, most important function
#' of the drake package. It runs all the steps of your
#' workflow in the correct order, skipping any work
#' that is already up to date.
#' See <https://github.com/ropensci/drake/blob/master/README.md#documentation>
#' for an overview of the documentation.
#' @seealso
#'   [drake_plan()],
#'   [drake_config()],
#'   [vis_drake_graph()],
#'   [evaluate_plan()],
#'   [outdated()],
#'   [triggers()]
#' @export
#' @return nothing
#' @inheritParams drake_config
#' @param config Master configuration list produced by both
#'   [make()] and [drake_config()].
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' config <- drake_config(my_plan)
#' outdated(config) # Which targets need to be (re)built?
#' make(my_plan, jobs = 2) # Build what needs to be built.
#' outdated(config) # Everything is up to date.
#' # Change one of your imported function dependencies.
#' reg2 = function(d) {
#'   d$x3 = d$x^3
#'   lm(y ~ x3, data = d)
#' }
#' outdated(config) # Some targets depend on reg2().
#' make(my_plan) # Rebuild just the outdated targets.
#' outdated(config) # Everything is up to date again.
#' vis_drake_graph(config) # See how they fit in an interactive graph.
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
  plan = drake::read_drake_plan(),
  targets = NULL,
  envir = parent.frame(),
  verbose = 1L,
  hook = NULL,
  cache = drake::get_cache(
    verbose = verbose, console_log_file = console_log_file),
  fetch_cache = NULL,
  parallelism = "loop",
  jobs = 1L,
  jobs_preprocess = 1L,
  packages = rev(.packages()),
  prework = character(0),
  prepend = NULL,
  command = NULL,
  args = NULL,
  recipe_command = NULL,
  log_progress = TRUE,
  skip_targets = FALSE,
  timeout = NULL,
  cpu = Inf,
  elapsed = Inf,
  retries = 0,
  force = FALSE,
  graph = NULL,
  trigger = drake::trigger(),
  skip_imports = FALSE,
  skip_safety_checks = FALSE,
  config = NULL,
  lazy_load = "eager",
  session_info = TRUE,
  cache_log_file = NULL,
  seed = NULL,
  caching = "master",
  keep_going = FALSE,
  session = NULL,
  pruning_strategy = NULL,
  makefile_path = NULL,
  console_log_file = NULL,
  ensure_workers = TRUE,
  garbage_collection = FALSE,
  template = list(),
  sleep = function(i) 0.01,
  hasty_build = drake::default_hasty_build,
  memory_strategy = c("speed", "memory", "lookahead"),
  layout = NULL,
  lock_envir = TRUE
) {
  force(envir)
  if (is.null(config)) {
    config <- drake_config(
      plan = plan,
      targets = targets,
      envir = envir,
      seed = seed,
      verbose = verbose,
      hook = hook,
      parallelism = parallelism,
      jobs = jobs,
      jobs_preprocess = jobs_preprocess,
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
      skip_targets = skip_targets,
      skip_imports = skip_imports,
      skip_safety_checks = skip_safety_checks,
      lazy_load = lazy_load,
      session_info = session_info,
      cache_log_file = cache_log_file,
      caching = caching,
      keep_going = keep_going,
      session = session,
      pruning_strategy = pruning_strategy,
      makefile_path = makefile_path,
      console_log_file = console_log_file,
      ensure_workers = ensure_workers,
      garbage_collection = garbage_collection,
      template = template,
      sleep = sleep,
      hasty_build = hasty_build,
      memory_strategy = memory_strategy,
      layout = layout,
      lock_envir = lock_envir
    )
  }
  make_with_config(config = config)
  invisible()
}

#' @title Run [make()],
#'   on an existing internal configuration list.
#' @description Use [drake_config()]
#' to create the `config` argument.
#' @export
#' @keywords internal
#' @seealso [make()], [drake_config()]
#' @return nothing
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
make_with_config <- function(config = drake::read_drake_config()) {
  if (!is.null(config$session)) {
    # Deprecated on 2018-12-18.
    warning(
      "The ", sQuote("session"), " argument of make() and drake_config() ",
      "is deprecated. make() will NOT run in a separate callr session. ",
      "For reproducibility, you may wish to try make(lock_envir = TRUE). ",
      "Details: https://github.com/ropensci/drake/issues/623.",
      call. = FALSE
    )
  }
  runtime_checks(config = config)
  store_drake_config(config = config)
  initialize_session(config = config)
  do_prework(config = config, verbose_packages = config$verbose)
  if (!config$skip_imports) {
    make_imports(config)
  }
  if (!config$skip_targets) {
    make_targets(config)
  }
  drake_cache_log_file(
    file = config$cache_log_file,
    cache = config$cache,
    jobs = config$jobs
  )
  conclude_session(config = config)
  invisible()
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
#' @seealso [make()], [drake_config()],
#'   [make_targets()]
#' @return nothing
#' @param config a configuration list returned by [drake_config()]
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
make_imports <- function(config = drake::read_drake_config()) {
  config$schedule <- imports_graph(config = config)
  if (on_windows() && config$jobs > 1L) {
    process_imports_parLapply(config) # nocov
  } else {
    process_imports_mclapply(config)
  }
  invisible()
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
#' @seealso [make()], [drake_config()],
#'   [make_imports()]
#' @return nothing
#' @param config a configuration list returned by [drake_config()]
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
make_targets <- function(config = drake::read_drake_config()) {
  config$schedule <- targets_graph(config = config)
  if (config$parallelism == "hasty") {
    run_hasty(config)
    return(invisible(config))
  }
  outdated <- outdated(config, do_prework = FALSE, make_imports = FALSE)
  if (!length(outdated)) {
    console_up_to_date(config = config)
    return(invisible())
  }
  up_to_date <- setdiff(config$all_targets, outdated)
  config$schedule <- igraph::delete_vertices(config$schedule, v = up_to_date)
  run_parallel_backend(config = config)
  console_up_to_date(config = config)
  invisible()
}
