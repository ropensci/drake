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
#' clean() # Start from scratch next time around.
#' })
#' }
make <- function(
  plan = drake::read_drake_plan(),
  targets = NULL,
  envir = parent.frame(),
  verbose = 1L,
  hook = NULL,
  cache = drake::get_cache(
    verbose = verbose,
    console_log_file = console_log_file
  ),
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
  hasty_build = NULL,
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
  initialize_session(config = config)
  if (!config$skip_imports) {
    process_imports(config)
  }
  if (!config$skip_targets) {
    process_targets(config)
  }
  conclude_session(config)
  invisible()
}
