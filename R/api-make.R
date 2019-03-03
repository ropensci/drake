#' @title Run your project (build the outdated targets).
#' @description This is the central, most important function
#' of the drake package. It runs all the steps of your
#' workflow in the correct order, skipping any work
#' that is already up to date.
#' See <https://github.com/ropensci/drake/blob/master/README.md#documentation>
#' for an overview of the documentation.
#' @section Interactive mode:
#' In interactive sessions, consider [r_make()], [r_outdated()], etc.
#' rather than [make()], [outdated()], etc. The `r_*()` `drake` functions
#' are more reproducible when the session is interactive.
#'
#' A serious drake workflow should be consistent and reliable,
#' ideally with the help of a master R script.
#' This script should begin in a fresh R session,
#' load your packages and functions in a dependable manner,
#' and then run `make()`. Example:
#' <https://github.com/wlandau/drake-examples/tree/master/gsp>.
#' Batch mode, especially within a container, is particularly helpful.
#'
#' Interactive R sessions are still useful,
#' but they easily grow stale.
#' Targets can falsely invalidate if you accidentally change
#' a function or data object in your environment.
#' So in interactive mode, `make()` now pauses with a menu
#' to protect you from environment-related instability.
#'
#' You can control this menu with the `drake_make_menu` global option.
#' Run `options(drake_make_menu = TRUE)` to show the menu once per session
#' and `options(drake_make_menu = FALSE)` to disable it entirely.
#' You may wish to add a call to `options()` in your local `.Rprofile` file.
#' @section Self-invalidation:
#' It is possible to construct a workflow that tries to invalidate itself.
#' Example:
#' ```r
#' plan <- drake_plan(
#'   x = {
#'     data(mtcars)
#'     mtcars$mpg
#'   },
#'   y = mean(x)
#' )
#' ```
#' In this plan, the very act of building `x`
#' changes the dependencies of `x`.
#' In other words, without safeguards, `x` would not be up to date at
#' the end of `make(plan)`. 
#' Please try to avoid workflows that modify the global environment.
#' Otherwise, `make(plan)` will throw an error.
#' To avoid this error, you can run `make(plan, lock_envir = FALSE)`.
#' There are legitimate use cases for `lock_envir = FALSE`
#' (example: <https://ropenscilabs.github.io/drake-manual/hpc.html#parallel-computing-within-targets>) # nolint
#' but most workflows should stick with the default `lock_envir = TRUE`.
#' @seealso
#'   [drake_plan()],
#'   [drake_config()],
#'   [vis_drake_graph()],
#'   [outdated()]
#' @export
#' @return nothing
#' @inheritParams drake_config
#' @param config Master configuration list produced by both
#'   [make()] and [drake_config()].
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
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
#' if (requireNamespace("visNetwork", quietly = TRUE)) {
#' vis_drake_graph(config) # See how they fit in an interactive graph.
#' make(my_plan, cache_log_file = TRUE) # Write a text log file this time.
#' vis_drake_graph(config) # The colors changed in the graph.
#' }
#' clean() # Start from scratch next time around.
#' }
#' })
#' }
make <- function(
  plan,
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
  lib_loc = NULL,
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
      lib_loc = lib_loc,
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
  config$ht_get_hash <- ht_new() # Memoize getting hashes from the cache.
  on.exit(ht_clear(config$ht_get_hash)) # Needs to be empty afterwards.
  if (!config$skip_imports) {
    process_imports(config)
  }
  if (is.character(config$parallelism)) {
    config$schedule <- pretrim_schedule(config)
  }
  abort <- FALSE
  if (prompt_intv_make(config)) {
    abort <- abort_intv_make(config) # nocov
  }
  if (abort) {
    return(invisible()) # nocov
  }
  if (!config$skip_targets) {
    process_targets(config)
  }
  conclude_session(config)
  invisible()
}
