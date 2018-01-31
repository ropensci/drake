#' @title Create the internal runtime parameter list
#' used internally in [make()].
#' @description This configuration list
#' is also required for functions such as [outdated()] and
#' [vis_drake_graph()]. It is meant to be specific to
#' a single call to [make()], and you should not modify
#' it by hand afterwards. If you later plan to call [make()]
#' with different arguments (especially `targets`),
#' you should refresh the config list with another call to
#' [drake_config()]. For changes to the
#' `targets` argument
#' specifically, it is important to recompute the config list
#' to make sure the internal workflow network has all the targets you need.
#' Modifying the `targets` element afterwards will have no effect
#' and it could lead to false negative results from
#' [outdated()]
#' @export
#' @return The master internal configuration list of a project.
#' @seealso [make_with_config()], [make()],
#' [drake_plan()], [vis_drake_graph()]
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
#' @param fetch_cache same as for [make()]
#' @param timeout same as for [make()]
#' @param cpu same as for [make()]
#' @param elapsed same as for [make()]
#' @param retries same as for [make()]
#' @param force same as for [make()]
#' @param log_progress logical, whether to clear
#' the cached progress of the targets readable by
#' @param graph igraph object representing the workflow plan network.
#' Overrides `skip_imports`.
#' @param trigger same as for [make()]
#' @param imports_only logical, whether to skip building the targets
#' in `plan` and just import objects and files.
#' @param skip_imports logical, whether to totally neglect to
#' process the imports and jump straight to the targets. This can be useful
#' if your imports are massive and you just want to test your project,
#' but it is bad practice for reproducible data analysis.
#' This argument is overridden if you supply your own `graph` argument.
#' @param skip_safety_checks logical, whether to skip the safety checks
#' on your workflow to save time. Use at your own peril.
#' @param lazy_load same as for [make()]
#' @param session_info same as for [make()]
#' @param cache_log_file same as for [make()]
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Get the code with drake_example("basic").
#' # Construct the master internal configuration list.
#' con <- drake_config(my_plan)
#' # These functions are faster than otherwise
#' # because they use the configuration list.
#' outdated(config = con) # Which targets are out of date?
#' missed(config = con) # Which imports are missing?
#' # In make(..., jobs = n), it would be silly to set `n` higher than this:
#' max_useful_jobs(config = con)
#' # Show a visNetwork graph
#' vis_drake_graph(config = con)
#' # Get the underlying node/edge data frames of the graph.
#' dataframes_graph(config = con)
#' })
#' }
drake_config <- function(
  plan = drake_plan(),
  targets = drake::possible_targets(plan),
  envir = parent.frame(),
  verbose = 1,
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
  timeout = Inf,
  cpu = timeout,
  elapsed = timeout,
  retries = 0,
  force = FALSE,
  log_progress = FALSE,
  graph = NULL,
  trigger = drake::default_trigger(),
  imports_only = FALSE,
  skip_imports = FALSE,
  skip_safety_checks = FALSE,
  lazy_load = FALSE,
  session_info = TRUE,
  cache_log_file = NULL
){
  force(envir)
  seed <- get_valid_seed()
  plan <- sanitize_plan(plan)
  targets <- sanitize_targets(plan, targets)
  parallelism <- match.arg(
    parallelism,
    choices = parallelism_choices(distributed_only = FALSE)
  )
  prework <- add_packages_to_prework(
    packages = packages,
    prework = prework
  )
  if (is.null(cache)) {
    cache <- recover_cache(
      force = force, verbose = verbose, fetch_cache = fetch_cache)
  }
  if (!force){
    assert_compatible_cache(cache = cache)
  }
  # A storr_rds() cache should already have the right hash algorithms.
  cache <- configure_cache(
    cache = cache,
    log_progress = log_progress,
    overwrite_hash_algos = FALSE,
    jobs = jobs
  )
  trigger <- match.arg(arg = trigger, choices = triggers())
  if (is.null(graph)){
    graph <- build_drake_graph(plan = plan, targets = targets,
      envir = envir, verbose = verbose, jobs = jobs)
  } else {
    graph <- prune_drake_graph(graph = graph, to = targets, jobs = jobs)
  }
  cache_path <- force_cache_path(cache)
  list(
    plan = plan, targets = targets, envir = envir,
    cache = cache, cache_path = cache_path, fetch_cache = fetch_cache,
    parallelism = parallelism, jobs = jobs, verbose = verbose, hook = hook,
    prepend = prepend, prework = prework, command = command,
    args = args, recipe_command = recipe_command, graph = graph,
    short_hash_algo = cache$get("short_hash_algo", namespace = "config"),
    long_hash_algo = cache$get("long_hash_algo", namespace = "config"),
    seed = seed, trigger = trigger,
    timeout = timeout, cpu = cpu, elapsed = elapsed, retries = retries,
    imports_only = imports_only, skip_imports = skip_imports,
    skip_safety_checks = skip_safety_checks, log_progress = log_progress,
    lazy_load = lazy_load, session_info = session_info,
    cache_log_file = cache_log_file
  )
}

add_packages_to_prework <- function(packages, prework) {
  packages <- c("methods", packages) %>% unique
  if (!length(packages))
    return(prework)
  paste0("if(!R.utils::isPackageLoaded(\"", packages, "\")) require(",
    packages, ")", sep = "") %>% c(prework)
}

#' @title Do the prework in the `prework`
#' argument to [make()].
#' @export
#' @keywords internal
#' @description For internal use only.
#' The only reason this function is exported
#' is to set up parallel socket (PSOCK) clusters
#' without too much fuss.
#' @return Inivisibly returns `NULL`.
#' @param config internal configuration list
#' @param verbose_packages logical, whether to print
#' package startup messages
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Get the code with drake_example("basic").
#' # Create a master internal configuration list with prework.
#' con <- drake_config(my_plan, prework = c("library(knitr)", "x <- 1"))
#' # Do the prework. Usually done at the beginning of `make()`,
#' # and for distributed computing backends like "future_lapply",
#' # right before each target is built.
#' do_prework(config = con, verbose_packages = TRUE)
#' identical(x, 1) # Should be TRUE.
#' })
#' }
do_prework <- function(config, verbose_packages) {
  wrapper <- ifelse(verbose_packages, invisible,
    base::suppressPackageStartupMessages)
  for (code in config$prework) wrapper(eval(parse(text = code),
    envir = config$envir))
  invisible()
}

#' @title List the possible targets for the `targets`
#' argument to [make()], given a workflow plan
#' data frame.
#' @description Intended for internal use only.
#' @seealso [make()]
#' @keywords internal
#' @export
#' @return Character vector of possible targets given the workflow plan.
#' @param plan workflow plan data frame
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Get the code with drake_example("basic").
#' # List the possible targets you could choose for the
#' # `targets` argument to make(). You may choose any subset.
#' possible_targets(my_plan)
#' })
#' }
possible_targets <- function(plan = drake_plan()) {
  plan <- sanitize_plan(plan)
  as.character(plan$target)
}

#' @title Store an internal configuration list
#' from [drake_config()].
#' @description Exported for demonstration and tinkering purposes
#' only. Not meant to be called by the user.
#' @export
#' @keywords internal
#' @param config Internal configuration list
#' @return Nothing.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Get the code with drake_example("basic").
#' config <- drake_config(my_plan)
#' store_drake_config(config = config)
#' read_drake_config()
#' })
#' }
store_drake_config <- function(config) {
  save_these <- setdiff(names(config), "envir")  # envir could get massive.
  lightly_parallelize(
    save_these,
    function(item){
      config$cache$set(
        key = item,
        value = config[[item]],
        namespace = "config"
      )
    },
    jobs = config$jobs
  )
  invisible()
}
