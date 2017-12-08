#' @title Function \code{drake_config}
#' @description Compute the internal runtime parameter list of
#' \code{\link{make}()}.
#' @details This list is used internally, and you will need it
#' for functions like \code{\link{outdated}()} and
#' \code{\link{vis_drake_graph}()}. It is meant to be specific to
#' a single call to \code{\link{make}()}, and you should not modify
#' it by hand afterwards. If you later plan to call \code{\link{make}()}
#' with different arguments (especially \code{targets}),
#' you should refresh the config list with another call to
#' \code{\link{drake_config}()}. For changes to the
#' \code{targets} argument
#' specifically, it is important to recompute the config list
#' to make sure the internal workflow network has all the targets you need.
#' Modifying the \code{targets} element afterwards will have no effect
#' and it could lead to false negative results from
#' \code{\link{outdated}()}
#' @export
#' @return The master internal configuration list of a project.
#' @seealso \code{\link{make_with_config}}, \code{\link{make}},
#' \code{\link{plan_drake}}, \code{\link{vis_drake_graph}}
#' @param plan same as for \code{\link{make}}
#' @param targets same as for \code{\link{make}}
#' @param envir same as for \code{\link{make}}
#' @param verbose same as for \code{\link{make}}
#' @param hook same as for \code{\link{make}}
#' @param parallelism same as for \code{\link{make}}
#' @param jobs same as for \code{\link{make}}
#' @param packages same as for \code{\link{make}}
#' @param prework same as for \code{\link{make}}
#' @param prepend same as for \code{\link{make}}
#' @param command same as for \code{\link{make}}
#' @param args same as for \code{\link{make}}
#' @param recipe_command same as for \code{\link{make}}
#' @param cache same as for \code{\link{make}}
#' @param timeout same as for \code{\link{make}}
#' @param cpu same as for \code{\link{make}}
#' @param elapsed same as for \code{\link{make}}
#' @param retries same as for \code{\link{make}}
#' @param force same as for \code{\link{make}}
#' @param clear_progress logical, whether to clear
#' the cached progress of the targets readable by
#' @param graph igraph object representing the workflow plan network.
#' Overrides \code{skip_imports}.
#' @param trigger same as for \code{\link{make}}
#' @param imports_only logical, whether to skip building the targets
#' in \code{plan} and just import objects and files.
#' @param skip_imports logical, whether to totally neglect to
#' process the imports and jump straight to the targets. This can be useful
#' if your imports are massive and you just want to test your project,
#' but it is bad practice for reproducible data analysis.
#' This argument is overridden if you supply your own \code{graph} argument.
#' @param skip_safety_checks logical, whether to skip the safety checks
#' on your workflow to save time. Use at your own peril.
#' @param store_meta same as for \code{\link{make}}
#' @param lazy_load same as for \code{\link{make}}
#' @param session_info same as for \code{\link{make}}
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Load drake's canonical example.
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
  plan = plan_drake(),
  targets = drake::possible_targets(plan),
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
  clear_progress = FALSE,
  graph = NULL,
  trigger = drake::default_trigger(),
  imports_only = FALSE,
  skip_imports = FALSE,
  skip_safety_checks = FALSE,
  store_meta = TRUE,
  lazy_load = FALSE,
  session_info = TRUE
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
    cache <- recover_cache(force = force, verbose = verbose)
  }
  if (!force){
    assert_compatible_cache(cache = cache)
  }
  # A storr_rds() cache should already have the right hash algorithms.
  cache <- configure_cache(
    cache = cache,
    clear_progress = clear_progress,
    overwrite_hash_algos = FALSE
  )
  trigger <- match.arg(arg = trigger, choices = triggers())
  if (is.null(graph)){
    graph <- build_drake_graph(plan = plan, targets = targets,
      envir = envir, verbose = verbose, jobs = jobs)
  } else {
    graph <- prune_drake_graph(graph = graph, to = targets, jobs = jobs)
  }
  list(
    plan = plan, targets = targets, envir = envir, cache = cache,
    parallelism = parallelism, jobs = jobs, verbose = verbose, hook = hook,
    prepend = prepend, prework = prework, command = command,
    args = args, recipe_command = recipe_command, graph = graph,
    short_hash_algo = cache$get("short_hash_algo", namespace = "config"),
    long_hash_algo = cache$get("long_hash_algo", namespace = "config"),
    seed = seed, trigger = trigger,
    timeout = timeout, cpu = cpu, elapsed = elapsed, retries = retries,
    imports_only = imports_only, skip_imports = skip_imports,
    skip_safety_checks = skip_safety_checks, store_meta = store_meta,
    lazy_load = lazy_load, session_info = session_info
  )
}

add_packages_to_prework <- function(packages, prework) {
  packages <- c("methods", packages) %>% unique
  if (!length(packages))
    return(prework)
  paste0("if(!R.utils::isPackageLoaded(\"", packages, "\")) require(",
    packages, ")", sep = "") %>% c(prework)
}

#' @title Internal function \code{do_prework}
#' @export
#' @keywords internal
#' @description Run the \code{prework} of a \code{\link{make}()}.
#' For internal use only.
#' the only reason this function is exported
#' is to set up PSOCK clusters efficiently.
#' @return Inivisibly returns \code{NULL}.
#' @param config internal configuration list
#' @param verbose_packages logical, whether to print
#' package startup messages
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Load drake's canonical example.
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

#' @title Function \code{possible_targets}
#' @description internal function, returns the list of
#' possible targets that you can select with the \code{targets}
#' argument to \code{\link{make}()}.
#' @seealso \code{\link{make}}
#' @export
#' @return Character vector of possible targets given the workflow plan.
#' @param plan workflow plan data frame
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Load the canonical drake example.
#' # List the possible targets you could choose for the
#' # `targets` argument to make(). You may choose any subset.
#' possible_targets(my_plan)
#' })
#' }
possible_targets <- function(plan = plan_drake()) {
  plan <- sanitize_plan(plan)
  as.character(plan$target)
}

#' @title Internal function \code{store_drake_config}
#' @description Store an internal configuration list
#' from \code{\link{drake_config}()}.
#' Exported for demonstration and tinkering purposes.
#' @export
#' @keywords internal
#' @param config Internal configuration list
#' @return Nothing.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example()
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
