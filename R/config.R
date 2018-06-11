#' @title Create the internal runtime parameter list
#'   used internally in [make()].
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
#'   [drake_plan()], [vis_drake_graph()]
#' @param plan workflow plan data frame.
#'   A workflow plan data frame is a data frame
#'   with a `target` column and a `command` column.
#'   (See the details in the [drake_plan()] help file
#'   for descriptions of the optional columns.)
#'   Targets are the objects and files that drake generates,
#'   and commands are the pieces of R code that produce them.
#'   Use the function [drake_plan()] to generate workflow plan
#'   data frames easily, and see functions [plan_analyses()],
#'   [plan_summaries()], [evaluate_plan()],
#'   [expand_plan()], and [gather_plan()] for
#'   easy ways to generate large workflow plan data frames.
#'
#' @param targets character vector, names of targets to build.
#'   Dependencies are built too. Together, the `plan` and
#'   `targets` comprise the workflow network
#'   (i.e. the `graph` argument).
#'   Changing either will change the network.
#'
#' @param envir environment to use. Defaults to the current
#'   workspace, so you should not need to worry about this
#'   most of the time. A deep copy of `envir` is made,
#'   so you don't need to worry about your workspace being modified
#'   by `make`. The deep copy inherits from the global environment.
#'   Wherever necessary, objects and functions are imported
#'   from `envir` and the global environment and
#'   then reproducibly tracked as dependencies.
#'
#' @param verbose logical or numeric, control printing to the console.
#'   Use `pkgconfig` to set the default value of `verbose` 
#'   for your R session:
#'   for example, `pkgconfig::set_config("drake::verbose" = 2)`.
#'   \describe{
#'     \item{0 or `FALSE`:}{print nothing.}
#'     \item{1 or `TRUE`:}{print only targets to build.}
#'     \item{2:}{in addition, print checks and cache info.}
#'     \item{3:}{in addition, print any potentially missing items.}
#'     \item{4:}{in addition, print imports. Full verbosity.}
#'   }
#'
#' @param hook function with at least one argument.
#'   The hook is as a wrapper around the code that drake uses
#'   to build a target (see the body of `drake:::build_in_hook()`).
#'   Hooks can control the side effects of build behavior.
#'   For example, to redirect output and error messages to text files,
#'   you might use the built-in [silencer_hook()], as in
#'   `make(my_plan, hook = silencer_hook)`.
#'   The silencer hook is useful for distributed parallelism,
#'   where the calling R process does not have control over all the
#'   error and output streams. See also [output_sink_hook()]
#'   and [message_sink_hook()].
#'   For your own custom hooks, treat the first argument as the code
#'   that builds a target, and make sure this argument is actually evaluated.
#'   Otherwise, the code will not run and none of your targets will build.
#'   For example, \code{function(code){force(code)}} is a good hook
#'   and \code{function(code){message("Avoiding the code")}} is a bad hook.
#'
#' @param skip_targets logical, whether to skip building the targets
#'   in `plan` and just import objects and files.
#'
#' @param imports_only deprecated. Use `skip_targets` instead.
#'
#' @param parallelism character, type of parallelism to use.
#'   To list the options, call [parallelism_choices()].
#'   For detailed explanations, see the
#'   [high-performance computing chapter](https://ropenscilabs.github.io/drake-manual/store.html) # nolint
#'   of the user manual.
#'
#' @param jobs number of parallel processes or jobs to run.
#'   See [max_useful_jobs()] or [vis_drake_graph()]
#'   to help figure out what the number of jobs should be.
#'   Windows users should not set `jobs > 1` if
#'   `parallelism` is `"mclapply"` because
#'   [mclapply()] is based on forking. Windows users
#'   who use `parallelism = "Makefile"` will need to
#'   download and install Rtools.
#'
#'   Imports and targets are processed separately, and they usually
#'   have different parallelism needs. To use at most 2 jobs at a time
#'   for imports and at most 4 jobs at a time for targets, call
#'   `make(..., jobs = c(imports = 2, targets = 4))`.
#'
#'   For `"future_lapply"` parallelism, `jobs`
#'   only applies to the imports.
#'   To set the max number of jobs for `"future_lapply"`
#'   parallelism, set the `workers`
#'   argument where it exists: for example, call
#'   `future::plan(multisession(workers = 4))`,
#'   then call \code{\link{make}(your_plan, parallelism = "future_lapply")}.
#'   You might also try `options(mc.cores = jobs)`,
#'   or see `future::.options`
#'   for environment variables that set the max number of jobs.
#'
#'   If `parallelism` is `"Makefile"`,  Makefile-level parallelism is
#'   only used for targets in your workflow plan data frame, not imports.  To
#'   process imported objects and files, drake selects the best parallel backend
#'   for your system and uses the number of jobs you give to the `jobs`
#'   argument to [make()]. To use at most 2 jobs for imports and at
#'   most 4 jobs for targets, run
#'   `make(..., parallelism = "Makefile", jobs = c(imports = 2, targets = 4))` or
#'   `make(..., parallelism = "Makefile", jobs = 2, args = "--jobs=4")`.
#'
#' @param packages character vector packages to load, in the order
#'   they should be loaded. Defaults to `rev(.packages())`, so you
#'   should not usually need to set this manually. Just call
#'   [library()] to load your packages before `make()`.
#'   However, sometimes packages need to be strictly forced to load
#'   in a certain order, especially if `parallelism` is
#'   `"Makefile"`. To do this, do not use [library()]
#'   or [require()] or [loadNamespace()] or
#'   [attachNamespace()] to load any libraries beforehand.
#'   Just list your packages in the `packages` argument in the order
#'   you want them to be loaded.
#'   If `parallelism` is `"mclapply"`,
#'   the necessary packages
#'   are loaded once before any targets are built. If `parallelism` is
#'   `"Makefile"`, the necessary packages are loaded once on
#'   initialization and then once again for each target right
#'   before that target is built.
#'
#' @param prework character vector of lines of code to run
#'   before build time. This code can be used to
#'   load packages, set options, etc., although the packages in the
#'   `packages` argument are loaded before any prework is done.
#'   If `parallelism` is `"mclapply"`, the `prework`
#'   is run once before any targets are built. If `parallelism` is
#'   `"Makefile"`, the prework is run once on initialization
#'   and then once again for each target right before that target is built.
#'
#' @param prepend lines to prepend to the Makefile if `parallelism`
#'   is `"Makefile"`. See the [high-performance computing guide](https://ropenscilabs.github.io/drake-manual/store.html) # nolint
#'   to learn how to use `prepend`
#'   to take advantage of multiple nodes of a supercomputer.
#'
#' @param command character scalar, command to call the Makefile
#'   generated for distributed computing.
#'   Only applies when `parallelism` is `"Makefile"`.
#'   Defaults to the usual `"make"`
#'   ([default_Makefile_command()]),
#'   but it could also be
#'   `"lsmake"` on supporting systems, for example.
#'   `command` and `args` are executed via
#'   \code{\link{system2}(command, args)} to run the Makefile.
#'   If `args` has something like `"--jobs=2"`, or if
#'   `jobs >= 2` and `args` is left alone, targets
#'   will be distributed over independent parallel R sessions
#'   wherever possible.
#'
#' @param args command line arguments to call the Makefile for
#'   distributed computing. For advanced users only. If set,
#'   `jobs` and `verbose` are overwritten as they apply to the
#'   Makefile.
#'   `command` and `args` are executed via
#'   \code{\link{system2}(command, args)} to run the Makefile.
#'   If `args` has something like `"--jobs=2"`, or if
#'   `jobs >= 2` and `args` is left alone, targets
#'   will be distributed over independent parallel R sessions
#'   wherever possible.
#'
#' @param recipe_command Character scalar, command for the
#'   Makefile recipe for each target.
#'
#' @param log_progress logical, whether to log the progress
#'   of individual targets as they are being built. Progress logging
#'   creates a lot of little files in the cache, and it may make builds
#'   a tiny bit slower. So you may see gains in storage efficiency
#'   and speed with
#'   `make(..., log_progress = FALSE)`. But be warned that
#'   [progress()] and [in_progress()]
#'   will no longer work if you do that.
#'
#' @param cache drake cache as created by [new_cache()].
#'   See also [get_cache()], [this_cache()],
#'   and [recover_cache()]
#'
#' @param fetch_cache character vector containing lines of code.
#'   The purpose of this code is to fetch the `storr` cache
#'   with a command like [storr_rds()] or [storr_dbi()],
#'   but customized. This feature is experimental. It will turn out
#'   to be necessary if you are using both custom non-RDS caches
#'   and distributed parallelism (`parallelism = "future_lapply"`
#'   or `"Makefile"`) because the distributed R sessions
#'   need to know how to load the cache.
#'
#' @param timeout Seconds of overall time to allow before imposing
#'   a timeout on a target. Passed to `R.utils::withTimeout()`.
#'   Assign target-level timeout times with an optional `timeout`
#'   column in `plan`.
#'
#' @param cpu Seconds of cpu time to allow before imposing
#'   a timeout on a target. Passed to `R.utils::withTimeout()`.
#'   Assign target-level cpu timeout times with an optional `cpu`
#'   column in `plan`.
#'
#' @param elapsed Seconds of elapsed time to allow before imposing
#'   a timeout on a target. Passed to `R.utils::withTimeout()`.
#'   Assign target-level elapsed timeout times with an optional `elapsed`
#'   column in `plan`.
#'
#' @param retries Number of retries to execute if the target fails.
#'   Assign target-level retries with an optional `retries`
#'   column in `plan`.
#'
#' @param force Force `make()` to build your targets even if some
#'   about your setup is not quite right: for example, if you are using
#'   a version of drake that is not back compatible with your project's cache.
#'
#' @param graph An `igraph` object from the previous `make()`.
#'   Supplying a pre-built graph could save time.
#'   The graph is constructed by [build_drake_graph()].
#'   You can also get one from \code{\link{config}(my_plan)$graph}.
#'   Overrides `skip_imports`.
#'
#' @param trigger Name of the trigger to apply to all targets.
#'   Ignored if `plan` has a `trigger` column.
#'   Must be in [triggers()].
#'   See [triggers] for explanations of the choices.
#'
#' @param skip_imports logical, whether to totally neglect to
#'   process the imports and jump straight to the targets. This can be useful
#'   if your imports are massive and you just want to test your project,
#'   but it is bad practice for reproducible data analysis.
#'   This argument is overridden if you supply your own `graph` argument.
#'
#' @param skip_safety_checks logical, whether to skip the safety checks
#'   on your workflow. Use at your own peril.
#'
#' @param lazy_load either a character vector or a logical. Choices:
#'   - `"eager"`: no lazy loading. The target is loaded right away
#'     with [assign()].
#'   - `"promise"`: lazy loading with [delayedAssign()]
#'   - `"bind"`: lazy loading with active bindings:
#'     [bindr::populate_env()].
#'   - `TRUE`: same as `"promise"`.
#'   - `FALSE`: same as `"eager"`.
#'
#'   `lazy_load` should not be `"promise"`
#'   for `"parLapply"` parallelism combined with `jobs` greater than 1.
#'   For local multi-session parallelism and lazy loading, try
#'   `library(future); future::plan(multisession)` and then
#'   `make(..., parallelism = "future_lapply", lazy_load = "bind")`.
#'
#'   If `lazy_load` is `"eager"`,
#'   drake prunes the execution environment before each target/stage,
#'   removing all superfluous targets
#'   and then loading any dependencies it will need for building.
#'   In other words, drake prepares the environment in advance
#'   and tries to be memory efficient.
#'   If `lazy_load` is `"bind"` or `"promise"`, drake assigns
#'   promises to load any dependencies at the last minute.
#'   Lazy loading may be more memory efficient in some use cases, but
#'   it may duplicate the loading of dependencies, costing time.
#'
#' @param session_info logical, whether to save the `sessionInfo()`
#'   to the cache. This behavior is recommended for serious [make()]s
#'   for the sake of reproducibility. This argument only exists to
#'   speed up tests. Apparently, `sessionInfo()` is a bottleneck
#'   for small [make()]s.
#'
#' @param cache_log_file Name of the cache log file to write.
#'   If `TRUE`, the default file name is used (`drake_cache.log`).
#'   If `NULL`, no file is written.
#'   If activated, this option uses
#'   [drake_cache_log_file()] to write a flat text file
#'   to represent the state of the cache
#'   (fingerprints of all the targets and imports).
#'   If you put the log file under version control, your commit history
#'   will give you an easy representation of how your results change
#'   over time as the rest of your project changes. Hopefully,
#'   this is a step in the right direction for data reproducibility.
#'
#' @param seed integer, the root pseudo-random seed to use for your project.
#'   To ensure reproducibility across different R sessions,
#'   `set.seed()` and `.Random.seed` are ignored and have no affect on
#'   `drake` workflows. Conversely, `make()` does not change `.Random.seed`,
#'   even when pseudo-random numbers are generated.
#'
#'   On the first call to `make()` or `drake_config()`, `drake`
#'   uses the random number generator seed from the `seed` argument.
#'   Here, if the `seed` is `NULL` (default), `drake` uses a `seed` of `0`.
#'   On subsequent `make()`s for existing projects, the project's
#'   cached seed will be used in order to ensure reproducibility.
#'   Thus, the `seed` argument must either be `NULL` or the same
#'   seed from the project's cache (usually the `.drake/` folder).
#'   To reset the random number generator seed for a project,
#'   use `clean(destroy = TRUE)`.
#'
#' @param caching character string, only applies to `"future"` parallelism.
#'
#' @param keep_going logical, whether to still keep running [make()]
#'   if targets fail.
#'
#' @param session An optional `callr` function if you want to
#'   build all your targets in a separate master session:
#'   for example, `make(plan = my_plan, session = callr::r_vanilla)`.
#'   Running `make()` in a clean, isolated
#'   session can enhance reproducibility.
#'   But be warned: if you do this, [make()] will take longer to start.
#'   If `session` is `NULL` (default), then [make()] will just use
#'   your current R session as the master session. This is slightly faster,
#'   but it causes [make()] to populate your workspace/environment
#'   with the last few targets it builds.
#'
#' @param pruning_strategy Character scalar, either `"speed"` (default)
#'   or `"memory"`. These are alternative approaches to how `drake`
#'   keeps non-import dependencies in memory when it builds a target.
#'   If `pruning_strategy` is `"memory"`, `drake` removes all targets
#'   from memory (i.e. `config$envir`) except the direct dependencies
#'   of the target is about to build. This is suitable for data so large
#'   that the optimal strategy is to minimize memory consumption.
#'   If `pruning_strategy` is `"speed"`, `drake` loads all the dependencies
#'   and keeps in memory everything that will eventually be a
#'   dependency of a downstream target. This strategy consumes more
#'   memory, but does more to minimize the number of times data is
#'   read from storage/disk.
#'
#' @param makefile_path Path to the `Makefile` for
#'   `make(parallelism = "Makefile")`. If you set this argument to a
#'   non-default value, you are responsible for supplying this same
#'   path to the `args` argument so `make` knows where to find it.
#'   Example: `make(parallelism = "Makefile", makefile_path = ".drake/.makefile", command = "make", args = "--file=.drake/.makefile")` # nolint
#'
#' @param console_log_file character scalar or `NULL`.
#'   If `NULL`, console output will be printed
#'   to the R console using `message()`.
#'   Otherwise, `console_log_file` should be the name of a flat file.
#'   Console output will be appended to that file.
#'
#' @param ensure_workers logical, whether the master process
#'   should wait for the workers to post before assigning them
#'   targets. Should usually be `TRUE`. Set to `FALSE`
#'   for `make(parallelism = "future_lapply", jobs = n)`
#'   (`n > 1`) when combined with `future::plan(future::sequential)`. 
#'   This argument only applies to parallel computing with persistent workers
#'   (`make(parallelism = x)`, where `x` could be `"mclapply"`,
#'   `"parLapply"`, or `"future_lapply"`).
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
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
  plan = read_drake_plan(),
  targets = drake::possible_targets(plan),
  envir = parent.frame(),
  verbose = drake::default_verbose(),
  hook = default_hook,
  cache = drake::get_cache(
    verbose = verbose, force = force, console_log_file = console_log_file),
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
  skip_targets = FALSE,
  skip_imports = FALSE,
  skip_safety_checks = FALSE,
  lazy_load = "eager",
  session_info = TRUE,
  cache_log_file = NULL,
  seed = NULL,
  caching = c("worker", "master"),
  keep_going = FALSE,
  session = NULL,
  imports_only = NULL,
  pruning_strategy = c("speed", "memory"),
  makefile_path = "Makefile",
  console_log_file = NULL,
  ensure_workers = TRUE
){
  force(envir)
  unlink(console_log_file)
  if (!is.null(imports_only)){
    warning(
      "Argument imports_only is deprecated. Use skip_targets instead.",
      call. = FALSE
    ) # May 4, 2018
  }
  plan <- sanitize_plan(plan)
  targets <- sanitize_targets(plan, targets)
  parallelism <- parse_parallelism(parallelism)
  jobs <- parse_jobs(jobs)
  prework <- add_packages_to_prework(
    packages = packages,
    prework = prework
  )
  if (is.null(cache)) {
    cache <- recover_cache(
      force = force,
      verbose = verbose,
      fetch_cache = fetch_cache,
      console_log_file = console_log_file
    )
  }
  if (!force){
    assert_compatible_cache(cache = cache)
  }
  # A storr_rds() cache should already have the right hash algorithms.
  cache <- configure_cache(
    cache = cache,
    overwrite_hash_algos = FALSE,
    jobs = jobs,
    init_common_values = TRUE
  )
  seed <- choose_seed(supplied = seed, cache = cache)
  trigger <- match.arg(arg = trigger, choices = triggers())
  if (is.null(graph)){
    graph <- build_drake_graph(plan = plan, targets = targets,
      envir = envir, verbose = verbose, jobs = jobs,
      sanitize_plan = FALSE, console_log_file = console_log_file)
  } else {
    graph <- prune_drake_graph(graph = graph, to = targets, jobs = jobs)
  }
  cache_path <- force_cache_path(cache)
  lazy_load <- parse_lazy_arg(lazy_load)
  pruning_strategy <- match.arg(pruning_strategy)
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
    skip_targets = skip_targets, skip_imports = skip_imports,
    skip_safety_checks = skip_safety_checks, log_progress = log_progress,
    lazy_load = lazy_load, session_info = session_info,
    cache_log_file = cache_log_file, caching = match.arg(caching),
    evaluator = future::plan("next"), keep_going = keep_going,
    session = session, pruning_strategy = pruning_strategy,
    makefile_path = makefile_path, console_log_file = console_log_file,
    ensure_workers = ensure_workers
  )
}

add_packages_to_prework <- function(packages, prework) {
  packages <- unique(c("methods", packages))
  paste0("if(!R.utils::isPackageLoaded(\"", packages, "\")) require(",
    packages, ")", sep = "") %>% c(prework)
}

#' @title Do the prework in the `prework`
#'   argument to [make()].
#' @export
#' @keywords internal
#' @description For internal use only.
#' The only reason this function is exported
#' is to set up parallel socket (PSOCK) clusters
#' without too much fuss.
#' @return Inivisibly returns `NULL`.
#' @param config internal configuration list
#' @param verbose_packages logical, whether to print
#'   package startup messages
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
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
#'   argument to [make()], given a workflow plan
#'   data frame.
#' @description Intended for internal use only.
#' @seealso [make()]
#' @keywords internal
#' @export
#' @return Character vector of possible targets given the workflow plan.
#' @param plan workflow plan data frame
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' # List the possible targets you could choose for the
#' # `targets` argument to make(). You may choose any subset.
#' possible_targets(my_plan)
#' })
#' }
possible_targets <- function(plan = read_drake_plan()) {
  plan <- sanitize_plan(plan)
  as.character(plan$target)
}

#' @title Store an internal configuration list
#'   from [drake_config()].
#' @description Exported for demonstration and tinkering purposes
#' only. Not meant to be called by the user.
#' @export
#' @keywords internal
#' @param config Internal configuration list
#' @return Nothing.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' config <- drake_config(my_plan)
#' store_drake_config(config = config)
#' read_drake_config()
#' })
#' }
store_drake_config <- function(config) {
  config$cache$flush_cache() # Less data to save this way.
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

parse_jobs <- function(jobs){
  check_jobs(jobs)
  if (length(jobs) < 2){
    c(imports = jobs, targets = jobs)
  } else {
    jobs
  }
}

parse_parallelism <- function(parallelism){
  check_parallelism(parallelism)
  for (i in seq_along(parallelism)){
    parallelism[i] <- match.arg(
      arg = parallelism[i],
      choices = parallelism_choices(distributed_only = FALSE)
    )
  }
  if (length(parallelism) < 2){
    if (parallelism %in% parallelism_choices(distributed_only = TRUE)){
      c(imports = default_parallelism(), targets = parallelism)
    } else {
      c(imports = parallelism, targets = parallelism)
    }
  } else {
    parallelism
  }
}
