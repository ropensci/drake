#' @title Function config
#' @description Compute the internal runtime parameter list of
#' \code{\link{make}()}. This could save time if you are planning
#' multiple function calls of functions like \code{\link{outdated}()}
#' or \code{\link{plot_graph}()}. Drake needs to import and cache files
#' and objects to compute the configuration list, which in turn
#' supports user-side functions to help with visualization and parallelism.
#' The result differs from
#' \code{\link{make}(..., imports_only = TRUE, return_config = TRUE)}
#' in that the graph includes both the targets and the imports,
#' not just the imports.
#' @export
#' @seealso \code{\link{plan}}, \code{\link{make}}, \code{\link{plot_graph}}
#' @examples
#' \dontrun{
#' load_basic_example()
#' con <- config(my_plan)
#' outdated(my_plan, config = con)
#' missed(my_plan, config = con)
#' max_useful_jobs(my_plan, config = con)
#' plot_graph(my_plan, config = con)
#' dataframes_graph(my_plan, config = con)
#' }
#' @param plan same as for \code{\link{make}}
#' @param targets same as for \code{\link{make}}
#' @param envir same as for \code{\link{make}}
#' @param verbose same as for \code{\link{make}}
#' @param parallelism same as for \code{\link{make}}
#' @param jobs same as for \code{\link{make}}
#' @param packages same as for \code{\link{make}}
#' @param prework same as for \code{\link{make}}
#' @param prepend same as for \code{\link{make}}
#' @param command same as for \code{\link{make}}
#' @param args same as for \code{\link{make}}
#' @param short_hash_algo same as for \code{\link{make}}
#' @param long_hash_algo same as for \code{\link{make}}
config <- function(plan, targets = drake::possible_targets(plan),
  envir = parent.frame(), verbose = TRUE,
  parallelism = drake::default_parallelism(),
  jobs = 1, packages = (.packages()), prework = character(0),
  prepend = character(0), command = character(0), args = character(0),
  short_hash_algo = NULL,
  long_hash_algo = NULL
){
  force(envir)
  config <- make(imports_only = TRUE, return_config = TRUE,
    clear_progress = FALSE, plan = plan, targets = targets,
    envir = envir, verbose = verbose, parallelism = parallelism,
    jobs = jobs, packages = packages, prework = prework,
    prepend = prepend, command = command, args = args,
    short_hash_algo = short_hash_algo, long_hash_algo = long_hash_algo
  )
  config$graph <- build_graph(plan = plan, targets = targets,
    envir = envir, verbose = verbose)
  config
}

build_config <- function(plan, targets, envir, jobs,
  parallelism = drake::parallelism_choices(),
  verbose, packages, prework, prepend, command,
  args, clear_progress = TRUE,
  short_hash_algo, long_hash_algo
){
  plan <- sanitize_plan(plan)
  targets <- sanitize_targets(plan, targets)
  parallelism <- match.arg(parallelism)
  prework <- add_packages_to_prework(packages = packages,
    prework = prework)
  hash_algos <- choose_hash_algos(
    preferred_short = short_hash_algo,
    preferred_long = long_hash_algo
  )
  cache <- storr_rds(cache_dir, mangle_key = TRUE,
    hash_algorithm = hash_algos$short)
  cache$driver$path <- normalizePath(cache$driver$path)
  if (clear_progress){
    cache$clear(namespace = "progress")
  }
  graph <- build_graph(plan = plan,
    targets = targets, envir = envir, verbose = verbose)
  list(plan = plan, targets = targets, envir = envir, cache = cache,
    parallelism = parallelism, jobs = jobs, verbose = verbose,
    prepend = prepend, prework = prework, command = command,
    args = args, graph = graph,
    short_hash_algo = hash_algos$short, long_hash_algo = hash_algos$long,
    inventory = cache$list(),
    inventory_filemtime = cache$list(namespace = "filemtime"))
}

add_packages_to_prework <- function(packages, prework) {
  packages <- c("methods", packages) %>% unique
  if (!length(packages))
    return(prework)
  paste0("if(!R.utils::isPackageLoaded(\"", packages, "\")) require(",
    packages, ")", sep = "") %>% c(prework)
}

do_prework <- function(config, verbose_packages) {
  wrapper <- ifelse(verbose_packages, invisible,
    base::suppressPackageStartupMessages)
  for (code in config$prework) wrapper(eval(parse(text = code),
    envir = config$envir))
}

inventory <- function(config) {
  config$inventory <- config$cache$list()
  config$inventory_filemtime <- config$cache$list(namespace = "filemtime")
  config
}

#' @title Function \code{possible_targets}
#' @description internal function, returns the list of
#' possible targets that you can select with the \code{targets}
#' argument to \code{\link{make}()}.
#' @seealso \code{\link{make}}
#' @export
#' @return character vector of possible targets
#' @param plan workflow plan data frame
#' @examples
#' \dontrun{
#' load_basic_example()
#' possible_targets(my_plan)
#' }
possible_targets <- function(plan) {
  plan <- sanitize_plan(plan)
  c(as.character(plan$output), as.character(plan$target))
}

store_config <- function(config) {
  save_these <- setdiff(names(config), "envir")  # envir could get massive.
  lapply(save_these, function(item) config$cache$set(key = item,
    value = config[[item]], namespace = "config"))
}
