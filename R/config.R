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
#' con = config(my_plan)
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
config = function(plan, targets = drake::possible_targets(plan),
  envir = parent.frame(), verbose = TRUE, 
  parallelism = drake::default_parallelism(), jobs = 1,
  packages = (.packages()), prework = character(0)){
  force(envir)
  config = make(imports_only = TRUE, return_config = TRUE, clear_progress = FALSE,
      plan = plan, targets = targets, envir = envir,
      verbose = verbose, parallelism = parallelism, jobs = jobs,
      packages = packages, prework = prework)
  config$graph = build_graph(plan = plan, targets = targets, envir = envir,
                             verbose = verbose)
  config
}

build_config = function(plan, targets, envir, jobs,
  parallelism = drake::parallelism_choices(), verbose, packages,
  prework, prepend, command, args, clear_progress = TRUE){
  plan = sanitize_plan(plan)
  targets = sanitize_targets(plan, targets)
  parallelism = match.arg(parallelism)
  prework = add_packages_to_prework(packages = packages,
    prework = prework)
  cache = storr_rds(cachepath, mangle_key = TRUE, hash_algorithm = hash_algorithm())
  if(clear_progress) cache$clear(namespace = "progress")
  graph = build_graph(plan = plan, targets = targets,
    envir = envir, verbose = verbose)
  list(plan = plan, targets = targets, envir = envir, cache = cache,
    parallelism = parallelism, jobs = jobs, verbose = verbose,
    prepend = prepend, prework = prework, command = command, args = args,
    graph = graph, inventory = cache$list(),
    inventory_filemtime = cache$list(namespace = "filemtime"))
}

add_packages_to_prework = function(packages, prework){
  packages = c("methods", packages) %>% unique
  if(!length(packages)) return(prework)
  package_list = deparse(packages) %>% paste(collapse = "\n")
  paste0("if(!R.utils::isPackageLoaded(\"", packages, "\")) require(",
    packages, ")", sep = "") %>%
    c(prework)
}

do_prework = function(config, verbosePackages){
  wrapper = ifelse(verbosePackages, invisible, 
    suppressPackageStartupMessages)
  for(code in config$prework)
    wrapper(eval(parse(text = code), envir = config$envir))
}

inventory = function(config){
  config$inventory = config$cache$list()
  config$inventory_filemtime = config$cache$list(namespace = "filemtime")
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
possible_targets = function(plan){
  plan = sanitize_plan(plan)
  c(as.character(plan$output), as.character(plan$target))
}

store_config = function(config){
  save_these = setdiff(names(config), "envir") # envir could get massive.
  lapply(save_these, function(item)
    config$cache$set(key = item, value = config[[item]], 
      namespace = "config"))
}
