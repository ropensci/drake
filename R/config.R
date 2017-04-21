config = function(plan, targets, envir, jobs,
  parallelism = drake::parallelism_choices(), verbose, packages,
  prework, prepend, command, args){
  plan = sanitize_plan(plan)
  targets = sanitize_targets(plan, targets)
  parallelism = match.arg(parallelism)
  prework = add_packages_to_prework(packages = packages,
    prework = prework)
  cache = storr_rds(cachepath, mangle_key = TRUE)
  cache$clear(namespace = "status")
  envir = envir %>% as.list %>% list2env(parent = globalenv())
  lapply(ls(envir), function(target)
    if(is.function(envir[[target]]))
      environment(envir[[target]]) = envir)
  graph = build_graph(plan = plan, targets = targets,
    envir = envir)
  order = topological.sort(graph)$name
  list(plan = plan, targets = targets, envir = envir, cache = cache,
    parallelism = parallelism, jobs = jobs, verbose = verbose,
    prepend = prepend, prework = prework, command = command, args = args,
    graph = graph, order = order, inventory = cache$list(),
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
