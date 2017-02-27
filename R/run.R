#' @title Function \command{run}
#' @description Run your project.
#' @export
#' @param plan workflow plan data frame
#' @param targets targets to bulid
#' @param envir environment to use
#' @param verbose logical, whether to print output to the console
#' @param parallelism character, type of parallelism to use
#' @param jobs number of parallel processes or jobs
#' @param packages packages to load
#' @param prepwork prework to do before running the workflow
#' @param prepend lines to prepend to the Makefile
#' @param command command to call the Makefile
#' @param args command line arguments to the Makefile
run = function(plan, targets = plan$target, envir = parent.frame(), 
  verbose = TRUE, parallelism = c("mclapply", "Makefile"), jobs = 1, 
  packages = character(0), prework = character(0),
  prepend = character(0), command = "make", args = character(0)){
  force(envir)
  parallelism = match.arg(parallelism)
  prework = add_packages_to_prework(packages = packages, 
    prework = prework)
  args = arglist(plan = plan, targets = targets, envir = envir, 
    verbose = verbose, jobs = jobs, prework = prework, args = args)
  args$graph = build_graph(plan = args$plan, targets = args$targets, 
    envir = args$envir)
  if(parallelism == "mclapply") 
    run_mclapply(args)
  else if(parallelism == "Makefile")
    run_makefile(args)
}

add_packages_to_prework = function(packages, prework){
  if(!length(packages)) return(prework)
  package_commands = paste0("libary(", packages, ")")
  c(packages, prework)
}

arglist = function(plan, targets = plan$target, envir = parent.frame(),
  jobs = 1){
  targets = intersect(targets, plan$target)
  cache = storr_rds(cachepath, mangle_key = TRUE)
  cache$clear(namespace = "status")
  list(plan = plan, targets = targets, envir = envir, cache = cache, jobs = jobs)
}

run_mclapply = function(args){
  evals(args$prework, .with = args$envir)
  next_graph = args$graph
  while(length(V(next_graph))) 
    next_graph = parallel_stage(next_graph = next_graph, args = args)
}

parallel_stage = function(next_graph, args){
  number_dependencies = sapply(V(next_graph), 
    function(x) length(adjacent_vertices(next_graph, x, mode = "in")[[1]]))
  next_targets = which(!number_dependencies) %>% names
  prune_envir(next_targets = next_targets, args = args)
  mclapply(next_targets, build, mc.cores = args$jobs, args = args)
  delete_vertices(next_graph, v = next_targets)
}

prune_envir = function(next_targets, args){
  load_these = graphical_dependencies(targets = next_targets, args = args) %>% 
    Filter(f = is_not_file) %>% intersect(y = args$plan$target)
  unload_these = intersect(args$plan$target, ls(args$envir)) %>% setdiff(y = load_these)
  rm(list = unload_these, envir = args$envir)
  if(length(load_these)) loadd(list = load_these, envir = args$envir)
  invisible()
}

cachepath = ".drake"
