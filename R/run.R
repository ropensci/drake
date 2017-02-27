#' @title Function \command{run}
#' @description Run your project.
#' @export
#' @param plan workflow plan data frame
#' @param targets targets to bulid
#' @param envir environment to use
#' @param parallelism character, type of parallelism to use
#' @param jobs number of parallel processes or jobs
run = function(plan, targets = plan$target, envir = parent.frame(), 
  parallelism = c("mclapply", "Makefile"), jobs = 1){
  force(envir)
  parallelism = match.arg(parallelism)
  args = arglist(plan = plan, targets = targets, envir = envir,
    parallelism = parallelism, jobs = jobs)
  if(jobs < 2) 
    run_mclapply(args)
#  else if(parallelism == "Makefile")
#    run_makefile(args)
}

arglist = function(plan, targets = plan$target, envir = parent.frame(),
  jobs = 1){
  targets = intersect(targets, plan$target)
  cache = storr_rds(cachepath, mangle_key = TRUE)
  cache$clear(namespace = "status")
  args = list(plan = plan, targets = targets, envir = envir, cache = cache,
    graph = graph(plan, targets, envir), jobs = jobs)
}

run_mclapply = function(args)
  next_graph = args$graph
  while(length(V(graph))) 
    next_graph = parallel_stage(args)
}

parallel_stage = function(next_graph, args)
  number_dependencies = sapply(V(next_graph), 
    function(x) length(adjacent_vertices(next_graph, x, mode = "in")))
  next_targets = which(!number_dependencies)
  prune_envir(next_targets = next_targets, args)
  mclapply(next_targets, build, mc.cores = args$jobs, args = args)
  delete_vertices(graph, v = next_targets)
}

prune_envir = function(next_targets, args){
  load_these = graphical_dependencies(targets = next_targets, args = args) %>% 
    Filter(f = is_not_file) %>% intersect(y = args$plan$target)
  unload_these = intersect(args$plan$target, ls(args$envir)) %>% setdiff(y = load_these)
  rm(list = unload_these, envir = envir)
  lapply(load_these, loadd, envir = envir)
}

cachepath = ".drake"
