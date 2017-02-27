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
  targets = intersect(targets, plan$target)
  graph = graph(plan, targets, envir)
  cache = storr_rds(cachepath, mangle_key = TRUE)
  cache$clear(namespace = "status")
  if(jobs < 2) 
    run_mclapply(plan = plan, targets = targets, 
      envir = envir, graph = graph, jobs = jobs, cache = cache)
#  else if(parallelism == "Makefile")
#    run_makefile(plan = plan, targets = targets, envir = envir, 
#      graph = graph, 
#      jobs = jobs)
}

run_mclapply = function(plan, targets, envir, graph, jobs, cache){
  next_graph = entire_graph = graph
  while(length(V(graph))) 
    next_graph = parallel_stage(plan = plan, targets = targets, 
      envir = envir, next_graph = next_graph, 
      entire_graph = entire_graph, jobs = jobs, cache = cache)
}

parallel_stage = function(plan, targets, envir, next_graph, 
  entire_graph, processes, jobs, cache){
  number_dependencies = sapply(V(next_graph), 
    function(x) length(adjacent_vertices(next_graph, x, mode = "in")))
  next_targets = which(!number_dependencies)
  prune_envir(envir = envir, next_targets = next_targets, 
    entire_graph = entire_graph, plan = plan, cache = cache)
  mclapply(next_targets, build, mc.cores = jobs, plan = plan, envir = envir,
    cache = cache)
  delete_vertices(graph, v = next_targets)
}

prune_envir = function(envir, next_targets, entire_graph, plan, cache){
  load_these = graphical_dependencies(targets = next_targets, graph = entire_graph) %>% 
    Filter(f = is_not_file) %>% intersect(y = plan$target)
  unload_these = intersect(plan$target, ls(envir)) %>% setdiff(y = load_these)
  rm(list = unload_these, envir = envir)
  lapply(load_these, loadd, envir = envir) # REMEMBER TO SET ENVIRONMENT OF FUNCTION IN LOADS
}

cachepath = ".drake"
