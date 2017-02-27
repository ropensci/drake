#' @export
run = function(workflow, targets = workflow$target, envir = parent.frame(), 
  parallelism = c("mclapply", "Makefile"), jobs = 1){
  force(envir)
  parallelism = match.arg(parallelism)
  targets = intersect(targets, workflow$target)
  graph = graph(worflow, targets, envir)
  cache = storr_rds(cachepath, mangle_key = TRUE)
  cache$clear(namespace = "status")
  if(jobs < 2) 
    run_mclapply(workflow = workflow, targets = targets, 
      envir = envir, graph = graph, jobs = jobs, cache = cache)
  else if(parallelism == "Makefile")
    run_makefile(workflow = workflow, targets = targets, envir = envir, graph = graph, 
      jobs = jobs)
}

run_mclapply = function(workflow, targets, envir, graph, jobs, cache){
  next_graph = entire_graph = graph
  while(length(V(graph))) 
    next_graph = parallel_stage(workflow = workflow, targets = targets, envir = envir, next_graph = next_graph, 
      entire_graph = entire_graph, jobs = jobs, cache = cache)
}

parallel_stage = function(workflow, targets, envir, next_graph, entire_graph, processes, cache){
  number_dependencies = sapply(V(next_graph), 
    function(x) length(adjacent_vertices(next_graph, x, mode = "in")))
  next_targets = which(!number_dependencies)
  prune_envir(envir = envir, next_targets = next_targets, 
    entire_graph = entire_graph, workflow = workflow, cache = cache)
  mclapply(next_targets, build, mc.cores = processes, workflow = workflow, envir = envir,
    cache = cache)
  delete_vertices(graph, v = next_targets)
}

prune_envir = function(envir, next_targets, entire_graph, workflow, cache){
  load_these = graphical_dependencies(targets = next_targets, graph = entire_graph) %>% 
    Filter(f = is_not_file) %>% intersect(y = workflow$target)
  unload_these = intersect(workflow$target, ls(envir)) %>% setdiff(y = load_these)
  rm(list = unload_these, envir = envir)
  lapply(load_these, loadd, envir = envir) # REMEMBER TO SET ENVIRONMENT OF FUNCTION IN LOADS
}

cachepath = ".skipit"
