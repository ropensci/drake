run_mclapply = function(config){
  for(code in config$prework) eval(parse(text = code), envir = config$envir)
  graph_remaining_targets = config$graph
  while(length(V(graph_remaining_targets)))
    graph_remaining_targets = parallel_stage_mclapply(graph_remaining_targets = graph_remaining_targets,
      config = config)
}

parallel_stage_mclapply = function(graph_remaining_targets, config){
  next_targets = next_targets(graph_remaining_targets)
  prune_envir(next_targets = next_targets, config = config)
  mclapply(next_targets, build, mc.cores = config$jobs, config = config)
  delete_vertices(graph_remaining_targets, v = next_targets)
}
