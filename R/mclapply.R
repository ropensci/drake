run_mclapply = function(args){
  for(code in args$prework) eval(parse(text = code), envir = args$envir)
  graph_remaining_targets = args$graph
  while(length(V(graph_remaining_targets)))
    graph_remaining_targets = parallel_stage_mclapply(graph_remaining_targets = graph_remaining_targets,
      args = args)
}

parallel_stage_mclapply = function(graph_remaining_targets, args){
  next_targets = next_targets(graph_remaining_targets)
  prune_envir(next_targets = next_targets, args = args)
  mclapply(next_targets, build, mc.cores = args$jobs, args = args)
  delete_vertices(graph_remaining_targets, v = next_targets)
}
