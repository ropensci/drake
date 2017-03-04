run_mclapply = function(config){
  for(code in config$prework) eval(parse(text = code), envir = config$envir)
  graph_remaining_targets = config$graph
  while(length(V(graph_remaining_targets)))
    graph_remaining_targets = parallel_stage_mclapply(graph_remaining_targets = graph_remaining_targets,
      config = config)
}

parallel_stage_mclapply = function(graph_remaining_targets, config){
  candidates = next_targets(graph_remaining_targets)
  hash_list = hash_list(targets = candidates, config = config)
  build_these = Filter(candidates, f = function(target)
    should_build(target = target, hash_list = hash_list,
      config = config))
  hash_list = hash_list[build_these]
  if(length(build_these)){
    prune_envir(targets = build_these, config = config)
    mclapply(build_these, build, hash_list = hash_list, 
      config = config, mc.cores = config$jobs)
  }
  delete_vertices(graph_remaining_targets, v = candidates)
}
