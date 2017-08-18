run_parallel = function(config, worker){
  graph_remaining_targets = config$graph
  while(length(V(graph_remaining_targets)))
    graph_remaining_targets = parallel_stage(
      graph_remaining_targets = graph_remaining_targets,
      worker = worker, config = config)
}

parallel_stage = function(graph_remaining_targets, worker,
  config){
  config = inventory(config)
  remaining_targets = V(graph_remaining_targets) %>% names %>% intersect(config$targets)
  candidates = next_targets(graph_remaining_targets)
  if config$verbose
    cat("checking hashes for build phase, ", length(remaining_targets), "/", length(config$targets), " targets left\n", sep = "")
  hash_list = hash_list(targets = candidates, config = config)
  build_these = Filter(candidates, f = function(target)
    should_build(target = target, hash_list = hash_list,
      config = config))
  hash_list = hash_list[build_these]
  if(length(build_these))
    worker(targets = build_these, hash_list = hash_list, config = config)
  delete_vertices(graph_remaining_targets, v = candidates)
}
