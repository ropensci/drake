run_lapply = function(config){
  do_prework(config = config, verbosePackages = TRUE)
  run_parallel(config = config, worker = worker_lapply)
}

worker_lapply = function(targets, hash_list, config){
  prune_envir(targets = targets, config = config)
  lapply(X = targets, FUN = build,
    hash_list = hash_list, config = config)
  assign_to_envir(target = target, value = value, config = config)
}
