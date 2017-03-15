run_parLapply = function(config){
  run_parallel(config = config, worker = worker_parLapply)
}

worker_parLapply = function(targets, hash_list, config){
  parLapply(cl = config$cluster, X = targets, fun = build, 
    hash_list = hash_list, config = config)
}
