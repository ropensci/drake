run_mclapply = function(config){
  run_parallel(config = config, worker = worker_mclapply)
}

worker_mclapply = function(targets, hash_list, config){
  mclapply(targets, build, hash_list = hash_list,
    config = config, mc.cores = config$jobs)
}
