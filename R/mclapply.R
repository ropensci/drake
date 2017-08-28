run_mclapply <- function(config){
  do_prework(config = config, verbose_packages = TRUE)
  run_parallel(config = config, worker = worker_mclapply)
}

worker_mclapply <- function(targets, hash_list, config){
  prune_envir(targets = targets, config = config)
  values <- mclapply(targets, build, hash_list = hash_list,
    config = config, mc.cores = config$jobs)
  assign_to_envir(target = targets, value = values, config = config)
}
