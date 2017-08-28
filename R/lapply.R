run_lapply <- function(config){
  do_prework(config = config, verbose_packages = TRUE)
  run_parallel(config = config, worker = worker_lapply)
}

worker_lapply <- function(targets, hash_list, config){
  prune_envir(targets = targets, config = config)
  values <- lapply(X = targets, FUN = build,
    hash_list = hash_list, config = config)
  assign_to_envir(target = targets, value = values, config = config)
}
