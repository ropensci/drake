run_lapply <- function(config){
  run_staged_parallelism(config = config, worker = worker_lapply)
}

worker_lapply <- function(targets, meta_list, config){
  prune_envir(targets = targets, config = config)
  values <- lapply(
    X = targets,
    FUN = drake_build_worker,
    meta_list = meta_list,
    config = config
  )
  assign_to_envir(targets = targets, values = values, config = config)
}
