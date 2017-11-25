run_future_lapply <- function(config){
  prepare_distributed(config = config)
  run_parallel(config = config, worker = worker_future_lapply)
  finish_distributed(config = config)
}

worker_future_lapply <- function(targets, meta_list, config){
  targets <- intersect(targets, config$plan$target)
  if (!length(targets)){
    return()
  }
  future::future_lapply(
    x = targets,
    FUN = build_distributed,
    cache_path = config$cache$driver$path,
    meta_list = meta_list
  )
}
