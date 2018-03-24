run_future_lapply <- function(config){
  prepare_distributed(config = config)
  run_staged_parallelism(config = config, worker = worker_future_lapply)
  finish_distributed(config = config)
}

worker_future_lapply <- function(targets, meta_list, config){
  targets <- intersect(targets, V(targets_graph(config$schedule))$name)
  # Probably will not encounter this, but it is better to have:
  if (!length(targets)){ # nocov # nolint
    return()             # nocov
  }                      # nocov
  future::future_lapply(
    x = targets,
    FUN = build_distributed,
    cache_path = config$cache$driver$path,
    meta_list = meta_list,
    future.globals = FALSE
  )
}
