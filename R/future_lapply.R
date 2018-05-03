run_future_lapply <- function(config){
  prepare_distributed(config = config)
  config$workers <- as.character(seq_len(config$jobs))
  mc_init_worker_cache(config)
  tmp <- future.apply::future_lapply(
    X = c("0", config$workers),
    FUN = fl_process,
    cache_path = config$cache$driver$path,
    future.globals = FALSE
  )
  finish_distributed(config = config)
}

fl_process <- function(id, cache_path){
  config <- recover_drake_config(cache_path = cache_path)
  try_message({
    if (id == "0"){
      mc_master(config)
    } else {
      fl_worker(worker = id, config = config)
    }
  })
}

fl_worker <- function(worker, config){
  do_prework(config = config, verbose_packages = FALSE)
  mc_worker(worker = worker, config = config)
}
