run_future_lapply <- function(config){
  prepare_distributed(config = config)
  config$workers <- as.character(seq_len(config$jobs))
  mc_init_worker_cache(config)
  callr::r_bg(
    func = function(config){
      drake::mc_master(config)
    },
    args = list(config = config)
  )
  tmp <- future.apply::future_lapply(
    X = config$workers,
    FUN = fl_worker,
    cache_path = config$cache$driver$path,
    future.globals = FALSE
  )
  finish_distributed(config = config)
}

fl_worker <- function(worker, cache_path){
  try_message({
    config <- recover_drake_config(cache_path = cache_path)
    config$schedule <- targets_graph(config)
    do_prework(config = config, verbose_packages = FALSE)
    mc_worker(worker = worker, config = config)
  })
}
