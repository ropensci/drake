run_future_lapply <- function(config){
  prepare_distributed(config = config)
  mc_init_worker_cache(config, jobs = future::nbrOfWorkers())
  console_persistent_workers(config)
  path <- normalizePath(config$cache_path, winslash = "/")
  rscript <- grep(
    "Rscript",
    dir(R.home("bin"), full.names = TRUE),
    value = TRUE
  )
  tmp <- system2(
    rscript,
    shQuote(c("-e", paste0("drake::fl_master('", path, "')"))),
    wait = FALSE
  )
  future.apply::future_lapply(
    X = mc_worker_id(seq_len(future::nbrOfWorkers())),
    FUN = fl_worker,
    cache_path = config$cache$driver$path,
    future.globals = FALSE
  )
  finish_distributed(config = config)
}

#' @title Run the master process of the "future_lapply" backend.
#' @description Users should not need to call this function directly.
#' @export
#' @keywords internal
#' @return `NULL`
#' @param cache_path path to the drake cache
#' @examples
#' # No examples here. This function is not for end users.
fl_master <- function(cache_path){
  config <- recover_drake_config(cache_path)
  drake::mc_process(id = mc_worker_id(0), config = config)
}

fl_worker <- function(worker, cache_path){
  withCallingHandlers(
    expr = {
      config <- recover_drake_config(cache_path = cache_path)
      do_prework(config = config, verbose_packages = FALSE)
      mc_worker(worker = worker, config = config)
    },
    error = function(e){
      error_process(e = e, id = worker, config = config) # nocov
    }
  )
}
