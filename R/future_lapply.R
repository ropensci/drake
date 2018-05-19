run_future_lapply <- function(config){
  prepare_distributed(config = config)
  config$workers <- as.character(seq_len(config$jobs))
  mc_init_worker_cache(config)
  console_persistent_workers(config)
  path <- normalizePath(config$cache_path, winslash = "/")
  tmp <- system2(
    "Rscript",
    shQuote(c("-e", paste0("drake::fl_master('", path, "')"))),
    wait = FALSE
  )
  future.apply::future_lapply(
    X = config$workers,
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
  config$workers <- as.character(seq_len(config$jobs))
  drake::mc_process(id = "0", config = config)
}

fl_worker <- function(worker, cache_path){
  tryCatch(
    expr = {
      config <- recover_drake_config(cache_path = cache_path)
      on.exit(mc_set_done(worker = worker, config = config))
      do_prework(config = config, verbose_packages = FALSE)
      mc_worker(worker = worker, config = config)
    },
    error = function(e){
      error_process(e = e, id = worker, config = config) # nocov
    }
  )
}
