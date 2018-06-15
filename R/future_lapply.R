run_future_lapply <- function(config){
  prepare_distributed(config = config)
  mc_init_worker_cache(config)
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
    X = mc_worker_id(seq_len(config$jobs)),
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
    },
    warning = function(e){
      warning_process(e = e, id = worker, config = config) # nocov
    }
  )
}

warn_future_lapply <- function(config){
  if (!("future_lapply" %in% config$parallelism)){
    return()
  }
  if (inherits(future::plan(), "batchtools")){
    drake_warning(
      "a batchtools-powered future::plan() and ",
      "\"future_lapply\" parallelism are selected together.\n",
      "For these settings, file latency issues ",
      "among the nodes of your computing cluster ",
      "could undermine the integrity of the results.\n",
      "Also, the future_lapply()-style ",
      "persistent workers could terminate early ",
      "if your cluster has strict wall time limits.\n",
      "Either continue at your own risk ",
      "or select make(parallelism = \"future\") instead.\n",
      "For more information, please visit ",
      "https://github.com/ropensci/drake/issues/416 and ",
      "https://github.com/ropensci/drake/issues/417",
      config = config
    )
  }
}
