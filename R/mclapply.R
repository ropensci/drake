run_mclapply <- function(config){
  if (config$jobs < 2 && !length(config$debug)) {
    return(run_loop(config = config))
  }
  mc_init_worker_cache(config)
  tmp <- mclapply(
    X = mc_worker_id(c(0, seq_len(config$jobs))),
    FUN = mc_process,
    mc.cores = config$jobs + 1,
    config = config
  )
  invisible()
}

#' @title Internal function to launch
#' a master process or persistent worker.
#' @description For internal use only.
#' Exported only for the purpose of
#' using persistent workers in
#' `make(paralellism = "parLapply", jobs = n)`,
#' where `n > 1`.
#' @keywords internal
#' @export
#' @param id character scalar with the job id
#' @param config `drake_config()` list
#' @return nothing important
mc_process <- function(id, config){
  withCallingHandlers(
    expr = {
      if (identical(id, mc_worker_id(0))){
        mc_master(config)
      } else {
        mc_worker(worker = id, config = config)
      }
    },
    error = function(e){
      error_process(e = e, id = id, config = config) # nocov
    },
    warning = function(e){
      warning_process(e = e, id = id, config = config) # nocov
    }
  )
  invisible()
}

#' @title Internal function to launch
#' a master process.
#' @description For internal use only.
#' Exported only for the purpose of
#' using persistent workers in
#' `make(paralellism = "future_lapply", jobs = n)`,
#' where `n > 1`.
#' @keywords internal
#' @export
#' @param config `drake_config()` list
#' @return nothing important
mc_master <- function(config){
  on.exit(mc_conclude_workers(config))
  config$queue <- new_target_queue(config = config)
  while (!config$queue$empty()){
    config <- mc_refresh_queue_lists(config)
    mc_conclude_done_targets(config)
    mc_assign_ready_targets(config)
    Sys.sleep(mc_wait)
  }
}

mc_worker <- function(worker, config){
  ready_queue <- mc_get_ready_queue(worker, config)
  done_queue <- mc_get_done_queue(worker, config)
  while (TRUE){
    while (is.null(msg <- mc_try_consume(ready_queue))){
      Sys.sleep(mc_wait)
    }
    if (identical(msg$message, "done")){
      return()
    }
    target <- msg$title
    build_check_store(
      target = target,
      config = config,
      downstream = config$cache$list(namespace = "mc_protect"),
      flag_attempt = TRUE
    )
    mc_ack(msg)
    mc_publish(queue = done_queue, title = target, message = "target")
  }
}

warn_mclapply_windows <- function(
  parallelism,
  jobs,
  os = this_os()
){
  if (
    "mclapply" %in% parallelism &&
    targets_setting(jobs) > 1 &&
    identical(os, "windows")
  ){
    warning(
      "Demoting to one job at a time (no parallel computing). ",
      "The mclapply parallel backend does not support ",
      "multiple jobs on Windows. Windows users should use the ",
      "parLapply backend intead (Windows default), or an other ",
        "Windows-compatible backend.",
      call. = FALSE
    )
  }
}
