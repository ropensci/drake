run_mclapply <- function(config){
  if (config$jobs < 2 && !length(config$debug)) {
    return(run_loop(config = config))
  }
  config$workers <- as.character(seq_len(config$jobs))
  mc_init_worker_cache(config)
  tmp <- mclapply(
    X = c("0", config$workers),
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
      if (id == "0"){
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
  on.exit(mc_set_done_all(config))
  config$queue <- new_target_queue(config = config)
  while (TRUE){
    outboxes <- mc_outboxes(config)
    inboxes <- mc_inboxes(config)
    if (!mc_work_remains(outboxes = outboxes, config = config)){
      return()
    }
    mc_clear_outboxes(outboxes = outboxes, config = config)
    mc_assign_targets(inboxes = inboxes, config = config)
    Sys.sleep(mc_wait)
  }
}

mc_worker <- function(worker, config){
  on.exit(file.remove(db))
  db <- file.path(config$scratch_dir, worker, ".db")
  inbox <- ensure_queue("upcoming_targets", db = db)
  outbox <- ensure_queue("finished_targets", db = db)
  while (TRUE){
    while (is.null(msg <- mc_try_consume(inbox))){
      Sys.sleep(mc_wait)
    }
    if (identical(msg$title, "done")){
      return()
    }
    target <- msg$message
    build_check_store(
      target = target,
      config = config,
      downstream = config$cache$list(namespace = "mc_protect")
    )
    mc_publish(queue = outbox, title = "target", message = target)
  }
}

mc_init_worker_cache <- function(config){
  for (namespace in c("mc_protect")){
    config$cache$clear(namespace = namespace)
  }
  fs::dir_create(file.path(config$scratch_dir))
  lapply(
    X = igraph::V(config$schedule)$name,
    FUN = function(target){
      config$cache$set(key = target, value = TRUE, namespace = "mc_protect")
    }
  )
  invisible()
}

mc_conclude_target <- function(worker, config){
  target <- config$cache$get(key = worker, namespace = "mc_target")
  if (is.na(target)){
    return()
  }
  config$cache$del(key = target, namespace = "mc_protect")
  revdeps <- dependencies(
    targets = target,
    config = config,
    reverse = TRUE
  ) %>%
    intersect(y = config$queue$list())
  config$queue$decrease_key(targets = revdeps)
  if (
    identical(
      get_progress_single(target = target, cache = config$cache),
      "finished"
    ) && target %in% config$plan$target
  ){
    set_attempt_flag(key = worker, config = config)
  }
  # For the sake of the master process, so that the same target
  # does not get concluded twice:
  mc_set_target(worker = worker, target = NA, config = config)
}

mc_work_remains <- function(config){
  empty <- config$queue$empty()
  all_done <- mc_all_done(config)
  if (!empty && all_done){
    stop("workers finished without completing their work.") # nocov
  }
  !empty || !all_done
}

mc_get <- function(worker, namespace, config){
  out <- NA
  while (is.na(out)){
    out <- safe_get(key = worker, namespace = namespace, config = config)
    Sys.sleep(mc_wait)
  }
  out
}

mc_get_target <- function(worker, config){
  mc_get(worker = worker, namespace = "mc_target", config = config)
}

mc_get_status <- function(worker, config){
  mc_get(worker = worker, namespace = "mc_status", config = config)
}

mc_is_not_ready <- function(worker, config){
  identical("not ready", mc_get_status(worker = worker, config = config))
}

mc_is_idle <- function(worker, config){
  identical("idle", mc_get_status(worker = worker, config = config))
}

mc_is_running <- function(worker, config){
  identical("running", mc_get_status(worker = worker, config = config))
}

mc_is_done <- function(worker, config){
  identical("done", mc_get_status(worker = worker, config = config))
}

mc_all_done <- function(config){
  for (worker in config$workers){
    if (!mc_is_done(worker, config)){
      return(FALSE)
    }
  }
  TRUE
}

mc_set_status <- function(worker, status, config){
  on.exit(filelock::unlock(status_lock))
  status_lock <- filelock::lock(
    file.path(
      config$scratch_dir,
      "lock",
      paste0("worker", worker, "_status.lock")
    )
  )
  config$cache$duplicate(
    key_src = status,
    key_dest = worker,
    namespace_src = "common",
    namespace_dest = "mc_status"
  )
}

mc_set_target <- function(worker, target, config){
  config$cache$set(key = worker, value = target, namespace = "mc_target")
}

mc_set_not_ready <- function(worker, config){
  mc_set_status(worker = worker, status = "not ready", config = config)
}

mc_set_ready <- function(worker, config){
  if (mc_is_not_ready(worker = worker, config = config)){
    mc_set_idle(worker = worker, config = config)
  }
}

mc_set_idle <- function(worker, config){
  mc_set_status(worker = worker, status = "idle", config = config)
}

mc_set_running <- function(worker, config){
  mc_set_status(worker = worker, status = "running", config = config)
}

mc_set_done <- function(worker, config){
  # May be called more than necessary in varying execution order,
  # and that is okay. Should be a better situation with a
  # proper message queue.
  mc_set_status(worker = worker, status = "done", config = config)
}

mc_set_done_all <- function(config){
  lapply(
    X = config$workers,
    FUN = mc_set_done,
    config = config
  )
}

mc_wait <- 1e-9

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

mc_should_assign_target <- function(worker, target, config){
  if (!length(target)){
    return(FALSE)
  }
  if (
    !("workers" %in% colnames(config$plan)) ||
    !(target %in% config$plan$target)
  ){
    return(TRUE)
  }
  allowed_workers <- as.integer(
    unlist(
      config$plan$workers[config$plan$target == target]
    )
  )
  allowed_workers <- allowed_workers %% config$jobs
  allowed_workers[allowed_workers == 0] <- config$jobs
  if (!length(allowed_workers)){
    return(TRUE)
  }
  as.integer(worker) %in% allowed_workers
}
