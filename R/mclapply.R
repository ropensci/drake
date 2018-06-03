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
  tryCatch(
    expr = {
      if (id == "0"){
        mc_master(config)
      } else {
        mc_worker(worker = id, config = config)
      }
    },
    error = function(e){
      error_process(e = e, id = id, config = config) # nocov
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
  while (mc_work_remains(config)){
    assert_cache(config$cache)
    for (worker in config$workers){
      if (mc_is_idle(worker = worker, config = config)){
        mc_conclude_target(worker = worker, config = config)
        if (!config$queue$size()){
          mc_set_done(worker = worker, config = config)
          next
        }
        target <- config$queue$peek0()
        should_assign <- mc_should_assign_target(
          worker = worker,
          target = target,
          config = config
        )
        if (should_assign){
          config$queue$pop0()
          mc_set_target(worker = worker, target = target, config = config)
          mc_set_running(worker = worker, config = config)
        }
      }
    }
    Sys.sleep(mc_wait)
  }
}

mc_worker <- function(worker, config){
  on.exit(mc_set_done(worker = worker, config = config))
  mc_set_ready(worker = worker, config = config)
  while (TRUE){
    if (mc_is_idle(worker = worker, config = config)){
      Sys.sleep(mc_wait)
      next
    } else if (mc_is_done(worker = worker, config = config)){
      break
    }
    target <- mc_get_target(worker = worker, config = config)
    build_check_store(
      target = target,
      config = config,
      downstream = config$cache$list(namespace = "mc_protect")
    )
    mc_set_idle(worker = worker, config = config)
  }
}

mc_init_worker_cache <- function(config){
  for (namespace in c("mc_lock", "mc_protect", "mc_status", "mc_target")){
    config$cache$clear(namespace = namespace)
  }
  lapply(
    X = config$workers,
    FUN = function(worker){
      mc_set_not_ready(worker = worker, config = config)
      mc_set_target(worker = worker, target = NA, config = config)
    }
  )
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
  if (target %in% config$plan$target){
    set_attempt_flag(key = worker, config = config)
  }
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

mc_set <- function(worker, value, namespace, config){
  mc_with_lock(
    config$cache$set(key = worker, value = value, namespace = namespace),
    worker = worker,
    config = config
  )
}

mc_set_status <- function(worker, status, config){
  mc_set(
    worker = worker,
    value = status,
    namespace = "mc_status",
    config = config
  )
}

mc_set_target <- function(worker, target, config){
  mc_set(
    worker = worker,
    value = target,
    namespace = "mc_target",
    config = config
  )
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
  mc_set_status(worker = worker, status = "done", config = config)
}

mc_set_done_all <- function(config){
  lapply(
    X = config$workers,
    FUN = mc_set_done,
    config = config
  )
}

mc_with_lock <- function(code, worker, config){
  on.exit(just_try(config$cache$del(key = worker, namespace = "mc_lock")))
  while (config$cache$exists(key = worker, namespace = "mc_lock")){
    Sys.sleep(mc_wait)
  }
  config$cache$set(key = worker, value = TRUE, namespace = "mc_lock")
  force(code)
}

mc_wait <- 1e-6

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
