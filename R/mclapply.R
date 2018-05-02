run_mclapply <- function(config){
  config$jobs <- safe_jobs(config$jobs)
  if (config$jobs < 2) {
    return(run_lapply(config = config))
  }
  config$workers <- as.character(seq_len(config$jobs))
  mc_init_worker_cache(config)
  tmp <- mclapply(
    X = c("0", config$workers),
    FUN = mc_process,
    mc.cores = config$jobs + 1,
    mc.preschedule = FALSE,
    config = config
  )
  invisible()
}

mc_process <- function(id, config){
  try_message({
    if (id == "0"){
      mc_master(config)
    } else {
      mc_worker(worker = id, config = config)
    }
  })
}

mc_master <- function(config){
  config$queue <- new_target_queue(config = config)
  while (mc_work_remains(config)){
    for (worker in config$workers){
      if (mc_is_idle(worker = worker, config = config)){
        mc_conclude_worker(worker = worker, config = config)
        if (!config$queue$size()){
          mc_set_done(worker = worker, config = config)
          next
        }
        target <- config$queue$pop0(what = "names")
        if (!length(target)){
          next
        }
        mc_set_target(worker = worker, target = target, config = config)
        mc_set_running(worker = worker, config = config)
      }
    }
    Sys.sleep(mc_wait)
  }
}

mc_worker <- function(worker, config){
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
      downstream = config$cache$list(namespace = "protect")
    )
    mc_set_idle(worker = worker, config = config)
  }
}

mc_init_worker_cache <- function(config){
  config$cache$clear(namespace = "workers")
  lapply(
    X = config$workers,
    FUN = function(worker){
      mc_set_idle(worker = worker, config = config)
      mc_set_target(worker = worker, target = NA, config = config)
    }
  )
  lapply(
    X = igraph::V(config$schedule)$name,
    FUN = function(target){
      config$cache$set(key = target, value = TRUE, namespace = "protect")
    }
  )
  invisible()
}

mc_conclude_worker <- function(worker, config){
  target <- config$cache$get(key = worker, namespace = "mc_target")
  if (is.na(target)){
    return()
  }
  config$cache$del(key = target, namespace = "protect")
  revdeps <- dependencies(
    targets = target,
    config = config,
    reverse = TRUE
  ) %>%
    intersect(y = config$queue$list(what = "names"))
  config$queue$decrease_key(names = revdeps)
  flag_attempt <- !get_attempt_flag(config) &&
    get_progress_single(target = target, cache = config$cache) == "finished" &&
    target %in% config$plan$target
  if (flag_attempt){
    set_attempt_flag(config)
  }
  mc_set_target(worker = worker, target = NA, config = config)
}

mc_work_remains <- function(config){
  !config$queue$empty() ||
    !mc_all_done(config)
}

mc_all_done <- function(config){
  for (worker in config$workers){
    if (!mc_is_done(worker, config)){
      return(FALSE)
    }
  }
  TRUE
}

mc_is_status <- function(worker, status, config){
  while (TRUE){
    worker_status <- try(
      config$cache$get(key = worker, namespace = "mc_status"),
      silent = TRUE
    )
    if (worker_status %in% c("done", "idle", "running")){
      break
    }
    Sys.sleep(mc_wait) # nocov
  }
  identical(status, worker_status)
}

mc_is_done <- function(worker, config){
  mc_is_status(worker = worker, status = "done", config = config)
}

mc_is_idle <- function(worker, config){
  mc_is_status(worker = worker, status = "idle", config = config)
}

mc_set_status <- function(worker, status, config){
  config$cache$set(key = worker, value = status, namespace = "mc_status")
}

mc_set_done <- function(worker, config){
  mc_set_status(worker = worker, status = "done", config = config)
}

mc_set_idle <- function(worker, config){
  mc_set_status(worker = worker, status = "idle", config = config)
}

mc_set_running <- function(worker, config){
  mc_set_status(worker = worker, status = "running", config = config)
}

mc_get_target <- function(worker, config){
  while (TRUE){
    target <- try(
      config$cache$get(key = worker, namespace = "mc_target"),
      silent = TRUE
    )
    if (is.character(target)){
      break
    }
    Sys.sleep(mc_wait) # nocov
  }
  target
}

mc_set_target <- function(worker, target, config){
  config$cache$set(key = worker, value = target, namespace = "mc_target")
}

mc_wait <- 1e-9

warn_mclapply_windows <- function(
  parallelism,
  jobs,
  os = this_os()
){
  parallelism <- match.arg(
    parallelism,
    choices = parallelism_choices(distributed_only = FALSE)
  )
  if (
    identical(parallelism, "mclapply") &&
    jobs_targets(jobs) > 1 &&
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

# Get rid of this:

worker_mclapply <- function(targets, meta_list, config){
  prune_envir(targets = targets, config = config)
  jobs <- safe_jobs(config$jobs)
  values <- mclapply(
    X = targets,
    FUN = drake_build_worker,
    meta_list = meta_list,
    config = config,
    mc.cores = jobs
  )
  assign_to_envir_batch(targets = targets, values = values, config = config)
}
