run_mclapply <- function(config){
  do_prework(config = config, verbose_packages = FALSE)
  config$jobs <- safe_jobs(config$jobs)
  if (config$jobs < 2) {
    return(run_lapply(config = config))
  }
  config$workers <- as.character(seq_len(config$jobs))
  mc_initialize_cache(config)
  tmp <- mclapply(
    X = c("0", config$workers),
    FUN = mclapply_process,
    mc.cores = config$jobs + 1,
    mc.preschedule = FALSE,
    config = config
  )
  invisible()
}

mclapply_process <- function(id, config){
  if (id == "0"){
    mclapply_master(config)
  } else {
    mclapply_worker(worker = id, config = config)
  }
}

mclapply_master <- function(config){
  config$queue <- new_target_queue(config = config)
  while (mc_work_remains(config)){
    for (worker in config$workers){
      if (mc_is_idle(worker, config)){
        mc_conclude_worker(worker = worker, config = config)
        target <- config$queue$pop0(what = "names")

        print(config$queue$list("priorities")) ## TODO: fix queue

        if (!length(target)){
          next
        }
        mc_assign_worker(worker = worker, target = target, config = config)
      }
    }
    Sys.sleep(1e-1)
  }
  mc_terminate_workers(config)
}

mc_assign_worker <- function(worker, target, config){
  config$cache$set(
    key = worker,
    value = target,
    namespace = "workers"
  )
}

mc_terminate_workers <- function(config){
  lapply(X = config$workers, FUN = mc_set_done, config = config)
}

mc_set_idle <- function(worker, config){
  config$cache$set(key = worker, value = TRUE, namespace = "workers")
}

mc_set_done <- function(worker, config){
  config$cache$set(key = worker, value = FALSE, namespace = "workers")
}

mclapply_worker <- function(worker, config){
  while (TRUE){
    target <- config$cache$get(key = worker, namespace = "workers")
    if (identical(target, FALSE)){
      return()
    } else if (identical(target, TRUE)){
      Sys.sleep(1e-1)
      next
    }
    meta <- drake_meta(target = target, config = config)
    if (!should_build_target(
      target = target,
      meta = meta,
      config = config
    )){
      mc_set_idle(worker = worker, config = config)
      next
    }
    meta$start <- proc.time()
    prune_envir(
      targets = target,
      config = config,
      downstream = config$cache$list(namespace = "protect")
    )
    value <- build_and_store(target = target, meta = meta, config = config)
    assign(x = target, value = value, envir = config$envir)
    mc_set_idle(worker = worker, config = config)
  }
}

mc_initialize_cache <- function(config){
  config$cache$clear(namespace = "workers")
  lapply(
    X = config$workers,
    FUN = mc_set_idle,
    config = config
  )
  lapply(
    X = igraph::V(config$schedule)$name,
    FUN = function(target){
      config$cache$set(key = target, value = TRUE, namespace = "protect")
    }
  )
  invisible()
}

mc_work_remains <- function(config){
  !config$queue$empty() ||
    !mc_all_idle(config)
}

mc_all_idle <- function(config){
  for (worker in config$workers){
    if (!mc_is_idle(worker, config)){
      return(FALSE)
    }
  }
  TRUE
}

mc_is_idle <- function(worker, config){
  config$cache$get(key = worker, namespace = "workers")
  identical(TRUE, config$cache$get(key = worker, namespace = "workers"))
}

mc_running_targets <- function(config){
  lapply(
    X = config$workers,
    FUN = function(worker){
      if (mc_is_idle(worker = worker, config = config)){
        NULL
      } else {
        config$cache$get(key = worker, namespace = "workers")
      }
    }
  ) %>%
    unlist
}

mc_conclude_worker <- function(worker, config){
  target <- config$cache$get(key = worker, namespace = "workers")
  if (!is.character(target)){
    return()
  }
  config$cache$del(key = target, namespace = "protect")
  revdeps <- dependencies(
    targets = target,
    config = config,
    reverse = TRUE
  ) %>%
    intersect(y = config$queue$list(what = "names"))
  if (!get_attempt_flag(config) && target %in% config$plan$target){
    set_attempt_flag(config)
  }
  queue$decrease_key(names = revdeps)
  mc_set_idle(worker = worker, config = config)
}

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
