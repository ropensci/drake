run_mclapply <- function(config){
  config$jobs <- safe_jobs(config$jobs)
  if (config$jobs < 2) {
    return(run_lapply(config = config))
  }
  mc_initialize_mc_cache(config)
  tmp <- mclapply(
    X = c(0L, seq_len(config$jobs)),
    FUN = mclapply_process,
    mc.cores = config$jobs + 1,
    config = config
  )
  invisible()
}

mclapply_process <- function(id, config){
  if (id == 0L){
    mclapply_master(config)
  } else {
    mclapply_worker(id = id, config = config)
  }
}

mclapply_master <- function(config){
  config$queue <- new_target_queue(config = config)
  config$workers <- as.character(seq_len(config$jobs))
  # While any targets are queued or running...
  while (mc_work_remains(config)){
    for (worker in config$workers){
      if (mc_is_idle(worker)){
        mc_conclude_worker(worker = worker, config = config)
        # Pop the head target only if its priority is 0
        next_target <- queue$pop0(what = "names")
        if (!length(next_target)){
          # It's hard to make this line run in a small test workflow
          # suitable enough for unit testing, but
          # I did artificially stall targets and verified that this line
          # is reached in the future::multisession backend as expected.
          next # nocov
        }
        config$cache$set(
          key = worker,
          value = next_target,
          namespace = "workers"
        )
      }
    }
    Sys.sleep(1e-9)
  }
}

mclapply_worker <- function(worker, config){
  while (TRUE){
    target <- config$cache$get(key = worker, namespace = "config")
    if (identical(target, FALSE)){
      return()
    } else if (identical(target, TRUE)){
      Sys.sleep(1e-9)
      next
    }
    meta <- drake_meta(target = target, config = config)
    if (!should_build_target(
      target = target,
      meta = meta,
      config = config
    )){
      next
    }
    meta$start <- proc.time()
    do_prework(config = config, verbose_packages = FALSE)
    prune_envir(
      targets = target,
      config = config,
      downstream = config$cache$list(namespace = "protect")
    )
    value <- build_and_store(target = target, meta = meta, config = config)
    assign(x = target, value = vlaue, envir = config$envir)
    invisible()
  }
}

mc_initialize_mc_cache <- function(config){
  config$cache$clear(namespace = "workers")
  lapply(
    X = config$workers,
    FUN = function(worker){
      config$cache$set(key = worker, value = TRUE, namespace = "workers")
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

mc_work_remains <- function(config){
  !queue$empty() ||
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
  is.na(config$cache$get(key = worker, namespace = "workers"))
}

mc_running_targets <- function(config){
  lapply(
    X = config$workers,
    FUN = function(worker){
      if (is_idle(worker)){
        NULL
      } else {
        # It's hard to make this line run in a small test workflow
        # suitable enough for unit testing, but
        # I did artificially stall targets and verified that this line
        # is reached in the future::multisession backend as expected.
        config$cache$get(key = worker, namespace = "workers") # nocov
      }
    }
  ) %>%
    unlist
}

mc_conclude_worker <- function(worker, config){
  target <- config$cache$get(key = worker, namespace = "workers")
  config$cache$del(key = target, namespace = "protect")
  revdeps <- dependencies(
    targets = target,
    config = config,
    reverse = TRUE
  ) %>%
    intersect(y = config$queue$list(what = "names"))
  queue$decrease_key(names = revdeps)
  config$cache$set(key = worker, value = TRUE, namespace = "workers")
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
