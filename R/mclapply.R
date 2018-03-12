run_mclapply <- function(config){
  run_staged_parallelism(config = config, worker = worker_mclapply)
}

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
  assign_to_envir(targets = targets, values = values, config = config)
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
