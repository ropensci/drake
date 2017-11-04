run_mclapply <- function(config){
  do_prework(config = config, verbose_packages = TRUE)
  run_parallel(config = config, worker = worker_mclapply)
}

worker_mclapply <- function(targets, meta_list, config){
  prune_envir(targets = targets, config = config)
  jobs <- safe_jobs(config$jobs)
  values <- mclapply(targets, build, meta_list = meta_list,
    config = config, mc.cores = jobs)
  assign_to_envir(target = targets, value = values, config = config)
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
  if (parallelism == "mclapply" & jobs > 1 & os == "windows"){
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
