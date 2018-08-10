run_clustermq <- function(config){
  assert_pkgs("clustermq")
  config$workers <- clustermq::workers(
    n_jobs = config$jobs,
    template = config$template
  )
  on.exit(config$workers$cleanup())
  config$workers$set_common_data(
    export = list(config = config),
    fun = NULL,
    const = list(),
    rettype = NULL,
    common_seed = NULL
  )
  config$workers$send_common_data()
  config$queue <- new_priority_queue(config = config)
  cmq_master(config)
}

cmq_master <- function(config){
  while (cmq_work_remains(config)){
    msg <- w$receive_data()
    if (identical(msg$id, "WORKER_READY")) {
      cmq_conclude_job(msg = msg, config = config)
      cmq_next_target(config)
    } else if (identical(msg$id, "WORKER_DONE")) {
      w$disconnect_worker()
    } else if (identical(msg$id, "WORKER_ERROR")) {
      stop("worker error")
    }
  }
}

cmq_work_remains <- function(config){
  !config$queue$empty() || config$workers$workers_running > 0
}

cmq_next_target <- function(config){
  target <- config$queue$pop0()
  if (!length(target)){
    return()
  }
  meta <- drake_meta(target = target, config = config)
  if (!should_build_target(target = target, meta = meta, config = config)){
    console_skip(target = target, config = config)
    return()
  }
  meta$start <- proc.time()
  announce_build(target = target, meta = meta, config = config)
  prune_envir(config)
  config$workers$send_call(
    cmq_build,
    list(
      target = target,
      meta = meta,
      envir = config$envir,
      config = config
    )
  )
}

cmq_build <- function(target, meta, envir, config){
  config$envir <- envir
  if (identical(config$garbage_collection, TRUE)){
    gc()
  }
  do_prework(config = config, verbose_packages = FALSE)
  build <- just_build(target = target, meta = meta, config = config)
  build$checksum <- mc_output_file_checksum(target, config)
  build
}

cmq_conclude_job <- function(msg, config){
  build <- msg$result
  conclude_build(
    target = target,
    value = build$value,
    meta = build$meta,
    config = config
  )
  revdeps <- dependencies(
    targets = target,
    config = config,
    reverse = TRUE
  ) %>%
    intersect(y = config$queue$list())
  config$queue$decrease_key(targets = revdeps)
  mc_wait_checksum(
    target = target,
    checksum = build$checksum,
    config = config
  )
}
