run_clustermq <- function(config){
  assert_pkgs("clustermq")
  config$workers <- clustermq::workers(
    n_jobs = config$jobs,
    template = config$template
  )
  on.exit(config$workers$cleanup())
  cmq_send_data(config)
  config$queue <- new_priority_queue(config = config)
  cmq_master(config)
}

cmq_exports <- function(config){
  export <- list()
  if (identical(config$envir, globalenv())){
    export <- as.list(config$envir, all.names = TRUE) # nocov
  }
  export$config <- config
  export
}

cmq_send_data <- function(config){
  config$workers$set_common_data(
    export = cmq_exports(config),
    fun = function(){},
    const = list(),
    rettype = list(),
    common_seed = config$seed,
    token = "token"
  )
  msg <- config$workers$receive_data()
  config$workers$send_common_data()
}

cmq_master <- function(config){
  while (cmq_work_remains(config)){
    msg <- tryCatch (
      config$workers$receive_data(),
      error = function(e){
        list(id = "MESSAGE_ERROR") # Just trying again seems to work.
      }
    )
    if (msg$id %in% c("WORKER_READY", "WORKER_UP")) {
      cmq_conclude_job(msg = msg, config = config)
      cmq_send_target(config)
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

cmq_send_target <- function(config){
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
  prune_envir(targets = target, config = config, jobs = config$jobs_imports)
  config$workers$send_call(
    expr = drake::cmq_build(
      target = target,
      meta = meta,
      envir = envir,
      config = config
    ),
    env = list(
      target = target,
      meta = meta,
      envir = config$envir,
      config = config
    )
  )
}

#' @title Build a target using the clustermq backend
#' @description For internal use only
#' @export
#' @keywords internal
#' @inheritParams drake_build
#' @param target target name
#' @param meta list of metadata
#' @param envir environment with dependencies
#' @param config [drake_config()] configuration list
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
  if (is.null(build)){
    return()
  }
  conclude_build(
    target = build$target,
    value = build$value,
    meta = build$meta,
    config = config
  )
  revdeps <- dependencies(
    targets = build$target,
    config = config,
    reverse = TRUE
  ) %>%
    intersect(y = config$queue$list())
  config$queue$decrease_key(targets = revdeps)
  set_attempt_flag(key = build$target, config = config)
  mc_wait_checksum(
    target = build$target,
    checksum = build$checksum,
    config = config
  )
}
