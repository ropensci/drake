run_clustermq <- function(config){
  assert_pkgs("clustermq")
  config$workers <- clustermq::workers(
    n_jobs = config$jobs,
    template = config$template
  )
  on.exit(config$workers$cleanup())
  cmq_set_common_data(config)
  config$queue <- new_priority_queue(config = config)
  cmq_master(config)
}

cmq_set_common_data <- function(config){
  export <- list()
  if (identical(config$envir, globalenv())){
    export <- as.list(config$envir, all.names = TRUE) # nocov
  }
  export$config <- config
  config$workers$set_common_data(
    export = export,
    fun = identity,
    const = list(),
    rettype = list(),
    common_seed = config$seed,
    token = "token"
  )
}

cmq_master <- function(config){
  while (cmq_work_remains(config)){
    msg <- cmq_get_msg(config)
    cmq_conclude_job(msg = msg, config = config)
    if (identical(msg$id, "WORKER_UP")){
      config$workers$send_common_data()
    } else if (identical(msg$id, "WORKER_READY")) {
      if (cmq_work_remains(config)){
        cmq_send_target(config)
      } else {
        config$workers$send_shutdown_worker()
      }
    } else if (identical(msg$id, "WORKER_DONE")) {
      w$disconnect_worker()
    } else if (identical(msg$id, "WORKER_ERROR")) {
      stop("clustermq worker error") # nocov
    }
  }
}

cmq_get_msg <- function(config){
  while (TRUE){
    msg <- try(config$workers$receive_data())
    if (!inherits(msg, "try-error")){
      return(msg)
    }
    Sys.sleep(mc_wait)
  }
}

cmq_work_remains <- function(config){
  !config$queue$empty()
}

cmq_send_target <- function(config){
  target <- config$queue$pop0()
  if (!length(target)){
    config$workers$send_wait()
    return()
  }
  meta <- drake_meta(target = target, config = config)
  if (!should_build_target(target = target, meta = meta, config = config)){
    console_skip(target = target, config = config)
    config$workers$send_wait()
    return()
  }
  meta$start <- proc.time()
  announce_build(target = target, meta = meta, config = config)
  prune_envir(targets = target, config = config, jobs = config$jobs_imports)
  deps <- cmq_deps_list(target = target, config = config)
  config$workers$send_call(
    expr = drake::cmq_build(
      target = target,
      meta = meta,
      deps = deps,
      config = config
    ),
    env = list(target = target, meta = meta, deps = deps)
  )
}

cmq_deps_list <- function(target, config){
  deps <- dependencies(targets = target, config = config) %>%
    intersect(config$plan$target)
  lapply(
    X = deps,
    FUN = function(name){
      config$envir[[name]]
    }
  ) %>%
    setNames(deps)
}

#' @title Build a target using the clustermq backend
#' @description For internal use only
#' @export
#' @keywords internal
#' @inheritParams drake_build
#' @param target target name
#' @param meta list of metadata
#' @param deps named list of target dependencies
#' @param config a [drake_config()] list
cmq_build <- function(target, meta, deps, config){
  if (identical(config$garbage_collection, TRUE)){
    gc()
  }
  do_prework(config = config, verbose_packages = FALSE)
  for (dep in names(deps)){
    config$envir[[dep]] <- deps[[dep]]
  }
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
  mc_wait_outfile_checksum(
    target = build$target,
    checksum = build$checksum,
    config = config
  )
}
