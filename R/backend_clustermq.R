drake_backend_clustermq <- function(config) {
  assert_pkg("clustermq", version = "0.8.8")
  config$queue <- priority_queue(
    config = config,
    jobs = config$settings$jobs_preprocess
  )
  cmq_local_main(config)
  if (config$queue$empty()) {
    return()
  }
  config$workers <- clustermq::workers(
    n_jobs = config$settings$jobs,
    template = config$settings$template,
    log_worker = config$settings$log_worker
  )
  config$logger$disk("set common data")
  suppressWarnings(cmq_set_common_data(config))
  config$counter <- new.env(parent = emptyenv())
  config$counter$remaining <- config$queue$size()
  cmq_main(config)
}

cmq_local_main <- function(config) {
  continue <- TRUE
  while (!config$queue$empty() && continue) {
    continue <- cmq_local_main_iter(config)
  }
}

cmq_local_main_iter <- function(config) {
  target <- config$queue$peek0()
  if (no_hpc(target, config)) {
    config$queue$pop0()
    cmq_local_build(target, config)
    return(TRUE)
  }
  meta <- drake_meta_(target = target, config = config)
  if (!handle_triggers(target, meta, config)) {
    return(FALSE)
  }
  config$queue$pop0()
  cmq_conclude_target(target, config)
  TRUE
}

cmq_set_common_data <- function(config) {
  export <- list()
  if (identical(config$envir, globalenv())) {
    export <- as.list(config$envir, all.names = TRUE) # nocov
  }
  export$config <- hpc_config(config)
  do.call(what = config$workers$env, args = export)
}

cmq_main <- function(config) {
  on.exit(config$workers$cleanup())
  config$logger$disk("begin scheduling targets")
  while (config$counter$remaining > 0) {
    cmq_main_iter(config)
  }
}

cmq_main_iter <- function(config) {
  build <- config$workers$recv()
  cmq_conclude_build(build = build, config = config)
  if (!config$queue$empty()) {
    cmq_next_target(config)
  } else {
    config$workers$send_shutdown()
  }
}

cmq_conclude_build <- function(build, config) {
  if (is.null(build)) {
    return()
  }
  if (inherits(build, "try-error")) {
    stop0(attr(build, "condition")$message) # nocov
  }
  caching <- hpc_caching(build$target, config)
  if (identical(caching, "worker")) {
    cmq_report_warnings(build)
    wait_checksum(
      target = build$target,
      value = build$value,
      checksum = build$checksum,
      config = config
    )
  } else {
    build <- unserialize_build(build)
    wait_outfile_checksum(
      target = build$target,
      value = build$value,
      checksum = build$checksum,
      config = config
    )
    conclude_build(build = build, config = config)
  }
  cmq_conclude_target(target = build$target, config = config)
}

cmq_report_warnings <- function(build) {
  if (length(build$warnings)) {
    warning(
      "target ",
      build$target,
      " warnings:\n",
      multiline_message(build$warnings),
      call. = FALSE
    )
  }
}

cmq_next_target <- function(config) {
  target <- config$queue$pop0()
  # Longer tests will catch this:
  if (!length(target)) {
    config$workers$send_wait() # nocov
    return() # nocov
  }
  if (no_hpc(target, config)) {
    config$workers$send_wait()
    cmq_local_build(target, config)
  } else {
    cmq_send_target(target, config)
  }
}

cmq_send_target <- function(target, config) {
  meta <- drake_meta_(target = target, config = config)
  if (handle_triggers(target, meta, config)) {
    cmq_conclude_target(target = target, config = config)
    config$workers$send_wait()
    return()
  }
  announce_build(target = target, config = config)
  caching <- hpc_caching(target, config)
  deps <- NULL
  if (identical(caching, "main")) {
    manage_memory(target = target, config = config, jobs = 1)
    deps <- cmq_deps_list(target, config)
  }
  spec <- hpc_spec(target, config)
  config_tmp <- get_hpc_config_tmp(config)
  config$logger$disk("build on an hpc worker", target = target)
  config$workers$send(
    cmd = drake::cmq_build(
      target = target,
      meta = meta,
      deps = deps,
      spec = spec,
      config_tmp = config_tmp,
      config = config
    ),
    target = target,
    meta = meta,
    deps = deps,
    spec = spec,
    config_tmp = config_tmp
  )
}

cmq_deps_list <- function(target, config) {
  spec <- config$spec[[target]]
  keys_static <- spec$deps_build$memory
  keys_dynamic <- spec$deps_dynamic_whole
  keys_subtargets <- spec$deps_dynamic
  vals_static <- lapply(
    keys_static,
    get,
    envir = config$envir_targets,
    inherits = FALSE
  )
  vals_dynamic <- lapply(
    keys_dynamic,
    get,
    envir = config$envir_dynamic,
    inherits = FALSE
  )
  vals_subtargets <- lapply(
    keys_subtargets,
    get,
    envir = config$envir_subtargets,
    inherits = FALSE
  )
  names(vals_static) <- keys_static
  names(vals_dynamic) <- keys_dynamic
  names(vals_subtargets) <- keys_subtargets
  list(
    static = vals_static,
    dynamic = vals_dynamic,
    subtargets = vals_subtargets
  )
}

#' @title Build a target using the clustermq backend
#' `r lifecycle::badge("stable")`
#' @description For internal use only
#' @export
#' @keywords internal
#' @param target Target name.
#' @param meta List of metadata.
#' @param deps Named list of target dependencies.
#' @param spec Internal, part of the full `config$spec`.
#' @param config_tmp Internal, extra parts of `config` that the workers need.
#' @param config A [drake_config()] list.
cmq_build <- function(target, meta, deps, spec, config_tmp, config) {
  config$spec <- spec
  config <- restore_hpc_config_tmp(config_tmp, config)
  do_prework(config = config, verbose_packages = FALSE)
  caching <- hpc_caching(target, config)
  if (identical(caching, "main")) {
    cmq_assign_deps(deps, config)
  } else {
    manage_memory(target = target, config = config, jobs = 1)
  }
  build <- try_build(target = target, meta = meta, config = config)
  if (identical(caching, "main")) {
    build$checksum <- get_outfile_checksum(target, build$value, config)
    build <- classify_build(build, config)
    build <- serialize_build(build)
    return(build)
  }
  conclude_build(build = build, config = config)
  checksum <- get_checksum(target, build$value, config)
  value <- hpc_worker_build_value(target, build$value, config)
  list(
    target = target,
    value = value,
    checksum = checksum,
    warnings = build$meta$warnings
  )
}

cmq_assign_deps <- function(deps, config) {
  for (key in names(deps$static)) {
    assign(
      x = key,
      value = deps$static[[key]],
      envir = config$envir_targets,
      inherits = FALSE
    )
  }
  for (key in names(deps$dynamic)) {
    assign(
      x = key,
      value = deps$dynamic[[key]],
      envir = config$envir_dynamic,
      inherits = FALSE
    )
  }
  for (key in names(deps$subtargets)) {
    assign(
      x = key,
      value = deps$subtargets[[key]],
      envir = config$envir_subtargets,
      inherits = FALSE
    )
  }
}

cmq_local_build <- function(target, config) {
  config$logger$disk("build locally", target = target)
  local_build(target, config, downstream = NULL)
  cmq_conclude_target(target = target, config = config)
}

cmq_conclude_target <- function(target, config) {
  decrease_revdep_keys(queue = config$queue, target = target, config = config)
  config$counter$remaining <- config$counter$remaining - 1L
  config$logger$progress()
}
