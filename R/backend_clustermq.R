drake_backend.clustermq <- function(config) {
  assert_pkg("clustermq", version = "0.8.8")
  config$queue <- priority_queue(
    config = config,
    jobs = config$jobs_preprocess
  )
  cmq_local_master(config)
  if (config$queue$empty()) {
    return()
  }
  config$workers <- clustermq::workers(
    n_jobs = config$jobs,
    template = config$template
  )
  config$logger$minor("set common data")
  cmq_set_common_data(config)
  config$counter <- new.env(parent = emptyenv())
  config$counter$remaining <- config$queue$size()
  cmq_master(config)
}

cmq_local_master <- function(config) {
  continue <- TRUE
  while (!config$queue$empty() && continue) {
    continue <- cmq_local_master_iter(config)
  }
}

cmq_local_master_iter <- function(config) {
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
  config$workers$set_common_data(
    export = export,
    fun = identity,
    const = list(),
    rettype = list(),
    pkgs = character(0),
    common_seed = config$seed,
    token = "set_common_data_token"
  )
}

cmq_master <- function(config) {
  on.exit(config$workers$finalize())
  config$logger$minor("begin scheduling targets")
  while (config$counter$remaining > 0) {
    cmq_master_iter(config)
  }
  if (config$workers$cleanup()) {
    on.exit()
  }
}

cmq_master_iter <- function(config) {
  msg <- config$workers$receive_data()
  cmq_conclude_build(msg = msg, config = config)
  if (!identical(msg$token, "set_common_data_token")) {
    config$logger$minor("sending common data")
    config$workers$send_common_data()
  } else if (!config$queue$empty()) {
    cmq_next_target(config)
  } else {
    config$workers$send_shutdown_worker()
  }
}

cmq_conclude_build <- function(msg, config) {
  build <- msg$result
  if (is.null(build)) {
    return()
  }
  if (inherits(build, "try-error")) {
    stop(attr(build, "condition")$message, call. = FALSE) # nocov
  }
  cmq_conclude_target(target = build$target, config = config)
  caching <- hpc_caching(build$target, config)
  if (identical(caching, "worker")) {
    wait_checksum(
      target = build$target,
      checksum = build$checksum,
      config = config
    )
    return()
  } else {
    build <- unserialize_build(build)
  }
  wait_outfile_checksum(
    target = build$target,
    checksum = build$checksum,
    config = config
  )
  conclude_build(build = build, config = config)
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
  if (identical(caching, "master")) {
    manage_memory(target = target, config = config, jobs = 1)
    deps <- cmq_deps_list(target, config)
  }
  layout <- hpc_layout(target, config)
  config$workers$send_call(
    expr = drake::cmq_build(
      target = target,
      meta = meta,
      deps = deps,
      layout = layout,
      config = config
    ),
    env = list(target = target, meta = meta, deps = deps, layout = layout)
  )
}

cmq_deps_list <- function(target, config) {
  layout <- config$layout[[target]]
  keys_static <- layout$deps_build$memory
  keys_dynamic <- layout$deps_dynamic
  vals_static <- lapply(
    keys_static,
    get,
    envir = config$envir_targets,
    inherits = FALSE
  )
  vals_dynamic <- lapply(
    keys_dynamic,
    get,
    envir = config$envir_subtargets,
    inherits = FALSE
  )
  names(vals_static) <- keys_static
  names(vals_dynamic) <- keys_dynamic
  list(static = vals_static, dynamic = vals_dynamic)
}

#' @title Build a target using the clustermq backend
#' \lifecycle{stable}
#' @description For internal use only
#' @export
#' @keywords internal
#' @param target Target name.
#' @param meta List of metadata.
#' @param deps Named list of target dependencies.
#' @param layout Internal, part of the full `config$layout`.
#' @param config A [drake_config()] list.
cmq_build <- function(target, meta, deps, layout, config) {
  config$logger$minor("build on an hpc worker", target = target)
  config$layout <- layout
  do_prework(config = config, verbose_packages = FALSE)
  caching <- hpc_caching(target, config)
  if (identical(caching, "master")) {
    cmq_assign_deps(deps, config)
  } else {
    manage_memory(target = target, config = config, jobs = 1)
  }
  build <- try_build(target = target, meta = meta, config = config)
  if (identical(caching, "master")) {
    build$checksum <- get_outfile_checksum(target, config)
    build <- classify_build(build, config)
    build <- serialize_build(build)
    return(build)
  }
  conclude_build(build = build, config = config)
  list(target = target, checksum = get_checksum(target, config))
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
      envir = config$envir_subtargets,
      inherits = FALSE
    )
  }
}

cmq_local_build <- function(target, config) {
  config$logger$minor("build locally", target = target)
  local_build(target, config, downstream = NULL)
  cmq_conclude_target(target = target, config = config)
}

cmq_conclude_target <- function(target, config) {
  decrease_revdep_keys(queue = config$queue, target = target, config = config)
  config$counter$remaining <- config$counter$remaining - 1L
}
