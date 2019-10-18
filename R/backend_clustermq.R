backend_clustermq <- function(config) {
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
  while (!config$queue$empty()) {
    target <- config$queue$peek0()
    if (identical(config$layout[[target]]$hpc, FALSE)) {
      config$queue$pop0()
      cmq_local_build(target, config)
      next
    }
    meta <- drake_meta_(target = target, config = config)
    if (!handle_triggers(target, meta, config)) {
      return()
    }
    config$queue$pop0()
    cmq_conclude_target(target, config)
  }
}

cmq_set_common_data <- function(config) {
  export <- list()
  if (identical(config$envir, globalenv())) {
    export <- as.list(config$envir, all.names = TRUE) # nocov
  }
  export$config <- cmq_config(config)
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

cmq_config <- function(config) {
  discard <- c(
    "imports",
    "layout",
    "plan",
    "targets",
    "trigger"
  )
  for (x in discard) {
    config[[x]] <- NULL
  }
  config$cache$flush_cache()
  config
}

cmq_master <- function(config) {
  on.exit(config$workers$finalize())
  config$logger$minor("begin scheduling targets")
  while (config$counter$remaining > 0) {
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
  if (config$workers$cleanup()) {
    on.exit()
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
  caching <- caching(build$target, config)
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
  if (identical(config$layout[[target]]$hpc, FALSE)) {
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
  announce_build(target = target, meta = meta, config = config)
  caching <- caching(target, config)
  if (identical(caching, "master")) {
    manage_memory(target = target, config = config, jobs = 1)
    deps <- cmq_deps_list(target = target, config = config)
  } else {
    deps <- NULL
  }
  layout <- config$layout[[target]]
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
  deps <- config$layout[[target]]$deps_build$memory
  out <- lapply(
    X = deps,
    FUN = function(name) {
      config$eval[[name]]
    }
  )
  names(out) <- deps
  out
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
  config$layout <- list()
  config$layout[[target]] <- layout
  do_prework(config = config, verbose_packages = FALSE)
  caching <- caching(target, config)
  if (identical(caching, "master")) {
    for (dep in names(deps)) {
      config$eval[[dep]] <- deps[[dep]]
    }
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

caching <- function(target, config) {
  out <- config$layout[[target]]$caching %||NA% config$caching
  match.arg(out, choices = c("master", "worker"))
}

cmq_local_build <- function(target, config) {
  config$logger$minor("build locally", target = target)
  local_build(target, config, downstream = NULL)
  cmq_conclude_target(target = target, config = config)
}

cmq_conclude_target <- function(target, config) {
  decrease_revdep_keys(queue = config$queue, target = target, config = config)
  config$counter$remaining <- config$counter$remaining - 1
}
