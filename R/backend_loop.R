backend_loop <- function(config) {
  if (config$lock_envir) {
    lock_environment(config$envir)
    on.exit(unlock_environment(config$envir))
  }
  config$lock_envir <- FALSE
  config$targets <- igraph::topo_sort(config$envir_graph$graph)$name
  config$deferred <- ht_new()
  while (length(config$targets)) {
    config <- loop_target(config)
  }
  invisible()
}

loop_target <- function(config) {
  targets <- config$targets
  target <- targets[1]
  meta <- drake_meta_(target = target, config = config)
  if (handle_triggers(target, meta, config)) {
    config$targets <- targets[-1]
    return(config)
  }
  should_register_dynamic <- is_dynamic(target, config) &&
    !is_subtarget(target, config) &&
    !ht_exists(config$deferred, target)
  if (should_register_dynamic) {
    announce_build(target, config)
    register_subtargets(target, config)
    targets <- c(config$layout[[target]]$subtargets, targets)
    config$targets <- targets
    ht_set(config$deferred, target)
    return(config)
  }
  loop_build(
    target = target,
    meta = meta,
    config = config,
    downstream = targets[-1]
  )
  config$targets <- targets[-1]
  config
}

loop_build <- function(target, meta, config, downstream) {
  if (!is_dynamic(target, config) || is_subtarget(target, config)) {
    announce_build(target, config)
  }
  manage_memory(
    target,
    config,
    downstream = downstream,
    jobs = config$jobs_preprocess
  )
  build <- try_build(target = target, meta = meta, config = config)
  conclude_build(build = build, config = config)
  invisible()
}
