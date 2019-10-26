backend_loop <- function(config) {
  if (config$lock_envir) {
    lock_environment(config$envir)
    on.exit(unlock_environment(config$envir))
  }
  config$lock_envir <- FALSE
  targets <- igraph::topo_sort(config$graph)$name
  deferred <- ht_new()
  while (length(targets)) {
    target <- targets[1]
    meta <- drake_meta_(target = target, config = config)
    if (handle_triggers(target, meta, config)) {
      targets <- targets[-1]
      next
    }
    should_defer <- is_dynamic(target, config) &&
      !is_subtarget(target, config) &&
      !ht_exists(deferred, target)
    if (should_defer) {
      announce_build(target, meta, config)
      config <- register_subtargets(target, config)
      targets <- c(config$layout[[target]]$subtargets, targets)
      ht_set(deferred, target)
      next
    }
    loop_build(
      target = target,
      meta = meta,
      config = config,
      downstream = targets[-1]
    )
    targets <- targets[-1]
  }
  invisible()
}

loop_build <- function(target, meta, config, downstream) {
  if (!is_dynamic(target, config) || is_subtarget(target, config)) {
    announce_build(target, meta, config)
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
