backend_loop <- function(config) {
  if (config$lock_envir) {
    lock_environment(config$envir)
    on.exit(unlock_environment(config$envir))
  }
  config$lock_envir <- FALSE
  targets <- igraph::topo_sort(config$graph)$name
  while (length(targets)) {
    target <- targets[1]
    meta <- drake_meta_(target = target, config = config)
    if (handle_triggers(target, meta, config)) {
      targets <- targets[-1]
      next
    }
    if (is_dynamic(target, config)) {
      targets <- c(subtarget_names(target[1], config), targets)
      config <- register_subtargets(target, config)
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
  announce_build(target, meta, config)
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
