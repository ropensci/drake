backend_loop <- function(config) {
  if (config$lock_envir) {
    lock_environment(config$envir)
    on.exit(unlock_environment(config$envir))
  }
  config$lock_envir <- FALSE
  targets <- igraph::topo_sort(config$graph)$name
  for (i in seq_along(targets)) {
    loop_build(
      target = targets[i],
      config = config,
      downstream = targets[-seq_len(i)]
    )
  }
  invisible()
}

loop_build <- function(target, config, downstream) {
  meta <- drake_meta_(target = target, config = config)
  if (!should_build_target(target, meta, config)) {
    log_msg("skip", target, config = config)
    return()
  }
  announce_build(target, meta, config)
  manage_memory(target, config, downstream = downstream)
  build <- build_target(target = target, meta = meta, config = config)
  conclude_build(build = build, config = config)
  invisible()
}
