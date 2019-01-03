run_loop <- function(config) {
  targets <- igraph::topo_sort(config$schedule)$name
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
  meta <- drake_meta(target = target, config = config)
  if (!should_build_target(target, meta, config)) {
    console_skip(target = target, config = config)
    return()
  }
  announce_build(target, meta, config)
  set_attempt_flag(key = target, config = config)
  manage_memory(target, config, downstream = downstream)
  build <- build_target(target = target, meta = meta, config = config)
  conclude_build(build = build, config = config)
  invisible()
}
