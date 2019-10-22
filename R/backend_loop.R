backend_loop <- function(config) {
  if (config$lock_envir) {
    lock_environment(config$envir)
    on.exit(unlock_environment(config$envir))
  }
  config$lock_envir <- FALSE
  targets <- igraph::topo_sort(config$graph)$name
  for (i in seq_along(targets)) {
    local_build(
      target = targets[i],
      config = config,
      downstream = targets[-seq_len(i)]
    )
  }
  invisible()
}
