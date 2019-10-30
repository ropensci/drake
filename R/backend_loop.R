backend_loop <- function(config) {
  if (config$lock_envir) {
    lock_environment(config$envir)
    on.exit(unlock_environment(config$envir))
  }
  config$lock_envir <- FALSE
  config$ht_registered <- ht_new()
  config$envir_loop <- new.env(parent = emptyenv())
  config$envir_loop$targets <- igraph::topo_sort(config$envir_graph$graph)$name
  while (length(config$envir_loop$targets)) {
    loop_check(config)
  }
}

loop_check <- function(config) {
  n_previous <- n_graph(config)
  local_build(
    target = config$envir_loop$targets[1L],
    config = config,
    downstream = config$envir_loop$targets[-1L]
  )
  if (n_graph(config) <= n_previous) {
    config$envir_loop$targets <- config$envir_loop$targets[-1L]
  }
}
