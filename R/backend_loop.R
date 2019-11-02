backend_loop <- function(config) {
  if (config$lock_envir) {
    lock_environment(config$envir)
    on.exit(unlock_environment(config$envir))
  }
  config$lock_envir <- FALSE
  config$envir_loop <- new.env(parent = emptyenv())
  config$envir_loop$targets <- igraph::topo_sort(config$envir_graph$graph)$name
  config$envir_loop$count <- igraph::gorder(config$envir_graph$graph)
  while (length(config$envir_loop$targets)) {
    loop_check(config)
  }
}

loop_check <- function(config) {
  target <- config$envir_loop$targets[1L]
  targets <- config$envir_loop$targets[-1L]
  decrement <- !is_unregistered_dynamic(target, config)
  local_build(target = target, config = config, downstream = targets)
  if (decrement) {
    config$envir_loop$targets <- targets
  }
}
