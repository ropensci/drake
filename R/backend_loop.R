drake_backend_loop <- function(config) {
  config$envir_loop <- new.env(parent = emptyenv())
  config$envir_loop$targets <- igraph::topo_sort(config$envir_graph$graph)$name
  while (length(config$envir_loop$targets)) {
    loop_check(config)
  }
}

loop_check <- function(config) {
  targets <- config$envir_loop$targets
  local_build(target = targets[1], config = config, downstream = targets[-1])
  config$envir_loop$targets <- config$envir_loop$targets[-1]
  config$logger$progress()
}
