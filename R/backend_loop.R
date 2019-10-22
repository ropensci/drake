backend_loop <- function(config) {
  if (config$lock_envir) {
    lock_environment(config$envir)
    on.exit(unlock_environment(config$envir))
  }
  config$lock_envir <- FALSE
  targets <- igraph::topo_sort(config$graph)$name
  ht_parents <- ht_new()
  while(length(targets)) {
    new_dynamic <- subtarget_names(targets[1], config)
    ht_set(ht = ht_parents, x = new_dynamic, value = targets[1])
    targets <- c(new_dynamic, targets)
    parent <- ht_safe_get(ht_parents, targets[1])
    target <- classify_dynamic(targets[1], parent)
    local_build(
      target = target,
      config = config,
      downstream = targets[-1]
    )
    targets <- targets[-1]
  }
  invisible()
}
