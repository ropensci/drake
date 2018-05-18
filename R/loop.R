run_loop <- function(config){
  targets <- igraph::topo_sort(config$schedule)$name
  for (i in seq_along(targets)){
    build_check_store(
      target = targets[i],
      config = config,
      downstream = targets[-seq_len(i)],
      flag_attempt = TRUE
    )
  }
  invisible()
}
