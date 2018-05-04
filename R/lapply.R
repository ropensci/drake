run_lapply <- function(config){
  lapply(
    X = igraph::topo_sort(config$schedule)$name,
    FUN = build_check_store,
    config = config,
    flag_attempt = TRUE
  )
  invisible()
}
