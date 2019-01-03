run_loop <- function(config) {
  targets <- igraph::topo_sort(config$schedule)$name
  if (config$jobs > 1L) {
    warning(
      "In make(), `parallelism` should not be \"loop\" if `jobs` > 1",
      call. = FALSE
    )
  }
  for (i in seq_along(targets)) {
    check_build_store(
      target = targets[i],
      config = config,
      downstream = targets[-seq_len(i)],
      flag_attempt = TRUE
    )
  }
  invisible()
}
