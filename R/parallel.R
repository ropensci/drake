run_parallel_backend <- function(config){
  get(
    paste0("run_", config$parallelism),
    envir = getNamespace("drake")
  )(config)
}

run_parallel <- function(config, worker) {
  config$graph_remaining_targets <- config$graph
  config <- exclude_imports_if(config = config)
  while (length(V(config$graph_remaining_targets))){
    config <- parallel_stage(worker = worker, config = config)
  }
  invisible()
}

parallel_stage <- function(worker, config) {
  build_these <- character(0)
  meta_list <- list()
  while (TRUE){
    new_leaves <- leaf_nodes(graph = config$graph_remaining_targets) %>%
      setdiff(y = build_these)
    meta_list <- c(
      meta_list,
      meta_list(targets = new_leaves, config = config, store = TRUE)
    )
    do_build <- lightly_parallelize(
      X = new_leaves,
      FUN = should_build,
      jobs = config$jobs,
      meta_list = meta_list,
      config = config
    ) %>%
      unlist
    build_these <- c(build_these, new_leaves[do_build])
    if (!all(do_build)){
      trim_these <- new_leaves[!do_build]
      config$graph_remaining_targets <- delete_vertices(
        graph = config$graph_remaining_targets,
        v = trim_these
      )
    } else {
      break
    }
  }
  intersect(build_these, config$plan$target) %>%
    increment_attempt_flag(config = config)
  if (length(build_these)){
    worker(targets = build_these, meta_list = meta_list,
      config = config)
  }
  config$graph_remaining_targets <-
    delete_vertices(config$graph_remaining_targets, v = build_these)
  invisible(config)
}

exclude_imports_if <- function(config){
  if (!length(config$skip_imports)){
    config$skip_imports <- FALSE
  }
  if (!config$skip_imports){
    return(config)
  }
  delete_these <- setdiff(
    V(config$graph_remaining_targets)$name,
    config$plan$target
  )
  config$graph_remaining_targets <- delete_vertices(
    graph = config$graph_remaining_targets,
    v = delete_these
  )
  config
}

leaf_nodes <- function(graph){
  is_leaf <- igraph::degree(graph, mode = "in") == 0
  V(graph)[is_leaf]$name
}

lightly_parallelize <- function(X, FUN, jobs = 1, ...) {
  jobs <- safe_jobs(jobs)
  if (is.atomic(X)){
    lightly_parallelize_atomic(X = X, FUN = FUN, jobs = jobs, ...)
  } else {
    mclapply(X = X, FUN = FUN, mc.cores = jobs, ...)
  }
}

lightly_parallelize_atomic <- function(X, FUN, jobs = 1, ...){
  jobs <- safe_jobs(jobs)
  keys <- unique(X)
  index <- match(X, keys)
  values <- mclapply(X = keys, FUN = FUN, mc.cores = jobs, ...)
  values[index]
}

safe_jobs <- function(jobs){
  ifelse(on_windows(), 1, jobs)
}

on_windows <- function(){
  this_os() == "windows"
}

this_os <- function(){
  Sys.info()["sysname"] %>%
    tolower %>%
    unname
}

parallelism_warnings <- function(config){
  warn_mclapply_windows(
    parallelism = config$parallelism,
    jobs = config$jobs,
    os = this_os()
  )
}

use_default_parallelism <- function(parallelism){
  parallelism <- match.arg(
    parallelism,
    choices = parallelism_choices(distributed_only = FALSE)
  )
  if (parallelism %in% parallelism_choices(distributed_only = TRUE)){
    parallelism <- default_parallelism()
  }
  parallelism
}
