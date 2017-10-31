run_parallel_backend <- function(config){
  get(
    paste0("run_", config$parallelism),
    envir = getNamespace("drake")
  )(config)
}

run_parallel <- function(config, worker) {
  config$graph_remaining_targets <- config$graph
  while (length(V(config$graph_remaining_targets))){
    config <- parallel_stage(worker = worker, config = config)
  }
  config
}

parallel_stage <- function(worker, config) {
  config <- inventory(config)
  remaining_targets <- V(config$graph_remaining_targets) %>%
    names %>% intersect(config$targets)
  candidates <- next_targets(config$graph_remaining_targets)
  console_many_targets(targets = candidates,
    message = "check", config = config)
  hash_list <- hash_list(targets = candidates, config = config)
  build_these <- Filter(candidates,
    f = function(target)
      should_build(target = target, hash_list = hash_list, config = config))
  config$attempted_targets <- c(
    config$attempted_targets,
    intersect(build_these, config$plan$target)
  )
  hash_list <- hash_list[build_these]
  if (length(build_these)){
    worker(targets = build_these, hash_list = hash_list,
      config = config)
  }
  config$graph_remaining_targets <-
    delete_vertices(config$graph_remaining_targets, v = candidates)
  invisible(config)
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
