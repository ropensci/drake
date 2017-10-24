run_parallel <- function(config, worker) {
  graph_remaining_targets <- config$graph
  while (length(V(graph_remaining_targets)))
    graph_remaining_targets <-
      parallel_stage(graph_remaining_targets = graph_remaining_targets,
      worker = worker, config = config)
}

parallel_stage <- function(graph_remaining_targets, worker, config) {
  config <- inventory(config)
  remaining_targets <- V(graph_remaining_targets) %>%
    names %>% intersect(config$targets)
  candidates <- next_targets(graph_remaining_targets)
  console_many_targets(targets = candidates,
    message = "check", config = config)
  hash_list <- hash_list(targets = candidates, config = config)
  build_these <- Filter(candidates,
    f = function(target)
      should_build(target = target, hash_list = hash_list, config = config))
  hash_list <- hash_list[build_these]
  if (length(build_these)){
    worker(targets = build_these, hash_list = hash_list,
      config = config)
  }
  delete_vertices(graph_remaining_targets, v = candidates)
}

lightly_parallelize <- function(X, FUN, jobs = 1, ...) {
  jobs <- safe_jobs(jobs)
  mclapply(X = X, FUN = FUN, mc.cores = jobs, ...)
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
  warn_future_jobs(config)
}
