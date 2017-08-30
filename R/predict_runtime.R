predict_runtime <- function(plan, from_scratch = FALSE, config = NULL, ...){
  
  if (missing(config))
    config = config(plan = plan, ...)
  
  times = build_times()
  graph_remaining_targets = config$graph
  i = 1
  total_time = duration(0)
  
  while(length(V(graph_remaining_targets))) {
    
    candidates = next_targets(graph_remaining_targets)
    targets = Filter(x = candidates, . %>% is_in(plan$target))
    
    # Filter out targets if they have already been built
    if (!from_scratch)
      targets = Filter(x = targets, . %>% target_current(hashes(., config), config) %>% not)
    
    if (length(targets)) {
      cat("Build stage", i, "\n")
      cat("  Targets:", targets, "\n")
      
      untimed = setdiff(targets, times$target)
      time = times[times$target %in% targets,]$elapsed
      if (length(time)) {
        time = time %>% max %>% as.duration
        cat("  Est build time:", time %>% format, "\n")
        total_time = total_time + time
      }
      if (length(untimed)) {
        cat("  Untimed targets:", untimed, "\n")
      }
      
      i = i + 1
    }
    
    graph_remaining_targets = delete_vertices(graph_remaining_targets, v = candidates)
  }
  
  cat("\nTOTAL BUILD TIME:", total_time %>% format, "\n")
  cat(" ", setdiff(plan$target, times$target) %>% length, "untimed targets (never built)\n")
  cat("  (assuming max_useful_jobs)\n")
  cat("  (not including hashing and storage time [yet])\n")
  
  invisible(total_time)
}
