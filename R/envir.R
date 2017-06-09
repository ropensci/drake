assign_to_envir = Vectorize(function(target, value, config){
  if(is_file(target) | !(target %in% config$plan$target)) return()
  assign(x = target, value = value, envir = config$envir)
}, c("target", "value"))

prune_envir = function(targets, config){
  downstream = lapply(targets, function(vertex)
    subcomponent(config$graph, v = vertex, mode = "out")$name) %>%
    unlist %>% unique
  already_loaded = ls(envir = config$envir, all.names = TRUE) %>%
    intersect(y = config$plan$target)
  load_these = 
    nonfile_target_dependencies(targets = targets, config = config) %>%
    setdiff(y = c(targets, already_loaded))
  keep_these = 
    nonfile_target_dependencies(targets = downstream, config = config)
  discard_these = setdiff(x = config$plan$target, y = keep_these) %>%
    Filter(f = is_not_file) %>% 
    intersect(y = already_loaded)
  rm(list = discard_these, envir = config$envir)
  if(length(load_these)) 
    loadd(list = load_these, envir = config$envir)
  invisible()
}

nonfile_target_dependencies = function(targets, config){
  dependencies(targets = targets, config = config) %>% 
    Filter(f = is_not_file) %>% 
    intersect(y = config$plan$target)
}
