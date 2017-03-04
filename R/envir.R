load_dependencies = function(targets, config){
  already_loaded = ls(envir = config$envir) %>%
    intersect(y = config$plan$target)
  load_these = 
    nonfile_target_dependencies(targets = targets, 
      config = config) %>%
    setdiff(y = already_loaded)
  if(length(load_these)) 
    loadd(list = load_these, envir = config$envir)
  invisible()
}

nonfile_target_dependencies = function(targets, config){
  dependencies(targets = targets, config = config) %>% 
    Filter(f = is_not_file) %>% 
    intersect(y = config$plan$target)
}
