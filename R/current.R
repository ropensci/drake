should_build = function(target, hash_list, config){
  !(target %in% config$plan$target) |
  !target_current(target = target, 
     hashes = hash_list[[target]], config = config)
}

target_current = function(target, hashes, config){
  if(!(target %in% config$cache$list())) return(FALSE)
  if(!file_current(target = target, hashes = hashes, config = config)) 
    return(FALSE)
  identical(config$cache$get(target, namespace = "depends"),
    hashes$depends)
}

file_current = function(target, hashes, config){
  if(!is_file(target)) return(TRUE)
  if(!file.exists(unquote(target))) return(FALSE)
  identical(config$cache$get(target)$value, hashes$file)
}
