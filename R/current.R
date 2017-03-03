target_current = function(target, hashes, config){
  if(!(target %in% config$cache$list())) return(FALSE)
  if(!file_current(target, hashes, config)) return(FALSE)
  identical(config$cache$get(target, namespace = "depends"),
    hashes$depends)
}

file_current = function(target, hashes, config){
  if(!is_file(target)) return(TRUE)
  if(!file.exists(unquote(target))) return(FALSE)
  identical(config$cache$get(target)$value, hashes$file)
}
