target_current = function(target, hashes, args){
  if(!(target %in% cached())) return(FALSE)
  if(!file_current(target, hashes, args)) return(FALSE)
  identical(args$cache$get(target, namespace = "depends"),
    hashes$depends)
}

file_current = function(target, hashes, args){
  if(!is_file(target)) return(TRUE)
  if(!file.exists(unquote(target))) return(FALSE)
  identical(args$cache$get(target)$value, hashes$file)
}
