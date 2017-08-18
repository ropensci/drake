should_build = function(target, hash_list, config){
  !(target %in% config$plan$target) |
  !target_current(target = target, 
     hashes = hash_list[[target]], config = config)
}

target_current = function(target, hashes, config){
  # When overwriting console lines with \r, longer target names that precede
  # shorter target names won't be overwritten. So we pad with spaces with
  # sprintf to clear our these spaces.
  cat("comparing hash for", sprintf("%-50.50s", target), "\r") 
  if(!(target %in% config$inventory)) return(FALSE)
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
