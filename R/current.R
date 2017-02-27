is_current = function(target, dependency_hash, file_hash, cache){
  if(!file_is_current(target = target, 
    file_hash = file_hash, cache = cache)) 
    return(FALSE)
  identical(cache$get_hash(target, namespace = "depends"), 
    dependency_hash)
}

file_is_current = function(target, file_hash, cache){
  if(!is_file(target)) return(TRUE)
  if(is.na(file_hash)) return(FALSE)
  if(!(target %in% cache$list())) return(FALSE)
  if(file_hash != cache$get(target)$value) return(FALSE)
}

dependency_hash = function(target, plan, graph, cache){
  command = plan$command[plan$target == target] %>% tidy
  graphical_dependencies(target, graph) %>% 
    sapply(FUN = cache$get_hash) %>% 
    c(command) %>% digest(algo = "md5")
}

file_hash = function(target, cache){
  if(is_not_file(target)) return(as.character(NA))
  filename = unquote(target)
  if(!file.exists(filename)) return(as.character(NA))
  old_mtime = ifelse(target %in% cache$list(namespace = "filemtime"), 
    cache$get(key = target, namespace = "filemtime"), -Inf)
  new_mtime = file.mtime(filename)
  do_rehash = file.size(filename) < 1e5 | new_mtime > old_mtime
  if(do_rehash) md5sum(filename) %>% unname
  else cache$get(target)
}

tidy = function(x){
  out = parse(text = x) %>% as.character %>% paste(collapse = "\n")
  paste("{\n", out, "\n}")
}

