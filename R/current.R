is_current = function(target, dependency_hash, file_hash, args){
  cached = intersect(args$cache$list(), args$cache$list(namespace = "depends"))
  if(!(target %in% cached)) return(FALSE)
  if(!file_is_current(target = target, 
    file_hash = file_hash, args = args)) 
    return(FALSE)
  identical(args$cache$get(target, namespace = "depends"), 
    dependency_hash)
}

file_is_current = function(target, file_hash, args){
  if(!is_file(target)) return(TRUE)
  if(!file.exists(unquote(target))) return(FALSE)
  if(!(target %in% args$cache$list())) return(FALSE)
  if(file_hash != args$cache$get(target)$value) return(FALSE)
  TRUE
}

dependency_hash = function(target, args){
  command = get_command(target = target, args = args)
  graphical_dependencies(target, args) %>% 
    sapply(FUN = args$cache$get_hash) %>% 
    c(command) %>% digest(algo = "md5")
}

file_hash = function(target, args){
  if(is_not_file(target)) return(as.character(NA))
  filename = unquote(target)
  if(!file.exists(filename)) return(as.character(NA))
  old_mtime = ifelse(target %in% args$cache$list(namespace = "filemtime"), 
    args$cache$get(key = target, namespace = "filemtime"), -Inf)
  new_mtime = file.mtime(filename)
  do_rehash = file.size(filename) < 1e5 | new_mtime > old_mtime
  if(do_rehash) md5sum(filename) %>% unname
  else args$cache$get(target)
}

tidy = function(x){
  out = parse(text = x) %>% as.character %>% paste(collapse = "\n")
  paste("{\n", out, "\n}")
}

get_command = function(target, args){
  args$plan$command[args$plan$target == target] %>% tidy
}

