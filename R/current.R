is_current = function(target, hashes, args){
  if(!(target %in% cached())) return(FALSE)
  if(!is_current_file(target, hashes, args)) return(FALSE)
  identical(args$cache$get(target, namespace = "depends"),
    hashes$depends)
}

is_current_file = function(target, hashes, args){
  if(!is_file(target)) return(TRUE)
  if(!file.exists(unquote(target))) return(FALSE)
  identical(args$cache$get(target)$value, hashes$file)
}

hashes = function(target, args){
  list(depends = dependency_hash(target, args),
    file = file_hash(target, args))
}

dependency_hash = function(target, args){
  command = get_command(target = target, args = args)
  graphical_dependencies(target, args) %>% 
    self_hash(args = args) %>%
    c(command) %>% digest(algo = "md5")
}

self_hash = Vectorize(function(target, args){
  if(target %in% cached()) args$cache$get_hash(target)
  else as.character(NA)
}, "target", USE.NAMES = FALSE)

file_hash = function(target, args){
  if(is_not_file(target)) return(as.character(NA))
  filename = unquote(target)
  if(!file.exists(filename)) return(as.character(NA))
  old_mtime = ifelse(target %in% args$cache$list(namespace = "filemtime"),
    args$cache$get(key = target, namespace = "filemtime"), -Inf)
  new_mtime = file.mtime(filename)
  do_rehash = file.size(filename) < 1e5 | new_mtime > old_mtime
  if(do_rehash) rehash_file(target)
  else args$cache$get(target)
}

rehash_file = function(target){
  unquote(target) %>% md5sum %>% unname
}

tidy = function(x){
  out = parse(text = x) %>% as.character %>% paste(collapse = "\n")
  paste("{\n", out, "\n}")
}

get_command = function(target, args){
  args$plan$command[args$plan$target == target] %>% tidy
}
