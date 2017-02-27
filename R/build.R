build = function(target, args){
  hashes = hashes(target = target, args = args)
  current = is_current(target = target, hashes = hashes, args = args)
  if(current) return()
  args$cache$set(key = target, value = "in progress", 
    namespace = "status")
  import = !(target %in% args$plan$target)
  if(import) 
    value = import_target(target = target, hashes = hashes, args = args)
  else 
    value = build_target(target = target, hashes = hashes, args = args)
  store_target(target = target, value = value, hashes = hashes,
    import = import, args = args)
  args$cache$set(key = target, value = hashes$depends, 
    namespace = "depends")
  args$cache$set(key = target, value = "finished", namespace = "status")
}

build_target = function(target, hashes, args){
  console("build", target, args)
  command = get_command(target = target, args = args)
  eval(parse(text = command), envir = args$envir)
}

import_target = function(target, hashes, args){
  console("import", target, args)
  if(is_file(target)) return(hashes$file)
  else if(target %in% ls(args$envir)) value = args$envir[[target]]
  else tryCatch(value <- get(target), error = function(e)
    stop("Could not find ", target, " to import."))
  value
}

store_target = function(target, value, hashes, import, args){
  if(is_file(target))
    store_file(target, hashes = hashes,
      import = import, args = args)
  else if(is.function(value))
    store_function(target = target, value = value, import = import,
      hashes = hashes, args = args)
  else
    store_object(target = target, value = value, import = import,
      args = args)
  args$cache$set(key = target, value = hashes$depends,
    namespace = "depends")
}


store_object = function(target, value, import, args){
  args$cache$set(key = target, 
    value = list(type = "object", value = value, import = import))
}

store_file = function(target, hashes, import, args){
  hash = ifelse(import, hashes$file, rehash_file(target))
  args$cache$set(key = target, 
    value = list(type = "file", value = hash, import = import))
  args$cache$set(key = target, value = file.mtime(unquote(target)), 
    namespace = "filemtime")
}

store_function = function(target, value, import, hashes, args){
  string = deparse(value)
  args$cache$set(key = target,
    value = list(type = "function", value = string, import = import,
      depends = hashes$depends)) # for nested functions
}
