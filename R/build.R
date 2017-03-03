build = function(target, args){
  hashes = hashes(target = target, args = args)
  imported = !(target %in% args$plan$target)
  target_current = target_current(target = target, 
    hashes = hashes, args = args)
  do_build = imported | !target_current
  if(!do_build) return(invisible())
  args$cache$set(key = target, value = "in progress", 
    namespace = "status")
  console(imported = imported, target = target, args = args)
  if(imported)
    value = imported_target(target = target, hashes = hashes, args = args)
  else if(!target_current)
    value = build_target(target = target, hashes = hashes, args = args)
  store_target(target = target, value = value, hashes = hashes,
    imported = imported, args = args)
  args$cache$set(key = target, value = hashes$depends,
    namespace = "depends")
  args$cache$set(key = target, value = "finished", namespace = "status")
}

build_target = function(target, hashes, args){
  command = get_command(target = target, args = args)
  eval(parse(text = command), envir = args$envir)
}

imported_target = function(target, hashes, args){
  if(is_file(target)) return(hashes$file)
  else if(target %in% ls(args$envir)) value = args$envir[[target]]
  else tryCatch(value <- get(target), error = function(e)
    stop("could not import ", target))
  value
}

store_target = function(target, value, hashes, imported, args){
  if(is_file(target))
    store_file(target, hashes = hashes,
      imported = imported, args = args)
  else if(is.function(value))
    store_function(target = target, value = value, imported = imported,
      hashes = hashes, args = args)
  else
    store_object(target = target, value = value, imported = imported,
      args = args)
  args$cache$set(key = target, value = hashes$depends,
    namespace = "depends")
}


store_object = function(target, value, imported, args){
  args$cache$set(key = target, 
    value = list(type = "object", value = value, imported = imported))
}

store_file = function(target, hashes, imported, args){
  hash = ifelse(imported, hashes$file, rehash_file(target))
  args$cache$set(key = target, 
    value = list(type = "file", value = hash, imported = imported))
  args$cache$set(key = target, value = file.mtime(unquote(target)), 
    namespace = "filemtime")
}

store_function = function(target, value, hashes, imported, args){
  string = deparse(value)
  args$cache$set(key = target,
    value = list(type = "function", value = string, imported = imported,
      depends = hashes$depends)) # for nested functions
}
