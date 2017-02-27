build = function(target, args){
  dependency_hash = dependency_hash(target = target, args = args)
  filehash = filehash(target = target, args = args)
  if(is_current(target = target, args = args)) return()
  cache$set(key = target, value = "in progress", namespace = "status")
  if(target %in% plan$target) 
    build_target(target = target, filehash = filehash, args = args)
  else 
    import_target(target = target, args = args)
  cache$set(key = target, value = dependency_hash, namespace = "depends")
  cache$set(key = target, value = "finished", namespace = "status")
}

build_target = function(target, args){
  console("build", target)
  value = eval(parse(text = args$plan$command), args$envir = envir)
  if(is_file(target)) store_file(target, hash = NULL)
  else if(is.function(value)) 
    store_function(target = target, value = value, args = args)
  else store_object(target = target, value = value, args = args)
}

import_target = function(target, filehash, args){
  console("import", target)
  if(is_file(target)) 
    store_file(target = target, filehash = filehash, args = args)
  if(target %in% ls(envir)) value = envir[[target]]
  else if(target %in% ls(globalenv())) value = globalenv()[[target]]
  else stop("Could not find ", target, " to import.")
  if(is.function(value)) 
    store_function(target = target, value = value, args = args)
  else store_object(target = target, value = value, args = args)
}

store_object = function(target, value, cache){
  cache$set(key = target, value = list(type = "object", value = value))
}

store_file = function(target, filehash, args = args){
  filename = unquote(target)
  if(!length(hash)) hash = md5sum(filename)
  args$cache$set(key = target, value = list(type = "file", value = filehash))
  args$cache$set(key = target, value = file.mtime(filename), 
    namespace = "filemtime")
}

store_function = function(target, value, args){
  command = deparse(value)
  args$cache$set(key = target, 
    value = list(type = "function", value = command))
}
