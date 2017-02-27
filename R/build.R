build = function(target, plan, envir, cache){
  dependency_hash = dependency_hash(target = target, 
    plan = plan, envir = envir, cache = cache)
  file_hash = file_hash(target = target, cache = cache)
  if(is_current(target = target, dependency_hash = dependency_hash, 
    file_hash = file_hash, cache = cache)) return()
  cache$set(key = target, value = "in progress", namespace = "status")
  if(target %in% plan$target) build_target(target)
  else import_target(target = target, file_hash = file_hash)
  cache$set(key = target, value = dependency_hash, namespace = "depends")
  cache$set(key = target, value = "finished", namespace = "status")
}

build_target = function(target){
  console("build", target)
  value = eval(parse(text = plan$command), envir = envir)
  if(is_file(target)) store_file(target, hash = NULL)
  else if(is.function(value)) 
    store_function(target = target, value = value)
  else store_object(target = target, value = value)
}

import_target = function(target, file_hash){
  console("import", target)
  if(is_file(target)) store_file(target = target, hash = file_hash)
  if(target %in% ls(envir)) value = envir[[target]]
  else if(target %in% ls(globalenv())) value = globalenv()[[target]]
  else stop("Could not find ", target, " to import.")
  if(is.function(value)) store_function(target = target, value = value)
  else store_object(target, value)
}

store_object = function(target, value){
  cache$set(key = target, value = list(type = "object", value = value))
}

store_file = function(target, hash){
  filename = unquote(target)
  if(!length(hash)) hash = md5sum(filename)
  cache$set(key = target, value = list(type = "file", value = hash))
  cache$set(key = target, value = file.mtime(filename), namespace = "filemtime")
}

store_function = function(target, value){
  command = deparse(value)
  cache$set(key = target, value = list(type = "function", value = command))
}
