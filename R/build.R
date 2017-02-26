build = function(target, workflow, envir, cache){
  dependency_hash = dependency_hash(target, workflow, envir, cache)
  file_hash = file_hash(target, workflow, cache)
  if(is_current(target, dependency_hash, cache)) return()
  cache$set(key = target, value = "in progress", namespace = "status")
  if(target %in% workflow$target) build_target(target)
  else import_target(target, file_hash)
  cache$set(key = target, value = dependency_hash, namespace = "depends")
  cache$set(key = target, value = "done", namespace = "status")
}

build_target = function(target){
  console("build", target)
  value = eval(parse(text = workflow$command), envir = envir)
  if(is_file(target)) store_file(target, hash = NULL)
  else if(is.function(value)) store_function(target, value)
  else store_object(target, value)
}

import_target = function(target, file_hash){
  console("import", target)
  if(is_file(target)) store_file(target, hash = file_hash)
  if(target %in% ls(envir)) value = envir[[target]]
  else if(target %in% ls(globalenv())) value = globalenv()[[target]]
  else stop("Could not find ", target, " to import.")
  if(is.function(value)) store_function(target, value)
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
  code = deparse(value)
  cache$set(key = target, value = list(type = "function", value = code))
}
