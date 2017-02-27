build = function(target, args){
  dependency_hash = dependency_hash(target = target, args = args)
  file_hash = file_hash(target = target, args = args)
  if(is_current(target = target, dependency_hash = dependency_hash,
    file_hash = file_hash, args = args)) return()
  args$cache$set(key = target, value = "in progress", namespace = "status")
  if(target %in% args$plan$target) 
    build_target(target = target, args = args)
  else 
    import_target(target = target, file_hash = file_hash, args = args)
  args$cache$set(key = target, value = dependency_hash, namespace = "depends")
  args$cache$set(key = target, value = "finished", namespace = "status")
}

build_target = function(target, args){
  console("build", target, args)
  command = get_command(target = target, args = args)
  value = eval(parse(text = command), envir = args$envir)
  if(is_file(target)) store_file(target, file_hash = NULL, args = args)
  else if(is.function(value)) 
    store_function(target = target, value = value, args = args)
  else store_object(target = target, value = value, args = args)
}

import_target = function(target, file_hash, args){
  console("import", target, args)
  if(is_file(target)){
    store_file(target = target, file_hash = file_hash, args = args)
    return()
  }
  if(target %in% ls(args$envir)) value = args$envir[[target]]
  else tryCatch(value <- get(target), error = function(e)
    stop("Could not find ", target, " to import."))
  if(is.function(value)) 
    store_function(target = target, value = value, args = args)
  else store_object(target = target, value = value, args = args)
}

store_object = function(target, value, args){
  args$cache$set(key = target, value = list(type = "object", value = value))
}

store_file = function(target, file_hash, args){
  filename = unquote(target)
  if(!length(file_hash)) file_hash = md5sum(filename)
  args$cache$set(key = target, value = list(type = "file", value = file_hash))
  args$cache$set(key = target, value = file.mtime(filename), 
    namespace = "filemtime")
}

store_function = function(target, value, args){
  string = deparse(value)
  args$cache$set(key = target, 
    value = list(type = "function", value = string))
}
