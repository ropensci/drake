build = function(target, args){
  hashes = hashes(target = target, args = args)
  current = is_current(target = target, hashes = hashes, args = args)
  if(current) return()
  args$cache$set(key = target, value = "in progress", 
    namespace = "status")
  if(target %in% args$plan$target) 
    build_target(target = target, hashes = hashes, args = args)
  else 
    import_target(target = target, hashes = hashes, args = args)
  args$cache$set(key = target, value = hashes$depends, 
    namespace = "depends")
  args$cache$set(key = target, value = "finished", namespace = "status")
}

build_target = function(target, hashes, args){
  console("build", target, args)
  command = get_command(target = target, args = args)
  value = eval(parse(text = command), envir = args$envir)
  if(is_file(target))
    store_file(target, hash = rehash_file(target), 
      imported = FALSE, args = args)
  else if(is.function(value)) 
    store_function(target = target, value = value, imported = FALSE,
      hashes = hashes, args = args)
  else 
    store_object(target = target, value = value, imported = FALSE, 
      args = args)
}

import_target = function(target, hashes, args){
  console("import", target, args)
  if(is_file(target)){
    store_file(target = target, hash = hashes$file,
      imported = TRUE, args = args)
    return()
  }
  if(target %in% ls(args$envir)) value = args$envir[[target]]
  else tryCatch(value <- get(target), error = function(e)
    stop("Could not find ", target, " to import."))
  if(is.function(value)) 
    store_function(target = target, value = value, hashes = hashes,
      imported = TRUE, args = args)
  else 
    store_object(target = target, value = value,
      imported = TRUE, args = args)
}

store_object = function(target, value, imported, args){
  args$cache$set(key = target, 
    value = list(type = "object", value = value, imported = imported))
}

store_file = function(target, hash, imported, args){
  args$cache$set(key = target, 
    value = list(type = "file", value = hash, imported = imported))
  args$cache$set(key = target, value = file.mtime(unquote(target)), 
    namespace = "filemtime")
}

store_function = function(target, value, imported, hashes, args){
  string = deparse(value)
  args$cache$set(key = target,
    value = list(type = "function", value = string, imported = imported,
      depends = hashes$depends)) # for nested functions
}
