build = function(target, hash_list, config){
  hashes = hash_list[[target]]
  config$cache$set(key = target, value = "in progress", 
    namespace = "status")
  imported = !(target %in% config$plan$target)
  console(imported = imported, target = target, config = config) 
  if(imported)
    value = imported_target(target = target, hashes = hashes, 
      config = config)
  else
    value = build_target(target = target, hashes = hashes, 
    config = config)
  store_target(target = target, value = value, hashes = hashes,
    imported = imported, config = config)
  config$cache$set(key = target, value = hashes$depends,
    namespace = "depends")
  config$cache$set(key = target, value = "finished", namespace = "status")
  value
}

build_target = function(target, hashes, config){
  command = get_command(target = target, config = config)
  eval(parse(text = command), envir = config$envir)
}

imported_target = function(target, hashes, config){
  if(is_file(target)) return(hashes$file)
  else if(target %in% ls(config$envir)) value = config$envir[[target]]
  else value = tryCatch(get(target), error = function(e)
    console(imported = NA, target = target, config = config))
  value
}

store_target = function(target, value, hashes, imported, config){
  if(is_file(target))
    store_file(target, hashes = hashes,
      imported = imported, config = config)
  else if(is.function(value))
    store_function(target = target, value = value, imported = imported,
      hashes = hashes, config = config)
  else
    store_object(target = target, value = value, imported = imported,
      config = config)
  config$cache$set(key = target, value = hashes$depends,
    namespace = "depends")
}

store_object = function(target, value, imported, config){
  config$cache$set(key = target, 
    value = list(type = "object", value = value, imported = imported))
}

store_file = function(target, hashes, imported, config){
  hash = ifelse(imported, hashes$file, rehash_file(target))
  config$cache$set(key = target, 
    value = file.mtime(eply::unquote(target)), namespace = "filemtime")
  config$cache$set(key = target,
    value = list(type = "file", value = hash, imported = imported))
}

store_function = function(target, value, hashes, imported, config){
  config$cache$set(key = target, value = value, namespace = "functions")
  string = deparse(value)
  config$cache$set(key = target,
    value = list(type = "function", value = string, imported = imported,
      depends = hashes$depends)) # for nested functions
}
