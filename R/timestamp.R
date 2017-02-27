timestampdir = file.path(cachepath,  "ts")

timestamp = function(x){
  file.path(timestampdir, x)
}

timestamps = function(args){
  dir_empty(timestampdir)
  targets = intersect(args$order, args$plan$target)
  lapply(targets, function(target){
    dependency_hash = dependency_hash(target, args)
    file_hash = file_hash(target, args)
    current = is_current(target = target, 
      dependency_hash = dependency_hash, 
      file_hash = file_hash, args = args)
    if(current)
      file_overwrite(timestamp(target))
  })
  invisible()
}

file_overwrite = function(x){
  unlink(x)
  file.create(x)
}

dir_empty = function(x){
  unlink(x, recursive = TRUE)
  dir.create(x)
}
