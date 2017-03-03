timestampdir = file.path(cachepath,  "ts")

timestamp = function(x){
  file.path(timestampdir, x)
}

timestamps = function(config){
  dir_empty(timestampdir)
  targets = intersect(config$order, config$plan$target)
  lapply(targets, function(target){
    hashes = hashes(target, config)
    current = target_current(target = target, 
      hashes = hashes, config = config) 
    if(current) file_overwrite(timestamp(target))
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
