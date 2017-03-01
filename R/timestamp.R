timestampdir = file.path(cachepath,  "ts")

timestamp = function(x){
  file.path(timestampdir, x)
}

timestamps = function(args){
  dir_empty(timestampdir)
  targets = intersect(args$order, args$plan$target)
  lapply(targets, function(target){
    hashes = hashes(target, args)
    current = target_current(target = target, 
      hashes = hashes, args = args) 
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
