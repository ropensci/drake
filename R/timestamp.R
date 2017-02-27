timestampdir = file.path(cachepath,  "ts")

timestamp = function(x){
  file.path(timestampdir, x)
}

timestamps = function(x){
  plan = x$plan
  target = plan$target[!is.na(plan$command)]
  dir_empty(timestampdir)
  lapply(target, function(name){
    depends_stamp = x$depends_stamp(name)
    if(!x$should_update_target(name, depends_stamp))
      file_overwrite(timestamp(name))
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
