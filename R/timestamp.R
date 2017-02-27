timestampdir = file.path(cache_path,  "timestamps")

timestamp = function(x){
  file.path(timestampdir, x)
}

timestamps = function(x){
  plan = x$plan
  output = plan$output[!is.na(plan$code)]
  dir_empty(timestampdir)
  lapply(output, function(name){
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
