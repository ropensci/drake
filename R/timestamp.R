time_stamps = function(config){
  dir_empty(time_stamp_dir)
  write_time_stamp_template()
  targets = intersect(config$order, config$plan$target)
  lapply(targets, function(target){
    hashes = hashes(target, config)
    current = target_current(target = target, 
      hashes = hashes, config = config)
    if(current) write_time_stamp(target)
  })
  invisible()
}

time_stamp = function(x){
  key = base64_urlencode(x) %>%
    gsub(pattern = "^-", replacement = "t-")
  file.path(time_stamp_dir, key)
}

write_time_stamp = function(target){
  file.copy(time_stamp_template, time_stamp(target),
    overwrite = TRUE, copy.date = TRUE)
}

time_stamp_dir = file.path(cachepath,  "ts")
time_stamp_template = file.path(cachepath, "timestamp")

dir_empty = function(x){
  unlink(x, recursive = TRUE)
  dir.create(x)
}

file_overwrite = function(x){
  unlink(x)
  file.create(x)
}

write_time_stamp_template = function(){
  zip = system.file("timestamp.zip", 
    package = "drake", mustWork = TRUE)
  unzip(zip, exdir = cachepath, setTimes = TRUE)
}
