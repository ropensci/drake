time_stamps = function(config, outdated){
  dir_empty(time_stamp_dir)
  write_time_stamp_template()
  targets = intersect(V(config$graph)$name, config$plan$target)
  stamp_these = setdiff(targets, outdated)
  lapply(stamp_these, write_time_stamp)
  invisible()
}

safe_encode = function(x){
  base32_encode(x)
  # Collisions may occur for base 64 encoding on case-insensitive file systems.
  # paste0("t", base64_urlencode(x))
}

time_stamp = function(x){
  if(!length(x)) return(character(0))
  key = safe_encode(x) # must begin with alphanumeric
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
