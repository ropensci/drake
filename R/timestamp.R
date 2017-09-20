time_stamps <- function(config, outdated){
  dir_empty(time_stamp_dir)
  write_time_stamp_template()
  targets <- intersect(V(config$graph)$name, config$plan$target)
  stamp_these <- setdiff(targets, outdated)
  lapply(stamp_these, write_time_stamp)
  return(invisible())
}

safe_encode <- function(x){
  base32_encode(x)
  # for details on this, see issue #47.
}

time_stamp <- function(x){
  if (!length(x)){
    return(character(0))
  }
  key <- safe_encode(x) # must begin with alphanumeric
  return(file.path(time_stamp_dir, key))
}

write_time_stamp <- function(target){
  file.copy(
    time_stamp_template,
    time_stamp(target),
    overwrite = TRUE,
    copy.date = TRUE
    )
}

time_stamp_dir <- file.path(cache_dir,  "ts")
time_stamp_template <- file.path(cache_dir, "timestamp")

dir_empty <- function(x){
  unlink(x, recursive = TRUE, force = TRUE)
  dir.create(x)
}

file_overwrite <- function(x){
  unlink(x, force = TRUE)
  file.create(x)
}

write_time_stamp_template <- function(){
  zip <- system.file(
    "timestamp.zip",
    package = "drake",
    mustWork = TRUE
    )
  unzip(zip, exdir = cache_dir, setTimes = TRUE)
}
