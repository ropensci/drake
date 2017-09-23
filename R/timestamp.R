time_stamps <- function(config, outdated){
  dir_empty(time_stamp_dir)
  write_time_stamp_template()
  targets <- intersect(V(config$graph)$name, config$plan$target)
  stamp_these <- setdiff(targets, outdated)
  lapply(stamp_these, write_time_stamp, config = config)
  return(invisible())
}

safe_encode <- Vectorize(function(x, hash_algo){
  digest(
    object = x,
    algo = hash_algo,
    file = FALSE
  )
},
"x")

time_stamp <- function(x, config){
  if (!length(x)){
    return(character(0))
  }
  key <- safe_encode(x = x, hash_algo = config$short_hash_algo)
  return(file.path(time_stamp_dir, key))
}

write_time_stamp <- function(target, config){
  file.copy(
    time_stamp_template,
    time_stamp(x = target, config = config),
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
