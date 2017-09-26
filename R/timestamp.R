time_stamps <- function(config, outdated){
  cache_path <- cache_path(config$cache)
  stamp_dir <- time_stamp_dir(cache_path)
  dir_empty(stamp_dir)
  write_time_stamp_template(cache_path)
  targets <- intersect(V(config$graph)$name, config$plan$target)
  stamp_these <- setdiff(targets, outdated)
  lapply(stamp_these, write_time_stamp, config = config)
  return(invisible())
}

safe_encode <- Vectorize(function(x, hash_algo){
  digest::digest(
    object = x,
    algo = hash_algo,
    file = FALSE
  )
},
"x")

time_stamp <- function(target, config){
  if (!length(target)){
    return(character(0))
  }
  safe_encode(x = target, hash_algo = config$short_hash_algo)
}

time_stamp_file <- function(target, config){
  stamp <- time_stamp(target = target, config)
  dir <- time_stamp_dir(cache_path(config$cache))
  file.path(dir, stamp)
}

time_stamp_target <- function(target, config){
  stamp <- time_stamp(target = target, config)
  dir <- time_stamp_dir(cache_value_macro)
  file.path(dir, stamp)
}

write_time_stamp <- function(target, config){
  cache_path <- cache_path(config$cache)
  template <- time_stamp_template(cache_path)
  file.copy(
    template,
    time_stamp_file(target = target, config = config),
    overwrite = TRUE,
    copy.date = TRUE
  )
}

time_stamp_dir <- function(cache_path){
  file.path(cache_path, "ts")
}

time_stamp_template <- function(cache_path){
  file.path(cache_path, "timestamp")
}

dir_empty <- function(x){
  unlink(x, recursive = TRUE, force = TRUE)
  dir.create(x)
}

file_overwrite <- function(x){
  unlink(x, force = TRUE)
  file.create(x)
}

write_time_stamp_template <- function(cache_path){
  zip <- system.file(
    "timestamp.zip",
    package = "drake",
    mustWork = TRUE
  )
  unzip(zip, exdir = cache_path, setTimes = TRUE)
}
