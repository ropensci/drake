#' @title Write dummy timestamp files for
#'   \code{\link{make}(..., parallelism = "Makefile")}.
#' @description For `"Makefile"` parallelism,
#' dummy timestamp files tell the Makefile
#' which targets need to be built and which can be skipped.
#' This function is for internal use only. It is only exported
#' to flesh out some of the examples in the help files.
#' @export
#' @keywords internal
#' @return nothing
#' @param config Internal master configuration list
#'   produced by [drake_config()].
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' config <- drake_config(my_plan) # Master internal configuration list
#' time_stamps(config)
#' # Now look in '.drake/ts' for dummy timestamp files.
#' })
#' }
time_stamps <- function(config){
  cache_path <- config$cache_path
  stamp_dir <- time_stamp_dir(cache_path)
  dir_empty(stamp_dir)
  write_time_stamp_template(cache_path)
  build_these <- first_outdated(config = config)
  if (length(build_these)){
    set_attempt_flag(key = "_attempt", config = config)
  }
  stamp_these <- setdiff(config$plan$target, build_these)
  lightly_parallelize(
    stamp_these, write_time_stamp, jobs = config$jobs, config = config)
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
