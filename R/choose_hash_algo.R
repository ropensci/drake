#' @title Default short hash algorithm
#' @export
#' @descrition This is the hash algorithm
#' used most of the time. These hashes
#' are used as file names in the cache
#' and elsewhere, so they cannot be too long.
#' Windows has an upper limit on file path sizes.
default_short_hash_algo <- function() {
  "xxhash64"
}

#' @title Default long hash algorithm
#' @export
#' @description This hash is long in order
#' to avoid collisions. It is used
#' used for hashes that are not file names,
#' so we do not need to worry about it being too long.
default_long_hash_algo <- function() {
  "sha256"
}

#' @title Function available_hash_algos
#' @export
#' @description List the available hash algorithms.
available_hash_algos <- function(){
  eval(formals(digest::digest)$algo)
}

choose_hash_algos <- function(
  preferred_short = default_short_hash_algo(),
  preferred_long = default_long_hash_algo()
){
  preferred_short = match.arg(
    arg = preferred_short,
    choices = available_hash_algos()
  )
  preferred_long = match.arg(
    arg = preferred_long,
    choices = available_hash_algos()
  )
  old <- old_hash_algos()
  short <- ifelse(
    is.null(old$short),
    preferred_short,
    old$short
  )
  long <- ifelse(
    is.null(old$long),
    preferred_long,
    old$long
  )
  list(short = short, long = long)
}

old_hash_algos <- function(){
  tryCatch(
    {
      try_old_hash_algos()
    },
    error = function(e){
      list(short = "md5", long = "md5")
    }
  )
}

try_old_hash_algos <- function(){
  if (!file.exists(cache_dir)) {
    return(NULL)
  }
  algo_file <- file.path(cache_dir, "config", "hash_algorithm")
  short <- scan(algo_file, quiet = TRUE, what = character())
  cache <- storr::storr_rds(
    cache_dir,
    mangle_key = TRUE,
    hash_algorithm = short,
  )
  last_build <- cache$get("sessionInfo", namespace = "session")
  drake_version <- last_build$otherPkgs$drake$Version
  built_after_4.1.0 <- compareVersion(drake_version, "4.1.0") > 0
  long <- ifelse(
    built_after_4.1.0,
    cache$get("long_hash_algo", namespace = "config"),
    "md5"
  )
  list(short = short, long = long)
}
