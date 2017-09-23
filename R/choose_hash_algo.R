#' @title Default short hash algorithm
#' @export
#' @description This is the hash algorithm
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
  preferred_short = NULL,
  preferred_long = NULL
){
  old <- old_hash_algos()
  short <- choose_one_algo(
    old = old$short,
    preferred = preferred_short,
    default = default_short_hash_algo(),
    type = "short"
  ) %>%
    match.arg(choices = available_hash_algos())
  long <- choose_one_algo(
    old = old$long,
    preferred = preferred_long,
    default = default_long_hash_algo(),
    type = "long"
  ) %>%
    match.arg(choices = available_hash_algos())
  list(short = short, long = long)
}

choose_one_algo <- function(old, preferred, default, type) {
  if (!length(old) & !length(preferred)) {
    default
  } else if (!length(old) & length(preferred)) {
    preferred
  } else if (length(old) & !length(preferred)) {
    old
  } else if (length(old) & length(preferred)) {
    warn_different_algo(to = old, from = preferred, type = type)
    old
  }
}

warn_different_algo <- function(to, from, type) {
  warning(
    "Using ", to, " instead of ", from, " for ", type,
    "_hash_algorithm because the last build used ", to, ". ",
    "To avoid this warning, use make(..., ", type,
    "_hash_algorithm = ", to, ")."
  )
}

old_hash_algos <- function(){
  tryCatch({
      try_old_hash_algos()
    },
    error = function(e){
      stop("Could not recover hash algorithms of previous make().")
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
  previously_built <- "sessionInfo" %in% cache$list(namespace = "session")
  if (!previously_built) {
    return(list(short = short, long = NULL))
  }
  last_build <- cache$get("sessionInfo", namespace = "session")
  drake_version <- last_build$otherPkgs$drake$Version # nolint
  built_after_4.1.0 <- compareVersion(drake_version, "4.1.0") > 0 # nolint
  long <- ifelse(
    built_after_4.1.0, # nolint
    cache$get("long_hash_algo", namespace = "config"),
    "md5"
  )
  list(short = short, long = long)
}
