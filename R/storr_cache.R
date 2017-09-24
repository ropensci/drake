new_storr_rds_cache <- function(
  path,
  hash_algo,
  short_hash_algo,
  long_hash_algo
){
  cache <- storr::storr_rds(
    path = path,
    mangle_key = TRUE,
    hash_algorithm = short_hash_algo
  )
  configure_cache(
    cache = cache,
    short_hash_algo = short_hash_algo,
    long_hash_algo = long_hash_algo
  )
}

get_storr_rds_cache <- function(path){
  if (!file.exists(path)) {
    return(NULL)
  }
  hash_algo_file <- file.path(path, "config", "hash_algorithm")
  hash_algo <- scan(hash_algo_file, quiet = TRUE, what = character())
  cache <- storr::storr_rds(
    path = path,
    mangle_key = TRUE,
    hash_algorithm = hash_algo,
  )
  configure_cache(
    cache = cache,
    short_hash_algo = hash_algo,
    long_hash_algo = "md5",
    overwrite_hash_algos = FALSE,
    clear_progress = FALSE
  )
}
