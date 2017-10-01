new_storr_environment_cache <- function(
  short_hash_algo,
  long_hash_algo,
  path = NULL,
  envir = NULL
){
  cache <- storr::storr_environment(
    hash_algorithm = short_hash_algo,
    envir = envir
  )
  configure_cache(
    cache = cache,
    short_hash_algo = short_hash_algo,
    long_hash_algo = long_hash_algo,
  )
}

get_storr_environment_cache <- function(...){
  new_storr_environment_cache(...)
}
