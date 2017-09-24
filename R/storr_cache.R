set_storr_hashes <- function(
  cache,
  short_hash_algo, 
  long_hash_algo
){
  cache$set(
    key = "short_hash_algo",
    value = short_hash_algo,
    namespace = "config"
  )
  cache$set(
    key = "long_hash_algo",
    value = long_hash_algo,
    namespace = "config"
  )
  cache
}

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
  set_storr_hashes(
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
  hash_algo <- scan(algo_file, quiet = TRUE, what = character())
  cache <- storr::storr_rds(
    cache_dir,
    mangle_key = TRUE,
    hash_algorithm = hash_algo,
  )
  config_keys <- cache$list(namespace = "config")
  if (!("short_hash_algo" %in% config_keys)){
    cache$set(
      key = "short_hash_algo",
      value = hash_algo,
      namespace = "config"
    )
  }
  # Assume md5 for old caches without a specified long hash.
  # That should enforce back compatibility with drake 4.1.0.
  if (!("long_hash_algo" %in% config_keys)){
    cache$set(
      key = "long_hash_algo",
      value = "md5",
      namespace = "config"
    )
  }
  cache
}
