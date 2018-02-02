get_previous_seed <- function(cache){
  if (cache$exists(key = "seed", namespace = "config")){
    cache$get(key = "seed", namespace = "config")
  } else {
    get_valid_seed()
  }
}

# From withr
get_valid_seed <- function(seed = get_seed()){
  if (is.null(seed)) {
    runif(1L)
    seed <- get_seed()
  }
  seed
}

# From withr
get_seed <- function(){
  get0(".Random.seed", globalenv(), mode = "integer")
}

# A numeric hash that could be used as a
# random number generator seed. Generated
# from an arbitrary object x.
seed_from_object <- function(x) {
  hash <- digest::digest(x, algo = "murmur32")
  hexval <- paste0("0x", hash)
  utils::type.convert(hexval) %% .Machine$integer.max
}
