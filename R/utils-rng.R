choose_seed <- function(supplied, cache) {
  supplied %||%
    get_previous_seed(cache = cache) %||%
    0L
}

get_previous_seed <- function(cache) {
  if (cache$exists(key = "seed", namespace = "session")) {
    cache$get(key = "seed", namespace = "session")
  } else {
    NULL
  }
}

# A numeric hash that could be used as a
# random number generator seed. Generated
# from arguments of basic types such as
# numerics and characters.
seed_from_basic_types <- function(...) {
  x <- paste0(..., collapse = "")
  hash <- digest::digest(x, algo = "murmur32", serialize = FALSE)
  hexval <- paste0("0x", hash)
  utils::type.convert(hexval) %% .Machine$integer.max
}
