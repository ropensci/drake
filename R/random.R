choose_seed <- function(supplied, cache){
  previous <- get_previous_seed(cache = cache)
  seed_conflict <-
    !is.null(previous) &&
    !is.null(supplied) &&
    !identical(previous, supplied)
  if (seed_conflict){
    stop(
      "You supplied a seed of ", supplied,
      " to either make() or drake_config(). ",
      "Your project already has a different seed: ", previous, ". ",
      "Use read_drake_seed() to see the seed for yourself. ",
      "To reset the project's seed, you will have to destroy the cache ",
      "and restart from scratch: clean(destroy = TRUE). This may seem ",
      "excessive and inconvenient, but it ensures reproducible results.",
      call. = FALSE
    )
  }
  (previous %||% supplied) %||% 0
}

get_previous_seed <- function(cache){
  if (cache$exists(key = "seed", namespace = "config")){
    cache$get(key = "seed", namespace = "config")
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
