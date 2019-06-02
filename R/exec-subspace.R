get_from_subspace <- function(key, subspace, namespace, cache) {
  object <- safe_get(
    key = key, namespace = namespace, config = list(cache = cache))
  if (subspace %in% names(object)) {
    object[[subspace]]
  } else {
    NA_character_
  }
}
