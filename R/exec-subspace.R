# Use subspaces of storr namespaces
# to avoid writing too many little files
# in rds caches.

exists_in_subspace <- function(key, subspace, namespace, cache) {
  if (!cache$exists(key = key, namespace = namespace)) {
    return(FALSE)
  }
  object <- cache$get(key = key, namespace = namespace)
  subspace %in% names(object)
}

list_subspace <- function(subspace, namespace, cache, jobs) {
  if (!inherits(cache, "storr")) {
    return(character(0))
  }
  parallel_filter(
    x = cache$list(namespace = namespace),
    f = function(key) {
      exists_in_subspace(
        key = key,
        subspace = subspace,
        namespace = namespace,
        cache = cache
      )
    },
    jobs = jobs
  )
}

get_from_subspace <- function(key, subspace, namespace, cache) {
  object <- safe_get(
    key = key, namespace = namespace, config = list(cache = cache))
  if (subspace %in% names(object)) {
    object[[subspace]]
  } else {
    NA_character_
  }
}

set_in_subspaces <- function(key, values, subspaces, namespace, cache) {
  stopifnot(identical(length(values), length(subspaces)))
  if (cache$exists(key = key, namespace = namespace)) {
    object <- cache$get(key = key, namespace = namespace)
    for (i in seq_along(values)) {
      object[[subspaces[i]]] <- values[[i]]
    }
    cache$set(key = key, value = object, namespace = namespace)
  } else {
    object <- list(target = key)
    for (i in seq_along(values)) {
      object[[i + 1]] <- values[[i]]
    }
    names(object) <- c("target", subspaces)
    cache$set(key = key, value = object, namespace = namespace)
  }
}
