# Use subspaces of storr namespaces
# to avoid writing too many little files
# in rds caches.

exists_in_subspace <- function(key, subspace, namespace, cache){
  if (!cache$exists(key = key, namespace = namespace)){
    return(FALSE)
  }
  object <- cache$get(key = key, namespace = namespace)
  subspace %in% names(object)
}

list_subspace <- function(subspace, namespace, cache, jobs){
  if (!inherits(cache, "storr")){
    return(character(0))
  }
  parallel_filter(
    x = cache$list(namespace = namespace),
    f = function(key){
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

get_from_subspace <- function(key, subspace, namespace, cache){
  object <- safe_get(
    key = key, namespace = namespace, config = list(cache = cache))
  if (subspace %in% names(object)){
    object[[subspace]]
  } else {
    NA
  }
}

set_in_subspace <- function(key, value, subspace, namespace, cache){
  if (cache$exists(key = key, namespace = namespace)){
    object <- cache$get(key = key, namespace = namespace)
    object[[subspace]] <- value
    cache$set(key = key, value = object, namespace = namespace)
  } else {
    object <- list(key, value)
    names(object) <- c("target", subspace)
    cache$set(key = key, value = object, namespace = namespace)
  }
}
