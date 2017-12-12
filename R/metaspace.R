exists_in_meta <- function(key, metaspace, cache){
  if (!inherits(cache, "storr")){
    return(FALSE)
  }
  if (!cache$exists(key = key, namespace = "meta")){
    return(FALSE)
  }
  meta <- cache$get(key = key, namespace = "meta")
  metaspace %in% names(meta)
}

list_meta <- function(metaspace, cache, jobs){
  if (!inherits(cache, "storr")){
    return(character(0))
  }
  parallel_filter(
    x = cache$list(namespace = "meta"),
    f = function(key){
      exists_in_meta(key = key, metaspace = metaspace, cache = cache)
    },
    jobs = jobs
  )
}

get_from_meta <- function(key, metaspace, cache){
  if (!inherits(cache, "storr")){
    stop("cache is missing.")
  }
  meta <- safe_get(key = key, namespace = "meta", config = list(cache = cache))
  if (metaspace %in% names(meta)){
    meta[[metaspace]]
  } else {
    NA
  }
}

set_in_meta <- function(key, value, metaspace, cache){
  if (!inherits(cache, "storr")){
    stop("cache is missing.")
  }
  if (cache$exists(key = key, namespace = "meta")){
    meta <- cache$get(key = key, namespace = "meta")
    meta[[metaspace]] <- value
    cache$set(key = key, value = meta, namespace = "meta")
  } else {
    meta <- list(key, value)
    names(meta) <- c("target", metaspace)
    cache$set(key = key, value = meta, namespace = "meta")
  }
}
