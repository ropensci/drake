should_build <- function(target, hash_list, config){
  if (hash_list[[target]]$imported) {
    return(TRUE)
  }
  !target_current(
    target = target,
    hashes = hash_list[[target]],
    config = config
  )
}

target_current <- function(target, hashes, config){
  if (!(target %in% config$inventory$reproducibly_tracked)){
    return(FALSE)
  }
  if (!file_current(target = target, hashes = hashes, config = config)){
    return(FALSE)
  }
  identical(
    config$cache$get(target, namespace = "commands"),
    hashes$command
  ) &
  identical(
    config$cache$get(target, namespace = "depends"),
    hashes$depends
  )
}

file_current <- function(target, hashes, config){
  if (!is_file(target)){
    return(TRUE)
  }
  if (!file.exists(unquote(target))){
    return(FALSE)
  }
  identical(
    config$cache$get(target, namespace = "reproducibly_tracked"),
    hashes$file
  )
}

log_target_attempts <- Vectorize(function(targets, config){
  config$cache$set(key = targets, value = targets,
    namespace = "target_attempts")
  invisible()
},
"targets")
