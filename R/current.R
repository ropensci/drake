should_build <- function(target, meta_list, config){
  if (meta_list[[target]]$imported) {
    return(TRUE)
  }
  !target_current(
    target = target,
    meta = meta_list[[target]],
    config = config
  )
}

target_current <- function(target, meta, config){
  if (!(target %in% config$inventory$reproducibly_tracked)){
    return(FALSE)
  }
  if (!file_current(target = target, meta = meta, config = config)){
    return(FALSE)
  }
  identical(
    config$cache$get(target, namespace = "commands"),
    meta$command
  ) &
  identical(
    config$cache$get(target, namespace = "depends"),
    meta$depends
  )
}

file_current <- function(target, meta, config){
  if (!is_file(target)){
    return(TRUE)
  }
  if (!file.exists(unquote(target))){
    return(FALSE)
  }
  identical(
    config$cache$get(target, namespace = "reproducibly_tracked"),
    meta$file
  )
}

log_target_attempts <- Vectorize(function(targets, config){
  config$cache$set(key = targets, value = targets,
    namespace = "target_attempts")
  invisible()
},
"targets")
