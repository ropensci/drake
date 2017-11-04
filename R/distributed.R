prepare_distributed <- function(config){
  this_cache_path <- cache_path(config$cache)
  if (identical(globalenv(), config$envir)){
    save(
      list = ls(config$envir, all.names = TRUE),
      envir = config$envir,
      file = globalenv_file(this_cache_path)
    )
  }
  target_attempts <- outdated(config = config)
  log_target_attempts(targets = target_attempts, config = config)
  config$cache$set(key = "envir", value = config$envir, namespace = "config")
  invisible()
}

build_distributed <- function(target, cache_path){
  config <- recover_config(cache_path = cache_path)
  do_prework(config = config, verbose_packages = FALSE)
  prune_envir(targets = target, config = config)
  meta_list <- meta_list(targets = target, config = config)
  config$old_hash <- self_hash(target = target, config = config)
  current <- target_current(
    target = target,
    meta = meta_list[[target]],
    config = config
  )
  if (!current){
    build(
      target = target,
      meta_list = meta_list,
      config = config
    )
  }
  invisible()
}

recover_config <- function(cache_path){
  cache <- this_cache(cache_path)
  config <- read_config(cache = cache)
  if (identical(globalenv(), config$envir)){
    dir <- cache_path
    file <- globalenv_file(dir)
    load(file = file, envir = config$envir)
  }
  thorough_inventory(config)
}
