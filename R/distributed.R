prepare_distributed <- function(config){
  this_cache_path <- cache_path(config$cache)
  if (identical(globalenv(), config$envir)){
    save(
      list = ls(config$envir, all.names = TRUE),
      envir = config$envir,
      file = globalenv_file(this_cache_path)
    )
  }
  config$attempted_targets <- outdated(
    plan = config$plan,
    targets = config$targets,
    envir = config$envir,
    verbose = config$verbose,
    cache = config$cache,
    jobs = config$jobs,
    parallelism = config$parallelism,
    packages = config$packages,
    prework = config$prework
  )
  config$cache$set("config", config, namespace = "distributed")
  invisible(config)
}

build_distributed <- function(target, cache_path){
  cache <- this_cache(cache_path)
  config <- cache$get("config", namespace = "distributed")
  if (identical(globalenv(), config$envir)){
    dir <- cache_path
    file <- globalenv_file(dir)
    load(file = file, envir = config$envir)
  }
  config <- inventory(config)
  do_prework(config = config, verbose_packages = FALSE)
  prune_envir(targets = target, config = config)
  hash_list <- hash_list(targets = target, config = config)
  config$old_hash <- self_hash(target = target, config = config)
  current <- target_current(
    target = target,
    hashes = hash_list[[target]],
    config = config
  )
  if (!current){
    build(
      target = target,
      hash_list = hash_list,
      config = config
    )
  }
  invisible(config)
}
