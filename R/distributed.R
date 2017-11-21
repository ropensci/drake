prepare_distributed <- function(config){
  this_cache_path <- cache_path(config$cache)
  if (identical(globalenv(), config$envir)){
    save(
      list = ls(config$envir, all.names = TRUE),
      envir = config$envir,
      file = globalenv_file(this_cache_path)
    )
  }
  config$cache$set(key = "envir", value = config$envir, namespace = "config")
  config$outdated_targets <- outdated(
    config = config,
    make_imports = !config$skip_imports
  )
  increment_attempt_flag(targets = config$outdated_targets, config = config)
  invisible(config)
}

finish_distributed <- function(config){
  dir <- cache_path(config$cache)
  file <- globalenv_file(dir)
  unlink(file, force = TRUE)
}

build_distributed <- function(target, cache_path){
  config <- recover_drake_config(cache_path = cache_path)
  do_prework(config = config, verbose_packages = FALSE)
  prune_envir(targets = target, config = config)
  console_many_targets(
    targets = target,
    pattern = "check",
    color = "check",
    config = config
  )
  meta <- meta(target = target, config = config, store = TRUE)
  config$old_hash <- self_hash(target = target, config = config)
  do_build <- should_build_target(
    target = target,
    meta = meta,
    config = config
  )
  if (do_build){
    drake_build(
      target = target,
      meta = meta,
      config = config
    )
  }
  invisible()
}

recover_drake_config <- function(cache_path){
  cache <- this_cache(cache_path, verbose = FALSE)
  config <- read_drake_config(cache = cache)
  if (identical(globalenv(), config$envir)){
    dir <- cache_path(cache = cache)
    file <- globalenv_file(dir)
    load(file = file, envir = config$envir)
  }
  config
}
