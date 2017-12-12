#' @title Internal function \code{store_drake_config}
#' @description Do the preparatory work
#' for \code{\link{make}()} with a distributed computing
#' backend (see the \code{parallelism} argument).
#' @export
#' @keywords internal
#' @param config Internal configuration list from
#' \code{\link{drake_config}()}.
#' @return Nothing.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example()
#' config <- drake_config(my_plan)
#' prepare_distributed(config = config)
#' })
#' }
prepare_distributed <- function(config){
  if (!file.exists(config$cache_path)){
    dir.create(config$cache_path)
  }
  saveRDS(
    config$fetch_cache,
    file = file.path(config$cache_path, "fetch_cache.rds")
  )
  if (identical(globalenv(), config$envir)){
    save(
      list = ls(config$envir, all.names = TRUE),
      envir = config$envir,
      file = globalenv_file(config$cache_path)
    )
  }
  config$cache$set(key = "envir", value = config$envir, namespace = "config")
  invisible()
}

finish_distributed <- function(config){
  dir <- cache_path(config$cache)
  file <- globalenv_file(dir)
  unlink(file, force = TRUE)
}

build_distributed <- function(target, meta_list, cache_path){
  config <- recover_drake_config(cache_path = cache_path)
  do_prework(config = config, verbose_packages = FALSE)
  prune_envir(targets = target, config = config)
  if (is.null(meta_list)){
    meta_list <- meta_list(targets = target, config = config)
    do_build <- should_build_target(
      target = target,
      meta = meta_list[[target]],
      config = config
    )
    if (!do_build){
      return(invisible())
    }
  }
  drake_build(
    target = target,
    meta = meta_list[[target]],
    config = config
  )
  invisible()
}

recover_drake_config <- function(cache_path){
  fetch_cache <- tryCatch(
    readRDS(file.path(cache_path, "fetch_cache.rds")),
    error = error_null
  )
  cache <- this_cache(cache_path, verbose = FALSE, fetch_cache = fetch_cache)
  config <- read_drake_config(cache = cache)
  if (identical(globalenv(), config$envir)){
    dir <- cache_path(cache = cache)
    file <- globalenv_file(dir)
    load(file = file, envir = config$envir)
  }
  config
}
