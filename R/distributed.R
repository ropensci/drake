#' @title Do the preparatory work
#'   for [make()] with a distributed computing
#'   backend (see the `parallelism` argument of [make()]).
#' @description For internal use only. Exported to flesh out some
#' of the more advanced examples.
#' @export
#' @keywords internal
#' @param config Internal configuration list from
#'   [drake_config()].
#' @return Nothing.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Get the code with drake_example("basic").
#' config <- drake_config(my_plan)
#' prepare_distributed(config = config)
#' })
#' }
prepare_distributed <- function(config){
  if (!file.exists(config$cache_path)){
    dir.create(config$cache_path)
  }
  writeLines(
    text = as.character(config$fetch_cache),
    con = file.path(config$cache_path, fetch_cache_file)
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
  build_and_store(
    target = target,
    meta = meta_list[[target]],
    config = config
  )
  invisible()
}

recover_drake_config <- function(cache_path){
  fetch_cache <- tryCatch(
    readLines(con = file.path(cache_path, fetch_cache_file)) %>%
      paste0(collapse = "\n"),
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

fetch_cache_file <- "fetch_cache.R"
