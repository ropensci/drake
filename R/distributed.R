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
#' load_mtcars_example() # Get the code with drake_example("mtcars").
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
  # Always save globalenv() because config$envir could inherit from it
  # and so drake might look for stuff there.
  save(
    list = setdiff(ls(globalenv(), all.names = TRUE), config$plan$target),
    envir = globalenv(),
    file = globalenv_file(config$cache_path)
  )
  for (item in c("envir", "schedule")){
    config$cache$set(key = item, value = config[[item]], namespace = "config")
  }
  invisible()
}

finish_distributed <- function(config){
  dir <- cache_path(config$cache)
  file <- globalenv_file(dir)
  unlink(file, force = TRUE)
}

build_distributed <- function(target, cache_path, check = TRUE){
  config <- recover_drake_config(cache_path = cache_path)
  eval(parse(text = "base::require(drake, quietly = TRUE)"))
  do_prework(config = config, verbose_packages = FALSE)
  if (check){
    build_check_store(target = target, config = config)
  } else {
    prune_envir(targets = target, config = config)
    build_and_store(target = target, config = config)
  }
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
  # Always load globalenv() because config$envir could inherit from it
  # and so drake might look for stuff there.
  dir <- cache_path(cache = cache)
  file <- globalenv_file(dir)
  load(file = file, envir = globalenv())
  config
}

fetch_cache_file <- "fetch_cache.R"
