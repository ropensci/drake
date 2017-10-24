run_future_lapply <- function(config){
  prepare_distributed(config = config)
  run_parallel(config = config, worker = worker_future_lapply)
}

worker_future_lapply <- function(targets, hash_list, config){
  future::future_lapply(
    x = targets,
    FUN = build_distributed,
    cache_path = config$cache$driver$path
  )
}

warn_future_jobs <- function(config){
  if (config$parallelism == "future_lapply" && config$jobs > 1)
  warning(
    "The 'jobs' argument to make() is totally ignored ",
    "for 'future_lapply' parallelism. The default number of ",
    "workers is the number of targets in a given parallelizable stage. ",
    "To avoid using too many workers, use something like ",
    "'options(mc.cores = your_max_jobs)', for example. ",
    "See ?future::future.options for a list of environment ",
    "variables that restrict the number of workers ",
    "for different 'future_lapply' backends.",
    call. = FALSE
  )
}

#' @title Function \code{backend}
#' @export
#' @seealso \code{\link{workflow}}, \code{\link{make}},
#' @description Select the specific \code{future}-style
#' parallel backend for \code{make(..., parallelism = "future_lapply")}.
#' For more information, please read the documentation,
#' tutorials, and vignettes of the R packages
#' \code{future} and \code{future.batchtools}.
#' @details The \code{backend()} function is exactly
#' the same as \code{future::plan()}.
#' We provide it only because \code{workflow()} conflicts
#' with \code{future::plan()}.
#' @param ... arguments to \code{future::plan()}.
#' @examples
#' \dontrun{
#' load_basic_example()
#' library(future) # Use workflow() instead of plan()
#' backend(multicore) # Parallel backend for future_lapply()
#' make(my_plan, parallelism = "future_lapply")
#' clean() # Erase the targets to start from scratch.
#' backend(multisession) # Use separate background R sessions.
#' make(my_plan, parallelism = "future_lapply")
#' clean()
#' library(future.batchtools) # More heavy-duty future-style parallel backends
#' # https://github.com/HenrikBengtsson/future.batchtools#choosing-batchtools-backend # nolint
#' backend(batchtools_local)
#' make(my_plan, parallelism = "future_lapply")
#' clean()
#' }
backend <- function(...){
  future::plan(...)
}
