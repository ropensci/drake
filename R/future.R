run_future_lapply <- function(config){
  prepare_distributed(config = config)
  run_parallel(config = config, worker = worker_future_lapply)
  finish_distributed(config = config)
}

worker_future_lapply <- function(targets, meta_list, config){
  targets <- intersect(targets, config$plan$target)
  if (!length(targets)){
    return()
  }
  future::future_lapply(
    x = targets,
    FUN = build_distributed,
    cache_path = config$cache$driver$path
  )
}

#' @title Function \code{backend}
#' @export
#' @seealso \code{\link{workplan}}, \code{\link{make}},
#' @description Select the specific \code{future}-style
#' parallel backend for \code{make(..., parallelism = "future_lapply")}.
#' For more information, please read the documentation,
#' tutorials, and vignettes of the R packages
#' \code{future} and \code{future.batchtools}.
#' @details The \code{backend()} function is exactly
#' the same as \code{future::plan()}.
#' We provide it only because \code{workplan()} conflicts
#' with \code{future::plan()}.
#' @param ... arguments to \code{future::plan()}.
#' @examples
#' \dontrun{
#' load_basic_example()
#' library(future) # Use workplan() instead of plan()
#' backend(multicore) # Same as future::plan(multicore)
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
