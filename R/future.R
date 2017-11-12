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

#' @title Function \code{future_backend}
#' @export
#' @seealso \code{\link{workplan}}, \code{\link{make}},
#' @description Select the specific \code{future}-style
#' parallel backend for \code{make(..., parallelism = "future_lapply")}.
#' For more information, please read the documentation,
#' tutorials, and vignettes of the R packages
#' \code{future} and \code{future.batchtools}.
#' @details The \code{future_backend()} function is exactly
#' the same as \code{future::plan()}.
#' We provide it only because \code{workplan()} conflicts
#' with \code{future::plan()}.
#' @return The same return value as \code{future::plan()}.
#' @param ... arguments to \code{future::plan()}.
#' @examples
#' \dontrun{
#' load_basic_example() # Load the canonical drake example.
#' # For future-based backends, you will need the "future" package,
#' # and for high-performance computing, maybe future.batchtools.
#' library(future)
#' future_backend(multicore) # Same as future::plan(multicore)
#' # Run the project with future-powered multicore parallelism.
#' # The number of jobs is automatic: however many would benefit.
#' # Set options(mc.cores = 2) to cap the number of jobs at 2.
#' make(my_plan, parallelism = "future_lapply")
#' clean() # Erase the targets to start from scratch.
#' future_backend(multisession) # Use separate background R sessions.
#' # This next make() is similar to "parLapply" parallelism.
#' make(my_plan, parallelism = "future_lapply")
#' clean() # Remove targets/imports from the cache, start from scratch.
#' # More heavy-duty future-style parallel backends
#' library(future.batchtools)
#' # https://github.com/HenrikBengtsson/future.batchtools#choosing-batchtools-backend # nolint
#' future_backend(batchtools_local) # Test out future.batchtools locally.
#' # Should proceed prety much like the multisession backend.
#' make(my_plan, parallelism = "future_lapply")
#' }
future_backend <- function(...){
  future::plan(...)
}
