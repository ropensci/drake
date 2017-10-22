run_future_lapply <- function(config){
  # Any generated globals and loaded packages need to get on the PSOCK cluster.
  do_prework(config = config, verbose_packages = TRUE)
  # Need clusterExport()s like in run_parLapply()
  run_parallel(config = config, worker = worker_future_lapply)
}

worker_future_lapply <- function(targets, hash_list, config){
  # If the config$envir is globalenv() (which is most of the time),
  # We need to call prune_envir() on each of the PSOCK workers.
  prune_envir(targets = targets, config = config)
  values <- future::future_lapply(
    x = targets, FUN = build,
    hash_list = hash_list, config = config
  )
  # If the config$envir is globalenv() (which is most of the time),
  # We need to call assign_to_envir() on each of the PSOCK workers.
  assign_to_envir(target = targets, value = values, config = config)
}

#' @title Function \code{backend}
#' @export
#' @seealso \code{\link{plan}}, \code{\link{make}},
#' @description Select the specific \code{future}-style
#' parallel backend for \code{make(..., parallelism = "future_lapply")}.
#' For more information, please read the documentation,
#' tutorials, and vignettes of the R packages
#' \code{future} and \code{future.batchtools}.
#' @details The \code{backend()} function is exactly
#' the same as \code{future::plan()}.
#' We provide it only because \code{drake::plan()} conflicts
#' with \code{future::plan()}.
#' @param ... arguments to \code{future::plan()}.
#' @examples
#' \dontrun{
#' load_basic_example()
#' library(future) # Use workflow() or drake::plan() instead of plan()
#' backend(multicore) # Parallel backend for future_lapply()
#' make(my_plan, parallelism = "future_lapply")
#' clean() # Erase the targets to start from scratch.
#' backend(multisession) # Use separate background R sessions.
#' make(my_plan, parallelism = "future_lapply")
#' clean()
#' library(future.batchtools) # More heavy-duty future-style parallel backends
#' backend(batchtools_local) # https://github.com/HenrikBengtsson/future.batchtools#choosing-batchtools-backend # nolint
#' make(my_plan, parallelism = "future_lapply")
#' clean()
#' }
backend <- function(...){
  future::plan(...)
}
