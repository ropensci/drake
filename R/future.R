run_future_lapply <- function(config){
  do_prework(config = config, verbose_packages = TRUE) # needs the 'future' equivalent clusterCall()
  run_parallel(config = config, worker = worker_future_lapply)
}

worker_future_lapply <- function(targets, hash_list, config){
  prune_envir_future(targets = targets, config = config)
  values <- future::future_lapply(
    X = targets, FUN = build,
    hash_list = hash_list, config = config
  )
  assign_to_envir_future(target = targets, value = values, config = config)
}

prune_envir_parLapply <- function(targets = targets, config = config) { # nolint
  prune_envir(targets = targets, config = config)
  if (identical(config$envir, globalenv()))
    clusterCall(cl = config$cluster, fun = prune_envir, targets = targets, # needs the 'future' equivalent clusterCall()
                config = config)
}

assign_to_envir_parLapply <- # nolint
  function(target, value, config) {
    assign_to_envir(target = target, value = value, config = config)
    if (identical(config$envir, globalenv()))
      clusterCall(cl = config$cluster, fun = assign_to_envir, # needs the 'future' equivalent clusterCall()
                  target = target, value = value, config = config)
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
#' load_basic_examples()
#' library(future) # Use workflow() or drake::plan() instead of plan()
#' backend(multicore) # Parallel backend for future_lapply()
#' make(my_plan, parallelism = "future_lapply", jobs = 4)
#' clean() # Erase the targets to start from scratch.
#' backend(multisession) # Use separate background R sessions.
#' make(my_plan, parallelism = "future_lapply", jobs = 4)
#' clean()
#' library(future.batchtools) # More heavy-duty future-style parallel backends
#' backend(batchtools_local) # https://github.com/HenrikBengtsson/future.batchtools#choosing-batchtools-backend # nolint
#' make(my_plan, parallelism = "future_lapply", jobs = 4)
#' clean()
#' }
backend <- function(...){
  future::plan(...)
}
