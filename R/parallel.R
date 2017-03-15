#' @title Function \code{parallelism_choices}
#' @description List the types of supported parallel computing.
#' @export
#' @return Character vector listing the types of parallel
#' computing supported.
#' @details Run \code{make(..., parallelism = x, jobs = n)} for any of
#' the following values of \code{x} to distribute targets over parallel
#' units of execution.
#' \describe{
#'  \item{"mclapply"}{uses multiple processes in a single R session.
#'  This is single-node, (potentially) multicore computing.
#'  Does not work on Windows for \code{jobs > 1}
#'  because \code{\link{mclapply}()} is based on forking.}
#'  \item{"Makefile"}{uses multiple R sessions
#'  by creating and running a Makefile.
#'  The Makefile is NOT standalone.
#'  DO NOT run outside of \code{\link{make}()} or \code{\link{make}()}.
#'  Windows users will need to download and intall Rtools.
#'  As explained in the vignettes, you can use the \code{prepend}
#'  to \code{\link{make}()} or \code{\link{make}()} to distribute
#'  targets over multiple nodes of a supercomputer. Use this
#'  approach for true distributed computing.}}
#'  \item{"parLapply"}{launches multiple processes in a single R session
#'  using \code{parallel::\link{parLapply}()}.
#'  This is single-node, (potentially) multicore computing.
#'  It requires more overhead than the \code{"mclapply"} option,
#'  but it works on Windows.}
parallelism_choices = function(){
  c("parLapply", "mclapply", "Makefile")
}

run_parallel = function(config, worker){
  graph_remaining_targets = config$graph
  while(length(V(graph_remaining_targets)))
    graph_remaining_targets = parallel_stage(
      graph_remaining_targets = graph_remaining_targets,
      worker = worker, config = config)
}

parallel_stage = function(graph_remaining_targets, worker, 
  config){
  candidates = next_targets(graph_remaining_targets)
  hash_list = hash_list(targets = candidates, config = config)
  build_these = Filter(candidates, f = function(target)
    should_build(target = target, hash_list = hash_list,
      config = config))
  hash_list = hash_list[build_these]
  if(length(build_these)){
    prune_envir(targets = build_these, config = config)
    values = worker(targets = build_these,
      hash_list = hash_list, config = config)
    assign_to_envir(target = build_these, value = values,
      config = config)
  }
  delete_vertices(graph_remaining_targets, v = candidates)
}

run_lapply = function(config){
  run_parallel(config = config, worker = worker_lapply)
}

worker_lapply = function(targets, hash_list, config){
  lapply(X = targets, FUN = build,
    hash_list = hash_list, config = config)
}
