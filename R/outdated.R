#' @title Function outdated
#' @description Check which targets are out of date and need to be rebuilt.
#' IMPORTANT: you must be in the root directory of your project.
#' @export
#' @seealso \code{\link{plan}}, \code{\link{make}}, \code{\link{plot_graph}}
#' @examples
#' \dontrun{
#' load_basic_example()
#' outdated(my_plan)
#' make(my_plan)
#' outdated(my_plan)
#' }
#' @param plan same as for \code{\link{make}}
#' @param targets same as for \code{\link{make}}
#' @param envir same as for \code{\link{make}}
#' @param verbose same as for \code{\link{make}}
#' @param parallelism same as for \code{\link{make}}
#' @param jobs same as for \code{\link{make}}
#' @param packages same as for \code{\link{make}}
#' @param prework same as for \code{\link{make}}
#' @param config option internal runtime parameter list of 
#' \code{\link{make}(...)},
#' produced with \code{\link{get_config}()}.
#' Computing this
#' in advance could save time if you plan multiple calls to 
#' \code{outdated()}.
outdated =  function(plan, targets = drake::possible_targets(plan),
  envir = parent.frame(), verbose = TRUE, 
  parallelism = drake::default_parallelism(), jobs = 1,
  packages = (.packages()), prework = character(0), config = NULL){
  force(envir)
  if(is.null(config))
    config = get_config(plan = plan, targets = targets, envir = envir,
      verbose = verbose, parallelism = parallelism, jobs = jobs,
      packages = packages, prework = prework)

  all_targets = intersect(V(config$graph)$name, config$plan$target)
  rebuild = Filter(x = all_targets, f = function(target){
    hashes = hashes(target, config)
    !target_current(target = target, hashes = hashes, config = config)
  })
  if(!length(rebuild)) return(invisible(character(0)))
  else lapply(rebuild, function(vertex)
    subcomponent(config$graph, v = vertex, mode = "out")$name) %>%
    unlist %>% unique %>% sort
}
