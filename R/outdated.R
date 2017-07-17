#' @title Function outdated
#' @description Check which targets are out of date and need to be rebuilt.
#' IMPORTANT: you must be in the root directory of your project.
#' @export
#' @seealso \code{\link{plan}}, \code{\link{make}}, \code{\link{plot_graph}}
#' @param plan Workflow plan data frame: that is, a data frame with a "target"
#' column and a "command" column. See \code{\link{plan}()}.
#' @param targets Character vector of targets. status() shows the status
#' of all the dependencies of the targets listed.
#' @param envir environment with your imported objects. Defaults to your workspace.
#' @param verbose logical, whether be verbose. If \code{TRUE}, objects are listed
#' as they are imported. (Objects need to be imported to check the status.)
#' @examples
#' \dontrun{
#' load_basic_example()
#' outdated(my_plan)
#' make(my_plan)
#' outdated(my_plan)
#' }
outdated = function(plan, targets = drake::possible_targets(plan),
  envir = parent.frame(), verbose = FALSE){
  force(envir)
  config = config(plan = plan, targets = targets, envir = envir, 
                  verbose = verbose, parallelism = parallelism_choices()[1],
                  jobs = 1, packages = character(0), prework = character(0), 
                  prepend = character(0), command = character(0), 
                  args = character(0))
  check_config(config)
  
  imports = setdiff(config$order, config$plan$target)
  for(import in imports){ # Strict order needed. Might parallelize later.
    config = inventory(config)
    hash_list = hash_list(targets = import, config = config)
    build(target = import, hash_list = hash_list, config = config)
  }
  
  all_targets = intersect(config$order, config$plan$target)
  rebuild = Filter(x = all_targets, f = function(target){
    hashes = hashes(target, config)
    !target_current(target = target, hashes = hashes, config = config)
  })
  if(!length(rebuild)) return(invisible(character(0)))
  else lapply(rebuild, function(vertex)
    subcomponent(config$graph, v = vertex, mode = "out")$name) %>%
    unlist %>% unique %>% sort
}
