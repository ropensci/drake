#' @title Function outdated
#' @description Check which targets are out of date and need to be rebuilt.
#' IMPORTANT: you must be in the root directory of your project.
#' @details \code{outdated()} is sensitive to the alternative triggers
#' described at
#' \url{https://github.com/wlandau-lilly/drake/blob/master/vignettes/debug.Rmd#test-with-triggers}. # nolint
#' For example, even if \code{outdated(...)} shows everything up to date,
#' \code{outdated(..., trigger = "always")} will show
#' all targets out of date.
#' @export
#' @seealso \code{\link{missed}}, \code{\link{workplan}},
#' \code{\link{make}}, \code{\link{vis_drake_graph}}
#' @return Character vector of the names of outdated targets.
#' @param config option internal runtime parameter list of
#' \code{\link{make}(...)},
#' produced with \code{\link{config}()}.
#' @param make_imports logical, whether to make the imports first.
#' Set to \code{FALSE} to save some time and risk obselete output.
#' \code{config$envir} is ignored.
#' Overrides all the other arguments if not \code{NULL}.
#' For example,
#' \code{plan} is replaced with \code{config$plan}.
#' @examples
#' \dontrun{
#' config <- load_basic_example() # Load the canonical example of drake.
#' outdated(config = config) # Which targets are out of date?
#' config <- make(my_plan) # Run the projects, build the targets.
#' # Now, everything should be up to date (no targets listed).
#' outdated(config = config)
#' # outdated() is sensitive to triggers.
#' # See the "debug" vignette for more on triggers.
#' config$trigger <- "always"
#' outdated(config = config)
#' }
outdated <-  function(config, make_imports = TRUE){
  if (make_imports){
    make_imports(config = config)
  }
  all_targets <- intersect(V(config$graph)$name, config$plan$target)
  meta_list <- meta_list(targets = all_targets, config = config)
  is_outdated <- lightly_parallelize(
    all_targets,
    function(target){
      meta <- meta_list[[target]]
      should_build_target(target = target, meta = meta, config = config)
    },
    jobs = config$jobs
  ) %>%
    unlist
  outdated_targets <- all_targets[is_outdated]
  if (!length(outdated_targets)){
    return(character(0))
  } else{
    downstream_nodes(
      from = outdated_targets, graph = config$graph, jobs = config$jobs)
  }
}

#' @title Function \code{missed}
#' @description Report any import objects required by your workplan
#' plan but missing from your workspace.
#' IMPORTANT: you must be in the root directory of your project.
#' @export
#' @seealso \code{\link{outdated}}
#' @return Character vector of names of missing objects and files.
#'
#' @param config internal runtime parameter list of
#' \code{\link{make}(...)},
#' produced by both \code{\link{drake_config}()} and \code{\link{make}()}.
#'
#' @examples
#' \dontrun{
#' config <- load_basic_example() # Load the canonical example.
#' missed(config) # All the imported files and objects should be present.
#' rm(reg1) # Remove an import dependency from you workspace.
#' missed(config) # Should report that reg1 is missing.
#' }
missed <- function(config = NULL){
  graph <- config$graph
  imports <- setdiff(V(graph)$name, plan$target)
  missing <- Filter(
    x = imports,
    f = function(x){
      missing_import(x, envir = envir)
    }
  )
  if (!length(missing)){
    return(character(0))
  }
  return(missing)
}
