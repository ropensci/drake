first_outdated <- function(config) {
  graph <- targets_graph(config)
  out <- character(0)
  old_leaves <- NULL
  while (TRUE){
    new_leaves <- leaf_nodes(graph) %>%
      setdiff(y = out)
    do_build <- lightly_parallelize(
      X = new_leaves,
      FUN = should_build_target,
      jobs = config$jobs,
      config = config
    ) %>%
      unlist
    out <- c(out, new_leaves[do_build])
    if (all(do_build)){
      break
    } else {
      graph <- delete_vertices(graph, v = new_leaves[!do_build])
    }
    old_leaves <- new_leaves
  }
  out
}

#' @title List the targets that are out of date.
#' @description Outdated targets will be rebuilt in the next
#'   [make()].
#' @details `outdated()` is sensitive to the alternative triggers
#' described at
#' <https://github.com/ropensci/drake/blob/master/vignettes/debug.Rmd#test-with-triggers>. # nolint
#' For example, even if `outdated(...)` shows everything up to date,
#' `outdated(..., trigger = "always")` will show
#' all targets out of date.
#' You must use a fresh `config` argument with an up-to-date
#' `config$targets` element that was never modified by hand.
#' If needed, rerun [drake_config()] early and often.
#' See the details in the help file for [drake_config()].
#' @export
#' @seealso [missed()], [drake_plan()],
#'   [make()], [vis_drake_graph()]
#' @return Character vector of the names of outdated targets.
#' @param config option internal runtime parameter list of
#'   \code{\link{make}(...)},
#'   produced with [drake_config()].
#'   You must use a fresh `config` argument with an up-to-date
#'   `config$targets` element that was never modified by hand.
#'   If needed, rerun [drake_config()] early and often.
#'   See the details in the help file for [drake_config()].
#' @param make_imports logical, whether to make the imports first.
#'   Set to `FALSE` to save some time and risk obsolete output.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' # Recopute the config list early and often to have the
#' # most current information. Do not modify the config list by hand.
#' config <- drake_config(my_plan)
#' outdated(config = config) # Which targets are out of date?
#' config <- make(my_plan) # Run the projects, build the targets.
#' # Now, everything should be up to date (no targets listed).
#' outdated(config = config)
#' # outdated() is sensitive to triggers.
#' # See the "debug" vignette for more on triggers.
#' config$trigger <- "always"
#' outdated(config = config)
#' })
#' }
outdated <-  function(
  config = drake::read_drake_config(),
  make_imports = TRUE
){
  do_prework(config = config, verbose_packages = config$verbose)
  if (make_imports){
    make_imports(config = config)
  }
  first_targets <- first_outdated(config = config)
  later_targets <- downstream_nodes(
    from = first_targets,
    graph = config$graph,
    jobs = config$jobs
  )
  c(first_targets, later_targets) %>%
    as.character %>%
    unique %>%
    sort
}

#' @title Report any import objects required by your drake_plan
#'   plan but missing from your workspace.
#' @description Checks your workspace/environment and
#' file system.
#' @export
#' @seealso [outdated()]
#' @return Character vector of names of missing objects and files.
#'
#' @param config internal runtime parameter list of
#'   \code{\link{make}(...)},
#'   produced by both [drake_config()] and [make()].
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' config <- load_mtcars_example() # Get the code with drake_example("mtcars").
#' missed(config) # All the imported files and objects should be present.
#' rm(reg1) # Remove an import dependency from you workspace.
#' missed(config) # Should report that reg1 is missing.
#' })
#' }
missed <- function(config = drake::read_drake_config()){
  imports <- setdiff(V(config$graph)$name, config$plan$target)
  is_missing <- lightly_parallelize(
    X = imports,
    FUN = function(x){
      missing_import(x, envir = config$envir)
    },
    jobs = config$jobs
  ) %>%
    as.logical
  if (!any(is_missing)){
    return(character(0))
  }
  imports[is_missing]
}
