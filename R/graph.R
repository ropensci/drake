#' @title Function \code{build_graph}
#' @description Make a graph of the dependency structure of your workflow.
#' @details This function returns an igraph object representing how
#' the targets in your workflow depend on each other.
#' (\code{help(package = "igraph")}). To plot the graph, call
#' to \code{\link{plot.igraph}()} on your graph, or just use
#' \code{\link{plot_graph}()} from the start.
#' @seealso \code{\link{plot_graph}}
#' @export
#' @param plan workflow plan data frame, same as for function
#' \code{\link{make}()}.
#' @param targets names of targets to bulid, same as for function
#' \code{\link{make}()}.
#' @param envir environment to import from, same as for function
#' \code{\link{make}()}.
#' @param verbose logical, whether to output messages to the console.
#' @param cache optional drake cache, as from \code{\link{new_cache}()}.
#' The cache is used to check for back-compatibility with projects
#' created with drake 4.2.0 or earlier. In these legacy projects,
#' packages will not be tracked as dependencies. In projects created with
#' a version of drake later than 4.2.0, packages are formally tracked as
#' imports.
#' @examples
#' \dontrun{
#' load_basic_example()
#' g <- build_graph(my_plan)
#' class(g)
#' }
build_graph <- function(
  plan,
  targets = drake::possible_targets(plan),
  envir = parent.frame(),
  verbose = TRUE,
  cache = NULL
  ){
  force(envir)
  plan <- sanitize_plan(plan)
  targets <- sanitize_targets(plan, targets)
  imports <- as.list(envir)
  assert_unique_names(
    imports = names(imports),
    targets = plan$target,
    envir = envir,
    verbose = verbose
  )
  true_import_names <- setdiff(names(imports), targets)
  imports <- imports[true_import_names]
  import_deps <- lapply(imports, import_dependencies)
  command_deps <- lapply(plan$command, command_dependencies)
  names(command_deps) <- plan$target
  dependency_list <- c(command_deps, import_deps)
  if (account_for_packages(cache)) {
    dependency_list <- append_package_dependencies(
      dependency_list, config = config)
  }
  keys <- names(dependency_list)
  vertices <- c(keys, unlist(dependency_list)) %>% unique
  from <- unlist(dependency_list) %>%
    unname()
  to <- rep(keys, times = sapply(dependency_list, length))
  edges <- rbind(from, to) %>%
    as.character()
  graph <- make_empty_graph() +
    vertex(vertices) +
    edge(edges)
  ignore <- lapply(
    targets,
    function(vertex){
      subcomponent(graph = graph, v = vertex, mode = "in")$name
    }
    ) %>%
  unlist() %>%
  unique() %>%
  setdiff(x = vertices)
  graph <- delete_vertices(graph, v = ignore)
  if (!is_dag(graph)){
    stop("Workflow is circular (chicken and egg dilemma).")
  }
  return(graph)
}

#' @title Function \code{tracked}
#' @description Print out which objects, functions, files, targets, etc.
#' are reproducibly tracked.
#' @export
#' @param plan workflow plan data frame, same as for function
#' \code{\link{make}()}.
#' @param targets names of targets to bulid, same as for function
#' \code{\link{make}()}.
#' @param envir environment to import from, same as for function
#' \code{\link{make}()}.
#' @param cache optional drake cache, as from \code{\link{new_cache}()}.
#' The cache is used to check for back-compatibility with projects
#' created with drake 4.2.0 or earlier. In these legacy projects,
#' packages will not be tracked as dependencies. In projects created with
#' a version of drake later than 4.2.0, packages are formally tracked as
#' imports.
#' @examples
#' \dontrun{
#' load_basic_example()
#' tracked(my_plan)
#' }
tracked <- function(
  plan,
  targets = drake::possible_targets(plan),
  envir = parent.frame(),
  cache = NULL
  ){
  force(envir)
  graph <- build_graph(plan = plan, targets = targets, envir = envir,
    cache = cache)
  V(graph)$name
}

assert_unique_names <- function(imports, targets, envir, verbose){
  if (anyDuplicated(targets)){
    duplicated <- which(table(targets) > 1) %>%
      names()
    stop(
      "Duplicate targets in workflow plan:\n",
      multiline_message(duplicated)
      )
  }
  common <- intersect(imports, targets)
  if (verbose & length(common)){
    cat(
      "Unloading targets from environment:\n",
      multiline_message(common), "\n", sep = ""
      )
  }
  remove(list = common, envir = envir)
}

append_package_dependencies <- function(dependency_list, config) {
  x <- c(names(dependency_list), unlist(dependency_list)) %>%
    clean_dependency_list
  sapply(x, package_of_name) %>%
    Filter(f = length) %>%
    c(dependency_list)
}

account_for_packages <- function(cache){
  if (is.null(cache)){
    cache <- recover_cache()
  }
  version <- cache$get("initial_drake_version", namespace = "session")
  compareVersion(version, "4.2.0") > 0
}
