#' @title Function \code{build_graph}
#' @description Make a graph of the dependency structure of your workplan.
#' @details This function returns an igraph object representing how
#' the targets in your workplan depend on each other.
#' (\code{help(package = "igraph")}). To plot the graph, call
#' to \code{\link{plot.igraph}()} on your graph, or just use
#' \code{\link{plot_graph}()} from the start.
#' @seealso \code{\link{plot_graph}}
#' @export
#' @return An igraph object representing
#' the workflow plan dependency network.
#'
#' @param plan workflow plan data frame, same as for function
#' \code{\link{make}()}.
#'
#' @param targets names of targets to build, same as for function
#' \code{\link{make}()}.
#'
#' @param envir environment to import from, same as for function
#' \code{\link{make}()}.
#'
#' @param verbose logical, whether to output messages to the console.
#'
#' @param jobs number of jobs to accelerate the construction
#' of the dependency graph. A light \code{mclapply}-based
#' parallelism is used if your operating system is not Windows.
#'
#' @examples
#' \dontrun{
#' load_basic_example() # Load the canonical example for drake.
#' # Make the igraph network connecting all the targets and imports.
#' g <- build_graph(my_plan)
#' class(g) # "igraph"
#' }
build_graph <- function(
  plan = workplan(),
  targets = drake::possible_targets(plan),
  envir = parent.frame(),
  verbose = TRUE,
  jobs = 1
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
  console_many_targets(
    targets = names(imports),
    pattern = "connect",
    type = "import",
    config = list(verbose = verbose)
  )
  import_deps <- lightly_parallelize(
    imports, import_dependencies, jobs = jobs)
  console_many_targets(
    targets = plan$target,
    pattern = "connect",
    type = "target",
    config = list(verbose = verbose)
  )
  command_deps <- lightly_parallelize(
    plan$command, command_dependencies, jobs = jobs)
  names(command_deps) <- plan$target
  dependency_list <- c(command_deps, import_deps)
  keys <- names(dependency_list)
  vertices <- c(keys, unlist(dependency_list)) %>% unique
  from <- unlist(dependency_list) %>%
    unname()
  times <- vapply(
    X = dependency_list,
    FUN = length,
    FUN.VALUE = integer(1),
    USE.NAMES = TRUE
  )
  to <- rep(keys, times = times)
  edges <- rbind(from, to) %>%
    as.character()
  graph <- make_empty_graph() +
    vertex(vertices) +
    edge(edges)
  ignore <- lightly_parallelize(
    targets,
    function(vertex){
      subcomponent(graph = graph, v = vertex, mode = "in")$name
    },
    jobs = jobs
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
#' @return A character vector with the names of reproducibly-tracked targets.
#' @param plan workflow plan data frame, same as for function
#' \code{\link{make}()}.
#' @param targets names of targets to build, same as for function
#' \code{\link{make}()}.
#' @param envir environment to import from, same as for function
#' \code{\link{make}()}.
#' @param jobs number of jobs to accelerate the construction
#' of the dependency graph. A light \code{mclapply}-based
#' parallelism is used if your operating system is not Windows.
#' @param verbose logical, whether to print
#' progress messages to the console.
#' @examples
#' \dontrun{
#' load_basic_example() # Load the canonical example for drake.
#' # List all the targets/imports that are reproducibly tracked.
#' tracked(my_plan)
#' }
tracked <- function(
  plan = workplan(),
  targets = drake::possible_targets(plan),
  envir = parent.frame(),
  jobs = 1,
  verbose = TRUE
){
  force(envir)
  graph <- build_graph(
    plan = plan, targets = targets, envir = envir,
    jobs = jobs, verbose = verbose
  )
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
    message(
      "Unloading targets from environment:\n",
      multiline_message(common), sep = ""
    )
  }
  remove(list = common, envir = envir)
}

trim_graph <- function(config){
  if (!length(config$from)){
    return(config)
  }
  config <- sanitize_from(config)
  if (!length(config$order)){
    config$order <- length(V(config$graph))
  }
  config$graph <- igraph::make_ego_graph(
    graph = config$graph,
    order = config$order,
    nodes = config$from,
    mode = config$mode
  ) %>%
    do.call(what = igraph::union)
  config
}
