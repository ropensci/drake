#' @title Create the `igraph` dependency network of your project.
#' @description This function returns an igraph object representing how
#' the targets in your workflow plan data frame
#' depend on each other.
#' (`help(package = "igraph")`). To plot the graph, call
#' to [plot.igraph()] on your graph, or just use
#' [vis_drake_graph()] from the start.
#' @seealso [vis_drake_graph()]
#' @export
#' @return An igraph object representing
#'   the workflow plan dependency network.
#'
#' @inheritParams drake_config
#'
#' @param plan workflow plan data frame, same as for function
#'   [make()].
#'
#' @param targets names of targets to build, same as for function
#'   [make()].
#'
#' @param envir environment to import from, same as for function
#'   [make()].
#'
#' @param jobs number of jobs to accelerate the construction
#'   of the dependency graph. A light `mclapply()`-based
#'   parallelism is used if your operating system is not Windows.
#'
#' @param sanitize_plan logical, whether to sanitize the workflow plan first.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Get the code with drake_example("basic").
#' # Make the igraph network connecting all the targets and imports.
#' g <- build_drake_graph(my_plan)
#' class(g) # "igraph"
#' })
#' }
build_drake_graph <- function(
  plan = read_drake_plan(),
  targets = drake::possible_targets(plan),
  envir = parent.frame(),
  verbose = drake::default_verbose(),
  jobs = 1,
  sanitize_plan = TRUE
){
  force(envir)
  if (sanitize_plan){
    plan <- sanitize_plan(plan)
  }
  targets <- sanitize_targets(plan, targets)
  imports <- as.list(envir)
  assert_unique_names(
    imports = names(imports),
    targets = plan$target,
    envir = envir,
    verbose = verbose
  )
  import_names <- setdiff(names(imports), targets)
  imports <- imports[import_names]
  console_many_targets(
    targets = names(imports),
    pattern = "connect",
    type = "import",
    config = list(verbose = verbose)
  )
  imports_edges <- lightly_parallelize(
    X = seq_along(imports),
    FUN = function(i){
      imports_edges(name = import_names[[i]], value = imports[[i]])
    },
    jobs = jobs
  )
  console_many_targets(
    targets = plan$target,
    pattern = "connect",
    type = "target",
    config = list(verbose = verbose)
  )
  commands_edges <- lightly_parallelize(
    X = seq_len(nrow(plan)),
    FUN = function(i){
      commands_edges(target = plan$target[i], command = plan$command[i])
    },
    jobs = jobs
  )
  c(imports_edges, commands_edges) %>%
    do.call(what = rbind) %>%
    igraph::graph_from_data_frame() %>%
    prune_drake_graph(to = targets, jobs = jobs) %>%
    igraph::simplify(remove.multiple = TRUE, remove.loops = TRUE)
}

commands_edges <- function(target, command){
  deps <- command_dependencies(command)
  code_deps_to_edges(target = target, deps = deps)
}

imports_edges <- function(name, value){
  deps <- import_dependencies(value)
  code_deps_to_edges(target = name, deps = deps)
}

code_deps_to_edges <- function(target, deps){
  inputs <- clean_dependency_list(deps[setdiff(names(deps), "file_out")])
  edges <- NULL
  if (length(inputs)){
    data.frame(from = inputs, to = target, stringsAsFactors = FALSE)
  } else {
    # Loops will be removed.
    data.frame(from = target, to = target, stringsAsFactors = FALSE)
  }
}

#' @title Prune the dependency network of your project.
#' @export
#' @seealso [build_drake_graph()], [config()],
#'   [make()]
#' @description `igraph` objects are used
#' internally to represent the dependency network of your workflow.
#' See \code{\link{config}(my_plan)$graph} from the basic example.
#' @details For a supplied graph, take the subgraph of all combined
#' incoming paths to the vertices in `to`. In other words,
#' remove the vertices after `to` from the graph.
#' @return A pruned igraph object representing the dependency network
#'   of the workflow.
#' @param graph An igraph object to be pruned.
#' @param to Character vector, names of the vertices that draw
#'   the line for pruning. The pruning process removes all vertices
#'   downstream of `to`.
#' @param jobs Number of jobs for light parallelism (on non-Windows machines).
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Get the code with drake_example("basic").
#' # Build the igraph object representing the workflow dependency network.
#' # You could also use drake_config(my_plan)$graph
#' graph <- build_drake_graph(my_plan)
#' # The default plotting is not the greatest,
#' # but you will get the idea.
#' plot(graph)
#' # Prune the graph: that is, remove the nodes downstream
#' # from 'small' and 'large'
#' pruned <- prune_drake_graph(graph = graph, to = c("small", "large"))
#' plot(pruned)
#' })
#' }
prune_drake_graph <- function(
  graph, to = igraph::V(graph)$name, jobs = 1
){
  if (!inherits(graph, "igraph")){
    stop(
      "supplied graph must be an igraph object",
      call. = FALSE
    )
  }
  unlisted <- setdiff(to, V(graph)$name)
  if (length(unlisted)){
    warning(
      "supplied targets not in the workflow graph:\n",
      multiline_message(unlisted),
      call. = FALSE
    )
    to <- setdiff(to, unlisted)
  }
  if (!length(to)){
    warning(
      "cannot prune graph: no valid destination vertices supplied",
      call. = FALSE
    )
    return(graph)
  }
  ignore <- lightly_parallelize(
    X = to,
    FUN = function(vertex){
      subcomponent(graph = graph, v = vertex, mode = "in")$name
    },
    jobs = jobs
  ) %>%
    unlist() %>%
    unique() %>%
    setdiff(x = igraph::V(graph)$name)
  delete_vertices(graph = graph, v = ignore)
}

assert_unique_names <- function(imports, targets, envir, verbose){
  if (anyDuplicated(targets)){
    duplicated <- which(table(targets) > 1) %>%
      names()
    stop(
      "Duplicate targets in workflow plan:\n",
      multiline_message(duplicated),
      call. = FALSE
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

get_neighborhood <- function(graph, from, mode, order){
  if (!length(order)){
    order <- length(V(graph))
  }
  if (length(from)){
    from <- sanitize_nodes(nodes = from, choices = V(graph)$name)
    graph <- igraph::make_ego_graph(
      graph = graph,
      order = order,
      nodes = from,
      mode = mode
    ) %>%
      do.call(what = igraph::union)
  }
  graph
}

downstream_nodes <- function(from, graph, jobs){
  if (!length(from)){
    return(character(0))
  }
  lightly_parallelize(
    X = from,
    FUN = function(node){
      subcomponent(graph, v = node, mode = "out")$name
    },
    jobs = jobs
  ) %>%
    unlist() %>%
    unique() %>%
    sort()
}

leaf_nodes <- function(graph){
  is_leaf <- igraph::degree(graph, mode = "in") == 0
  V(graph)[is_leaf]$name
}

exclude_imports_if <- function(config){
  if (!length(config$skip_imports)){
    config$skip_imports <- FALSE
  }
  if (!config$skip_imports){
    return(config)
  }
  delete_these <- setdiff(
    V(config$execution_graph)$name,
    config$plan$target
  )
  config$execution_graph <- delete_vertices(
    graph = config$execution_graph,
    v = delete_these
  )
  config
}

subset_graph <- function(graph, subset){
  if (!length(subset)){
    return(graph)
  }
  subset <- intersect(subset, V(graph)$name)
  igraph::induced_subgraph(graph = graph, vids = subset)
}
