#' @title Prune the dependency network of your project.
#' @export
#' @seealso [drake_config()], [make()]
#' @description `igraph` objects are used
#' internally to represent the dependency network of your workflow.
#' See `drake_config(my_plan)$graph` from the mtcars example.
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
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' # Build the igraph object representing the workflow dependency network.
#' graph <- drake_config(my_plan)$graph
#' # The default plotting is not the greatest,
#' # but you will get the idea.
#' # plot(graph) # nolint
#' # Prune the graph: that is, remove the nodes downstream
#' # from 'small' and 'large'
#' pruned <- prune_drake_graph(graph = graph, to = c("small", "large"))
#' # plot(pruned) # nolint
#' })
#' }
prune_drake_graph <- function(
  graph, to = igraph::V(graph)$name, jobs = 1
) {
  if (!inherits(graph, "igraph")) {
    stop(
      "supplied graph must be an igraph object",
      call. = FALSE
    )
  }
  unlisted <- setdiff(to, V(graph)$name)
  if (length(unlisted)) {
    warning(
      "supplied targets not in the dependency graph:\n",
      multiline_message(unlisted),
      call. = FALSE
    )
    to <- setdiff(to, unlisted)
  }
  if (!length(to)) {
    warning(
      "cannot prune graph: no valid destination vertices supplied",
      call. = FALSE
    )
    return(graph)
  }
  ignore <- lightly_parallelize(
    X = to,
    FUN = function(vertex) {
      drake_subcomponent(graph = graph, v = vertex, mode = "in")$name
    },
    jobs = jobs
  )
  ignore <- unlist(ignore)
  ignore <- unique(ignore)
  ignore <- setdiff(igraph::V(graph)$name, ignore)
  delete_vertices(graph = graph, v = ignore)
}

deps_graph <- function(targets, graph, reverse = FALSE) {
  if (!length(targets)) {
    return(character(0))
  }
  opt <- igraph::igraph_opt("return.vs.es")
  on.exit(igraph::igraph_options(return.vs.es = opt))
  igraph::igraph_options(return.vs.es = FALSE)
  index <- adjacent_vertices(
    graph = graph,
    v = targets,
    mode = ifelse(reverse, "out", "in")
  )
  index <- unlist(index)
  index <- unique(index)
  igraph::V(graph)$name[index + 1]
}

get_neighborhood <- function(graph, from, mode, order) {
  if (!length(order)) {
    order <- length(V(graph))
  }
  if (length(from)) {
    from <- sanitize_nodes(nodes = from, choices = V(graph)$name)
    egos <- igraph::make_ego_graph(
      graph = graph,
      order = order,
      nodes = from,
      mode = mode
    )
    subset <- lapply(
      X = egos,
      FUN = function(graph) {
      igraph::V(graph)$name
    })
    subset <- clean_dependency_list(subset)
    graph <- subset_graph(graph = graph, subset = subset)
  }
  graph
}

downstream_nodes <- function(from, graph, jobs) {
  if (!length(from)) {
    return(character(0))
  }
  out <- lightly_parallelize(
    X = from,
    FUN = function(node) {
      drake_subcomponent(graph, v = node, mode = "out")$name
    },
    jobs = jobs
  )
  clean_dependency_list(out)
}

leaf_nodes <- function(graph) {
  is_leaf <- igraph::degree(graph, mode = "in") == 0
  V(graph)[is_leaf]$name
}

subset_graph <- function(graph, subset) {
  if (!length(subset)) {
    return(igraph::make_empty_graph())
  }
  subset <- intersect(subset, V(graph)$name)
  igraph::induced_subgraph(graph = graph, vids = subset)
}

drake_subcomponent <- function(...) {
  opt <- igraph::igraph_opt("return.vs.es")
  on.exit(igraph::igraph_options(return.vs.es = opt))
  igraph::igraph_options(return.vs.es = TRUE)
  igraph::subcomponent(...)
}
