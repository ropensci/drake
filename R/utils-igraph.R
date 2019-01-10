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

downstream_nodes <- function(graph, from) {
  g <- nbhd_graph(
    graph = graph,
    vertices = from,
    mode = "out",
    order = igraph::gorder(graph)
  )
  igraph::V(g)$name
}

leaf_nodes <- function(graph) {
  is_leaf <- igraph::degree(graph, mode = "in") == 0
  V(graph)[is_leaf]$name
}

nbhd_graph <- function(graph, vertices, mode, order = igraph::gorder(graph)) {
  if (!length(vertices)) {
    return(igraph::make_empty_graph())
  }
  vertices <- intersect(vertices, igraph::V(graph)$name)
  out <- igraph::make_ego_graph(
    graph = graph,
    order = order,
    nodes = vertices,
    mode = mode
  )
  do.call(what = igraph::union, args = out)
}

subset_graph <- function(graph, subset) {
  if (!length(subset)) {
    return(igraph::make_empty_graph())
  }
  subset <- intersect(subset, igraph::V(graph)$name)
  igraph::induced_subgraph(graph = graph, vids = subset)
}
