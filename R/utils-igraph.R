deps_graph <- function(targets, graph, reverse = FALSE) {
  if (!length(targets)) {
    return(character(0))
  }
  drake_adjacent_vertices(
    graph = graph,
    v = targets,
    mode = ifelse(reverse, "out", "in")
  )
}

drake_adjacent_vertices <- function(graph, v, mode) {
  opt <- igraph::igraph_opt("return.vs.es")
  on.exit(igraph::igraph_options(return.vs.es = opt))
  igraph::igraph_options(return.vs.es = FALSE)
  index <- adjacent_vertices(graph = graph, v = v, mode = mode)
  index <- unlist(index, use.names = FALSE)
  index <- unique(index)
  igraph::V(graph)$name[index + 1]
}

downstream_nodes <- function(graph, from) {
  nbhd_vertices(
    graph = graph,
    vertices = from,
    mode = "out",
    order = igraph::gorder(graph)
  )
}

leaf_nodes <- function(graph) {
  is_leaf <- igraph::degree(graph, mode = "in") == 0
  V(graph)[is_leaf]$name
}

nbhd_graph <- function(graph, vertices, mode, order) {
  vertices <- nbhd_vertices(
    graph = graph,
    vertices = vertices,
    mode = mode,
    order = order
  )
  igraph::induced_subgraph(graph = graph, vids = vertices)
}

nbhd_vertices <- function(graph, vertices, mode, order) {
  vertices <- intersect(vertices, igraph::V(graph)$name)
  from <- vertices
  level <- 0
  while (length(from) && level < order) {
    from <- drake_adjacent_vertices(graph, v = from, mode = mode)
    from <- setdiff(from, vertices)
    vertices <- c(vertices, from)
    level <- level + 1
  }
  vertices
}

subset_graph <- function(graph, subset) {
  if (!length(subset)) {
    return(igraph::make_empty_graph())
  }
  subset <- intersect(subset, igraph::V(graph)$name)
  igraph::induced_subgraph(graph = graph, vids = subset)
}

trim_vs_keep_cons <- function(graph, keep) {
  graph <- igraph::set_vertex_attr(
    graph,
    name = "delete",
    value = TRUE
  )
  graph <- igraph::set_vertex_attr(
    graph,
    name = "delete",
    index = intersect(igraph::V(graph)$name, keep),
    value = FALSE
  )
  delete <- igraph::V(graph)$name[igraph::V(graph)$delete]
  for (v in delete) {
    graph <- delete_v_keep_con(graph, v)
  }
  graph
}

delete_v_keep_con <- function(graph, v) {
  from <- drake_adjacent_vertices(graph, v, "in")
  to <- drake_adjacent_vertices(graph, v, "out")
  graph <- igraph::delete_vertices(graph, v = v)
  if (!length(from) || !length(to)) {
    return(graph)
  }
  nbhd_edges <- expand.grid(from = from, to = to, stringsAsFactors = FALSE)
  nbhd_graph <- igraph::graph_from_data_frame(nbhd_edges)
  igraph::union(graph, nbhd_graph)
}
