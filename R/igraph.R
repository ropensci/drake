all_targets <- function(config) {
  out <- V(config$graph)$name[!V(config$graph)$imported]
  out[!is_encoded_path(out)]
}

all_imports <- function(config) {
  V(config$graph)$name[V(config$graph)$imported]
}

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

downstream_nodes <- function(graph, from) {
  nbhd_vertices(
    graph = graph,
    vertices = from,
    mode = "out",
    order = igraph::gorder(graph)
  )
}

upstream_nodes <- function(graph, from) {
  nbhd_vertices(
    graph = graph,
    vertices = from,
    mode = "in",
    order = igraph::gorder(graph)
  )
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

drake_adjacent_vertices <- function(graph, v, mode) {
  opt <- igraph::igraph_opt("return.vs.es")
  on.exit(igraph::igraph_options(return.vs.es = opt))
  igraph::igraph_options(return.vs.es = FALSE)
  index <- adjacent_vertices(graph = graph, v = v, mode = mode)
  index <- unlist(index, use.names = FALSE)
  index <- unique(index)
  igraph::V(graph)$name[index + get_igraph_offset()]
}

get_igraph_offset <- function() {
  if (!is.null(igraph_offset$offset)) {
    return(igraph_offset$offset)
  }
  opt <- igraph::igraph_opt("return.vs.es")
  on.exit(igraph::igraph_options(return.vs.es = opt))
  igraph::igraph_options(return.vs.es = FALSE)
  test_graph <- igraph::make_graph(edges = c("a", "b"))
  adjacent <- igraph::adjacent_vertices(
    graph = test_graph,
    v = "a",
    mode = "out"
  )
  adjacent <- as.integer(adjacent)
  offset <- 2L - adjacent
  igraph_offset$offset <- offset
  offset
}

igraph_offset <- new.env(parent = emptyenv())

subset_graph <- function(graph, subset) {
  if (!length(subset)) {
    return(igraph::make_empty_graph())
  }
  subset <- intersect(subset, igraph::V(graph)$name)
  igraph::induced_subgraph(graph = graph, vids = subset)
}

leaf_nodes <- function(graph) {
  is_leaf <- igraph::degree(graph, mode = "in") == 0
  V(graph)[is_leaf]$name
}
