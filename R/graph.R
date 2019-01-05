create_drake_graph <- function(
  plan,
  layout,
  targets,
  cache,
  jobs,
  console_log_file,
  verbose
) {
  config <- list(
    plan = plan,
    jobs = jobs,
    verbose = verbose,
    console_log_file = console_log_file
  )
  edges <- memo_expr(
    cdg_create_edges(config, layout),
    cache,
    config,
    layout
  )
  memo_expr(
    cdg_finalize_graph(edges, targets, config),
    cache,
    edges,
    targets
  )
}

cdg_create_edges <- function(config, layout) {
  console_preprocess(text = "construct graph edges", config = config)
  edges <- lightly_parallelize(
    X = layout,
    FUN = cdg_node_to_edges,
    jobs = config$jobs
  )
  edges <- do.call(rbind, edges)
  cdg_edges_thru_file_out(edges)
}

cdg_node_to_edges <- function(node) {
  file_out <- node$deps_build$file_out
  node$deps_build$file_out <- NULL
  inputs <- clean_dependency_list(
    c(node$deps_build, node$deps_condition, node$deps_change)
  )
  out <- NULL
  if (length(inputs)) {
    out <- weak_tibble(from = inputs, to = node$target)
  }
  if (length(file_out)) {
    out <- rbind(
      out,
      weak_tibble(from = node$target, to = file_out)
    )
  }
  if (is.null(out) && length(node$target)) {
    out <- weak_tibble(from = node$target, to = node$target)
  }
  out
}

cdg_edges_thru_file_out <- function(edges) {
  file_out <- edges$to[is_encoded_path(edges$to)]
  file_out_edges <- lapply(
    X = file_out,
    FUN = cdg_transitive_edges,
    edges = edges
  )
  file_out_edges <- do.call(what = rbind, args = file_out_edges)
  edges <- rbind(edges, file_out_edges)
  edges[!duplicated(edges), ]
}

cdg_transitive_edges <- function(vertex, edges) {
  from <- unique(edges$from[edges$to == vertex])
  to <- unique(edges$to[edges$from == vertex])
  expand.grid(from = from, to = to, stringsAsFactors = FALSE)
}

cdg_finalize_graph <- function(edges, targets, config) {
  console_preprocess(text = "construct graph", config = config)
  file_out <- edges$to[edges$from %in% targets & is_encoded_path(edges$to)]
  to <- union(targets, file_out)
  graph <- igraph::graph_from_data_frame(edges)
  graph <- prune_drake_graph(graph, to = to, jobs = config$jobs)
  graph <- igraph::set_vertex_attr(graph, "imported", value = TRUE)
  index <- c(config$plan$target, file_out)
  index <- intersect(index, igraph::V(graph)$name)
  graph <- igraph::set_vertex_attr(
    graph = graph,
    name = "imported",
    index = index,
    value = FALSE
  )
  igraph::simplify(
    graph,
    remove.loops = TRUE,
    remove.multiple = TRUE,
    edge.attr.comb = "min"
  )
}
