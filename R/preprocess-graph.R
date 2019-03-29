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
  edges <- lightly_parallelize(
    X = layout,
    FUN = cdg_node_to_edges,
    jobs = config$jobs,
    config = config
  )
  edges <- data.frame(
    from = unlist(lapply(edges, `[[`, "from")),
    to = unlist(lapply(edges, `[[`, "to")),
    stringsAsFactors = FALSE
  )
  cdg_edges_thru_file_out(edges, config)
}

cdg_node_to_edges <- function(node, config) {
  console_msg(connect_msg, node$target, tier = 2L, config = config)
  file_out <- node$deps_build$file_out
  node$deps_build$file_out <- NULL
  inputs <- clean_nested_char_list(
    c(node$deps_build, node$deps_condition, node$deps_change)
  )
  out <- NULL
  if (length(inputs)) {
    out <- list(
      from = inputs,
      to = rep(node$target, length(inputs))
    )
  }
  if (length(file_out)) {
    out <- list(
      from = c(out$from, rep(node$target, length(file_out))),
      to = c(out$to, file_out)
    )
  }
  if (is.null(out)) {
    out <- list(from = node$target %||% character(0))
    out$to <- out$from
  }
  out
}

cdg_edges_thru_file_out <- function(edges, config) {
  console_msg(connect_msg, "output files", tier = 2L, config = config)
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
  console_msg(file_out_msg, vertex, tier = 2L, config = config)
  from <- unique(edges$from[edges$to == vertex])
  to <- unique(edges$to[edges$from == vertex])
  expand.grid(from = from, to = to, stringsAsFactors = FALSE)
}

cdg_finalize_graph <- function(edges, targets, config) {
  console_msg(finalize_msg, "graph edges", tier = 2L, config = config)
  file_out <- edges$to[edges$from %in% targets & is_encoded_path(edges$to)]
  to <- union(targets, file_out)
  console_msg(create_msg, "igraph", tier = 2L, config = config)
  graph <- igraph::graph_from_data_frame(edges)
  console_msg(process_msg, "neighborhoods", tier = 2L, config = config)
  graph <- nbhd_graph(
    graph = graph,
    vertices = to,
    mode = "in",
    order = igraph::gorder(graph)
  )
  console_msg(process_msg, "igraph attributes", tier = 2L, config = config)
  graph <- igraph::set_vertex_attr(graph, "imported", value = TRUE)
  index <- c(config$plan$target, file_out)
  index <- intersect(index, igraph::V(graph)$name)
  graph <- igraph::set_vertex_attr(
    graph = graph,
    name = "imported",
    index = index,
    value = FALSE
  )
  console_msg(finalize_msg, "igraph", tier = 2L, config = config)
  igraph::simplify(
    graph,
    remove.loops = TRUE,
    remove.multiple = TRUE,
    edge.attr.comb = "min"
  )
}
