create_drake_graph <- function(
  plan,
  spec,
  targets,
  cache,
  jobs,
  logger
) {
  config <- list(plan = plan, jobs = jobs, logger = logger, cache = cache)
  edges <- memo_expr(
    cdg_create_edges(config, spec),
    cache,
    plan,
    spec
  )
  memo_expr(
    cdg_finalize_graph(edges, targets, config),
    cache,
    edges,
    targets
  )
}

cdg_create_edges <- function(config, spec) {
  config$logger$minor("create graph edges")
  edges <- lightly_parallelize(
    X = spec,
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
  file_out <- node$deps_build$file_out
  node$deps_build$file_out <- NULL
  inputs <- clean_nested_char_list(
    c(
      node$deps_build,
      node$deps_dynamic,
      node$deps_condition,
      node$deps_change
    )
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
    out <- list(from = as.character(node$target))
    out$to <- out$from
  }
  out
}

cdg_edges_thru_file_out <- function(edges, config) {
  config$logger$minor("connect output files")
  file_out <- edges$to[is_encoded_path(edges$to)]
  file_out_edges <- lapply(
    X = file_out,
    FUN = cdg_transitive_edges,
    edges = edges,
    config = config
  )
  file_out_edges <- do.call(what = rbind, args = file_out_edges)
  edges <- rbind(edges, file_out_edges)
  edges[!duplicated(edges), ]
}

cdg_transitive_edges <- function(vertex, edges, config) {
  config$logger$minor("file_out", target = config$cache$display_keys(vertex))
  from <- unique(edges$from[edges$to == vertex])
  to <- unique(edges$to[edges$from == vertex])
  expand.grid(from = from, to = to, stringsAsFactors = FALSE)
}

cdg_finalize_graph <- function(edges, targets, config) {
  config$logger$minor("finalize graph edges")
  file_out <- edges$to[edges$from %in% targets & is_encoded_path(edges$to)]
  to <- union(targets, file_out)
  config$logger$minor("create igraph")
  graph <- igraph::graph_from_data_frame(edges)
  config$logger$minor("trim neighborhoods")
  graph <- nbhd_graph(
    graph = graph,
    vertices = to,
    mode = "in",
    order = igraph::gorder(graph)
  )
  config$logger$minor("add igraph attributes")
  graph <- igraph::set_vertex_attr(graph, "imported", value = TRUE)
  index <- c(config$plan$target, file_out)
  index <- intersect(index, igraph::V(graph)$name)
  graph <- igraph::set_vertex_attr(
    graph = graph,
    name = "imported",
    index = index,
    value = FALSE
  )
  config$logger$minor("finalize igraph")
  igraph::simplify(
    graph,
    remove.loops = TRUE,
    remove.multiple = TRUE,
    edge.attr.comb = "min"
  )
}
